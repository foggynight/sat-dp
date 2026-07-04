-- -*- eval: (setq haskell-process-args-cabal-repl '("sat-dpll")) -*-
-- ... or: M-x haskell-session-change-target, sat-dpll

module Main where

import Data.Char (toLower)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Debug.Trace (trace)
import Options.Applicative
import System.IO.Unsafe (unsafePerformIO)

import CNF
import DIMACS
import Util
import VarOrder

-- Pure Literal Elimination ----------------------------------------------------

data Polarity = Pol_None | Pol_Conflict | Pol_Positive | Pol_Negative
  deriving (Eq, Show)

varPolClause :: Clause -> Variable -> Polarity
varPolClause clause var
  | has_pos && has_neg = Pol_Conflict
  | has_pos            = Pol_Positive
  | has_neg            = Pol_Negative
  | otherwise          = Pol_None
  where has_pos = clauseHasLit var clause
        has_neg = clauseHasLit (-var) clause

mergePol :: Polarity -> Polarity -> Polarity
mergePol Pol_Conflict _ = Pol_Conflict
mergePol _ Pol_Conflict = Pol_Conflict
mergePol p1 Pol_None = p1
mergePol Pol_None p2 = p2
mergePol p1 p2 = if p1 == p2 then p1 else Pol_Conflict

varPolClauses :: [Clause] -> Variable -> Polarity
varPolClauses [] _ = Pol_None
varPolClauses (c:cs) var  =
  let pol = varPolClause c var
  in if pol == Pol_Conflict
     then pol
     else mergePol pol $ varPolClauses cs var

-- Pure Literal Elimination: Find literals which are pure (only positive or only
-- negative polarity), and eliminate clauses which contain a pure literal.
pureLitElim :: Variable -> [Clause] -> ([Literal], [Clause])
pureLitElim max_var clauses =
  let pure_lits = map var_pol_lit $ filter has_pol var_pols
      impure_clauses = filter (not . clauseHasLitAny pure_lits) clauses
  in (pure_lits, impure_clauses)
  where var_pols = [(var, varPolClauses clauses var) | var <- [1..max_var]]
        has_pol = \(_, pol) -> pol == Pol_Positive || pol == Pol_Negative
        var_pol_lit = \(var, pol) -> if pol == Pol_Positive then var else (-var)

pureLitElimRecM :: Variable -> [Clause] -> IO ([Literal], [Clause])
pureLitElimRecM max_var clauses = do
  let (pure_lits, impure_clauses) = pureLitElim max_var clauses
  if pure_lits == []
  then pure ([], clauses)
  else do
    putStrLn $ show pure_lits ++ " -> " ++ show impure_clauses
    (rec_pls, rec_ics) <- pureLitElimRecM max_var impure_clauses
    pure (pure_lits ++ rec_pls, rec_ics)

-- Unit Clause Propagation -----------------------------------------------------

-- DPLL ------------------------------------------------------------------------

{-# NOINLINE dpll_count #-}
dpll_count :: IORef Int
dpll_count = unsafePerformIO (newIORef 0)

dpll :: [Variable] -> [Clause] -> Int -> IO (Maybe [Literal])
dpll [] [] _ = do
  modifyIORef' dpll_count (+ 1)
  putStr " => SOLUTION (empty formula)"
  pure $ Just []
dpll vars clauses depth = do
  let print_prefix = '\n' : (concat $ replicate depth "    ")
  if elem [] clauses
  then do putStr " => BACKTRACK (empty clause)"
          modifyIORef' dpll_count (+ 1)
          pure Nothing
  else case vars of
         [] -> error $ "error: no variable to split but clauses remain: "
                    ++ show clauses
         (v:vs) -> dpll' v vs print_prefix
  where
    dpll' v vs print_prefix = do
      let clauses_pos = (conditionClauses v clauses)
      putStr $ concat [print_prefix, "+", show v, " -> ", show clauses_pos]
      next_pos <- dpll vs clauses_pos (depth + 1)

      if next_pos /= Nothing
      then pure $ Just v `consM` next_pos
      else do
        let clauses_neg = (conditionClauses (-v) clauses)
        putStr $ concat [print_prefix, show (-v), " -> ", show clauses_neg]
        next_neg <- dpll vs clauses_neg (depth + 1)

        if next_neg /= Nothing
        then pure $ Just (-v) `consM` next_neg
        else pure $ Nothing

solve_dpll :: CNF -> [Variable] -> IO ()
solve_dpll (CNF n_vars _ clauses) var_order = do
  putStrLn $ "Initial CNF: " ++ show clauses
  newline

  putStrLn $ "Variable Order: " ++ show var_order
  newline

  putStrLn "Pure literal elimination..."
  (pure_lits, impure_clauses) <- pureLitElimRecM n_vars clauses
  let impure_vars = filter (\v -> not $ elem v $ map abs pure_lits) var_order
  newline

  putStrLn $ "Assigned Literals:   " ++ show pure_lits
  putStrLn $ "Remaining Variables: " ++ show impure_vars
  putStrLn $ "Remaining Clauses:   " ++ show impure_clauses
  newline

  putStr $ "Searching for satisfying assignment..."
  result <- dpll impure_vars impure_clauses 0
  dpll_count' <- readIORef dpll_count
  putStrLn $ "\nBranches checked: " ++ show dpll_count'
  newline

  case result of
    Nothing   -> putStrLn "UNSAT"
    Just lits -> putStrLn $ "SAT: " ++ show (pure_lits ++ lits)

-- Main ------------------------------------------------------------------------

data Config = Config
  { conf_var_order :: String
  , conf_cnf_file :: String
  } deriving (Show)

configParser :: Parser Config
configParser = Config
  <$> strOption
  ( short 'R'
    <> long "order"
    <> value "numeric"
    <> showDefault
    <> help "Variable ordering strategy." )
  <*> strArgument
  ( metavar "CNF_FILE"
    <> value "--"
    <> showDefault
    <> help "Filename of input DIMACS CNF file." )

main :: IO ()
main = do
  config <- execParser opts

  let vo_func = case map toLower (conf_var_order config) of
                  "numeric" -> varOrderNumeric
                  "fewest"  -> varOrderFewestClauses
                  "jw"      -> varOrderJeroslowWang
                  _         -> trace "error: invalid variable order argument"
                                     varOrderNumeric

  dimacs_cnf <- case conf_cnf_file config of
                  "--" -> getContents
                  file -> readFile file

  let maybe_cnf = parseDIMACS dimacs_cnf
  case maybe_cnf of
    Nothing  -> putStrLn "error: invalid CNF"
    Just cnf -> do
      let var_order = vo_func cnf
      solve_dpll cnf var_order

  where opts = info (configParser <**> helper)
               ( fullDesc
                 <> header "sat-dpll - SAT solver using the DPLL algorithm." )
