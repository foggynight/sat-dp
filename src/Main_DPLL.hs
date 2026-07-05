-- -*- eval: (setq haskell-process-args-cabal-repl '("sat-dpll")) -*-
-- ... or: M-x haskell-session-change-target, sat-dpll

module Main where

import Control.Monad (when)
import Data.Char (toLower)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.List ((\\))
import Data.Maybe (mapMaybe)
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

-- Merge two polarities, ignores Pol_None, propagates Pol_Conflict, returns
-- given polarity when both are equal.
mergePol :: Polarity -> Polarity -> Polarity
mergePol Pol_Conflict _ = Pol_Conflict
mergePol _ Pol_Conflict = Pol_Conflict
mergePol p1 Pol_None = p1
mergePol Pol_None p2 = p2
mergePol p1 p2 = if p1 == p2 then p1 else Pol_Conflict

-- Determine the polarity of a variable in a clause.
varPolClause :: Clause -> Variable -> Polarity
varPolClause clause var
  | has_pos && has_neg = Pol_Conflict
  | has_pos            = Pol_Positive
  | has_neg            = Pol_Negative
  | otherwise          = Pol_None
  where has_pos = clauseHasLit var clause
        has_neg = clauseHasLit (-var) clause

-- Determine the polarity of a variable in a list of clauses.
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

-- Perform pure literal elimination recursively until no more pure literals are
-- found. IO to print log messages.
pureLitElimRecM :: Variable -> [Clause] -> IO ([Literal], [Clause])
pureLitElimRecM max_var clauses = do
  let (pure_lits, impure_clauses) = pureLitElim max_var clauses
  if pure_lits == []
  then pure ([], clauses)
  else do
    putStrLn $ concat
      ["[PLE] Lits: ", show pure_lits, " -> CNF: ", show impure_clauses]
    (rec_pls, rec_ics) <- pureLitElimRecM max_var impure_clauses
    pure (pure_lits ++ rec_pls, rec_ics)

-- Unit Clause Propagation -----------------------------------------------------

-- Determine if clause contains unit a literal.
clauseUnitLit :: Clause -> Maybe Literal
clauseUnitLit [] = Nothing
clauseUnitLit [lit] = Just lit
clauseUnitLit (lit:lits)
  | null $ filter (/= lit) lits = Just lit
  | otherwise                   = Nothing

-- Search for unit literals (single unassigned literal in a clause), and
-- condition formula over each literal.
unitClauseProp :: String -> [Clause] -> IO ([Literal], [Clause])
unitClauseProp prefix clauses = do
  let unit_lits = mapMaybe clauseUnitLit clauses
      new_clauses = mapMaybe (conditionClauseLits unit_lits) clauses
  when (not $ null unit_lits) $
    putStr $ concat
      [prefix, "Unit Lits: ", show unit_lits, " -> CNF: ", show new_clauses]
  pure (unit_lits, new_clauses)

-- DPLL ------------------------------------------------------------------------

{-# NOINLINE dpll_count #-}
dpll_count :: IORef Int
dpll_count = unsafePerformIO (newIORef 0)

dpll_print :: String -> Variable -> [Clause] -> IO ()
dpll_print prefix var clauses =
  putStr $ concat [prefix, "Assign Lit: ", show var, " -> CNF: ", show clauses]

dpll :: [Variable] -> [Clause] -> Int -> IO (Maybe [Literal])
dpll [] [] _ = do
  modifyIORef' dpll_count (+ 1)
  putStr " => SOLUTION (empty formula)"
  pure $ Just []
dpll vars clauses depth = do
  if elem [] clauses
  then do putStr " => BACKTRACK (empty clause)"
          modifyIORef' dpll_count (+ 1)
          pure Nothing
  else case vars of
         [] -> error $ "error: no variable to split but clauses remain: "
                    ++ show clauses
         (v:vs) -> do (lits, cs) <- unitClauseProp print_prefix clauses
                      if null lits
                      then splitOnVar v vs clauses
                      else do maybe_lits <- dpll ((v:vs) \\ lits) cs (depth + 1)
                              pure $ Just lits `appendM` maybe_lits
  where
    print_prefix = "\n[Depth " ++ show depth ++ "] "
    splitOnVar :: Variable -> [Variable] -> [Clause] -> IO (Maybe [Literal])
    splitOnVar v vs cs = do
      -- Check positive v branch.
      let cs_pos = (conditionClauses v cs)
      dpll_print print_prefix v cs_pos
      next_pos <- dpll vs cs_pos (depth + 1)

      if next_pos /= Nothing
      then pure $ Just v `consM` next_pos
      else do
        -- Check negative v branch.
        let cs_neg = (conditionClauses (-v) cs)
        dpll_print print_prefix (-v) cs_neg
        next_neg <- dpll vs cs_neg (depth + 1)

        if next_neg /= Nothing
        then pure $ Just (-v) `consM` next_neg
        else pure $ Nothing

solve_dpll :: CNF -> [Variable] -> IO ()
solve_dpll (CNF n_vars _ clauses) var_order = do
  putStrLn $ "Initial CNF: " ++ show clauses
  newline

  putStrLn $ "Variable Order: " ++ show var_order
  newline

  putStrLn "Performing pure literal elimination..."
  (pure_lits, impure_clauses) <- pureLitElimRecM n_vars clauses
  let impure_vars = filter (\v -> not $ elem v $ map abs pure_lits) var_order
  putStrLn $ "Pure Literals Found: " ++ (show $ length pure_lits)
  newline

  putStrLn $ "Assigned Literals:   " ++ show pure_lits
  putStrLn $ "Remaining Variables: " ++ show impure_vars
  putStrLn $ "Current CNF:         " ++ show impure_clauses
  newline

  result <-
    if impure_clauses == []
    then do pure $ Just []
    else do
      putStr $ "Searching for satisfying assignment..."
      result <- dpll impure_vars impure_clauses 0
      dpll_count' <- readIORef dpll_count
      putStrLn $ "\nTerminal nodes checked: " ++ show dpll_count'
      newline
      pure result

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
