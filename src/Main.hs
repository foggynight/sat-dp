-- SAT_DP.hs - DP Algorithm using Bucket Elimination in Haskell
-- Copyright (C) 2026 Robert Coffey

import Control.Exception (assert)
import Control.Monad.Extra (mapMaybeM)
import Data.Char (isSpace, toLower)
import Data.Int (Int64)
import Data.List (nub, partition)
import Data.Maybe (mapMaybe)
import Debug.Trace (trace)
import Text.Read (readMaybe)

-- CNF -------------------------------------------------------------------------

type Variable = Int64    -- variable (> 0)
type Literal = Variable  -- variable + sign
type Clause = [Literal]
data CNF = CNF
  { cnf_n_vars :: Int64
  , _cnf_n_clauses :: Int64
  , cnf_clauses :: [Clause]
  }

instance Show CNF where
  show cnf = show (cnf_clauses cnf)

litHasVar :: Variable -> Literal -> Bool
litHasVar var lit = (var == abs lit)

clauseHasVar :: Variable -> Clause -> Bool
clauseHasVar 0 clause = (clause == [])
clauseHasVar _ [] = False
clauseHasVar var (lit:lits) = (abs lit == var) || clauseHasVar var lits

clauseIsTautology :: Clause -> Bool
clauseIsTautology [] = False
clauseIsTautology (l:lits) = elem (-l) lits || clauseIsTautology lits

-- TODO: Rewrite such that monadic action handled outside? This should be pure.
-- Generate var-resolvent given two clauses. Removes duplicate literals and
-- returns Nothing if resolvent is tautology.
resolve :: Variable -> Clause -> Clause -> IO (Maybe Clause)
resolve var c1 c2 = do
  result <- do
    if (elem var c1 && elem (-var) c2) || (elem (-var) c1 && elem var c2)
    then do
      putStr $ (show var) ++ ": " ++ (show c1) ++ " " ++ (show c2) ++ " -> "
      let f = not . litHasVar var
      let resolvent = nub $ filter f (c1 ++ c2)
      putStr $ show resolvent
      if clauseIsTautology resolvent
      then do putStr (" (tautology)\n")
              pure Nothing
      else do newline
              pure $ Just resolvent
    else do pure Nothing
  pure result

-- Generate all var-resolvents given a list of clauses.
resolveAll :: Variable -> [Clause] -> IO [Clause]
resolveAll _ [] = pure []
resolveAll var (c:cs) = do
  var_resolvents <- mapMaybeM (resolve var c) cs
  rest_resolvents <- resolveAll var cs
  pure $ var_resolvents ++ rest_resolvents

-- DP --------------------------------------------------------------------------

-- Bucket with variable label, contains only clauses that contain that variable.
-- Bucket "0" represents the bucket of empty clauses.
data Bucket = Bucket
  { buk_var :: Variable
  , buk_clauses :: [Clause]
  }

instance Show Bucket where
  show buk = show (buk_var buk) ++ ": " ++ show (buk_clauses buk)

-- Create and fill buckets in order of vars.
-- For each clause, place clause in first bucket whose variable is in clause.
fillBuckets :: [Variable] -> [Clause] -> [Bucket]
fillBuckets vars [] = map (\v -> Bucket v []) vars
fillBuckets [] _ = assert (False) []  -- TODO: ERROR: clauses remain
fillBuckets (v:vars) clauses =
  let (clauses_with, clauses_without) = partition (clauseHasVar v) clauses
  in Bucket v clauses_with : fillBuckets vars clauses_without

-- Insert clause into buckets such that clause is placed into the first bucket
-- in list which represents a variable that is in clause.
insertClause :: Clause -> [Bucket] -> [Bucket]
insertClause clause [] =
  trace ("error: no bucket found for clause: " ++ show clause) []
insertClause clause (b:buckets) =
  if clauseHasVar (buk_var b) clause
  then Bucket (buk_var b) (clause : (buk_clauses b)) : buckets
  else b : insertClause clause buckets

insertClauses :: [Clause] -> [Bucket] -> [Bucket]
insertClauses [] buckets = buckets
insertClauses (c:clauses) buckets =
  let new_buks = (insertClause c buckets)
  in insertClauses clauses new_buks

resolveBuckets :: [Bucket] -> IO [Bucket]
resolveBuckets [] = pure []
resolveBuckets (b:buckets) = do
  rs <- resolveAll (buk_var b) (buk_clauses b)
  rest <- resolveBuckets (insertClauses rs buckets)
  pure $ b : rest

-- Parser ----------------------------------------------------------------------

-- Expects string of form "p cnf n_vars n_clauses".
-- Returns Maybe (n_vars, n_clauses).
parseDIMACSHeader :: String -> Maybe (Int64, Int64)
parseDIMACSHeader line = case words line of
  [_, _, str_n_vars, str_n_clauses] -> do
    n_vars    <- readMaybe str_n_vars
    n_clauses <- readMaybe str_n_clauses
    Just (n_vars, n_clauses)
  _ -> Nothing

-- Expects string of form "lit ... lit 0" (lit not zero, zero or more lits).
parseDIMACSClause :: String -> Maybe Clause
parseDIMACSClause line = case words line of
  [] -> Nothing
  xs -> let literals = init xs
            end = last xs
        in if (end /= "0")
           then Nothing
           else Just (map (\x -> read x :: Literal) literals)

-- TODO: Check if variable out of range or wrong number of clauses.
-- Parse string containing DIMACS CNF format header and clauses.
-- Skips comment lines and empty lines.
parseDIMACS :: String -> Maybe CNF
parseDIMACS file_str =
  let relev_lines = filter (\x -> x /= "" && toLower (head x) /= 'c')
                    (map (dropWhile isSpace) (lines file_str))
      header_line : clause_lines = relev_lines  -- TODO: Handle error cases.
      (n_vars, n_clauses) = case parseDIMACSHeader header_line of
                              Just (x, y) -> (x, y)
                              Nothing     -> (0, 0)
  in if (n_vars, n_clauses) == (0, 0)
     then Nothing
     else let clauses = mapMaybe parseDIMACSClause clause_lines
          in if n_clauses /= (fromIntegral $ length clauses)
             then Nothing
             else Just $ CNF n_vars n_clauses clauses

-- Main ------------------------------------------------------------------------

newline :: IO ()
newline = putStrLn ""

main' :: String -> IO ()
main' str = do
  let maybe_cnf = parseDIMACS str
  case maybe_cnf of
    Nothing  -> putStrLn "error: invalid CNF"
    Just cnf -> do
      putStr "Initial CNF: "
      print cnf
      newline

      let buckets = fillBuckets ([1..(cnf_n_vars cnf)] ++ [0]) (cnf_clauses cnf)
      putStrLn "Initial Buckets: "
      mapM_ print buckets
      newline

      putStrLn "Deriving resolvents..."
      res_buks <- resolveBuckets buckets
      newline

      putStrLn "Resolved Buckets: "
      mapM_ print res_buks
      newline

      let rev_buks = reverse res_buks

      if (buk_clauses (head rev_buks) == [])
      then putStrLn "SAT"
      else putStrLn "UNSAT"

main :: IO ()
main = do
  input <- getContents
  main' input
