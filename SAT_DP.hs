-- SAT_DP.hs - DP Algorithm using Bucket Elimination in Haskell
-- Copyright (C) 2026 Robert Coffey

import Data.Int (Int64)
import Data.Maybe (isNothing, mapMaybe)
import Text.Read (readMaybe)

type Literal = Int64
type Clause = [Literal]
newtype CNF = CNF [Clause]
  deriving (Show)

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
  "c" : xs -> Nothing
  xs -> let literals = init xs
            end = last xs
        in if (end /= "0")
           then Nothing
           else Just (map (\x -> read x :: Literal) literals)

-- TODO: Comments and blank lines.
parseDIMACS :: String -> Maybe CNF
parseDIMACS file_str =
  let header : clause_lines = lines file_str
      (n_vars, n_clauses) = case parseDIMACSHeader header of
             Just (x, y) -> (x, y)
             Nothing     -> (0, 0)
  in if (n_vars, n_clauses) == (0, 0)
     then Nothing
     else let clauses = mapMaybe parseDIMACSClause clause_lines
          in if n_clauses /= (fromIntegral $ length clauses)
             then Nothing
             else Just $ CNF clauses

_main :: String -> IO ()
_main str = do
  let maybe_cnf = parseDIMACS str
  if isNothing maybe_cnf
    then putStrLn "error: invalid CNF"
    else let Just cnf = maybe_cnf in print cnf

main :: IO ()
main = do
  input <- getContents
  _main input
