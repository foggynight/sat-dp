module DIMACS where

import Data.Char (isSpace, toLower)
import Data.Maybe (mapMaybe)
import Debug.Trace (trace)
import Text.Read (readMaybe)

import CNF

type DIMACS_CNF = String

parseFail :: String -> Maybe a
parseFail msg = trace ("error: parse failed: " ++ msg) Nothing

-- Expects string of form "p cnf n_vars n_clauses".
-- Returns Maybe (n_vars, n_clauses).
parseDIMACSHeader :: String -> Maybe (Int, Int)
parseDIMACSHeader line = case words line of
  ["p", "cnf", str_n_vars, str_n_clauses] -> do
    n_vars    <- readMaybe str_n_vars
    n_clauses <- readMaybe str_n_clauses
    Just (n_vars, n_clauses)
  _ -> trace "error: parse failed: invalid header" Nothing

-- Expects string of form "lit ... lit 0" (lit not zero, zero or more lits).
parseDIMACSClause :: Variable -> String -> Maybe Clause
parseDIMACSClause max_var line = case words line of
  [] -> parseFail "empty clause line"  -- NOTE: Should never reach here.
  xs -> let lits = init xs; end = last xs in
          if end /= "0"
          then parseFail "missing clause terminator"
          else let parsed = mapMaybe (\x -> readMaybe x :: Maybe Literal) lits
               in if (length parsed) /= (length lits)
                  then parseFail "failed to parse literal"
                  else if any (\lit -> lit == 0 || abs lit > max_var) parsed
                       then parseFail "literal out of range"
                       else Just parsed

-- TODO: Stop printing "not enough"/"too many" clauses when real error was
-- failed to parse literal.
parseDIMACS' :: [String] -> Maybe CNF
parseDIMACS' [] = parseFail "missing header"
parseDIMACS' (header_line : clause_lines) = do
  (n_vars, n_clauses) <- parseDIMACSHeader header_line
  let clauses = mapMaybe (\c -> parseDIMACSClause n_vars c) clause_lines
      diff = n_clauses - (fromIntegral $ length clauses)
  if diff > 0
  then parseFail "not enough clauses"
  else if diff < 0
       then parseFail "too many clauses"
       else Just $ CNF n_vars n_clauses clauses

-- Parse string containing DIMACS CNF format header and clauses.
-- Skips comment lines and empty lines.
parseDIMACS :: DIMACS_CNF -> Maybe CNF
parseDIMACS file_str =
  let relev_lines = filter (\x -> case x of
                               (c:_) -> toLower c /= 'c'
                               _     -> False)
                           (map (dropWhile isSpace) (lines file_str))
  in parseDIMACS' relev_lines
