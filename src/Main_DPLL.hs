-- -*- eval: (setq haskell-process-args-cabal-repl '("sat-dpll")) -*-
-- ... or: M-x haskell-session-change-target, sat-dpll

module Main where

import Control.Monad (when)
import Data.Char (toLower)
import Debug.Trace (trace)
import Options.Applicative

import CNF
import DIMACS
import Util
import VarOrder

-- DPLL ------------------------------------------------------------------------

solve_DPLL :: CNF -> [Variable] -> IO ()
solve_DPLL cnf var_order = pure ()

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
      when (last var_order /= 0) $
        error "error: variable order doesn't end with 0"
      solve_DPLL cnf var_order

  where opts = info (configParser <**> helper)
               ( fullDesc
                 <> header ( "SatDP - SAT solver using DP algorithm implemented"
                          ++ " with bucket elimination." ) )
