import Control.Monad (when)
import Data.Char (toLower)
import Data.List (sortOn)
import Data.Ord (Down(..))
import Debug.Trace (trace)
import Options.Applicative

import CNF
import DIMACS
import DP
import Util

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

type VarOrderF = CNF -> [Variable]

-- TODO: Add varOrder constructor function which takes weight function, and
-- rewrite to use varOrder constructor?

-- Numeric: Select variables in numerical order.
varOrderNumeric :: VarOrderF
varOrderNumeric cnf = [1 .. (cnf_n_vars cnf)] ++ [0]

-- Fewest Clauses: Select variable which occurs in the fewest clauses.
varOrderFewestClauses :: VarOrderF
varOrderFewestClauses cnf =
  let clauses = (cnf_clauses cnf)
      occurs = map (count_occurs clauses) [1 .. (cnf_n_vars cnf)] in
    (map fst $ sortOn snd occurs) ++ [0]
  where count_occurs clauses var =
          (var, sum $ map ((count var) . (map abs)) clauses)

-- Jerowslow-Wang: Variable exponentially higher weight in shorter clause, more
-- clauses higher weight, select variables to maximize weight.
varOrderJeroslowWang :: VarOrderF
varOrderJeroslowWang (CNF n_vars _ clauses) =
  let vars = [1 .. n_vars]
      vws = zip vars (map calc_weight vars)
  in (map fst $ sortOn (Down . snd) vws) ++ [0]
  where calc_weight var =
          sum $ map (\c -> 2 ^^ (- (length c)) :: Double)
                $ filter (clauseHasVar var) clauses

main' :: DIMACS_CNF -> VarOrderF -> IO ()
main' dimacs_cnf vo_func = do
  let maybe_cnf = parseDIMACS dimacs_cnf
  case maybe_cnf of
    Nothing  -> putStrLn "error: invalid CNF"
    Just cnf -> do
      putStr "Initial CNF: "
      print cnf
      newline

      let var_order = vo_func cnf
      when (last var_order /= 0) $ do
        error "error: variable order doesn't end with 0"
      putStr "Variable Order: "
      print var_order
      newline

      let init_buks = fillBuckets var_order (cnf_clauses cnf)
      putStrLn "Initial Buckets: "
      mapM_ print init_buks
      newline

      putStrLn "Deriving resolvents..."
      res_buks <- resolveBuckets init_buks
      let res_cnt =
            (sum $ map (length . buk_clauses) res_buks)
            - (cnf_n_clauses cnf)
      putStr $ "Resolvents added to buckets: " ++ show res_cnt
      newline; newline

      putStrLn "Resolved Buckets: "
      mapM_ print res_buks
      newline

      let solution = extractSolution res_buks
      case solution of
        Nothing   -> putStrLn "UNSAT"
        Just lits -> putStr $ "SAT: " ++ (show lits) ++ "\n"

main :: IO ()
main = do
  config <- execParser opts
  let var_order = case map toLower (conf_var_order config) of
                    "numeric" -> varOrderNumeric
                    "fewest"  -> varOrderFewestClauses
                    "jw"      -> varOrderJeroslowWang
                    _         -> trace "error: invalid variable order argument"
                                       varOrderNumeric
  dimacs_cnf <- case conf_cnf_file config of
                  "--" -> getContents
                  file -> readFile file
  main' dimacs_cnf var_order
  where opts = info (configParser <**> helper)
               ( fullDesc
                 <> header ( "SatDP - SAT solver using DP algorithm implemented"
                          ++ " with bucket elimination." ) )
