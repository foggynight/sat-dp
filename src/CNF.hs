module CNF where

import Data.List (nub)
import Data.Maybe (mapMaybe)

type Variable = Int    -- variable (> 0)
type Literal = Variable  -- variable + sign
type Clause = [Literal]
data CNF = CNF
  { cnf_n_vars :: Int
  , cnf_n_clauses :: Int
  , cnf_clauses :: [Clause]
  }

instance Show CNF where
  show cnf = show (cnf_clauses cnf)

data Resolvent = Resolvent
  { _res_p1 :: Clause   -- parent 1
  , _res_p2 :: Clause   -- parent 2
  , _res_res :: Clause  -- resolvent clause
  }

instance Show Resolvent where
  show (Resolvent p1 p2 res) =
    concat [(show p1), " ", (show p2), " -> ", (show res)]

litHasVar :: Variable -> Literal -> Bool
litHasVar var lit = (var == abs lit)

-- When called with var = 0, checks if clause is the empty clause.
clauseHasVar :: Variable -> Clause -> Bool
clauseHasVar 0 clause = (clause == [])
clauseHasVar _ [] = False
clauseHasVar var (lit:lits) = (abs lit == var) || clauseHasVar var lits

clauseIsTautology :: Clause -> Bool
clauseIsTautology [] = False
clauseIsTautology (l:lits) = elem (-l) lits || clauseIsTautology lits

-- Generate var-resolvent given two clauses. Removes duplicate literals.
resolve :: Variable -> Clause -> Clause -> Maybe Resolvent
resolve var c1 c2 = do
  if (elem var c1 && elem (-var) c2) || (elem (-var) c1 && elem var c2)
  then let f = not . litHasVar var
           res = nub $ filter f (c1 ++ c2)
       in Just $ Resolvent c1 c2 res
  else Nothing

-- Generate all var-resolvents given a list of clauses. Returns tuple of
resolveAll :: Variable -> [Clause] -> [Resolvent]
resolveAll _ [] = []
resolveAll var (c:cs) =
  let var_resolvents = mapMaybe (resolve var c) cs
  in var_resolvents ++ resolveAll var cs
