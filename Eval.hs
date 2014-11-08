module Eval (eval) where

import Prelude hiding (head, tail, id)
import Data.List hiding (head)
import Data.Function (on)
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Kaguya

type Evaluator = StateT Int Maybe

compose :: Substitution -> Substitution -> Substitution
compose lhs rhs = nubBy ((==) `on` fst) alist
  where
    alist = [ (var, subst lhs term) | (var, term) <- rhs ] ++ lhs

occur :: String -> Term -> Bool
occur var (Variable name) = var == name
occur var (Compound _ args) = any (occur var) args

subst :: Substitution -> Term -> Term
subst theta (Compound op args) = Compound op (map (subst theta) args)
subst theta (Variable n) =
  case lookup n theta of
    Just x  -> x
    Nothing -> Variable n

unify :: Term -> Term -> Maybe Substitution
unify (Variable var) term =
  if occur var term then
    Nothing
  else
    return $ [(var, term)]
unify term (Variable var) = unify (Variable var) term
unify (Compound op1 args1) (Compound op2 args2) | op1 == op2 = go args1 args2 []
  where
    go [] [] theta = return $ theta
    go (t1:ts1) (t2:ts2) theta = do
      phi <- unify t1 t2
      go (map (subst phi) ts1) (map (subst phi) ts2) (phi `compose` theta)
    go [] _ _ = Nothing
    go _ [] _ = Nothing
unify _ _ = Nothing

fresh :: Evaluator ()
fresh = modify (+ 1)

rename :: String -> Evaluator String
rename var = do
  id <- get
  return $ var ++ "$" ++ show id

alpha :: Term -> Evaluator Term
alpha (Compound op args) = Compound op <$> mapM alpha args
alpha (Variable var) = Variable <$> rename var

instantiate :: Clause -> Evaluator Clause
instantiate (Rule head body) = do
  fresh
  Rule <$> (alpha head) <*> (mapM alpha body)

testHead :: Clause -> Term -> Evaluator ([Term], Substitution)
testHead rule term = do
  Rule head body <- instantiate rule
  phi <- lift $ unify head term
  return $ (map (subst phi) body, phi)

testBody :: [Clause] -> [Term] -> Substitution -> Evaluator Substitution
testBody _ [] phi = return phi
testBody db (g:gs) phi = do
  theta <- resolve db g
  testBody db (map (subst theta) gs) (theta `compose` phi)

testClause :: [Clause] -> Clause -> Term -> Evaluator Substitution
testClause db rule term = do
  (goals, phi) <- testHead rule term
  testBody db goals phi

resolve :: [Clause] -> Term -> Evaluator Substitution
resolve db term = go db
  where
    go [] = mzero
    go (r:rs) = do
      s <- get
      case runStateT (testClause db r term) s of
        Nothing -> go rs
        Just (a,s') -> put s' >> return a

eval :: [Clause] -> Term -> Maybe Substitution
eval db term = evalStateT (resolve db term) 0
