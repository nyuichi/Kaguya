module Eval (eval) where

import Prelude hiding (head, tail)
import Data.List hiding (head)
import Data.Function (on)
import Kaguya

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

testHead :: Clause -> Term -> Maybe ([Term], Substitution)
testHead (Rule head body) term = do
  phi <- unify head term
  return $ (map (subst phi) body, phi)

testBody :: [Clause] -> [Term] -> Substitution -> Maybe Substitution
testBody _ [] phi = return phi
testBody db (g:gs) phi = do
  theta <- resolve db g
  testBody db (map (subst theta) gs) (theta `compose` phi)

testClause :: [Clause] -> Clause -> Term -> Maybe Substitution
testClause db rule term = do
  (goals, phi) <- testHead rule term
  testBody db goals phi

resolve :: [Clause] -> Term -> Maybe Substitution
resolve db term = go db
  where
    go [] = Nothing
    go (r:rs) = do
      case  testClause db r term of
        Nothing -> go rs
        Just phi -> return phi

eval :: [Clause] -> Term -> Maybe Substitution
eval db term = resolve db term
