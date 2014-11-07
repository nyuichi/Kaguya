module Eval where

import Prelude hiding (head, tail)
import Data.List hiding (head)
import Data.Function
import Kaguya

compose :: Substitution -> Substitution -> Substitution
compose lhs rhs = nubBy ((==) `on` fst) alist
  where
    alist = [ (var, subst lhs term) | (var, term) <- rhs ] ++ lhs

subst :: Substitution -> Term -> Term
subst theta (Compound op args) = Compound op (map (subst theta) args)
subst theta (Variable n) =
  case lookup n theta of
    Just x  -> x
    Nothing -> Variable n

unify :: Term -> Term -> Maybe Substitution
unify (Variable var) term = return $ [(var, term)]
unify term (Variable var) = return $ [(var, term)]
unify (Compound op1 args1) (Compound op2 args2) | op1 == op2 = go args1 args2 []
  where
    go [] [] theta = return $ theta
    go (t1:ts1) (t2:ts2) theta = do
      phi <- unify t1 t2
      go (map (subst phi) ts1) (map (subst phi) ts2) (phi `compose` theta)
    go [] _ _ = Nothing
    go _ [] _ = Nothing
unify _ _ = Nothing

resolve :: [Clause] -> Term -> [Substitution]
resolve rules term = do
  Rule head body <- rules
  case unify term head of
    Just theta -> go (map (subst theta) body) theta
    Nothing -> []
  where
    go [] theta = return $ theta
    go (g:gs) theta = do
      phi <- resolve rules g
      go (map (subst phi) gs) (phi `compose` theta)
