module Eval where

import Prelude hiding (head, tail, id)
import Data.List hiding (head)
import Data.Function (on)
import Control.Applicative
import Control.Monad
import Control.Monad.List
import Control.Monad.State
import Type

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

instantiate :: Rule -> Evaluator Rule
instantiate (PRule head body) = do
  fresh
  PRule <$> (alpha head) <*> (mapM alpha body)
instantiate (CRule head body) = do
  fresh
  CRule <$> (alpha head) <*> (return body)

testHead :: Rule -> Term -> Evaluator Substitution
testHead rule term =
  case unify head term of
    Nothing -> mzero
    Just phi -> return phi
  where
    head = case rule of
      PRule h _ -> h
      CRule h _ -> h

testBody :: Database -> [Term] -> Substitution -> Evaluator Substitution
testBody _ [] phi = return phi
testBody db (g:gs) phi = do
  theta <- resolve db g
  testBody db (map (subst theta) gs) (theta `compose` phi)

testClause :: Database -> Rule -> Term -> Evaluator Substitution
testClause db rule term = do
  r <- instantiate rule
  p <- testHead r term
  case r of
    PRule _ body ->
      testBody db (map (subst p) body) p
    CRule _ body -> do
      body p

resolve :: Database -> Term -> Evaluator Substitution
resolve db term = msum $ map (\r -> testClause db r term) db

eval :: Database -> Term -> IO [Substitution]
eval db term = runListT $ evalStateT (resolve db term) 0
