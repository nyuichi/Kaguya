module Kaguya where

data Term
  = Compound String [Term]
  | Variable String
  deriving Show

data Clause
  = Rule Term [Term]
  deriving Show

type Substitution
  = [(String, Term)]


toAtom :: String -> Term
toAtom a = Compound a []

toList :: [Term] -> Term
toList xs = foldr (\x y -> Compound "." [x,y]) empty xs

empty :: Term
empty = Compound "[]" []
