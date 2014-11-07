module Kaguya where

import Data.List

data Term
  = Compound String [Term]
  | Variable String

data Clause
  = Rule Term [Term]
  deriving Show

type Substitution
  = [(String, Term)]

instance Show Term where
  show (Variable var) = var
  show (Compound atom []) = atom
  show (Compound atom args) = atom ++ "(" ++ intercalate "," (map show args) ++ ")"

toAtom :: String -> Term
toAtom a = Compound a []

toList :: [Term] -> Term
toList xs = foldr (\x y -> Compound "." [x,y]) empty xs

empty :: Term
empty = Compound "[]" []
