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
