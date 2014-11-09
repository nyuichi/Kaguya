module Type where

import Data.List
import Control.Monad.Trans.List
import Control.Monad.Trans.State

type Evaluator = StateT Int (ListT IO)

data Term
  = Compound String [Term]
  | Variable String

data Clause
  = Rule Term [Term]
  deriving Show

data Rule
  = PRule Term [Term]
  | CRule Term (Substitution -> Evaluator Substitution)

type Database
  = [Rule]

type Substitution
  = [(String, Term)]

instance Show Term where
  show (Variable var) = var
  show (Compound "." [h,t]) = "[" ++ show h ++ "|" ++ show t ++ "]"
  show (Compound atom []) = atom
  show (Compound atom args) = atom ++ "(" ++ intercalate "," (map show args) ++ ")"

toAtom :: String -> Term
toAtom a = Compound a []

toList :: [Term] -> Term
toList xs = foldr (\x y -> Compound "." [x,y]) empty xs

empty :: Term
empty = Compound "[]" []
