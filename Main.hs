module Main where

import System.Environment

import Type
import Parser
import Eval

entry1 :: [String] -> Term
entry1 args = Compound "main" $ [ toList (map toAtom args) ]

entry2 :: Term
entry2 = Compound "main" $ []

main :: IO ()
main = do
  args <- getArgs
  text <- getContents
  case parse "" text of
    Left  e  -> print e
    Right cs -> do
      substs1 <- eval cs (entry1 args) -- main(Args) :- ...
      substs2 <- eval cs entry2        -- main :- ...
      case substs1 ++ substs2 of
        [] -> print False
        (x:_) -> print x
