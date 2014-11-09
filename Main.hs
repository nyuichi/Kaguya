module Main where

import System.Environment

import Type
import Parser
import Eval

entry :: [String] -> Term
entry args = Compound "main" $ [ toList (map toAtom args) ]

main :: IO ()
main = do
  args <- getArgs
  text <- getContents
  case parse "" text of
    Left  e  -> print e
    Right cs -> do
      substs <- eval cs (entry args)
      case substs of
        [] -> print False
        (x:_) -> print x
