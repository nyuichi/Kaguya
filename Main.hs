module Main where

import Text.Parsec (parse)
import Control.Monad

import Kaguya
import Parser
import Eval

entry :: Term
entry = Compound "main" []

main :: IO ()
main = do
  text <- getContents
  let Right cs = parse program "" text
  let substs = resolve cs entry
  substs `forM` \s ->
    print s
  return ()
