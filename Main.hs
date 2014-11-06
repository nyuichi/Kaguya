module Main where

import Text.Parsec (parse)
import Parser

main :: IO ()
main = do
  text <- getContents
  print $ parse program "" text
