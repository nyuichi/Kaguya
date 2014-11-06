module Parser where

import Text.Parsec

data Term
  = Compound String [Term]
  | Variable String
  deriving Show

data Clause
  = Fact Term
  | Rule Term [Term]
  | Query [Term]
  deriving Show

type Parser t = Parsec String () t

program :: Parser [Clause]
program = (spaces >> clause) `endBy` (between spaces spaces $ string ".")

atom :: Parser String
atom = do
  head <- lower
  tail <- many (alphaNum <|> char '_')
  return (head : tail)

arguments :: Parser [Term]
arguments = option [] (do
  char '('
  args <- (between spaces spaces term) `sepBy` (char ',')
  char ')'
  return args)

simpleCompound :: Parser Term
simpleCompound = do
  functor <- atom
  args    <- option [] arguments
  return $ Compound functor args

-- listCompound :: Parser Term
-- listCompound = do
--   char '['
--   args <- (between spaces spaces term) `sepBy` (char ',')
--   char ']'
--   return $ Compound "." args
--   where
--     go = do
--       case optionMaybe

compound :: Parser Term
compound = simpleCompound

var :: Parser Term
var = do
  head <- upper <|> char '_'
  tail <- many (alphaNum <|> char '_')
  return $ Variable (head : tail)

term :: Parser Term
term = var <|> compound

rule :: Parser Clause
rule = do
  head <- term
  spaces
  string ":-"
  spaces
  body <- (between spaces spaces term) `sepBy` (char ',')
  return $ Rule head body

fact :: Parser Clause
fact = do
  head <- term
  return $ Fact head

clause :: Parser Clause
clause = rule <|> fact

