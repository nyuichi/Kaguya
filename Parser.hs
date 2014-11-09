module Parser (parse, parseTerm) where

import Prelude hiding (head, tail)
import Text.Parsec hiding (parse)
import Type

type Parser t = Parsec String () t

-- lex

lexeme :: Parser t -> Parser t
lexeme p = do
  t <- p
  whiteSpace
  return t

whiteSpace :: Parser ()
whiteSpace = spaces

op :: String -> Parser String
op name = lexeme $ string name

-- parse

parse :: SourceName -> String -> Either ParseError [Clause]
parse = runParser program ()

parseTerm :: String -> Either ParseError Term
parseTerm = runParser term () ""

program :: Parser [Clause]
program = whiteSpace >> many clause

functor :: Parser String
functor = do
  head <- lower
  tail <- many (alphaNum <|> char '_')
  return (head : tail)

arguments :: Parser [Term]
arguments = option [] $ do
  op "("
  args <- term `sepBy` (op ",")
  op ")"
  return args

simpleCompound :: Parser Term
simpleCompound = do
  func <- functor
  args <- option [] arguments
  return $ Compound func args

listCompound :: Parser Term
listCompound = do
  op "["
  args <- term `sepBy` (op ",")
  rest <- option empty $ op "|" >> term
  op "]"
  return $ foldr (\x y -> Compound "." [x,y]) rest args

compound :: Parser Term
compound = try simpleCompound <|> listCompound

var :: Parser Term
var = lexeme $ do
  head <- upper <|> char '_'
  tail <- many (alphaNum <|> char '_')
  return $ Variable (head : tail)

term :: Parser Term
term = try var <|> compound

rule :: Parser Clause
rule = do
  head <- term
  op ":-"
  body <- term `sepBy` (op ",")
  return $ Rule head body

fact :: Parser Clause
fact = do
  head <- term
  return $ Rule head []

clause :: Parser Clause
clause = do
  c <- try rule <|> fact
  op "."
  return $ c
