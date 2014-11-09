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
op name = lexeme $ do
  s <- string name
  notFollowedBy $ oneOf "`~!@#$%^&*-=+\\|;:<>,./?"
  return s

parens :: Parser t -> Parser t
parens p = lexeme $ between (char '(') (char ')') p

brackets :: Parser t -> Parser t
brackets p = lexeme $ between (char '[') (char ']') p

comma :: Parser String
comma = op ","

-- parse

parse :: SourceName -> String -> Either ParseError [Clause]
parse = runParser program ()

parseTerm :: String -> Either ParseError Term
parseTerm = runParser term () ""

program :: Parser [Clause]
program = do
  whiteSpace
  cs <- many clause
  eof
  return cs

functor :: Parser String
functor = do
  head <- lower
  tail <- many (alphaNum <|> char '_')
  return (head : tail)

arguments :: Parser [Term]
arguments = parens $ term `sepBy` comma

simpleCompound :: Parser Term
simpleCompound = do
  func <- functor
  args <- arguments <|> lexeme (return [])
  return $ Compound func args

listCompound :: Parser Term
listCompound = brackets $ do
  args <- term `sepBy` comma
  rest <- option empty $ op "|" >> term
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
  body <- term `sepBy` comma
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
