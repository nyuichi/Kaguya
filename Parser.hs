module Parser (parse, parseExpr) where

import Prelude hiding (head, tail)
import Text.Parsec hiding (parse)
import Text.Parsec.Expr
import Text.Parsec.Language (haskell)
import Text.Parsec.Token (naturalOrFloat)
import Control.Monad.Identity
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
  notFollowedBy $ oneOf "`~!@#$%^&*-=+\\|;:<>./?"
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

parseExpr :: String -> Either ParseError Term
parseExpr = runParser expr () ""

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
arguments = parens $ expr `sepBy` comma

simpleCompound :: Parser Term
simpleCompound = do
  func <- functor
  args <- arguments <|> lexeme (return [])
  return $ Compound func args

listCompound :: Parser Term
listCompound = brackets $ do
  args <- expr `sepBy` comma
  rest <- option empty $ op "|" >> expr
  return $ foldr (\x y -> Compound "." [x,y]) rest args

compound :: Parser Term
compound = try simpleCompound <|> listCompound

var :: Parser Term
var = lexeme $ do
  head <- upper <|> char '_'
  tail <- many (alphaNum <|> char '_')
  return $ Variable (head : tail)

number :: Parser Term
number = do
  x <- naturalOrFloat haskell
  case x of
    Right f -> return $ Number f
    Left i -> return $ Number $ fromIntegral i

cut :: Parser Term
cut = do
  op "!"
  return $ Compound "!" []

term :: Parser Term
term = try (number <|> var <|> compound <|> cut <|> parens expr)

expr :: Parser Term
expr = buildExpressionParser table term

rule :: Parser Clause
rule = do
  head <- expr
  op ":-"
  body <- expr `sepBy` comma
  return $ Rule head body

fact :: Parser Clause
fact = do
  head <- expr
  return $ Rule head []

clause :: Parser Clause
clause = do
  c <- try rule <|> fact
  op "."
  return $ c

table :: OperatorTable String () Identity Term
table =
  [ [ prefix "-", prefix "+" ]
  , [ binary "*" AssocLeft, binary "/" AssocLeft ]
  , [ binary "+" AssocLeft, binary "-" AssocLeft ]
  , [ binary "=" AssocNone ]
  ]
  where
    binary  name assoc = Infix (op name >> return (\x y -> Compound name [x,y])) assoc
    prefix  name       = Prefix (op name >> return (\x -> Compound name [x]))
    postfix name       = Postfix (op name >> return (\x -> Compound name [x]))

