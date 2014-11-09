module Database where

import Prelude hiding (head)
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Writer

import Type
import Parser (parseTerm)
import Eval

define :: String -> ReaderT Substitution Evaluator Substitution -> Writer [(Term, Substitution -> Evaluator Substitution)] ()
define src cmd =
  case parseTerm src of
    Left _ -> return ()
    Right t -> tell [(t, \phi -> runReaderT cmd phi)]

ref :: String -> ReaderT Substitution Evaluator Term
ref name = do
  phi <- ask
  sym <- lift $ rename name
  return $ subst phi (Variable sym)

success :: ReaderT Substitution Evaluator Substitution
success = do
  phi <- ask
  return phi

builtinsDef :: Writer [(Term, Substitution -> Evaluator Substitution)] ()
builtinsDef = do

  define "write(X)" $ do
    t <- ref "X"
    liftIO $ print t
    success

builtins :: Database
builtins = map (\(t, f) -> CRule t f) $ execWriter builtinsDef

database :: [Clause] -> Database
database rules = builtins ++ do
  Rule head body <- rules
  return $ PRule head body
