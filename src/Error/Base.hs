module Error.Base (
             LispError (..)
             , LispResult (..)
             , unwrap
             ) where

import           Control.Monad.Except
import           Parser
import           Text.Parsec
import           Text.Parsec.Error
import           Text.ParserCombinators.Parsec

data LispError = Parse ParseError
               | BadForm String Expr
               | ArgCount Int [Expr]
               | UnknownFunction String
               | TypeMismatch String Expr

unwordsList :: [Expr] -> String
unwordsList = unwords . map show

instance Show LispError where
    show (Parse e)              = "Parser Error: " ++ show e
    show (BadForm s expr)       = "Bad Form: " ++ s ++ ": " ++ show expr
    show (ArgCount n es)        = "Invalid arity, expected " ++ show n ++ ", got value(s): " ++ unwordsList es
    show (UnknownFunction fn)   = "Cannot apply function: " ++ fn
    show (TypeMismatch msg got) = "Type mismatch, expected " ++ msg ++ ", got: " ++ show got

type LispResult = Either LispError

unwrap :: LispResult t -> t
unwrap (Right v) = v
unwrap (Left _)  = undefined -- should panic

