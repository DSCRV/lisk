module Error.Base ( LispError (..)
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
               | ArgCount String Int [Expr]
               | UnknownFunction String
               | TypeMismatch String Expr
               | UnboundVariable String

unwordsList :: [Expr] -> String
unwordsList = unwords . map show

literal :: String -> String
literal v = "`" <> v <> "`"

instance Show LispError where
    show (Parse e)              = "Parser Error: " ++ show e
    show (BadForm s expr)       = "Bad Form: " ++ literal s ++ ": " ++ show expr
    -- TODO: clean this up
    show (ArgCount fn n es)
      | null es = "Invalid arity, " ++ literal fn ++ " expects " ++ show n ++ " or more expression(s)!"
      | otherwise = "Invalid arity, " ++ literal fn ++ " expects " ++ show n ++ " or more expression(s), got value(s): " ++ unwordsList es
    show (UnknownFunction fn)   = "Cannot apply function: " ++ literal fn
    show (TypeMismatch msg got) = "Type mismatch, expected " ++ literal msg ++ ", got: " ++ show got
    show (UnboundVariable name) = "Possibly unbound variable: " ++ literal name

type LispResult = Either LispError

unwrap :: LispResult t -> t
unwrap (Right v) = v
unwrap (Left _)  = undefined -- should panic

