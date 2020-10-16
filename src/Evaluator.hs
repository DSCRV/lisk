module Evaluator (eval) where

import           Control.Monad.Except
import           Environment
import           Error.Base                    (LispError (..), LispResult (..),
                                                unwrap)
import           Operators
import           Parser
import           Text.ParserCombinators.Parsec

apply :: String -> [Expr] -> LispResult Expr
apply fn args = maybe
    (throwError $ UnknownFunction fn)
    ($ args)
    (lookup fn primitives)

eval :: Env -> Expr -> IOResult Expr
eval _ v@(StringLiteral s) = return v
eval _ v@(IntLiteral i)    = return v
eval _ v@(BoolLiteral b)   = return v
eval env (Id l)            = getVar env l
eval _ v@(FloatLiteral f)  = return v
eval env v@(Vector xs)     = liftM Vector $ mapM (eval env) xs
eval env (List[Id "quote", val])      = return val
eval env (List[Id "quasiquote", val]) = undefined
eval env (List[Id "unquote", val])    = eval env val
eval env (List [Id "set!", Id var, val]) = do
    e <- eval env val
    setVar env var e
    return e
eval env (List [Id "define", Id var, val]) = do
    e <- eval env val
    defineVar env var e
    return e
eval env (List (Id fn : args))        = mapM (eval env) args >>= liftLispResult . apply fn
eval env NoReturn = throwError $ BadForm "Invalid usage of non-returning expression" NoReturn

-- handle bad forms
eval env invalidForm = throwError $ BadForm "lisk can't recognize this form" invalidForm

