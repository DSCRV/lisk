module Evaluator (eval) where

import           Control.Applicative           ((*>))
import           Control.Arrow                 ((&&&))
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

evalUnquote :: Env -> Expr -> IOResult Expr
evalUnquote env (List [Id "unquote", vs]) = eval env vs
evalUnquote env (List [Id "quote", vs]) =
    fmap (List . ([Id "quote"] ++ ) . (: [])) (evalUnquote env vs)
evalUnquote env (List vs)   = List <$> mapM (evalUnquote env) vs
evalUnquote env (Vector vs) = Vector <$> mapM (evalUnquote env) vs
evalUnquote env literal = return literal

evalQuasiQuote :: Env -> Expr -> IOResult Expr
evalQuasiQuote env v@(Vector _) = evalUnquote env v
evalQuasiQuote env q@(List _)   = evalUnquote env q -- list of atoms which may be quoted or unquoted
evalQuasiQuote env literal      = return literal -- just behave like quote otherwise

eval :: Env -> Expr -> IOResult Expr
eval _ v@(StringLiteral s) = return v
eval _ v@(IntLiteral i)    = return v
eval _ v@(BoolLiteral b)   = return v
eval env (Id l)            = getVar env l
eval _ v@(FloatLiteral f)  = return v
eval env v@(Vector xs)     = Vector <$> mapM (eval env) xs
eval env (List[Id "quote", val])      = return val
eval env (List[Id "quasiquote", val]) = evalQuasiQuote env val
eval env v@(List[Id "unquote", val])    = throwError $ BadForm "Cannot use `unquote` outside quasiquote form" v
eval env (List [Id "set!", Id var, val]) = eval env val >>= uncurry (*>) . (setVar env var &&& pure)
eval env (List [Id "define", Id var, val]) = eval env val >>= uncurry (*>) . (defineVar env var &&& pure)
eval env (List (Id fn : args))        = mapM (eval env) args >>= liftLispResult . apply fn
eval env NoReturn = throwError $ BadForm "Invalid usage of non-returning expression" NoReturn

-- handle bad forms
eval env invalidForm = throwError $ BadForm "lisk can't recognize this form" invalidForm

