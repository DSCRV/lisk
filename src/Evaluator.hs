module Evaluator (eval) where

import           Base
import           Control.Applicative           ((*>))
import           Control.Arrow                 ((&&&))
import           Control.Monad.Except
import           Environment
import           Error.Base                    (LispError (..), LispResult (..),
                                                unwrap)
import           Operators
import           Text.ParserCombinators.Parsec
import qualified Data.Map as M

apply :: String -> [Expr] -> LispResult Expr
apply fn args = maybe
    (throwError $ UnknownFunction fn)
    ($ args)
    (M.lookup fn primitives)

evalUnquoteSplicing :: Env -> Expr -> IOResult Expr
evalUnquoteSplicing env (List xs) = List <$> mapM (eval env) xs
evalUnquoteSplicing env literal   = return literal

-- might be worth including unquote and unquote-splicing in lisk's prelude
evalUnquote :: Env -> Expr -> IOResult Expr
evalUnquote env (DottedList h t)                  = List . (:[]) <$> liftM2 DottedList (mapM (evalUnquote env) h) (evalUnquote env t)
evalUnquote env (Vector vs)                       = List . (:[]) . Vector <$> mapM (evalUnquote env) vs
evalUnquote env (List [Id "unquote", v])          = List . (:[]) <$> eval env v
evalUnquote env (List [Id "unquote-splicing", v]) = eval env v
evalUnquote env (List (Id "unquote-splicing":vs)) = List <$> mapM (eval env) vs
evalUnquote env (List (Id "unquote":vs))          = List . (:[]) . List <$> mapM (eval env) vs
evalUnquote env (List [Id "quote", vs])           = List . (:[]) <$> fmap (List . ([Id "quote"] ++ ) . (: [])) (evalUnquote env vs)
evalUnquote env (List vs)                         = List . concat <$> mapM ((unwrapList <$>) . evalUnquote env) vs
evalUnquote env literal                           = return $ List . (:[]) $ literal

evalQuasiQuote :: Env -> Expr -> IOResult Expr
evalQuasiQuote env v@(Vector _) = evalUnquote env v
evalQuasiQuote env q@(List _)   = evalUnquote env q -- list of atoms which may be quoted or unquoted
evalQuasiQuote env literal      = return literal -- just behave like quote otherwise

eval :: Env -> Expr -> IOResult Expr
eval _ v@(StringLiteral s)                    = return v
eval _ v@(Number i)                           = return v
eval env (Id l)                               = getVar env l
eval env v@(Vector xs)                        = Vector <$> mapM (eval env) xs
eval env (List[Id "quote", val])              = return val
eval env (List[Id "quasiquote", val])         = evalQuasiQuote env val
eval env v@(List[Id "unquote", val])          = throwError $ BadForm "Cannot use `unquote` outside quasiquote form" v
eval env v@(List[Id "unquote-splicing", val]) = throwError $ BadForm "Cannot use `unquote-splicing` outside quasiquote form" v
eval env (List [Id "set!", Id var, val]) = eval env val >>= uncurry (*>) . (setVar env var &&& pure)
eval env (List [Id "define", Id var, val]) = eval env val >>= uncurry (*>) . (defineVar env var &&& pure)
-- eval env (List (Id "lambda":List params:body)) = evalLambda params body env
eval env (List (Id fn : args))        = mapM (eval env) args >>= liftLispResult . apply fn

-- handle bad forms
eval env invalidForm = throwError $ BadForm "lisk can't recognize this form" invalidForm

unwrapList :: Expr -> [Expr]
unwrapList (List x) = x
unwrapList literal  = [literal]

-- evalLambda :: [Expr] -> Expr -> Env -> IOResult Expr
-- evalLambda params body env = do
--     extendedEnv <- manyBindings env
