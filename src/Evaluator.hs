module Evaluator (
    eval
    ) where

import           Control.Monad.Except
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

eval :: Expr -> LispResult Expr
eval v@(StringLiteral s) = return v
eval v@(IntLiteral i)    = return v
eval v@(BoolLiteral b)   = return v
eval v@(FloatLiteral f)  = return v
eval v@(Vector xs)       = liftM Vector $ mapM eval xs
-- handle quotes as literals
eval (List[Id "quote", val])      = return val
eval (List[Id "quasiquote", val]) = undefined
eval (List[Id "unquote", val])    = undefined
eval (List (Id fn : args))        = mapM eval args >>= apply fn

-- handle bad forms
eval invalidForm = throwError $ BadForm "lisk can't recognize this form" invalidForm

