module Evaluator (
    eval
    ) where

import           Control.Monad.Except
import           Error                         (LispError (..), LispResult (..),
                                                unwrap)
import           Operators
import           Parser
import           Text.ParserCombinators.Parsec

apply :: String -> [Expr] -> LispResult Expr
apply fn args =
    case lookup fn primitives of
      Just f -> f args
      _      -> throwError $ UnknownFunction fn

eval :: Expr -> LispResult Expr
eval v@(StringLiteral s)     = return v
eval v@(IntLiteral i)        = return v
eval v@(BoolLiteral b)       = return v
-- handle quotes as literals
eval (List[Id "quote", val]) = return val
eval (List (Id fn : args))   = mapM eval args >>= apply fn

-- handle bad forms
eval idk = throwError $ BadForm "lisk can't recognize this form" idk

