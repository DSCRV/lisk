module Evaluator (
    eval
    ) where

import           Operators
import           Parser
import           Text.ParserCombinators.Parsec

apply :: String -> [Expr] -> Expr
apply fn args =
    case lookup fn primitives of
      Just f -> f args
      _      -> BoolLiteral False -- TODO: error out instead

eval :: Expr -> Expr
eval v@(StringLiteral s)     = v
eval v@(IntLiteral i)        = v
eval v@(BoolLiteral b)       = v
-- handle quotes as literals
eval (List[Id "quote", val]) = val
eval (List (Id fn : args))   = apply fn $ map eval args

