module Operators (
    primitives
    ) where

import           Parser

primitives :: [(String, [Expr] -> Expr)]
primitives =
    [
    ("+", arithmetic (+))
    , ("-", arithmetic (-))
    , ("*", arithmetic (*))
    , ("/", arithmetic div)
    ]

arithmetic :: (Integer -> Integer -> Integer) -> [Expr] -> Expr
arithmetic op args = IntLiteral $ foldl1 op $ map unwrapNum args

unwrapNum :: Expr -> Integer
unwrapNum (IntLiteral n) = n
unwrapNum _              = undefined

