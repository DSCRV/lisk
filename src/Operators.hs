module Operators (
    primitives
    ) where

import           Control.Monad.Except
import           Error                (LispError (..), LispResult (..))
import           Parser

primitives :: [(String, [Expr] -> LispResult Expr)]
primitives =
    [
    ("+", arithmetic (+))
    , ("-", arithmetic (-))
    , ("*", arithmetic (*))
    , ("/", arithmetic div)
    ]

arithmetic :: (Integer -> Integer -> Integer) -> [Expr] -> LispResult Expr
arithmetic op args
    | length args < 2 = throwError $ ArgCount 2 args
    | otherwise = do
        as <- mapM unwrapNum args
        return . IntLiteral $ foldl1 op as

unwrapNum :: Expr -> LispResult Integer
unwrapNum (IntLiteral n) = return n
unwrapNum x              = throwError $ TypeMismatch "number" x

