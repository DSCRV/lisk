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
    , ("/", arithmetic (/))
    ]

data LispNumber = I Integer
                | F Double

instance Num LispNumber where
    (I a) + (I b) = I $ a + b
    (F a) + (F b) = F $ a + b
    (I a) - (I b) = I $ a - b
    (F a) - (F b) = F $ a - b
    (I a) * (I b) = I $ a * b
    (F a) * (F b) = F $ a * b

instance Fractional LispNumber where
    (I a) / (I b) = I $ a `div` b
    (F a) / (F b) = F $ a / b

arithmetic :: (LispNumber -> LispNumber -> LispNumber) -> [Expr] -> LispResult Expr
arithmetic op args
    | length args < 2 = throwError $ ArgCount 2 args
    | otherwise = do
        as <- mapM unwrapNum args
        return . wrapNum $ foldl1 op as

unwrapNum :: Expr -> LispResult LispNumber
unwrapNum (IntLiteral n)   =  return $ I n
unwrapNum (FloatLiteral n) =  return $ F n
unwrapNum x                = throwError $ TypeMismatch "number" x

wrapNum :: LispNumber -> Expr
wrapNum (I n) = IntLiteral n
wrapNum (F n) = FloatLiteral n
