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
    , (">", comparator (>))
    , ("<", comparator (<))
    , (">=", comparator (>=))
    , ("<=", comparator (<=))
    , ("=", comparator (==))
    , ("!=", comparator (/=))
    , ("not", unaryBool not)
    ]

data LispNumber = I Integer
                | F Double
                deriving (Eq, Ord)

instance Num LispNumber where
    -- TODO:
    -- float op anything = float
    -- int op int = int
    -- int op float = float
    (I a) + (I b) = I $ a + b
    (F a) + (F b) = F $ a + b
    (I a) - (I b) = I $ a - b
    (F a) - (F b) = F $ a - b
    (I a) * (I b) = I $ a * b
    (F a) * (F b) = F $ a * b

instance Fractional LispNumber where
    (I a) / (I b) = F $ fromIntegral a / fromIntegral b
    (F a) / (I b) = F $ a / fromIntegral b
    (I a) / (F b) = F $ fromIntegral a / b
    (F a) / (F b) = F $ a / b

arithmetic :: (LispNumber -> LispNumber -> LispNumber) -> [Expr] -> LispResult Expr
arithmetic op args
    | length args < 2 = throwError $ ArgCount 2 args
    | otherwise = do
        as <- mapM unwrapNum args
        return . wrapNum $ foldl1 op as

comparator :: (LispNumber -> LispNumber -> Bool) -> [Expr] -> LispResult Expr
comparator op args
    | length args < 2 = throwError $ ArgCount 2 args
    | otherwise = do
        as <- mapM unwrapNum args
        return . BoolLiteral . all (== True) $ zipWith op as (tail as)

unaryBool :: (Bool -> Bool) -> [Expr] -> LispResult Expr
unaryBool op args
  | length args /= 1 = throwError $ ArgCount 1 args
  | otherwise = BoolLiteral . op <$> unwrapBool (head args)

naryBool :: (Bool -> Bool -> Bool) -> [Expr] -> LispResult Expr
naryBool op args
  | length args < 2 = throwError $ ArgCount 2 args


unwrapNum :: Expr -> LispResult LispNumber
unwrapNum (IntLiteral n)   =  return $ I n
unwrapNum (FloatLiteral n) =  return $ F n
unwrapNum x                = throwError $ TypeMismatch "number" x

wrapNum :: LispNumber -> Expr
wrapNum (I n) = IntLiteral n
wrapNum (F n) = FloatLiteral n

unwrapBool :: Expr -> LispResult Bool
unwrapBool (BoolLiteral s) = return s
unwrapBool x               = throwError $ TypeMismatch "boolean" x
