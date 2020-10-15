module Operators (
    primitives
    ) where

import           Control.Monad.Except
import           Error.Base           (LispError (..), LispResult (..))
import           Parser

primitives :: [(String, [Expr] -> LispResult Expr)]
primitives = map (\(n, f) -> (n, f n))
    [
    ("+", arithmetic (+))
    , ("-", arithmetic (-))
    , ("*", arithmetic (*))
    , ("/", arithmetic (/))
    , (">", comparator (>))
    , ("<", comparator (<))
    , (">=", comparator (>=))
    , ("<=", comparator (<=))
    , ("=" , comparator (==))
    , ("!=" , comparator (/=))
    , ("not", unaryBool not)
    , ("or", naryBool  (||))
    , ("and", naryBool  (&&))
    , ("car", car)
    , ("cdr", cdr)
    , ("cons", cons)
    , ("null?", isNull)
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

type FName = String
type Arithmetic = LispNumber -> LispNumber -> LispNumber
type Comparator = LispNumber -> LispNumber -> Bool
type UnaryBool = Bool -> Bool
type NaryBool = Bool -> Bool -> Bool

arithmetic ::  Arithmetic -> FName -> [Expr] -> LispResult Expr
arithmetic op name args
    | null args = throwError $ ArgCount name 1 args
    | otherwise = do
        as <- mapM unwrapNum args
        return . wrapNum $ foldl1 op as

comparator :: Comparator -> FName -> [Expr] -> LispResult Expr
comparator op name args
    | length args < 2 = throwError $ ArgCount name 2 args
    | otherwise = do
        as <- mapM unwrapNum args
        return . BoolLiteral . all (== True) $ zipWith op as (tail as)

unaryBool :: UnaryBool -> FName -> [Expr] -> LispResult Expr
unaryBool op name args
  | length args /= 1 = throwError $ ArgCount name 1 args
  | otherwise = BoolLiteral . op <$> unwrapBool (head args)

naryBool :: NaryBool -> FName -> [Expr] -> LispResult Expr
naryBool op name args
  | length args < 2 = throwError $ ArgCount name 2 args
  | otherwise = do
      as <- mapM unwrapBool args
      return . BoolLiteral $ foldl1 op as

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

-- list primitives

car :: FName -> [Expr] -> LispResult Expr
car _ [List (x:xs)]          = return x
car _ [DottedList (x:xs) _ ] = return x
car _ [somethingElse]        = throwError $ TypeMismatch "pair" somethingElse
car fn mulArgs               = throwError $ ArgCount fn 1 mulArgs

cdr :: FName -> [Expr] -> LispResult Expr
cdr _ [List (x:xs)]         = return $ List xs
cdr _ [DottedList [_] tail] = return tail
cdr _ [DottedList (_:xs) t] = return $ DottedList xs t
cdr _ [somethingElse]       = throwError $ TypeMismatch "pair" somethingElse
cdr fn mulArgs              = throwError $ ArgCount fn 1 mulArgs

cons :: FName -> [Expr] -> LispResult Expr
cons _ [val, List []]            = return $ List [val]
cons _ [val, List ls]            = return $ List $ val:ls
cons _ [val, DottedList ls tail] = return $ DottedList (val:ls) tail
cons _ [v1, v2]                  = return $ DottedList [v1] v2
cons fn mulArgs                  = throwError $ ArgCount fn 2 mulArgs

isNull :: FName -> [Expr] -> LispResult Expr
isNull _ [List []] = return $ BoolLiteral True
isNull _ [List _]  = return $ BoolLiteral False
isNull _ [arg]     = throwError $ TypeMismatch "pair" arg
isNull fn args     = throwError $ ArgCount fn 1 args
