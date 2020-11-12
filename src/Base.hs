module Base (Expr (..)
            , Env (..)
            , LispNumber (..)
            ) where

import           Data.IORef

data LispNumber = I Integer
                | F Double
                deriving (Eq, Ord)

instance Num LispNumber where
    -- addition
    (I a) + (I b) = I $ a + b
    (F a) + (F b) = F $ a + b
    (F a) + (I b) = F $ a + fromIntegral b
    (I a) + (F b) = F b + I a

    -- subtraction
    (I a) - (I b) = I $ a - b
    (F a) - (F b) = F $ a - b
    (F a) - (I b) = F $ a - fromIntegral b
    (I a) - (F b) = F b - I a

    -- multiplication
    (I a) * (I b) = I $ a * b
    (F a) * (F b) = F $ a * b
    (F a) * (I b) = F $ a * fromIntegral b
    (I a) * (F b) = F b * I a

instance Fractional LispNumber where
    (I a) / (I b) = F $ fromIntegral a / fromIntegral b
    (F a) / (I b) = F $ a / fromIntegral b
    (I a) / (F b) = recip $ F b / I a
    (F a) / (F b) = F $ a / b
    recip (F x) = F $ 1 / x
    recip (I x) = F $ 1 / fromIntegral x

-- TODO: use LispNumber (src/Operators.hs) here instead of IntLiteral and FloatLiteral
-- TODO: add character literals: \#a \#b \#c \#space \#newline
-- TODO: add support for complex numbers, oct and hex numbers
data Expr = List [Expr]
          | Vector [Expr]
          | DottedList [Expr] Expr
          | StringLiteral String
          | Number LispNumber
          | BoolLiteral Bool
          | Id String
          | Function { params      :: [String]
                     , body        :: Expr
                     , extendedEnv :: Env
                     }
          deriving (Eq)

type Env = IORef [(String, IORef Expr)]

showLispList :: [Expr] -> String
showLispList = unwords . map show

instance Show Expr where
    show (DottedList xs x)   = "(" ++ showLispList xs ++ " . " ++ show x ++ ")"
    show (List xs)           = "(" ++ showLispList xs ++ ")"
    show (Vector xs)         = "#(" ++ showLispList xs ++ ")"
    show (StringLiteral s)   = "\"" ++ s ++ "\""
    show (Number (I n))      = show n
    show (Number (F n))      = show n
    show (BoolLiteral True)  = "#t"
    show (BoolLiteral False) = "#f"
    show (Id i)              = i
