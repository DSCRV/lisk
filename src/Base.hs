module Base (Expr (..)
            , Env (..)
            , Function (..)
            ) where

import           Data.IORef

-- TODO: use LispNumber (src/Operators.hs) here instead of IntLiteral and FloatLiteral
-- TODO: add character literals: \#a \#b \#c \#space \#newline
-- TODO: add support for complex numbers, oct and hex numbers
data Expr = List [Expr]
          | Vector [Expr]
          | DottedList [Expr] Expr
          | StringLiteral String
          | IntLiteral Integer
          | FloatLiteral Double
          | BoolLiteral Bool
          | Id String
          deriving (Eq)

data Function =
    Function {
    params        :: [String]
    , body        :: Expr
    , environment :: Env
    }

type Env = IORef [(String, IORef Expr)]

showLispList :: [Expr] -> String
showLispList = unwords . map show

instance Show Expr where
    show (DottedList xs x)   = "(" ++ showLispList xs ++ " . " ++ show x ++ ")"
    show (List xs)           = "(" ++ showLispList xs ++ ")"
    show (Vector xs)         = "#(" ++ showLispList xs ++ ")"
    show (StringLiteral s)   = "\"" ++ s ++ "\""
    show (IntLiteral n)      = show n
    show (FloatLiteral n)    = show n
    show (BoolLiteral True)  = "#t"
    show (BoolLiteral False) = "#f"
    show (Id i)              = i
