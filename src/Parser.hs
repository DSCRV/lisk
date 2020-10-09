module Parser ( parseLispValue
              , Expr(..)
              , parseString
              , parseInt
              , parseFloat
              , parseId
              , parseList
              , parseQuote
              , parseDottedList
              ) where

import           Control.Applicative           ((<$>))
import           Control.Monad                 (liftM)
import           Text.ParserCombinators.Parsec


type Ident = String

data Expr = List [Expr]
          | DottedList [Expr] Expr
          | StringLiteral String
          | IntLiteral Integer
          | FloatLiteral Double
          | BoolLiteral Bool
          | Id Ident
          deriving (Eq)

parseString :: Parser Expr
parseString = do
    char '"'
    innards <- many (noneOf "\"")
    char '"'
    return (StringLiteral innards)

parseInt :: Parser Expr
parseInt = IntLiteral . read <$> many1 digit

parseFloat :: Parser Expr
parseFloat = do
    characteristic <- many1 digit
    char '.'
    mantissa <- many1 digit
    return $ (FloatLiteral . read) $ characteristic ++ "." ++ mantissa

symbol :: Parser Char
symbol = oneOf "!#$%&|*+:/-=<?>@^_~"

parseId :: Parser Expr
parseId = do
    first <- letter <|> symbol
    rest <- many (letter <|> symbol <|> digit)
    let atom = first:rest
    return $ case atom of
               "#t" -> BoolLiteral True
               "#f" -> BoolLiteral False
               _    -> Id atom

whiteSpace :: Parser ()
whiteSpace = skipMany1 space

parseList :: Parser Expr
parseList = List <$> sepBy parseLispValue whiteSpace

parseDottedList :: Parser Expr
parseDottedList = do
    head <- endBy parseLispValue whiteSpace
    char '.'
    whiteSpace
    DottedList head <$> parseLispValue

parseQuote :: Parser Expr
parseQuote = do
    char '\''
    x <- parseLispValue
    return $ List [Id "quote", x]


parseLispValue :: Parser Expr
parseLispValue =
    try parseId
    <|> parseString
    <|> try parseFloat
    <|> parseInt
    <|> parseQuote
    -- TODO: figure out a way to have floats and dotted lists
    <|> do
        char '('
        x <- try parseList <|> parseDottedList
        char ')'
        return x
    <?> "expected lisp value!"

instance Show Expr where
    show (DottedList xs x)   = "(" ++ unwords (map show xs) ++ " . " ++ show x ++ ")"
    show (List xs)           = "(" ++ unwords (map show xs) ++ ")"
    show (StringLiteral s)   = "\"" ++ s ++ "\""
    show (IntLiteral n)      = show n
    show (FloatLiteral n)    = show n
    show (BoolLiteral True)  = "#t"
    show (BoolLiteral False) = "#f"
    show (Id i) = i
