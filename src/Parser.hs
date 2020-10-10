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

-- TODO: use LispNumber (src/Operators.hs) here instead of IntLiteral and FloatLiteral
data Expr = List [Expr]
          | DottedList [Expr] Expr
          | StringLiteral String
          | IntLiteral Integer
          | FloatLiteral Double
          | BoolLiteral Bool
          | Id String
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

type Alias = String
parseModifier :: Char -> Alias -> Parser Expr
parseModifier c alias = do
    char c
    x <- parseLispValue
    return $ List [Id alias, x]

parseQuote = parseModifier '\'' "quote"
parseQuasiquote = parseModifier '`' "quasiquote"
parseUnquote = parseModifier ',' "unquote"
-- TODO: add modifier for unquote splicing

parseLispValue :: Parser Expr
parseLispValue =
    try parseId
    <|> parseString
    <|> try parseFloat
    <|> parseInt
    <|> parseQuote
    <|> parseQuasiquote
    <|> parseUnquote
    <|> do
        char '('
        x <- try parseList <|> parseDottedList
        char ')'
        return x
    <?> "expected lisp value!"

showLispList :: [Expr] -> String
showLispList = unwords . map show

instance Show Expr where
    show (DottedList xs x)   = "(" ++ showLispList xs ++ " . " ++ show x ++ ")"
    show (List xs)           = "(" ++ showLispList xs ++ ")"
    show (StringLiteral s)   = "\"" ++ s ++ "\""
    show (IntLiteral n)      = show n
    show (FloatLiteral n)    = show n
    show (BoolLiteral True)  = "#t"
    show (BoolLiteral False) = "#f"
    show (Id i)              = i
