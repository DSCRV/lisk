module Parser ( parseLispValue
              , Expr(..)
              , parseString
              , parseInt
              , parseFloat
              , parseId
              , parseQuote
              ) where

import           Control.Applicative           ((<$>))
import           Text.ParserCombinators.Parsec

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
          | NoReturn
          deriving (Eq)

-- backslash double quote escapes a quote inside strings
quotedChar = noneOf ['\"'] <|> try (string "\\\"" >> return '"')

parseString :: Parser Expr
parseString = do
    char '"'
    innards <- many quotedChar
    char '"'
    return (StringLiteral innards)

parseSign :: Parser (Maybe Char)
parseSign = do
    sign <- optionMaybe (oneOf "+-")
    return $ case sign of
               Just '+' -> Nothing
               s        -> s

parseInt :: Parser Expr
parseInt = do
    sign <- parseSign
    val <- many1 digit
    return $ (IntLiteral . read) $ maybe val (:val) sign

parseFloat :: Parser Expr
parseFloat = do
    sign <- parseSign
    characteristic <- many1 digit
    char '.'
    mantissa <- many1 digit
    let fval = characteristic ++ "." ++ mantissa
    return $ (FloatLiteral . read) $ maybe fval (:fval) sign

parseVector :: Parser Expr
parseVector = do
    string "#(" >> optionalWhiteSpace
    x <- sepEndBy parseLispValue whiteSpace
    optionalWhiteSpace >> char ')'
    return $ Vector x

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
whiteSpace = skipMany1 $ oneOf [' ', '\n']

optionalWhiteSpace :: Parser ()
optionalWhiteSpace = skipMany $ oneOf [' ', '\n']

type Alias = String
parseModifier :: Char -> Alias -> Parser Expr
parseModifier c alias = do
    char c
    x <- parseLispValue
    return $ List [Id alias, x]

parseQuote = parseModifier '\'' "quote"
parseQuasiquote = parseModifier '`' "quasiquote"
parseUnquote = parseModifier ',' "unquote"
-- TODO: add modifier for unquote splicing: ,@

parseLispValue :: Parser Expr
parseLispValue =
    parseString
    <|> try parseFloat
    <|> try parseInt
    <|> try parseVector
    <|> try parseId
    <|> parseQuote
    <|> parseQuasiquote
    <|> parseUnquote
    -- handles lists and dotted lists
    <|> do
        char '(' >> optionalWhiteSpace
        x <- sepEndBy parseLispValue whiteSpace
        spaces
        t <- optionMaybe $ char '.' >> space >> parseLispValue
        optionalWhiteSpace >> char ')'
        return $ maybe (List x) (DottedList x) t
    <?> "lisp value"

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
    show NoReturn            = ";;; environment extension"
