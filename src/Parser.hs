module Parser ( parseLispValue
              , parseString
              , parseInt
              , parseFloat
              , parseId
              , parseQuote
              , parseComment
              ) where

import           Base                          (Expr (..), LispNumber(..))
import           Control.Applicative           ((<$>))
import           Control.Monad                 (void)
import           Text.Parsec.Char
import           Text.ParserCombinators.Parsec

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
    return $ (Number . I . read) $ maybe val (:val) sign

parseFloat :: Parser Expr
parseFloat = do
    sign <- parseSign
    characteristic <- many1 digit
    char '.'
    mantissa <- many1 digit
    let fval = characteristic ++ "." ++ mantissa
    return $ (Number . F . read) $ maybe fval (:fval) sign

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

-- atmosphere
parseComment :: Parser ()
parseComment = do
    char ';'
    -- get internals of comment by getting it from here
    void $ manyTill anyChar $ try $ eol <|> eof
        where eol = void endOfLine

whiteSpace::Parser()
whiteSpace = skipMany $ parseComment <|> nl <|> spc
    where nl = void endOfLine
          spc = void space

optionalWhiteSpace :: Parser ()
optionalWhiteSpace = void $ optionMaybe whiteSpace

type Alias = String
parseModifier :: String -> Alias -> Parser Expr
parseModifier s alias = do
    string s
    x <- parseLispValue
    return $ List [Id alias, x]

parseQuote           = parseModifier "'" "quote"
parseQuasiquote      = parseModifier "`" "quasiquote"
parseUnquote         = parseModifier "," "unquote"
parseUnquoteSplicing = parseModifier ",@" "unquote-splicing"

parseLispValue :: Parser Expr
parseLispValue =
        parseString
    <|> try parseFloat
    <|> try parseInt
    <|> try parseVector
    <|> try parseId
    <|> parseQuote
    <|> parseQuasiquote
    <|> try parseUnquoteSplicing
    <|> parseUnquote
    <|> do
        char '(' >> optionalWhiteSpace
        x <- sepEndBy parseLispValue whiteSpace
        spaces
        t <- optionMaybe $ char '.' >> space >> parseLispValue
        optionalWhiteSpace >> char ')'
        return $ maybe (List x) (DottedList x) t
    <?> "lisp value";

