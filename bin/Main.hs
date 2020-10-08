module Main where

import           Evaluator                     (eval)
import           Parser                        (Expr (..), parseLispValue)
import           System.Console.Readline
import           Text.ParserCombinators.Parsec

readExpr :: String -> Expr
readExpr inp =
    case parse parseLispValue "(unknown)" inp of
      Left err  -> StringLiteral $ show err
      Right val -> val

repl :: IO ()
repl = do
    inp <- readline "(lisk)> "
    case inp of
      Nothing -> return ()
      Just ",q" -> return ()
      Just line -> do
          addHistory line
          print . eval . readExpr $ line
          repl

main :: IO ()
main = repl
