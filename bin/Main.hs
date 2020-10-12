module Main where

import           Control.Monad                 (liftM)
import           Control.Monad.Except          (throwError)
import           Error                         (LispError (..), LispResult (..),
                                                unwrap)
import           Evaluator                     (eval)
import           Parser                        (Expr (..), parseLispValue)
import           System.Console.Readline
import           System.Environment            (getArgs)
import           Text.ParserCombinators.Parsec

readExpr :: String -> LispResult Expr
readExpr inp =
    case parse parseLispValue "(unknown)" inp of
      Left err  -> throwError $ Parse err
      Right val -> return val


repl :: IO ()
repl = do
    -- \u2020 † - obelisk
    inp <- readline "† "
    case inp of
      Nothing -> return ()
      Just ",q" -> return ()
      Just line -> do
          addHistory line
          putStrLn $ either show show $ eval =<< readExpr line
          repl

main :: IO ()
main = do
    args <- getArgs
    if null args
       then do
           putStrLn ";;; Entering lisk repl ..."
           repl
       else print $ eval =<< readExpr (head args)
