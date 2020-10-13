module Main where

import           Control.Monad                 (liftM)
import           Control.Monad.Except          (throwError)
import           Error.Base                    (LispError (..), LispResult (..),
                                                unwrap)
import           Error.Pretty                  (defaults, showError)
import           Evaluator                     (eval)
import           Parser                        (Expr (..), parseLispValue)
import           System.Console.Readline
import           System.Environment            (getArgs)
import           Text.ParserCombinators.Parsec

readExpr :: String -> LispResult Expr
readExpr inp =
    case parse parseLispValue "(lisk-repl)" inp of
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
          let pp = showError defaults
          either (putStrLn . pp line) print $ readExpr line >>= eval
          repl

main :: IO ()
main = do
    args <- getArgs
    if null args
       then do
           putStrLn ";;; Entering lisk repl ..."
           repl
       else print $ eval =<< readExpr (head args)
