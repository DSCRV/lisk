module Main where

import           Control.Monad                 (liftM)
import           Control.Monad.Except          (liftIO, runExceptT, throwError)
import           Environment
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

evalExpr :: Env -> String -> IO (LispResult String)
evalExpr env inp = runExceptT $ fmap show $
    (liftLispResult $ readExpr inp) >>= eval env

repl :: Env -> IO ()
repl env = do
    let pp = showError defaults
    inp <- readline "† "
    case inp of
      Nothing -> return ()
      Just ",q" -> return ()
      Just i -> do
          out <- evalExpr env i
          either (putStrLn . pp i) putStrLn out
          repl env


main :: IO ()
main = do
    args <- getArgs
    initEnv <- newEnv
    if null args
       then do
           putStrLn ";;; Entering lisk repl ..."
           repl initEnv
       else do
           let pp = showError defaults "(lisk-repl)"
           evalExpr initEnv (head args) >>= (either (putStrLn . pp) print)
