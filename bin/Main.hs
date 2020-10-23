module Main where

import           Base                          (Expr (..))
import           Control.Monad                 (liftM)
import           Control.Monad.Except          (liftIO, runExceptT, throwError)
import           Environment
import           Error.Base                    (LispError (..), LispResult (..),
                                                unwrap)
import           Error.Pretty                  (defaults, showError)
import           Evaluator                     (eval)
import           Parser                        (parseLispValue)
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
    liftLispResult (readExpr inp) >>= eval env

repl :: Env -> IO ()
repl env = do
    let pp = showError defaults
    inp <- readline "† "
    case inp of
      Nothing -> return ()
      Just ",q" -> return ()
      Just i -> do
          addHistory i
          evalExpr env i >>= either (putStrLn . pp i) putStrLn
          repl env


main :: IO ()
main = do
    args <- getArgs
    env <- newEnv
    if null args
       then do
           putStrLn ";;; Entering lisk repl ..."
           repl env
       else do
           let pp = showError defaults "(lisk-repl)"
           evalExpr env (head args) >>= either (putStrLn . pp) print
