module Environment ( Env
                   , setVar
                   , getVar
                   , defineVar
                   , manyBindings
                   , newEnv
                   , liftLispResult
                   , IOResult
                   ) where

import           Base                 (Env (..), Expr (..))
import           Control.Applicative  ((<$>))
import           Control.Monad        (mapM)
import           Control.Monad.Except
import           Data.IORef
import           Data.Maybe           (isJust)
import           Error.Base           (LispError (..), LispResult (..), unwrap)

newEnv :: IO Env
newEnv = newIORef []

type IOResult = ExceptT LispError IO

liftLispResult :: LispResult a -> IOResult a
liftLispResult (Left err)  = throwError err
liftLispResult (Right val) = return val

isBound :: Env -> String -> IO Bool
isBound env var = do
    ptr <- readIORef env
    return $ isJust $ lookup var ptr

-- env modifiers

getVar :: Env -> String -> IOResult Expr
getVar env var = do
    ptr <- liftIO $ readIORef env
    maybe (throwError $ UnboundVariable var)
          (liftIO . readIORef)
          $ lookup var ptr

setVar :: Env -> String -> Expr -> IOResult ()
setVar env var val = do
    ptr <- liftIO $ readIORef env
    maybe (throwError $ UnboundVariable var)
          (liftIO . flip writeIORef val)
          $ lookup var ptr

defineVar :: Env -> String -> Expr -> IOResult ()
defineVar env var val = do
    alreadyBound <- liftIO $ isBound env var
    if alreadyBound
       then setVar env var val
       else liftIO $ do
           newRef <- newIORef val
           ptr <- readIORef env
           writeIORef env $ (var, newRef):ptr

makeBind :: (String, Expr) -> IO (String, IORef Expr)
makeBind (var, val) = do
    n <- newIORef val
    return (var, n)

manyBindings :: Env -> [(String, Expr)] -> IO Env
manyBindings env binds = do
    ptr <- readIORef env
    extendedEnv <- (++ ptr) <$> mapM makeBind binds
    newIORef extendedEnv
