module Env
    ( nullEnv
    , isBound
    , getVar
    , setVar
    , defineVar
    , bindVars
    , keys
    , dump
    ) where

-- import           Control.Monad.Cont         (MonadCont)
-- import           Control.Monad.Except       (MonadError)
import           Control.Monad.Trans
import           Control.Monad.Trans.Except
import           Data.IORef
import           Data.Maybe                 (fromMaybe, isJust, isNothing)
import           Types

keys :: Env -> IO [String]
keys envRef = do
  env <- readIORef envRef
  return $ keys_ env
  where
    keys_ [] = []
    keys_ ((k, _) : rest) = k : keys_ rest

dump :: Env -> IO ()
dump envRef = do
  env <- readIORef envRef
  putStrLn "# Bindings:"
  dump_ env
  putStrLn "#"
  return ()
  where
    dump_ [] = return ()
    dump_ ((k, vRef):rest) = do
      v <- readIORef vRef
      putStrLn (k ++ ": " ++ show v)
      dump_ rest


isBound :: Env -> String -> IO Bool
isBound envRef var = fmap (isJust . lookup var) (readIORef envRef)

getVar :: Env -> String -> LispEval
getVar envRef var = do env <- liftIO $ readIORef envRef
                       maybe (lift . throwE $ UnboundVar "Getting an unbound variable" var)
                             (liftIO . readIORef)
                             (lookup var env)

setVar :: Env -> String -> LispVal -> LispEval
setVar envRef var value = do env <- liftIO $ readIORef envRef
                             maybe (lift . throwE $ UnboundVar "Setting an unbound variable" var)
                                   (liftIO . flip writeIORef value)
                                   (lookup var env)
                             return value

defineVar :: Env -> String -> LispVal -> LispEval
defineVar envRef var value = do
  alreadyDefined <- liftIO $ isBound envRef var
  if alreadyDefined
    then setVar envRef var value >> return value
    else liftIO $ do
      valueRef <- newIORef value
      env <- readIORef envRef
      writeIORef envRef ((var, valueRef) : env)
      return value

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
  where extendEnv bindings env = fmap (++ env) (mapM addBinding bindings)
        addBinding (var, value) = do ref <- newIORef value
                                     return (var, ref)

nullEnv :: IO Env
nullEnv = newIORef []
