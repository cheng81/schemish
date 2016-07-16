{-# LANGUAGE FlexibleContexts #-}
module Main where

import           Control.Applicative        ((<$>))
import           Control.Monad              (liftM, unless)
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.State.Strict (StateT, evalStateT, get, put)
import           Control.Monad.Trans        (lift)
import           Data.Char                  (isSpace)
import           Data.IORef                 (readIORef)
import           Env                        (bindVars, nullEnv)
import           Eval
import           IOFunc                     (ioPrimitives)
import           Lib
import           Parser
import           PrimitiveFunc              (primitives)
import           SpecialForm                (specialForms)
import           System.Console.Haskeline
import           System.Environment
import           System.IO
import           Types

primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= flip bindVars (map (makeFunc PrimitiveFunc) primitives
                                             ++map (makeFunc IOFunc) ioPrimitives
                                             ++map (makeFunc SpecialForm) specialForms
                                             )
  where makeFunc constructor (var, func) = (var, constructor func)


flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

stringToAction :: Env -> String -> LispEval
stringToAction env expr = lift (liftThrows (readExpr expr)) >>= eval env

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ show <$> runEval (stringToAction env expr)  -- (liftThrows (readExpr expr) >>= eval env) -- evalContT (liftThrows (readExpr expr) >>= eval env)
--evalString env expr = return $ extractValue $ trapError (fmap show $ readExpr expr >>= eval env)

loopRead :: String -> IO LispVal
loopRead pfx = do
  rest <- readPrompt ". "
  parseLine $ pfx ++ rest

parseLine :: String -> IO LispVal
-- parseLine str = return $ either (\s -> String "error") id $ readExpr str
parseLine str = either (\_ -> loopRead str) return $ readExpr str

evalExpr :: Env -> LispVal -> IO String
evalExpr env expr = runIOThrows $ show <$> runEval (eval env expr)

evalAndPrint :: Env -> LispVal -> IO ()
evalAndPrint env expr = evalExpr env expr >>= putStrLn

runOne :: [String] -> IO ()
runOne args = do --primitiveBindings >>= flip evalAndPrint expr
  env <- primitiveBindings >>= flip bindVars [("args", List $ map String $ drop 1 args)]
  runIOThrows (show <$> runEval (eval env (List [Atom "load", String (head args)]))) >>= hPutStrLn stderr

runReplLoop :: IO ()
runReplLoop = primitiveBindings >>= \env -> evalStateT (runInputT defaultSettings $ withInterrupt (replLoop promptStd)) (env, "")

promptStd = "schish$ "

dumpEnv :: Env -> IO ()
dumpEnv envRef = do
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


replLoop :: String -> InputT (StateT (Env, String) IO) ()
replLoop prompt = do
  maybeLine <- getInputLine prompt
  case maybeLine of
    Nothing -> return ()
    Just line ->
      if not $ null line
        then case line of
          ":quit" -> return ()
          ":env" -> do
            (e, _) <- lift get
            liftIO (dumpEnv e)
            replLoop promptStd
          _ -> do
            (e, pfx) <- lift get
            let eitherExpr = readExpr (pfx ++ line)
            case eitherExpr of
              Left err -> (lift . put) (e, pfx ++ line) >> handleInterrupt ((lift . put) (e, "") >> replLoop promptStd) (replLoop "... ")
              Right expr -> do
                liftIO (evalAndPrint e expr)
                (lift . put) (e, "")
                handleInterrupt (return ()) (replLoop promptStd)
        else replLoop prompt

main :: IO ()
main = do
  args <- getArgs
  if null args then runReplLoop else runOne args
  -- case length args of
  --   0 -> runRepl
  --   1 -> runOne $ head args -- evalAndPrint $ head args
  --   _ -> putStrLn "Program takes only 0 or 1 argument"
--   let evaled = fmap show $ readExpr (head args) >>= eval
--  evaled <- return $ liftM show $ readExpr (head args) >>= eval
--  putStrLn $ extractValue $ trapError evaled
-- main = getArgs >>= print . eval .readExpr . head
