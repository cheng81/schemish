{-# LANGUAGE FlexibleContexts #-}
module Main where

import           Control.Applicative                 ((<$>))
import           Control.Monad                       (liftM, unless)
import           Control.Monad.IO.Class              (liftIO)
import           Control.Monad.State.Strict          (StateT, evalStateT, get,
                                                      put)
import           Control.Monad.Trans                 (lift)
import           Data.Char                           (isSpace)
import           Data.IORef                          (readIORef)
import           Data.List                           (isPrefixOf)
import           Env                                 (bindVars, dump, keys,
                                                      nullEnv)
import           Eval
import           IOFunc                              (ioPrimitives)
import           Lib
import           Parser
import           PrimitiveFunc                       (primitives)
import           SpecialForm                         (specialForms)
import           System.Console.Haskeline
import           System.Console.Haskeline.Completion
import           System.Environment
import           System.IO
import           Types

primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= flip bindVars (map (makeFunc PrimitiveFunc) primitives
                                             ++map (makeFunc IOFunc) ioPrimitives
                                             ++map (makeFunc SpecialForm) specialForms
                                             )
  where makeFunc constructor (var, func) = (var, constructor func)

evalExpr :: Env -> LispVal -> IO String
evalExpr env expr = runIOThrows $ show <$> runEval (eval env expr)

evalAndPrint :: Env -> LispVal -> IO ()
evalAndPrint env expr = evalExpr env expr >>= putStrLn

runOne :: [String] -> IO ()
runOne args = do --primitiveBindings >>= flip evalAndPrint expr
  env <- primitiveBindings >>= flip bindVars [("args", List $ map String $ drop 1 args)]
  runIOThrows (show <$> runEval (eval env (List [Atom "load", String (head args)]))) >>= hPutStrLn stderr

runReplLoop :: IO ()
runReplLoop = primitiveBindings >>= \env -> evalStateT (runInputT (setComplete replComplete defaultSettings) $ withInterrupt (replLoop promptStd)) (env, "")

replComplete :: CompletionFunc (StateT (Env, String) IO)
replComplete = completeQuotedWord Nothing "\"\'" (const $ return []) symbolComplete

symbolComplete :: CompletionFunc (StateT (Env, String) IO)
symbolComplete = completeWord Nothing "() \t" completeEnv

completeEnv :: String -> (StateT (Env, String) IO) [Completion]
completeEnv s = do
    (envRef, _) <- get
    keys <- liftIO $ keys envRef
    return $ map simpleCompletion $ filter (isPrefixOf s) keys

promptStd = "schish$ "

replLoop :: String -> InputT (StateT (Env, String) IO) ()
replLoop prompt = do
  maybeLine <- getInputLine prompt
  case maybeLine of
    Nothing -> return ()
    Just line ->
      if not $ null line
        then case line of
          (';' : ';' : _) -> replLoop prompt
          ":quit" -> return ()
          ":env" -> do
            (e, _) <- lift get
            liftIO (dump e)
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
