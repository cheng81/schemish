module Main where

import           Control.Applicative ((<$>))
import           Control.Monad       (unless)
import           Env                 (bindVars)
import           Eval
import           Lib
import           Parser
import           System.Environment
import           System.IO
import           Types

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ fmap show $ liftThrows (readExpr expr) >>= eval env
--evalString env expr = return $ extractValue $ trapError (fmap show $ readExpr expr >>= eval env)

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
  result <- prompt
  unless (pred result) $ action result >> until_ pred prompt action
  -- if pred result
  --   then return ()
  --   else action result >> until_ pred prompt action

runOne :: [String] -> IO ()
runOne args = do --primitiveBindings >>= flip evalAndPrint expr
  env <- primitiveBindings >>= flip bindVars [("args", List $ map String $ drop 1 args)]
  runIOThrows (show <$> eval env (List [Atom "load", String (head args)])) >>= hPutStrLn stderr

runRepl :: IO ()
runRepl = primitiveBindings >>= until_ (== "quit") (readPrompt "Lisp>>> ") . evalAndPrint

main :: IO ()
main = do
  args <- getArgs
  if null args then runRepl else runOne args
  -- case length args of
  --   0 -> runRepl
  --   1 -> runOne $ head args -- evalAndPrint $ head args
  --   _ -> putStrLn "Program takes only 0 or 1 argument"
--   let evaled = fmap show $ readExpr (head args) >>= eval
--  evaled <- return $ liftM show $ readExpr (head args) >>= eval
--  putStrLn $ extractValue $ trapError evaled
-- main = getArgs >>= print . eval .readExpr . head
