module Main where

import           Control.Applicative        ((<$>))
import           Control.Monad              (liftM, unless)
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.State.Strict (StateT, evalStateT, get, put)
import           Control.Monad.Trans        (lift)

--import           Control.Monad.Trans.Cont (evalContT)
import           Data.Char                  (isSpace)
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

evalStringAndPrint :: Env -> String -> IO ()
evalStringAndPrint env expr = evalString env expr >>= putStrLn

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
  runIOThrows (show <$> runEval (eval env (List [Atom "load", String (head args)]))) >>= hPutStrLn stderr
--  runIOThrows $ evalContT (show <$> eval env (List [Atom "load", String (head args)])) >>= hPutStrLn stderr

isQuit (Atom "quit") = True
isQuit _ = False

runRepl :: IO ()
runRepl = primitiveBindings >>= until_ isQuit (readPrompt "Lisp>>> " >>= parseLine) . evalAndPrint

-- liftEnv :: IO Env -> InputT IO Env
-- liftEnv env = liftM env
--
-- runReplLoop = runInputT defaultSettings $ liftEnv primitiveBindings >>= replLoop
runReplLoop = primitiveBindings >>= evalStateT (runInputT defaultSettings replLoop)

replLoop :: InputT (StateT Env IO) ()
replLoop = do
  maybeLine <- getInputLine "schish>>> "
  case maybeLine of
    Nothing -> return ()
    Just line -> do
      let trimmedLine = dropWhile isSpace line
      if not $ null trimmedLine
        then case trimmedLine of
          "quit" -> return ()
          _ -> lift get >>= \e -> liftIO (evalStringAndPrint e trimmedLine) >> replLoop
          -- _ -> do
          --       _ <- lift $ evalStringAndPrint env trimmedLine
          --       replLoop env
        else replLoop

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
