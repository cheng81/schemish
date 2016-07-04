module Lib
    ( liftThrows
     ,runIOThrows
     ,runEval
    ) where

import           Control.Monad.Trans        (lift)
import           Control.Monad.Trans.Cont   (runContT)
import           Control.Monad.Trans.Except
import           Types

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwE err
liftThrows (Right val) = return val

-- run1 :: ContT String (ExceptT LispError IO) String -> ExceptT LispError IO String
-- run1 = evalContT

runEval :: LispEval -> IOThrowsError LispVal
runEval action = runContT action return

runIOThrows :: IOThrowsError String -> IO String
-- runIOThrows :: ContT String (ExceptT LispError IO) String -> IO String
-- runIOThrows action = fmap extractValue (evalContT (runExceptT (trapError action)))
runIOThrows action = fmap extractValue (runExceptT (trapError action))

--runLispEval :: LispEval -> IO LispVal
--runLispEval action = (`runContT` id) action >>= catchE >>= runExceptT

trapError action = catchE action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
