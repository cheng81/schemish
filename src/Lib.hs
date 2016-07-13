{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

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
liftThrows = either throwE return

runEval :: LispEval -> IOThrowsError LispVal
runEval action = runContT action return

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = fmap extractValue (runExceptT (trapError action))

trapError action = catchE action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
