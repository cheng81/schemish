module Lib
    ( liftThrows
     ,runIOThrows
    ) where

import           Control.Monad.Trans.Except
import           Types

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwE err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = fmap extractValue (runExceptT (trapError action))

trapError action = catchE action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
