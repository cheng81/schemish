module Lib
    ( liftThrows
    , runIOThrows
    ) where

import           Control.Monad.Except
import           Control.Monad.Trans.Either
import           Types

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = fmap extractValue (runEitherT (trapError action))

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
