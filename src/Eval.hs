module Eval
    ( eval
    , apply
    ) where

import           Control.Monad.Except
import           Data.Maybe           (isNothing)
import           Env                  (bindVars, getVar)
import           Lib                  (liftThrows)
import           Types

eval :: Env -> LispVal -> IOThrowsError LispVal
eval env val@(String _) = return val
eval env val@(Char _) = return val
eval env val@(Number _) = return val
eval env val@(Float _) = return val
eval env val@(Bool _) = return val
eval env (Atom id) = getVar env id
eval env (List (function : args)) = do
  func <- eval env function
  apply env func args
eval _ badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: Env -> LispVal -> [LispVal] -> IOThrowsError LispVal
apply env (SpecialForm func) args = func env args
apply env (PrimitiveFunc func) args = do
  argVals <- mapM (eval env) args
  liftThrows $ func argVals
apply env (IOFunc func) args = do
  argVals <- mapM (eval env) args
  func env argVals
apply env (Func params varargs body closure) args =
  if num params /= num args && isNothing varargs
    then throwError $ NumArgs (num params) args
    else do
      argVals <- mapM (eval env) args
      liftIO (bindVars closure $ zip params argVals) >>= bindVarArgs varargs >>= evalBody
  where remainingArgs = drop (length params) args
        num = toInteger . length
        -- evalBody env = last <$> mapM (trace ("eval function body " ++ show body) (eval env)) body
        evalBody env = last <$> mapM (eval env) body
        bindVarArgs arg env = case arg of
          Just argName -> liftIO $ bindVars env [(argName, List remainingArgs)]
          Nothing -> return env
