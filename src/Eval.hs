module Eval
    ( eval
    , apply
    ) where

import           Control.Monad.Trans        (lift, liftIO)
import           Control.Monad.Trans.Except
import           Data.Maybe                 (isNothing)
import           Env                        (bindVars, getVar)
import           Lib                        (liftThrows)
import           Types

eval :: Env -> LispVal -> LispEval
eval env val@Unit = return val
eval env val@(String _) = return val
eval env val@(Char _) = return val
eval env val@(Number _) = return val
eval env val@(Float _) = return val
eval env val@(Bool _) = return val
eval env val@(Port _) = return val
eval env val@(Continuation _) = return val
eval env (Atom id) = getVar env id
eval env (List (function : args)) = do
  func <- eval env function
  apply env func args
eval _ badForm = lift . throwE $ BadSpecialForm "Unrecognized special form" badForm

apply :: Env -> LispVal -> [LispVal] -> LispEval
apply env (SpecialForm func) args = func env args
apply env (Continuation k) [arg] = eval env arg >>= k
apply env (PrimitiveFunc func) args = do
  argVals <- mapM (eval env) args
  lift $ liftThrows $ func argVals
apply env (IOFunc func) args = do
  argVals <- mapM (eval env) args
  func env argVals
apply env (Func params varargs body closure) args =
  if num params /= num args && isNothing varargs
    then lift $ throwE $ NumArgs (num params) args
    else do
      argVals <- mapM (eval env) args
      let remainingArgs = drop (length params) argVals
      liftIO (bindVars closure $ zip params argVals) >>= bindVarArgs remainingArgs varargs >>= evalBody
  where num = toInteger . length
        -- evalBody env = last <$> mapM (trace ("eval function body " ++ show body) (eval env)) body
        evalBody env = last <$> mapM (eval env) body
        bindVarArgs remainingArgs arg env = case arg of
          Just argName -> liftIO $ bindVars env [(argName, List remainingArgs)]
          Nothing -> return env
apply _ notfunc _ = lift . throwE $ BadSpecialForm "Unrecognized function form" notfunc
