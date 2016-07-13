module IOFunc
    ( ioPrimitives
    ) where

import           Control.Applicative ((<$>))
import           Control.Monad.Trans
import           Env                 (bindVars)
import           Eval                (apply, eval)
import           Lib                 (liftThrows)
import           Parser              (readExpr, readExprList)
import           System.IO
import           Types

discardEnv :: ([LispVal] -> LispEval) -> (Env -> [LispVal] -> LispEval)
discardEnv func _ = func

ioPrimitives :: [(String, Env -> [LispVal] -> LispEval)]
ioPrimitives = [("apply", applyProc),
                ("open-input-file", discardEnv $ makePort ReadMode),
                ("open-output-file", discardEnv $ makePort WriteMode),
                ("close-input-port", discardEnv closePort),
                ("close-output-port", discardEnv closePort),
                ("read", discardEnv readProc),
                ("write", discardEnv writeProc),
                ("read-contents", discardEnv readContents),
                ("read-all", discardEnv readAll),
                ("load", loadFrm),
                ("display", displayFrm)]

displayFrm :: Env -> [LispVal] -> LispEval
displayFrm env [arg] = do
  _ <- liftIO $ print arg
  return Unit

loadFrm :: Env -> [LispVal] -> LispEval
loadFrm env [String filename] = lift (load filename) >>= fmap last . mapM (eval env)

applyProc :: Env -> [LispVal] -> LispEval
applyProc env [func, List args] = apply env func args
applyProc env (func : args)     = apply env func args

makePort :: IOMode -> [LispVal] -> LispEval
makePort mode [String filename] = fmap Port $ liftIO $ openFile filename mode

closePort :: [LispVal] -> LispEval
closePort [Port port] = liftIO $ hClose port >> return (Bool True)
closePort _           = return $ Bool False

readProc :: [LispVal] -> LispEval
readProc []          = readProc [Port stdin]
readProc [Port port] = liftIO (hGetLine port) >>= (lift . liftThrows) . readExpr

writeProc :: [LispVal] -> LispEval
writeProc [obj]            = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO $ hPrint port obj >> return (Bool True)

readContents :: [LispVal] -> LispEval
readContents [String filename] = fmap String $ liftIO $ readFile filename

load :: String -> IOThrowsError [LispVal]
load filename = liftIO (readFile filename) >>= liftThrows . readExprList

readAll :: [LispVal] -> LispEval
readAll [String filename] = List <$> lift (load filename)
