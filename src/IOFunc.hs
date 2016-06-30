module IOFunc
    ( ioPrimitives
    ) where

import           Control.Monad.Trans
import           Eval                (apply, eval)
import           Lib                 (liftThrows)
import           Parser              (readExpr, readExprList)
import           System.IO
import           Types

discardEnv :: ([LispVal] -> IOThrowsError LispVal) -> (Env -> [LispVal] -> IOThrowsError LispVal)
discardEnv func _ = func

ioPrimitives :: [(String, Env -> [LispVal] -> IOThrowsError LispVal)]
ioPrimitives = [("apply", applyProc),
                ("open-input-file", discardEnv $ makePort ReadMode),
                ("open-output-file", discardEnv $ makePort WriteMode),
                ("close-input-port", discardEnv closePort),
                ("close-output-port", discardEnv closePort),
                ("read", discardEnv readProc),
                ("write", discardEnv writeProc),
                ("read-contents", discardEnv readContents),
                ("read-all", discardEnv readAll),
                ("load", loadFrm)]

loadFrm env [String filename] = load filename >>= fmap last . mapM (eval env)

applyProc :: Env -> [LispVal] -> IOThrowsError LispVal
applyProc env [func, List args] = apply env func args
applyProc env (func : args)     = apply env func args

makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [String filename] = fmap Port $ liftIO $ openFile filename mode

closePort :: [LispVal] -> IOThrowsError LispVal
closePort [Port port] = liftIO $ hClose port >> return (Bool True)
closePort _           = return $ Bool False

readProc :: [LispVal] -> IOThrowsError LispVal
readProc []          = readProc [Port stdin]
readProc [Port port] = liftIO (hGetLine port) >>= liftThrows . readExpr

writeProc :: [LispVal] -> IOThrowsError LispVal
writeProc [obj]            = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO $ hPrint port obj >> return (Bool True)

readContents :: [LispVal] -> IOThrowsError LispVal
readContents [String filename] = fmap String $ liftIO $ readFile filename

load :: String -> IOThrowsError [LispVal]
load filename = liftIO (readFile filename) >>= liftThrows . readExprList

readAll :: [LispVal] -> IOThrowsError LispVal
readAll [String filename] = List <$> load filename
