{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell           #-}

module Eval
    ( eval
    , primitiveBindings
    ) where

import           Control.Lens         hiding (List, cons, noneOf)
import           Control.Lens.Extras  (is)
import           Control.Monad.Except
import           Data.IORef
import           Data.Maybe           (fromMaybe, isJust, isNothing)
import           Env
import           Lib                  (liftThrows)
import           Parser               (readExpr, readExprList)
import           System.IO
import           Types

makePrisms ''LispVal

primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= flip bindVars (map (makeFunc PrimitiveFunc) primitives
                                             ++map (makeFunc IOFunc) ioPrimitives)
  where makeFunc constructor (var, func) = (var, constructor func)

eval :: Env -> LispVal -> IOThrowsError LispVal
-- eval env val@(Atom _) = return val
eval env val@(String _) = return val
eval env val@(Char _) = return val
eval env val@(Number _) = return val
eval env val@(Float _) = return val
eval env val@(Bool _) = return val
eval env (Atom id) = getVar env id
eval env (List [Atom "quote", val]) = return val
eval env (List [Atom "if", pred, conseq, alt]) =
  do result <- eval env pred
     case result of
       Bool False -> eval env alt
       Bool True -> eval env conseq
       _ -> throwError $ TypeMismatch "boolean" result
eval env (List (Atom "cond" : clauses)) = cond env clauses
eval env (List (Atom "case" : key : clauses)) = -- lispCase (eval test) clauses
  do keyEvaled <- eval env key
     lispCase env keyEvaled clauses
eval env (List [Atom "set!", Atom var, form]) = eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) = eval env form >>= defineVar env var
eval env (List (Atom "define" : List (Atom var : params) : body)) =
  makeNormalFunc env params body >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : params) varargs : body)) =
  makeVarArgs varargs env params body >>= defineVar env var
eval env (List (Atom "lambda" : List params : body)) =
  makeNormalFunc env params body
eval env (List (Atom "lambda" : DottedList params varargs : body)) =
  makeVarArgs varargs env params body
eval env (List (Atom "lambda" : varargs@(Atom _) : body)) =
  makeVarArgs varargs env [] body
eval env (List [Atom "load", String filename]) =
  load filename >>= fmap last . mapM (eval env)
-- eval env (List (Atom func : args)) = mapM (eval env) args >>= liftThrows . apply func -- apply func $ map eval args
eval env (List (function : args)) = do
  func <- eval env function
  argVals <- mapM (eval env) args
  apply func argVals
eval _ badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

makeFunc :: Maybe String -> Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeFunc varargs env params body = return $ Func (map show params) varargs body env
makeNormalFunc = makeFunc Nothing
makeVarArgs = makeFunc . Just . show

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (IOFunc func) args = func args
apply (Func params varargs body closure) args =
  if num params /= num args && isNothing varargs
    then throwError $ NumArgs (num params) args
    else liftIO (bindVars closure $ zip params args) >>= bindVarArgs varargs >>= evalBody
  where remainingArgs = drop (length params) args
        num = toInteger . length
        -- evalBody env = last <$> mapM (trace ("eval function body " ++ show body) (eval env)) body
        evalBody env = last <$> mapM (eval env) body
        bindVarArgs arg env = case arg of
          Just argName -> liftIO $ bindVars env [(argName, List remainingArgs)]
          Nothing -> return env

applyProc :: [LispVal] -> IOThrowsError LispVal
applyProc [func, List args] = apply func args
applyProc (func : args)     = apply func args

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

car :: [LispVal] -> ThrowsError LispVal
car [List (x : _)]         = return x
car [DottedList (x : _) _] = return x
car [badArg]               = throwError $ TypeMismatch "pair" badArg
car badArgList             = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (_ : xs)]         = return $ List xs
cdr [DottedList [_] x]      = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [badArg]                = throwError $ TypeMismatch "pair" badArg
cdr badArgList              = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []] = return $ List [x1]
cons [x, List xs] = return $ List $ x : xs
cons [x, DottedList xs xlast] = return $ DottedList (x : xs) xlast
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList

eqv :: [LispVal] -> ThrowsError LispVal
eqv [Bool arg1, Bool arg2]             = return $ Bool $ arg1 == arg2
eqv [Number arg1, Number arg2]         = return $ Bool $ arg1 == arg2
eqv [Float arg1, Float arg2]           = return $ Bool $ arg1 == arg2
eqv [Atom arg1, Atom arg2]             = return $ Bool $ arg1 == arg2
eqv [String arg1, String arg2]         = return $ Bool $ arg1 == arg2
eqv [Char arg1, Char arg2]             = return $ Bool $ arg1 == arg2
eqv [DottedList xs x, DottedList ys y] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [List arg1, List arg2]             = return $ Bool $ (length arg1 == length arg2) && all eqvPair (zip arg1 arg2)
    where
      eqvPair (x1, x2) = case eqv [x1, x2] of
        Left _ -> False
        Right (Bool val) -> val
eqv [_, _]                             = return $ Bool False
eqv badArgList                         = throwError $ NumArgs 2 badArgList

equal :: [LispVal] -> ThrowsError LispVal
equal [arg1, arg2] = do
  primitiveEquals <- or <$> mapM (unpackEquals arg1 arg2)
                     [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
  eqvEquals <- eqv [arg1, arg2]
  return $ Bool (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) =
  do unpacked1 <- unpacker arg1
     unpacked2 <- unpacker arg2
     return $ unpacked1 == unpacked2
  `catchError` const (return False)

stringSymbol :: [LispVal] -> ThrowsError LispVal
stringSymbol [] = throwError $ NumArgs 1 []
stringSymbol (String name : _) = return $ Atom name
stringSymbol (notString : _) = throwError $ TypeMismatch "string" notString

symbolString :: [LispVal] -> ThrowsError LispVal
symbolString [] = throwError $ NumArgs 1 []
symbolString (Atom name : _) = return $ String name
symbolString (notAtom : _) = throwError $ TypeMismatch "symbol" notAtom

uniparamBool :: (LispVal -> Bool) -> [LispVal] -> ThrowsError LispVal
uniparamBool _ [] = throwError $ NumArgs 1 [] -- return $ Bool False
uniparamBool fn (arg : _) = return $ Bool $ fn arg

boolBinOp :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinOp unpacker op args = if length args /= 2
                             then throwError $ NumArgs 2 args
                             else do left <- unpacker $ head args
                                     right <- unpacker $ args !! 1
                                     return $ Bool $ left `op` right

numBoolBinop = boolBinOp unpackNum
strBoolBinop = boolBinOp unpackStr
boolBoolBinop = boolBinOp unpackBool

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr notString = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool

numericBinOp :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinOp op [] = throwError $ NumArgs 2 []
numericBinOp op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinOp op params = fmap (Number . foldl1 op) (mapM unpackNum params) -- mapM unpackNum params >>= return . Number . foldl1 op -- Number $ foldl1 op $ map unpackNum params

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
-- unpackNum (String n) = let parsed = reads n :: [(Integer, String)] in
--                             if null parsed
--                               then 0
--                               else fst $ head parsed
-- unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

ioPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives = [("apply", applyProc),
                ("open-input-file", makePort ReadMode),
                ("open-output-file", makePort WriteMode),
                ("close-input-port", closePort),
                ("close-output-port", closePort),
                ("read", readProc),
                ("write", writeProc),
                ("read-contents", readContents),
                ("read-all", readAll)]

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinOp (+)),
              ("-", numericBinOp (-)),
              ("*", numericBinOp (*)),
              ("/", numericBinOp div),
              ("mod", numericBinOp mod),
              ("quotient", numericBinOp quot),
              ("remainder", numericBinOp rem),
              ("number?", uniparamBool (is _Number)),
              ("float?", uniparamBool (is _Float)),
              ("symbol?", uniparamBool (is _Atom)),
              ("string?", uniparamBool (is _String)),
              ("char?", uniparamBool (is _Char)),
              ("bool?", uniparamBool (is _Bool)),
              ("list?", uniparamBool (is _List)),
              ("dottedlist?", uniparamBool (is _DottedList)),
              ("symbol->string", symbolString),
              ("string->symbol", stringSymbol),
              ("=", numBoolBinop (==)),
              ("<", numBoolBinop (<)),
              (">", numBoolBinop (>)),
              ("/=", numBoolBinop (/=)),
              (">=", numBoolBinop (>=)),
              ("<=", numBoolBinop (<=)),
              ("&&", boolBoolBinop (&&)),
              ("||", boolBoolBinop (||)),
              ("string=?", strBoolBinop (==)),
              ("string<?", strBoolBinop (<)),
              ("string>?", strBoolBinop (>)),
              ("string<=?", strBoolBinop (<=)),
              ("string>=?", strBoolBinop (>=)),
              ("car", car),
              ("cdr", cdr),
              ("cons", cons),
              ("eq?", eqv),
              ("eqv?", eqv),
              ("equal?", equal)]

lispCase :: Env -> LispVal -> [LispVal] -> IOThrowsError LispVal
lispCase _ _key [] = throwError $ BadSpecialForm "case" $ List []
lispCase env _key [List [Atom "else", expr]] = eval env expr
lispCase env key (List [List datums, expr] : clauses) = do
  isCase <- lispCaseDatum env key datums
  if isCase
    then eval env expr
    else lispCase env key clauses

lispCaseDatum :: Env -> LispVal -> [LispVal] -> IOThrowsError Bool
lispCaseDatum _ _ [] = return False
lispCaseDatum env key (x : xs) =
  do xEvaled <- eval env x
     case eqv [key, xEvaled] of
       Right (Bool True) -> return True
       Right (Bool False) -> lispCaseDatum env key xs

cond :: Env -> [LispVal] -> IOThrowsError LispVal
cond _ [] = throwError $ BadSpecialForm "cond" $ List []
cond env [List [Atom "else", expr]] = eval env expr
cond env (List [test, expr] : clauses) =
  do testRes <- eval env test
     case testRes of
       Bool False -> cond env clauses
       Bool True -> eval env expr
       _ -> throwError $ TypeMismatch "boolean" testRes
