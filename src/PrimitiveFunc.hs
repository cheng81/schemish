{-# LANGUAGE TemplateHaskell #-}

module PrimitiveFunc
    ( primitives
    , eqv -- needed elsewhere also
    ) where

import           Control.Lens         hiding (List, cons, noneOf)
import           Control.Lens.Extras  (is)
import           Control.Monad.Except
import           Types

makePrisms ''LispVal

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

--
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
