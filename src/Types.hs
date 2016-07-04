{-# LANGUAGE ExistentialQuantification #-}

module Types
    (
     Env
    ,ThrowsError
    ,IOThrowsError
    ,LispEval
    ,LispError(..)
    ,LispVal(..)
    ,Unpacker(..)
    ) where

import           Control.Lens                  hiding (List, cons, noneOf)
import           Control.Lens                  (makePrisms)
import           Control.Monad.Trans.Cont
--import           Control.Monad.Trans.Either
import           Control.Monad.Trans.Except
import           Data.IORef
import           System.IO                     (Handle)
import           Text.ParserCombinators.Parsec (ParseError)

type Env = IORef [(String, IORef LispVal)]
type ThrowsError = Either LispError
type IOThrowsError = ExceptT LispError IO

--type LispEval = ContT LispVal IOThrowsError LispVal
type LispEval = ContT LispVal (ExceptT LispError IO) LispVal

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | Float Double
             | String String
             | Char Char
             | Bool Bool
             | Continuation (LispVal -> LispEval)
             | SpecialForm (Env -> [LispVal] -> LispEval)
             | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
             | Func {params :: [String], vararg :: Maybe String,
                     body :: [LispVal], closure :: Env}
             | IOFunc (Env -> [LispVal] -> LispEval)
             | Port Handle
             -- deriving (Show)

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

instance Show LispError where
  show = showError

instance Show LispVal where
  show = showVal

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Char contents) = "'" ++ [contents] ++ "'"
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Float contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"
showVal (PrimitiveFunc _) = "<primitive>"
showVal (Continuation _) = "<continuation>"
showVal Func {params = args, vararg = varargs, body = body, closure = env} =
  "(lambda (" ++ unwords (map show args) ++
  case varargs of
    Nothing -> ") ...)"
    Just arg -> " . " ++ arg ++ ") ...)"
showVal (Port _) = "<IO port>"
showVal (IOFunc _) = "<IO primitive>"

showError :: LispError -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (NumArgs expected found) = "Expected " ++ show expected ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected ++ ", found " ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal
