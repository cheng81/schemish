module SpecialForm
    ( specialForms
    ) where

import           Control.Monad.Except
import           Env
import           Eval                 (apply, eval)
import           PrimitiveFunc        (eqv)
import           Types

specialForms :: [(String, Env -> [LispVal] -> IOThrowsError LispVal)]
specialForms = [("quote", quote),
                ("if", ifFrm),
                ("cond", cond),
                ("case", caseFrm),
                ("set!", setFrm),
                ("define", defFrm),
                ("lambda", lambdaFrm)]

caseFrm :: Env -> [LispVal] -> IOThrowsError LispVal
caseFrm env (key : clauses) =
  do keyEvaled <- eval env key
     lispCase env keyEvaled clauses

setFrm env [Atom var, form] = eval env form >>= setVar env var
setFrm _ args = throwError $ NumArgs 2 args

defFrm env [Atom var, form] = eval env form >>= defineVar env var
defFrm env (List (Atom var : params) : body) =
  makeNormalFunc env params body >>= defineVar env var
defFrm env (DottedList (Atom var : params) varargs : body) =
  makeVarArgs varargs env params body >>= defineVar env var

lambdaFrm env (List params : body) =
  makeNormalFunc env params body
lambdaFrm env (DottedList params varargs : body) =
  makeVarArgs varargs env params body
lambdaFrm env (varargs@(Atom _) : body) =
  makeVarArgs varargs env [] body

quote :: Env -> [LispVal] -> IOThrowsError LispVal
quote _ [val] = return val
quote _ args  = throwError $ NumArgs 1 args

ifFrm :: Env -> [LispVal] -> IOThrowsError LispVal
ifFrm env [pred, conseq, alt] =
  do result <- eval env pred
     case result of
       Bool False -> eval env alt
       Bool True -> eval env conseq
       _ -> throwError $ TypeMismatch "boolean" result
ifFrm _ args = throwError $ NumArgs 3 args

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

makeFunc :: Maybe String -> Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeFunc varargs env params body = return $ Func (map show params) varargs body env
makeNormalFunc = makeFunc Nothing
makeVarArgs = makeFunc . Just . show