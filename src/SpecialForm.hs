module SpecialForm
    ( specialForms
    ) where

import           Control.Applicative      ((<$>))
import           Control.Monad.Except
import           Control.Monad.Trans.Cont
import           Env
import           Eval                     (apply, eval)
import           PrimitiveFunc            (eqv)
import           Types

specialForms :: [(String, Env -> [LispVal] -> LispEval)]
specialForms = [("quote", quote),
                ("if", ifFrm),
                ("cond", cond),
                ("case", caseFrm),
                ("set!", setFrm),
                ("define", defFrm),
                ("lambda", lambdaFrm),
                ("call-with-current-continuation", callCCFrm),
                ("begin", seqFrm)]

seqFrm env stmts = last <$> mapM (eval env) stmts

callCCFrm :: Env -> [LispVal] -> LispEval
callCCFrm env [function] =
  callCC $ \k -> do func <- eval env function
                    apply env func [Continuation k]

caseFrm :: Env -> [LispVal] -> LispEval
caseFrm env (key : clauses) =
  do keyEvaled <- eval env key
     lispCase env keyEvaled clauses

setFrm env [Atom var, form] = eval env form >>= setVar env var
setFrm _ args = lift $ throwError $ NumArgs 2 args

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

quote :: Env -> [LispVal] -> LispEval
quote _ [val] = return val
quote _ args  = lift $ throwError $ NumArgs 1 args

ifFrm :: Env -> [LispVal] -> LispEval
ifFrm env [pred, conseq, alt] =
  do result <- eval env pred
     case result of
       Bool False -> eval env alt
       Bool True -> eval env conseq
       _ -> lift $ throwError $ TypeMismatch "boolean" result
ifFrm _ args = lift $ throwError $ NumArgs 3 args

lispCase :: Env -> LispVal -> [LispVal] -> LispEval
lispCase _ _key [] = lift . throwError $ BadSpecialForm "case" $ List []
lispCase env _key [List (Atom "else" : exprs)] = last <$> mapM (eval env) exprs
lispCase env key (List (List datums : exprs) : clauses) = do
  isCase <- lispCaseDatum env key datums
  case isCase of
    Bool True -> last <$> mapM (eval env) exprs
    Bool False -> lispCase env key clauses
  -- if isCase
  --   then last <$> mapM (eval env) exprs
  --   else lispCase env key clauses

lispCaseDatum :: Env -> LispVal -> [LispVal] -> LispEval
lispCaseDatum _ _ [] = return (Bool False)
lispCaseDatum env key (x : xs) =
  do xEvaled <- eval env x
     case eqv [key, xEvaled] of
       Right (Bool True) -> return (Bool True)
       Right (Bool False) -> lispCaseDatum env key xs

cond :: Env -> [LispVal] -> LispEval
cond _ [] = lift . throwError $ BadSpecialForm "cond" $ List []
cond env [List (Atom "else" : exprs)] = last <$> mapM (eval env) exprs
cond env (List (test : exprs) : clauses) =
  do testRes <- eval env test
     case testRes of
       Bool False -> cond env clauses
       Bool True -> last <$> mapM (eval env) exprs
       _ -> lift . throwError $ TypeMismatch "boolean" testRes

makeFunc :: Maybe String -> Env -> [LispVal] -> [LispVal] -> LispEval
makeFunc varargs env params body = return $ Func (map show params) varargs body env
makeNormalFunc = makeFunc Nothing
makeVarArgs = makeFunc . Just . show
