module FuncEvaluator where
import Control.Monad.Error
import Lexer
import FuncParser

data Val = ValInt Int |
           ValStr String |
           ValFun Env [Token] ASTree
         deriving (Eq, Read, Show)
                  
type Env = [[(String, Val)]]

eval :: Env -> ASTree -> StoneMonad (Env, Val)
eval e (ASLeaf (TokenNum _ n)) = return (e, ValInt n)
eval e (ASLeaf (TokenStr _ s)) = return (e, ValStr s)
eval e (ASLeaf (TokenID line var)) =
  case (envLookup var e) of
    Just val -> return (e, val)
    otherwise -> throwError (Err line ("Undefined variable " ++ var))
eval e1 (UnaryOp (TokenPunc line "-") primary) =
  do (e2, val) <- eval e1 primary
     case val of
       (ValInt n) -> return (e2, ValInt (-n))
       otherwise -> throwError (Err line "Type mismatch")
eval e1 (BinaryOp (TokenPunc line "+") left right) =
  do (e2, valL) <- eval e1 left
     (e3, valR) <- eval e2 right
     case (valL, valR) of
       (ValInt nL, ValInt nR) -> return (e3, ValInt (nL + nR))
       otherwise -> return (e3, ValStr (toString valL ++ (toString valR)))
eval e1 (BinaryOp (TokenPunc line "=") left right) =
  case left of
    ASLeaf (TokenID _ var) ->
      do (e2, val) <- eval e1 right
         return (assign e2 var val, val)
    otherwise -> throwError (Err line "Bad assignment")
eval e1 (BinaryOp (TokenPunc line "==") left right) =
    do (e2, valL) <- eval e1 left
       (e3, valR) <- eval e2 right
       return (e3, ValInt (if valL == valR then 1 else 0))
eval e1 (BinaryOp (TokenPunc line op) left right)
  | elem op ["-", "*", "/", "%", "<", "<=", ">", ">="] =
    do (e2, valL) <- eval e1 left
       (e3, valR) <- eval e2 right
       case (valL, valR) of
         (ValInt nL, ValInt nR) ->
           return (e3, ValInt (case op of
                                  "-" -> nL - nR
                                  "*" -> nL * nR
                                  "/" -> nL `div` nR
                                  "%" -> nL `mod` nR
                                  "<" -> if nL < nR then 1 else 0
                                  "<=" -> if nL <= nR then 1 else 0
                                  ">" -> if nL > nR then 1 else 0
                                  ">=" -> if nL >= nR then 1 else 0))
         otherwise -> throwError (Err line "Type mismatch")
eval e (EmptyState) = return (e, ValInt 0)
eval e1 (SeqState left right) =
  do (e2, valL) <- eval e1 left
     (e3, valR) <- eval e2 right
     return (e3, valR)
eval e1 (IfState cond thenBlock elseBlock) =
  do (e2, valCond) <- eval e1 cond
     case valCond of
       ValInt 0 -> eval e2 elseBlock
       otherwise -> eval e2 thenBlock
eval e1 (WhileState cond block) =
  do (e2, valCond) <- eval e1 cond
     case valCond of
       ValInt 0 -> return (e2, ValInt 0)
       otherwise -> do (e3, valBlock) <- eval e2 block
                       eval e3 (WhileState cond block)
eval e1 (FuncDef (TokenID {name = n}) params body) =
  let f = ValFun e1 params body in
  return (assign e1 n f, f)
eval e1 (FuncApply func args) =
  do (e2, f) <- eval e1 func
     case f of
       ValFun funenv params body ->
         let (e3, argvals) = evalArg e2 args
             bindings = makeBindings params argvals
         in do (_, val) <- eval (bindings:funenv) body
               return (e3, val)
       otherwise -> throwError (Err 0 ("Not a function: " ++ (show func)))
  where
    evalArg e [] = (e, [])
    evalArg e1 (a:args) =
      let (e2, vals) = evalArg e1 args in
      case eval e2 a of
        Right (e3, val) -> (e3, val:vals)
    makeBindings [] [] = []
    makeBindings ((TokenID {name = n}):params) (v:vals) =
      (n, v) : makeBindings params vals
eval e t = throwError (Err 0 ("Unimplemented " ++ (show t)))
            
envLookup :: String -> Env -> Maybe Val
envLookup var [] = Nothing
envLookup var (bindings:e) =
  case (lookup var bindings) of
    Just val -> Just val
    otherwise -> envLookup var e

assign :: Env -> String -> Val -> Env
assign e var val =
  case (envLookup var e) of
    Just _ -> assign' e var val
    otherwise -> putBinding e var val
  where
    assign' (bindings:e) var val =
      case (lookup var bindings) of
        Just _ -> (assign'' bindings var val):e
        Nothing -> bindings:(assign' e var val)
    assign'' ((var1, val1):bindings) var2 val2 =
      if var1 == var2
      then (var1, val2):bindings
      else (var1, val1):(assign'' bindings var2 val2)

putBinding :: Env -> String -> Val -> Env
putBinding (top:e) var val =
  ((var, val):top):e

toString :: Val -> String
toString (ValStr s) = s
toString (ValInt n) = show n
