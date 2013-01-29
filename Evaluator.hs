module Evaluator where
import Control.Monad.Error
import Lexer
import Parser

data Val = ValInt Int |
           ValStr String |
           ValFun [Token] ASTree |
           ValExit ExitTag Val 
         deriving (Eq)

data ExitTag = ExitFunc | ExitLoop
             deriving (Eq)

instance Show Val where
  show (ValInt num) = show num
  show (ValStr str) = show str
  show (ValFun arg body) = "<<func " ++ (show body) ++ " >>"
  show (ValExit ExitFunc val) = "<<return " ++ (show val) ++ " >>"
  show (ValExit ExitLoop val) = "<<exit from loop >>"
                  
type Env = [ (String, Val) ]

emptyEnv :: Env
emptyEnv = []

eval :: Env -> ASTree -> StoneMonad (Env, Val)
eval e (ASLeaf (TokenNum _ n)) = return (e, ValInt n)
eval e (ASLeaf (TokenStr _ s)) = return (e, ValStr s)
eval e (ASLeaf (TokenID line name)) =
  case (lookup name e) of
    Just val -> return (e, val)
    otherwise -> throwError (Err line ("Undefined variable " ++ name))
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
       otherwise -> return (e3, ValStr (show valL ++ (show valR)))
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
     case valL of
       (ValExit _ v) -> return (e2, valL)
       otherwise -> do (e3, valR) <- eval e2 right
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
                       case valBlock of
                         ValExit ExitFunc _ -> return (e3, valBlock)
                         ValExit ExitLoop v -> return (e3, v)
                         otherwise -> eval e3 (WhileState cond block)
eval e1 (ReturnState expr) =
  do (e2, val) <- eval e1 expr
     return (e2, ValExit ExitFunc val)
eval e (FuncDef (TokenID {name = n}) params body) =
  let f = ValFun params body in
  return (assign e n f, f)
eval e1 (FuncApply func args) =
  do (e2, f) <- eval e1 func
     case f of
       ValFun params body ->
         let (e3, argvals) = evalArg e2 args
             bindings = makeBindings params argvals
         in do (e4, v1) <- eval (bindings ++ e3) body
               let e5 = copyBack e3 bindings e4 in
                 case v1 of
                   ValExit ExitFunc v2 -> return (e5, v2)
                   ValExit ExitLoop _ -> throwError (Err 0 "Break or continue not in loop")
                   otherwise -> return (e5, v1)
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
    copyBack [] _ _ = []
    copyBack ((name, orig) : bs) formal result =
      case (lookup name formal) of
        Just _ -> (name, orig) : (copyBack bs formal result)
        Nothing ->
          case (lookup name result) of
            Just new -> (name, new) : (copyBack bs formal result)
            Nothing -> (name, orig) : (copyBack bs formal result)
eval e t = throwError (Err 0 ("Unimplemented " ++ (show t)))

assign :: Env -> String -> Val -> Env
assign [] name val = [ (name, val) ]
assign ((name1,val1):bs) name2 val2 =
  if name1 == name2
  then (name1,val2):bs
  else (name1,val1):(assign bs name2 val2)
