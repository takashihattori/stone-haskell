module BasicEvaluator where
import Control.Monad.Error
import Lexer
import BasicParser

data Val = ValInt Int |
           ValStr String
         deriving (Eq, Read, Show)
                  
type Env = [(String, Val)]

eval :: Env -> ASTree -> StoneMonad (Env, Val)
eval e (ASLeaf (TokenNum _ n)) = return (e, ValInt n)
eval e (ASLeaf (TokenStr _ s)) = return (e, ValStr s)
eval e (ASLeaf (TokenID line var)) =
  case (lookup var e) of
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
eval e t = throwError (Err 0 ("Unimplemented" ++ (show t)))
            
assign :: Env -> String -> Val -> Env
assign [] var val = [(var, val)]
assign ((var1, val1):env) var2 val2 =
  if var1 == var2
  then (var2, val2):env
  else (var1, val1):(assign env var2 val2)

toString :: Val -> String
toString (ValStr s) = s
toString (ValInt n) = show n
