module Parser where
import Control.Monad.Error
import Lexer

data ASTree = ASLeaf Token |
              EmptyState |
              SeqState { left::ASTree , right::ASTree } |
              IfState { cond::ASTree , thenblock::ASTree , elseblock::ASTree } |
              WhileState { cond::ASTree , body::ASTree } |
              UnaryOp { op::Token , primary::ASTree } |
              BinaryOp { op::Token , left::ASTree , right::ASTree } |
              FuncDef { fname::Token , params::[Token], body::ASTree } |
              FuncApply { func::ASTree , args::[ASTree] }
            deriving (Eq, Read, Show)

data Assoc = AssocRight | AssocLeft

precedence = [
  ("=", 1), 
  ("==", 2), (">", 2),  ("<", 2), (">=", 2),  ("<=", 2),
  ("+", 3), ("-", 3), 
  ("*", 4), ("/", 4), ("%", 4) ]

associativity = [
  (1, AssocRight),
  (2, AssocLeft),
  (3, AssocLeft),
  (4, AssocLeft) ]

data StoneError = Err { errLine::Int , message::String }

instance Error StoneError where
  noMsg = Err 0 "Unknown Error"
  strMsg s = Err 0 s

instance Show StoneError where
  show e = "In line " ++ (show $ errLine e) ++ ", " ++ (message e)
  
type StoneMonad = Either StoneError

parseProgram :: [Token] -> StoneMonad (ASTree, [Token])
parseProgram (TokenEOF _:ts) = return (EmptyState, [])
parseProgram (TokenEOL _:ts) = parseProgram ts
parseProgram (TokenPunc _ ";":ts) = parseProgram ts
parseProgram ts =
  do (tree1, t1:ts1) <- parseStatement ts
     case t1 of
       _ | isSentenceEnd t1 ->
         do (tree2, ts2) <- parseProgram ts1
            case tree2 of
              EmptyState -> return (tree1, ts1)
              otherwise -> return (SeqState tree1 tree2, ts2)
       TokenEOF _ -> return (tree1, [])
       otherwise -> throwError (Err (lineNum t1) ("Unexpected " ++ (show t1)))

parseDef :: [Token] -> StoneMonad (ASTree, [Token])
parseDef (TokenEOL _:ts) = parseDef ts
parseDef (t:ts) =
  case t of
    TokenID _ f -> do (params, ts1) <- parseParams ts
                      (block, ts2) <- parseBlock ts1
                      return (FuncDef t params block, ts2)
    otherwise -> throwError (Err (lineNum t) "No function name")

parseParams :: [Token] -> StoneMonad ([Token], [Token])
parseParams (TokenEOL _:ts) = parseParams ts
parseParams (t:ts) =
  case t of
    TokenPunc _ "(" ->
      do (params, t1:ts1) <- parseParams' [] ts
         case t1 of
           TokenPunc _ ")" -> return (params, ts1)
           otherwise -> throwError (Err (lineNum t1) "Invalid parameters")
    otherwise -> throwError (Err (lineNum t) "No parameters")
  
parseParams' :: [Token] -> [Token] -> StoneMonad ([Token], [Token])
parseParams' ps (TokenEOL _:ts) = parseParams' ps ts
parseParams' ps (t:ts) =
  case t of
    TokenID _ _ ->
      let ps' = ps ++ [t] in
      case ts of
        (TokenPunc _ ",":ts') -> parseParams' ps' ts'
        otherwise -> return (ps', ts)
    otherwise -> return (ps, t:ts)

parseStatement :: [Token] -> StoneMonad (ASTree, [Token])
parseStatement (TokenEOL _:ts) = parseStatement ts
parseStatement (TokenID _ "if":ts1) =
  do (tree1, ts2) <- parseExpr ts1
     (tree2, t2:ts3) <- parseBlock ts2
     case t2 of
       TokenID _ "else" ->
         do (tree3, ts4) <- parseBlock ts3
            return (IfState tree1 tree2 tree3, ts4)
       otherwise ->
         return (IfState tree1 tree2 EmptyState, (t2:ts3))
parseStatement (TokenID _ "while":ts1) =
  do (tree1, ts2) <- parseExpr ts1
     (tree2, ts3) <- parseBlock ts2
     return (WhileState tree1 tree2, ts3)
parseStatement (TokenID _ "結界":ts) = parseDef ts
parseStatement ts = parseSimple ts

parseSimple :: [Token] -> StoneMonad (ASTree, [Token])
parseSimple (TokenEOL _:ts) = parseSimple ts
parseSimple ts =
  do (tree, t1:ts1) <- parseExpr ts
     case t1 of
       _ | maybeArg t1 -> do (args, t2:ts2) <- parseArgList [] (t1:ts1)
                             return (FuncApply tree args, ts2)
       otherwise -> return (tree, t1:ts1)
  where
    maybeArg (TokenID _ _) = True
    maybeArg (TokenNum _ _) = True
    maybeArg (TokenStr _ _) = True
    maybeArg t = False

parseBlock :: [Token] -> StoneMonad (ASTree, [Token])
parseBlock (TokenEOL _:ts) = parseBlock ts
parseBlock (TokenPunc _ "{":ts1) =
  do (tree1, t:ts2) <- parseBlock' ts1
     case t of
       TokenPunc _ "}" -> return (tree1, ts2)
       otherwise -> throwError (Err (lineNum t) "Missing '}'")
parseBlock (t:_) = throwError (Err (lineNum t) "Block must begin with '{'")

parseBlock' :: [Token] -> StoneMonad (ASTree, [Token])
parseBlock' (TokenPunc line "}":ts) = return (EmptyState, TokenPunc line "}":ts)
parseBlock' (TokenEOL _:ts) = parseBlock' ts
parseBlock' (TokenPunc _ ";":ts) = parseBlock' ts
parseBlock' ts =
  do (tree1, t:ts1) <- parseStatement ts
     case t of
       _ | isSentenceEnd t ->
         do (tree2, ts2) <- parseBlock' ts1
            case tree2 of
              EmptyState -> return (tree1, ts1)
              otherwise -> return (SeqState tree1 tree2, ts2)
       otherwise -> return (tree1, t:ts1)
                   
parseExpr :: [Token] -> StoneMonad (ASTree, [Token])
parseExpr (TokenEOL _:ts) = parseExpr ts
parseExpr ts1 =
  do (tree, ts2) <- parseFactor ts1
     parseExpr' tree ts2

parseExpr' :: ASTree -> [Token] -> StoneMonad (ASTree, [Token])
parseExpr' tree1 (t1:ts1) =
  case isOperator t1 of
    Just p1 ->
      do (tree2, t2:ts2) <- parseFactor ts1
         (tree3, ts3) <- parseExpr'' tree1 t1 p1 tree2 (t2:ts2)
         parseExpr' tree3 ts3
    otherwise -> return (tree1, t1:ts1)

parseExpr'' :: ASTree -> Token -> Int -> ASTree -> [Token] ->
               StoneMonad (ASTree, [Token])
parseExpr'' tree1 t1 p1 tree2 (t2:ts2) =
  case isOperator t2 of
    Just p2 ->
      if needReduce p1 p2
      then doReduce tree1 t1 tree2 (t2:ts2)
      else do (tree3, t3:ts3) <- parseExpr' tree2 (t2:ts2)
              parseExpr'' tree1 t1 p1 tree3 (t3:ts3)
    otherwise -> doReduce tree1 t1 tree2 (t2:ts2)
  where
    needReduce p1 p2
      | p1 > p2 = True
      | p1 < p2 = False
      | otherwise =
        case (lookup p1 associativity) of
          Just AssocLeft -> True
          Just AssocRight -> False
    doReduce tree1 t1 tree2 ts =
      return (BinaryOp t1 tree1 tree2, ts)

isOperator :: Token -> Maybe Int
isOperator (TokenPunc _ s) = lookup s precedence
isOperator t = Nothing

parseFactor :: [Token] -> StoneMonad (ASTree, [Token])
parseFactor (TokenEOL _:ts) = parseFactor ts
parseFactor (t:ts1) =
  case t of
    TokenPunc _ "-" ->
      do (tree, ts2) <- parsePrimary ts1
         return (UnaryOp t tree, ts2)
    otherwise -> parsePrimary (t:ts1)

parsePrimary :: [Token] -> StoneMonad (ASTree, [Token])
parsePrimary (TokenEOL _:ts) = parsePrimary ts
parsePrimary ts =
  do (tree1, t1:ts1) <- parsePrimary' ts
     case t1 of
       TokenPunc _ "(" ->
         do (args, ts2) <- parseArgs ts1
            return (FuncApply tree1 args, ts2)
       otherwise -> return (tree1, t1:ts1)

parsePrimary' :: [Token] -> StoneMonad (ASTree, [Token])
parsePrimary' (TokenEOL _:ts) = parsePrimary' ts
parsePrimary' (TokenPunc _ "(":ts1) =
  do (tree, t:ts2) <- parseExpr ts1
     case t of
       TokenPunc _ ")" -> return (tree, ts2)
       otherwise -> throwError (Err (lineNum t) "Missing ')'")
parsePrimary' (t:ts)
  | isPrimary t = return (ASLeaf t, ts)
  | otherwise = throwError (Err (lineNum t) ("Unexpected " ++ (show t)))
  where
    isPrimary (TokenID _ _) = True
    isPrimary (TokenNum _ _) = True
    isPrimary (TokenStr _ _) = True
    isPrimary _ = False
  
parseArgs :: [Token] -> StoneMonad ([ASTree], [Token])
parseArgs (TokenEOL _:ts) = parseArgs ts
parseArgs (TokenPunc _ ")":ts) = return ([], ts)
parseArgs ts =
  do (trees, t1:ts1) <- parseArgList [] ts
     case t1 of
       TokenPunc _ ")" -> return (trees, ts1)
       otherwise -> throwError (Err (lineNum t1) "Missing ')'")
       
parseArgList :: [ASTree] -> [Token] -> StoneMonad ([ASTree], [Token])
parseArgList trees (TokenEOL _:ts) = parseArgList trees ts
parseArgList trees ts =
  do (tree1, t1:ts1) <- parseExpr ts
     let trees' = trees ++ [tree1] in
       case t1 of
         TokenPunc _ "," -> parseArgList trees' ts1
         otherwise -> return (trees', t1:ts1)

isSentenceEnd :: Token -> Bool
isSentenceEnd (TokenEOL _) = True
isSentenceEnd (TokenPunc _ ";") = True
isSentenceEnd _ = False
