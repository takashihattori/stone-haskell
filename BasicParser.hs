module BasicParser where
import Control.Monad.Error
import Lexer

data ASTree = ASLeaf Token |
              EmptyState |
              SeqState { left::ASTree , right::ASTree } |
              IfState { cond::ASTree , thenblock::ASTree , elseblock::ASTree } |
              WhileState { cond::ASTree , body::ASTree } |
              UnaryOp { op::Token , primary::ASTree } |
              BinaryOp { op::Token , left::ASTree , right::ASTree }
            deriving (Read, Show)

data Assoc = AssocRight | AssocLeft

precedence = [
  ("=", 1), 
  ("==", 2), (">", 2),  ("<", 2),
  ("+", 3), ("-", 3), 
  ("*", 4), ("/", 4), ("%", 4) ]

associativity = [
  (1, AssocRight),
  (2, AssocLeft),
  (3, AssocLeft),
  (4, AssocLeft) ]

data ParseError = Err { errLine::Int , message::String }

instance Error ParseError where
  noMsg = Err 0 "Syntax Error"
  strMsg s = Err 0 s

instance Show ParseError where
  show e = "In line " ++ (show $ errLine e) ++ ", " ++ (message e)
  
type ParseMonad = Either ParseError

parseProgram :: [Token] -> ParseMonad (ASTree, [Token])
parseProgram (TokenEOL _:ts) = return (EmptyState, ts)
parseProgram (TokenPunc _ ";":ts) = return (EmptyState, ts)
parseProgram ts = do (tree, t:remain) <- parseStatement ts
                     case t of
                       TokenEOL _ -> return (tree, remain)
                       TokenPunc _ ";" -> return (tree, remain)
                       otherwise -> throwError (Err (lineNum t)
                                                "EOL or ';' expected")

parseStatement :: [Token] -> ParseMonad (ASTree, [Token])
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
parseStatement ts = parseSimple ts

parseSimple :: [Token] -> ParseMonad (ASTree, [Token])
parseSimple (TokenEOL _:ts) = parseSimple ts
parseSimple ts = parseExpr ts

parseBlock :: [Token] -> ParseMonad (ASTree, [Token])
parseBlock (TokenEOL _:ts) = parseBlock ts
parseBlock (TokenPunc _ "{":ts1) =
  do (tree1, t:ts2) <- parseStatement ts1
     case t of
       _ | isSentenceEnd t -> do (tree2, ts3) <- parseStatement ts2
                                 return (SeqState tree1 tree2, ts3)
       TokenPunc _ "}" -> return (tree1, ts2)
       otherwise -> throwError (Err (lineNum t) "Missing '}'")
  where
    isSentenceEnd (TokenEOL _) = True
    isSentenceEnd (TokenPunc _ ";") = True
    isSentenceEnd _ = False
parseBlock (t:_) = throwError (Err (lineNum t) "Block must begin with '{'")
                   
parseExpr :: [Token] -> ParseMonad (ASTree, [Token])
parseExpr (TokenEOL _:ts) = parseExpr ts
parseExpr ts1 =
  do (tree, ts2) <- parseFactor ts1
     parseExpr' tree ts2

parseExpr' :: ASTree -> [Token] -> ParseMonad (ASTree, [Token])
parseExpr' tree1 (t1:ts1) =
  case isOperator t1 of
    Just p1 ->
      do (tree2, t2:ts2) <- parseFactor ts1
         (tree3, ts3) <- parseExpr'' tree1 t1 p1 tree2 (t2:ts2)
         parseExpr' tree3 ts3
    otherwise -> return (tree1, t1:ts1)

parseExpr'' :: ASTree -> Token -> Int -> ASTree -> [Token] ->
               ParseMonad (ASTree, [Token])
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

parseFactor :: [Token] -> ParseMonad (ASTree, [Token])
parseFactor (TokenEOL _:ts) = parseFactor ts
parseFactor (t:ts1) =
  case t of
    TokenPunc _ "-" ->
      do (tree, ts2) <- parsePrimary ts1
         return (UnaryOp t tree, ts2)
    otherwise -> parsePrimary (t:ts1)

parsePrimary :: [Token] -> ParseMonad (ASTree, [Token])
parsePrimary (TokenEOL _:ts) = parsePrimary ts
parsePrimary (TokenPunc _ "(":ts1) =
  do (tree, t:ts2) <- parseExpr ts1
     case t of
       TokenPunc _ ")" -> return (tree, ts2)
       otherwise -> throwError (Err (lineNum t) "Missing ')'")
parsePrimary (t:ts)
  | isPrimary t = return (ASLeaf t, ts)
  | otherwise = throwError (Err (lineNum t) ("Unexpected " ++ (show t)))
  where
    isPrimary (TokenID _ _) = True
    isPrimary (TokenNum _ _) = True
    isPrimary (TokenStr _ _) = True
    isPrimary _ = False
