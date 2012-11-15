module Lexer where
import Data.Char

data Token = TokenEOF { lineNum::Int } |
             TokenEOL { lineNum::Int } | 
             TokenID { lineNum::Int , name::String } |
 	     TokenNum { lineNum::Int , value::Int } |
             TokenStr { lineNum::Int , text::String } |
             TokenPunc { lineNum::Int , symbol::String }
           deriving (Eq, Read, Show)

lexer :: String -> [Token]
lexer cs = lexer' 1 cs

lexer' :: Int -> String -> [Token]
lexer' n cs = let x = readToken cs
                  token = fst x
                  remain = snd x
              in setLineNum n token :
                 case token of
                   TokenEOF _ -> []
                   TokenEOL _ -> lexer' (n+1) remain
                   _ -> lexer' n remain

setLineNum :: Int -> Token -> Token
setLineNum n t = t { lineNum = n }
-- setLineNum n (TokenEOF) = TokenEOF
-- setLineNum n (TokenEOL) = TokenEOL
-- setLineNum n (TokenID _ x) = TokenID n x
-- setLineNum n (TokenNum _ x) = TokenNum n x
-- setLineNum n (TokenStr _ x) = TokenStr n x
-- setLineNum n (TokenPunc _ x) = TokenPunc n x

readToken :: String -> (Token, String)
readToken "" = (TokenEOF 0, "")
readToken ('\n':cs) = (TokenEOL 0, cs)
readToken (c:cs) | isSpace c = readToken cs
readToken ('/':'/':cs) = (TokenEOL 0, skipComment cs)
readToken (c:cs) | isDigit c = (TokenNum 0 (read (fst x)::Int), snd x)
  where x = readNum [c] cs
readToken ('"':cs) = (TokenStr 0 (fst x), snd x)
  where x = readStr "" cs
readToken (c:cs)
  | isAlpha c || c == '_' || c >= 'あ' = (TokenID 0 (fst x), snd x)
  where x = readID [c] cs
readToken ('=':'=':cs) = (TokenPunc 0 "==", cs)
readToken ('<':'=':cs) = (TokenPunc 0 "<=", cs)
readToken ('>':'=':cs) = (TokenPunc 0 ">=", cs)
readToken ('&':'&':cs) = (TokenPunc 0 "&&", cs)
readToken ('|':'|':cs) = (TokenPunc 0 "||", cs)
readToken (c:cs) = (TokenPunc 0 [c], cs)

skipComment :: String -> String
skipComment ('\n':cs) = cs
skipComment (c:cs) = skipComment cs

readNum :: String -> String -> (String, String)
readNum cs "" = (cs, "")
readNum cs1 (c:cs2)
  | isDigit c = readNum (cs1 ++ [c]) cs2
  | otherwise = (cs1, c:cs2)

readStr :: String -> String -> (String, String)
readStr cs1 ('"':cs2) = (cs1, cs2)
readStr cs1 ('\\':'n': cs2) = readStr (cs1 ++ "\n") cs2
readStr cs1 ('\\':'"': cs2) = readStr (cs1 ++ "\"") cs2
readStr cs1 ('\\':'\\': cs2) = readStr (cs1 ++ "\\") cs2
readStr cs1 (c:cs2) = readStr (cs1 ++ [c]) cs2

readID :: String -> String -> (String, String)
readID cs "" = (cs, "")
readID cs1 (c:cs2)
  | isAlpha c || c == '_' || c >= 'あ' || isDigit c = readID (cs1 ++ [c]) cs2
  | otherwise = (cs1, c:cs2)



