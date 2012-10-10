import Lexer

main = do cs <- getContents
          print $ lexer cs
