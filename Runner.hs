import Lexer
import BasicParser

main = do cs <- getContents
          let x = parseProgram $ lexer cs
            in case x of
            (Right tree) -> print tree
            (Left e) -> print e

  
