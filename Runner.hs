import Lexer
import FuncParser
import FuncEvaluator

main = do cs <- getContents
          let x = parseProgram $ lexer cs
            in case x of
            (Right (tree, remain)) ->
              let y = eval emptyEnv tree
              in case y of
                (Right (env, val)) -> print val
                (Left err) -> print err
            (Left err) -> print err
  
