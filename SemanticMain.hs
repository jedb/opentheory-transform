import System.Environment( getArgs )
import Text.Printf
import Library.Parse
import Library.Semantic



main = do
    args <- getArgs
    list <- getLines (head args)
    let result = doSemanticCheck (map (stripReturn) list)
    printf result
