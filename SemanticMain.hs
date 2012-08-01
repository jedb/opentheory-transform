import System( getArgs )
import Text.Printf
import Parse
import Semantic



main = do
    args <- getArgs
    list <- getLines $ head args
    let result = doSemanticCheck (map (stripReturn) list)
    printf result
