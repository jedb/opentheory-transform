import System.Environment( getArgs )
import Text.Printf
import Library.Parse
import Library.ProofGraph


main = do
    args <- getArgs
    list <- getLines (head args)
    let result = doGraphGen (map (stripReturn) list)
    printf $ (show result) ++ "\n"
