import System( getArgs )
import Text.Printf
import Parse
import ProofGraph


main = do
    args <- getArgs
    list <- getLines $ head args
    let result = doGraphGen (map (stripReturn) list)
    printf $ (show result) ++ "\n"
