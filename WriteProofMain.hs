import System.Environment( getArgs )
import Text.Printf
import Library.Parse
import Library.ProofGraph
import Library.WriteProof



main = do
    args <- getArgs
    list <- getLines (head args)
    let graph = doGraphGen (map (stripReturn) list)
        trace = doWriteProof graph
    output trace

