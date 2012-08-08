import System( getArgs )
import Text.Printf
import Parse
import ProofGraph
import WriteProof



main = do
    args <- getArgs
    list <- getLines (head args)
    let graph = doGraphGen (map (stripReturn) list)
        trace = doWriteProof graph
    output trace

