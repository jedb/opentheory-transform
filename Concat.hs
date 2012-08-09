import System.Environment( getArgs )
import Text.Printf
import Parse
import ProofGraph
import WriteProof



main = do
    args <- getArgs
    x <- getLines $ args!!0
    y <- getLines $ args!!1
    let list = x ++ y
        graph = doGraphGen (map (stripReturn) list)
        trace = doWriteProof graph
    output trace
