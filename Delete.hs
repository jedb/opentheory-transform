import System.Environment( getArgs )
import Text.Printf
import Parse
import ProofGraph
import WriteProof

import qualified Data.Graph.Inductive.Graph as Graph



main = do
	args <- getArgs
	list <- getLines (head args)
	let graph = doGraphGen (map (stripReturn) list)
	    initList = filter (\x -> Graph.indeg graph x == 0) (Graph.nodes graph)
	    thmList = (map (\(_,y) -> y)) .
	              (filter (\(x,y) -> (show x) `notElem` (tail args))) .
	              (zip [1..]) $ initList
	    trace = writeAll graph thmList
	output trace
