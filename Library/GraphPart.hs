module Library.GraphPart (
	GraphPart,
	
	graphPart,
	makeGraphPart,

	nodes,
	edges,
	inputNode,
	outputNode,
	inputLab,
	outputLab,

	graphAdd,
	graphAddList,
	graphDel,
	size,
	addedSize,
	overlap,
	join
    ) where



import Data.Maybe
import Data.List
import Data.Map( Map )
import qualified Data.Map as Map
import Data.Graph.Inductive.Graph( Node, LNode, Edge, LEdge )
import qualified Data.Graph.Inductive.Graph as Graph
import Data.Graph.Inductive.Tree
import Library.ProofGraph


data GraphPart = GraphPart { getGraph :: Gr String (Int,Int)
                           , getInput :: Maybe (Node,Int)
                           , getOutput :: Maybe (Node,Int) }


graphPart :: [LNode String] -> [LEdge (Int,Int)] -> Maybe (Node,Int) -> Maybe (Node,Int) -> GraphPart
graphPart nodes edges =
	let graph = checkDupe (Graph.mkGraph nodes edges)
	in GraphPart graph


makeGraphPart :: PGraph -> Maybe (Node,Int) -> Maybe (Node,Int) -> GraphPart
makeGraphPart = GraphPart


nodes :: GraphPart -> [LNode String]
nodes = Graph.labNodes . getGraph


edges :: GraphPart -> [LEdge (Int,Int)]
edges = Graph.labEdges . getGraph


inputNode :: GraphPart -> Maybe Node
inputNode gpart = do
	input <- getInput gpart
	return (fst input)


outputNode :: GraphPart -> Maybe Node
outputNode gpart = do
	output <- getOutput gpart
	return (fst output)


inputLab :: GraphPart -> Maybe Int
inputLab gpart = do
	input <- getInput gpart
	return (snd input)


outputLab :: GraphPart -> Maybe Int
outputLab gpart = do
	output <- getOutput gpart
	return (snd output)



graphAdd :: GraphPart -> Maybe (Node,Int) -> [(Node,Int)] -> PGraph -> PGraph
graphAdd gpart i o graph =
	let (resolved, dict) = resolveNodeClash graph (getGraph gpart)
	    base = (Graph.insEdges (Graph.labEdges resolved)) . (Graph.insNodes (Graph.labNodes resolved)) $ graph

	    inputAdded = if (isNothing i || isNothing (getInput gpart))
	    	         then base
	    	         else Graph.insEdge (fromJust (Map.lookup (fst . fromJust . getInput $ gpart) dict),
	    	         	                 fst . fromJust $ i,
	    	         	                 (snd . fromJust . getInput $ gpart, snd . fromJust $ i)) base

	    outputAdded = if (o == [] || isNothing (getOutput gpart))
	    	          then inputAdded
	    	          else let outEdge = map (\(x,y) -> (x, fromJust (Map.lookup (fst . fromJust . getOutput $ gpart) dict),
	    	          	                                    (y, snd . fromJust . getOutput $ gpart))) o
	    	               in Graph.insEdges outEdge inputAdded

	    graph' = outputAdded

	in checkDupe graph'



graphAddList :: [(GraphPart, Maybe (Node,Int), [(Node,Int)])] -> PGraph -> PGraph
graphAddList partList graph =
	foldl' (\g (x,y,z) -> graphAdd x y z g) graph partList



graphDel :: GraphPart -> PGraph -> PGraph
graphDel gpart graph =
	let n = map fst . nodes $ gpart
	    e = map (\(a,b,_) -> (a,b)) . edges $ gpart
	in (Graph.delNodes n) . (Graph.delEdges e) $ graph



size :: GraphPart -> Int
size = Graph.noNodes . getGraph



addedSize :: GraphPart -> Maybe (Node,Int) -> [(Node,Int)] -> PGraph -> Int
addedSize gpart i o graph =
	let oldSize = Graph.noNodes graph
	    newSize = Graph.noNodes (graphAdd gpart i o graph)
	in newSize - oldSize



overlap :: GraphPart -> GraphPart -> Int
overlap one two =
	let added = Graph.noNodes (graphAdd one Nothing [] (getGraph two))
	    total = Graph.noNodes (getGraph one) + Graph.noNodes (getGraph two)
	in total - added



join :: GraphPart -> GraphPart -> GraphPart
join one two | (isJust (getOutput one) && isJust (getInput two)) =
	let (resolved, dict) = resolveNodeClash (getGraph one) (getGraph two)
	    base = (Graph.insEdges (Graph.labEdges resolved)) . (Graph.insNodes (Graph.labNodes resolved)) . getGraph $ one

	    from = (\(x,y) -> (fromJust (Map.lookup x dict), y)) . fromJust . getInput $ two
	    to = fromJust . getOutput $ one
	    ioEdge = (fst from, fst to, (snd from, snd to))

	    newOutput = (\(x,y) -> (fromJust (Map.lookup x dict), y)) . fromJust . getOutput $ two

	in makeGraphPart (checkDupe (Graph.insEdge ioEdge base)) (getInput one) (Just newOutput)


