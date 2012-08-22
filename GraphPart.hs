module GraphPart (
	graphPart,
	makeGraphPart,

	nodes,
	edges,
	inputNode,
	outputNode,
	inputLab,
	outputLab,

	graphAdd,
	graphDel,
	size,
	addedSize,
	overlap,
	join,

	checkDupe,
	nodeEquals,
	resolveNodeClash
    ) where



import Data.Maybe
import Data.List
import Data.Map( Map )
import qualified Data.Map as Map
import Data.Graph.Inductive.Graph( Node, LNode, Edge, LEdge )
import qualified Data.Graph.Inductive.Graph as Graph
import Data.Graph.Inductive.Tree


data GraphPart = GraphPart { getGraph :: Gr String (Int,Int)
                           , getInput :: Maybe (Node,Int)
                           , getOutput :: Maybe (Node,Int) }


graphPart :: [LNode String] -> [LEdge (Int,Int)] -> Maybe (Node,Int) -> Maybe (Node,Int) -> GraphPart
graphPart nodes edges =
	let graph = checkDupe (Graph.mkGraph nodes edges)
	in GraphPart graph


makeGraphPart :: Gr String (Int,Int) -> Maybe (Node,Int) -> Maybe (Node,Int) -> GraphPart
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



graphAdd :: GraphPart -> Maybe (Node,Int) -> Maybe (Node,Int) -> Gr String (Int,Int) -> Gr String (Int,Int)
graphAdd gpart i o graph =
	let (resolved, dict) = resolveNodeClash graph (getGraph gpart)
	    base = (Graph.insEdges (Graph.labEdges resolved)) . (Graph.insNodes (Graph.labNodes resolved)) $ graph

	    inputAdded = if (isNothing i || isNothing (getInput gpart))
	    	         then base
	    	         else Graph.insEdge (fromJust (Map.lookup (fst . fromJust . getInput $ gpart) dict),
	    	         	                 fst . fromJust $ i,
	    	         	                 (snd . fromJust . getInput $ gpart, snd . fromJust $ i)) base
	    outputAdded = if (isNothing o || isNothing (getOutput gpart))
	    	          then inputAdded
	    	          else Graph.insEdge (fst . fromJust $ o,
	    	          	                  fromJust (Map.lookup (fst . fromJust . getOutput $ gpart) dict),
	    	          	                  (snd . fromJust $ o, snd . fromJust . getOutput $ gpart)) inputAdded

	    graph' = outputAdded

	in checkDupe graph'



graphDel :: GraphPart -> Gr String (Int,Int) -> Gr String (Int,Int)
graphDel gpart graph =
	let n = map fst . nodes $ gpart
	    e = map (\(a,b,_) -> (a,b)) . edges $ gpart
	in (Graph.delNodes n) . (Graph.delEdges e) $ graph



size :: GraphPart -> Int
size = Graph.noNodes . getGraph



addedSize :: GraphPart -> Maybe (Node,Int) -> Maybe (Node,Int) -> Gr String (Int,Int) -> Int
addedSize gpart i o graph =
	let oldSize = Graph.noNodes graph
	    newSize = Graph.noNodes (graphAdd gpart i o graph)
	in newSize - oldSize



overlap :: GraphPart -> GraphPart -> Int
overlap one two =
	let addedSize = Graph.noNodes (graphAdd one Nothing Nothing (getGraph two))
	    totalSize = Graph.noNodes (getGraph one) + Graph.noNodes (getGraph two)
	in totalSize - addedSize



join :: GraphPart -> GraphPart -> GraphPart
join one two | (isJust (getOutput one) && isJust (getInput two)) =
	let (resolved, dict) = resolveNodeClash (getGraph one) (getGraph two)
	    base = (Graph.insEdges (Graph.labEdges resolved)) . (Graph.insNodes (Graph.labNodes resolved)) . getGraph $ one

	    from = (\(x,y) -> (fromJust (Map.lookup x dict), y)) . fromJust . getInput $ two
	    to = fromJust . getOutput $ one
	    ioEdge = (fst from, fst to, (snd from, snd to))

	    newOutput = (\(x,y) -> (fromJust (Map.lookup x dict), y)) . fromJust . getOutput $ two

	in makeGraphPart (checkDupe (Graph.insEdge ioEdge base)) (getInput one) (Just newOutput)



checkDupe :: Gr String (Int,Int) -> Gr String (Int,Int)
checkDupe graph =
	let f = (\g n ->
		        let list = filter (\x -> (x /= n) && (nodeEquals g n x)) (Graph.nodes g)
		        in if (list == []) then g else merge g n (head list))

	    merge = 
	    	(\g n r ->
	    	    let edgesFixed = map (\(a,b,c) -> (a,r,c)) (Graph.inn g n)
	    	    in (Graph.insEdges edgesFixed) . (Graph.delNode n) $ g)

	in foldl' f graph (Graph.nodes graph)



nodeEquals :: Gr String (Int,Int) -> Node -> Node -> Bool
nodeEquals graph one two =
	let edgesOne = sortBy sortFunc (Graph.out graph one)
	    edgesTwo = sortBy sortFunc (Graph.out graph two)
	    sortFunc = (\(_,_,x) (_,_,y) -> compare x y)
	    paired = zip (map (\(_,x,_) -> x) edgesOne) (map (\(_,x,_) -> x) edgesTwo)

	in (Graph.gelem one graph) &&
	   (Graph.gelem two graph) &&
	   (Graph.lab graph one == Graph.lab graph two) &&
	   (length edgesOne == length edgesTwo) &&
	   (all (\x -> nodeEquals graph (fst x) (snd x)) paired)



resolveNodeClash :: Gr String (Int,Int) -> Gr String (Int,Int) -> (Gr String (Int,Int), Map Int Int)
resolveNodeClash ref graph =
	let dict = Map.fromList (zip (Graph.nodes graph) (Graph.newNodes (Graph.noNodes graph) ref))
	    nodeList = map (\(x,y) -> (fromJust (Map.lookup x dict), y)) (Graph.labNodes graph)
	    edgeList = map (\(x,y,z) -> (fromJust (Map.lookup x dict),
	    	                         fromJust (Map.lookup y dict), z)) (Graph.labEdges graph)
	in (Graph.mkGraph nodeList edgeList, dict)

