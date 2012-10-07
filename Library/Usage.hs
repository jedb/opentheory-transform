module Library.Usage(
	UsageMap,
	usageMap,
	useSort,
	nodeOutput,
	getArg
	) where



import Data.Map( Map )
import qualified Data.Map as Map
import Data.Set( Set )
import qualified Data.Set as Set
import Data.List
import Data.Maybe
import Data.Graph.Inductive.Graph( Node, LNode, Edge, LEdge )
import qualified Data.Graph.Inductive.Graph as Graph
import Library.Parse
import Library.ProofGraph



type UsageMap = Map Node (Map (LEdge (Int,Int)) (Int,[Int]))


-- Takes a graph, a starting list of nodes, a set of nodes of interest, and
-- follows the starting nodes up the graph to find which edges the starting nodes
-- will encounter the nodes of interest through.
usageMap :: PGraph -> [Node] -> Set Node -> UsageMap
usageMap graph order interest =
    let unionFunc = (\a b ->
                Map.unionWith min a b)

        addFunc = (\index prev umap edge ->
                let node = snd3 edge
                    curIn = Graph.outdeg graph (fst3 edge)
                    prev' = (curIn - (fst . thd3 $ edge)):prev
                    toAdd = Map.singleton node (Map.singleton edge (index,prev'))
                in if (Set.member node interest)
                   then Map.unionWith unionFunc toAdd umap
                   else umap)

        f = (\umap (index,node,prev) ->
                let edgeList = Graph.out graph node
                    sucMapList = map (f Map.empty) (map (\x -> (index, snd3 x, (length edgeList - (fst . thd3 $ x)):prev)) edgeList)
                    umap' = foldl' (addFunc index prev) umap edgeList
                in Map.unionsWith unionFunc (umap':sucMapList))

    in foldl' f Map.empty (zip3 [1..] order (repeat []))



useSort :: (LEdge a, (Int,[Int])) -> (LEdge a, (Int,[Int])) -> Ordering
useSort (_,(w,x)) (_,(y,z)) =
	let check = compare w y
	in if (check == EQ)
	   then compare (reverse x) (reverse z)
	   else check



nodeOutput :: PGraph -> Node -> Int
nodeOutput graph node =
    let label = fromJust (Graph.lab graph node)
    in case label of
           "thm" -> 0
           "pop" -> 0
           "defineConst" -> 2
           "defineTypeOp" -> 5
           x -> 1



getArg :: PGraph -> Node -> Int -> Maybe Node
getArg graph node arg =
    let edge = filter (\x -> (fst . thd3 $ x) == arg) (Graph.out graph node)
    in if (edge == [])
       then Nothing
       else Just (snd3 . head $ edge)

