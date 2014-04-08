module Library.Cost(
	cost,
	nodeCost,
	listCost
	) where



import Data.Maybe
import Data.Graph.Inductive.Graph( Node )
import qualified Data.Graph.Inductive.Graph as Graph
import Library.ProofGraph



cost :: String -> Int
cost x = 1


nodeCost :: PGraph -> Node -> Int
nodeCost graph node =
	let label = fromJust (Graph.lab graph node)
	    nextCostLayer = map (nodeCost graph) (Graph.suc graph node)
	in (cost label) + (sum nextCostLayer)


listCost :: [String] -> Int
listCost = sum . (map cost)

