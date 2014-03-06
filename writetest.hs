import Library.WriteProof
import Library.ProofGraph
import Library.GraphPart
import Data.Graph.Inductive.Graph( Node, LNode, Edge, LEdge )
import qualified Data.Graph.Inductive.Graph as Graph
import Data.Map( Map )
import qualified Data.Map as Map
import Data.Set( Set )
import qualified Data.Set as Set
import Data.List
import Library.Usage
import Library.Parse



list = [-- create a var term, define a constant to it
        "\"c\"","\"x\"","const","\"t1\"","typeOp","nil","opType","1","def","constTerm","0","def","defineConst",
        
        -- hypothesis list for theorem
        "nil",

        -- equals name
        "\"=\"","const",

        -- equals type
        "\"->\"","typeOp","1","ref","\"->\"","typeOp","1","ref","\"bool\"","typeOp","nil","opType","nil","cons","cons","opType",
        "nil","cons","cons","opType",

        -- equals term
        "constTerm",

        -- the constant
        "\"c\"","const","1","remove","constTerm",

        -- construct the equation of constant = variable
        "appTerm",
        "0","remove",
        "appTerm",

        "thm","pop"]

list2 = ["\"x\"", "const", "\"bool\"", "typeOp", "nil", "opType", "constTerm", "assume", "1", "def", "1", "remove", "deductAntisym"]


singleNodes = (\g -> filter (\x -> nodeOutput g x == 1 && Graph.indeg g x > 1) (Graph.nodes g))
singleNodeSet = (\g -> Set.fromList (singleNodes g))


graph = doGraphGen list
graph2 = doGraphGen list2


edgeCheck :: (Ord d) => (a,b,(c,d)) -> (e,f,(g,d)) -> Ordering
edgeCheck = (\a b -> compare (snd . thd3 $ a) (snd . thd3 $ b))


f :: PGraph -> Node -> Map (LEdge (Int,Int)) Int -> PGraph
f = (\graph node edgemap ->
        let index = head (next 1 graph)
            edgeList = Map.toList edgemap

            sortFunc = (\(w,x) (y,z) -> compare x z)

            defEdge = fst (minimumBy sortFunc edgeList)
            refEdgeList = map fst (filter (\x -> fst x /= defEdge && fst x /= removeEdge) edgeList)
            removeEdge = fst (maximumBy sortFunc edgeList)

            defPart = genPart [index, "def"] True
            refPart = genPart [index, "ref"] False
            removePart = genPart [index, "remove"] False

            defNode = (fst3 defEdge,1)
            refNodeList = map (\x -> (fst3 x,1)) refEdgeList
            removeNode = (fst3 removeEdge,1)

            edgesRemoved = foldl' (\x (y,z) -> Graph.delLEdge y x) graph edgeList
            partsAdded = graphAddList [(defPart, Just (node,1) , [defNode]),
                                       (refPart, Nothing, refNodeList),
                                       (removePart, Nothing, [removeNode])] edgesRemoved
        in partsAdded)
