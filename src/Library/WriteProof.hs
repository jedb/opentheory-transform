module Library.WriteProof (
    write,
    writeAll,
    doWriteProof,
    singleCommands,
    next,
    genPart,
    writeGraph
    ) where



import Data.Maybe
import Data.Graph.Inductive.Graph( LNode, LEdge, Node, Edge, (&) )
import qualified Data.Graph.Inductive.Graph as Graph
import Data.Graph.Inductive.Tree
import Data.Map( Map, (!) )
import qualified Data.Map as Map
import Data.Set( Set )
import qualified Data.Set as Set
import Data.List
import Library.Stack( Stack, at, (<:>) )
import qualified Library.Stack as Stack
import Library.Parse( isNumber, fst3, snd3, thd3 )
import Library.Cost
import Library.ProofGraph
import Library.GraphPart
import Library.Usage



orderNodes :: PGraph -> [Node] -> [Node]
orderNodes graph nodeList = nodeList
--placeholder



-- buggy
multiCommandsSimple :: PGraph -> UsageMap -> [Node] -> PGraph
multiCommandsSimple graph usemap nodeList =
    let multiNodes = filter (\x -> nodeOutput graph x > 1 && x `notElem` nodeList) (Graph.nodes graph)
        umap = Map.filterWithKey (\n _ -> n `elem` multiNodes) usemap

        f = (\gr node edgemap ->
                let out = nodeOutput gr node
                    index = next out gr

                    edgeList = Map.toList edgemap
                    edgeToNode = (\x -> (fst3 x, fst . thd3 $ x))

                    sorted = sortBy (\(a,b) (c,d) -> compare (snd . thd3 $ a) (snd . thd3 $ c)) edgeList
                    grouped = groupBy (\x y -> (snd . thd3 . fst $ x) == (snd . thd3 . fst $ y)) sorted

                    defEdge = fst (minimumBy useSort edgeList)
                    removeEdges = map (fst . (maximumBy useSort)) grouped
                    refEdges = map (filter (\x -> x /= defEdge && x `notElem` removeEdges) . (map fst)) grouped

                    usedArgs = filter (\x -> x `elem` (map (snd . thd3) removeEdges)) [1..out]

                    defGen = (\num ->
                        if (num > out)
                        then let reqEdges = filter (\x -> (snd . thd3 . fst $ x) == (snd . thd3 $ defEdge) && fst x /= defEdge) edgeList
                                 refArg = (snd . thd3 $ defEdge) - 1
                             in if (reqEdges == [])
                                then [index!!refArg, "ref"] --remove
                                else [index!!refArg, "ref"]
                        else if (num == (snd . thd3 $ defEdge) && num == out)
                             then if (filter (\x -> x /= defEdge && (snd . thd3 $ x) == num) (map fst edgeList) == [])
                                  then []
                                  else [index!!(num-1), "def"]
                             else if (num `elem` usedArgs)
                                  then [index!!(num-1), "def", "pop"] ++ defGen (num+1)
                                  else ["pop"] ++ defGen (num+1))

                    defPart = (genPart (defGen 1) True, Just (node,1), [edgeToNode defEdge])

                    removeList = filter (\(x,y) -> y /= defEdge) (zip usedArgs removeEdges)
                    removeParts = map (\(x,y) -> (genPart [index!!(x-1), "ref"] False, Nothing, [edgeToNode y])) removeList

                    refList = filter (\(x,y) -> y /= []) (zip usedArgs refEdges)
                    refParts = map (\(x,y) -> (genPart [index!!(x-1), "ref"] False, Nothing, map edgeToNode y)) refList

                    partList = defPart:(removeParts ++ refParts)
                    edgesRemoved = foldl' (\x (y,z) -> Graph.delLEdge y x) gr edgeList
                    partsAdded = graphAddList partList edgesRemoved
                in partsAdded)

    in foldl' (\g n -> f g n (fromJust (Map.lookup n umap))) graph multiNodes



singleCommands :: PGraph -> UsageMap -> [Node] -> PGraph
singleCommands graph usemap nodeList =
    let singleNodes = filter (\x -> nodeOutput graph x == 1 && Graph.indeg graph x > 1) (Graph.nodes graph)
        umap = Map.filterWithKey (\n _ -> n `elem` singleNodes) usemap

        s = (\gr node edgemap ->
                let index = head (next 1 gr)
                    edgeList = Map.toList edgemap

                    defEdge = fst (minimumBy useSort edgeList)
                    removeEdge = fst (maximumBy useSort edgeList)
                    refEdgeList = filter (\x -> x /= defEdge && x /= removeEdge) (map fst edgeList)

                    defPart = genPart [index, "def"] True
                    refPart = genPart [index, "ref"] False
                    removePart = genPart [index, "ref"] False

                    defNode = (fst3 defEdge, fst . thd3 $ defEdge)
                    removeNode = (fst3 removeEdge, fst . thd3 $ removeEdge)
                    refNodeList = map (\x -> (fst3 x, fst . thd3 $ x)) refEdgeList

                    partList = [(defPart, Just (node, 1), [defNode]), (removePart, Nothing, [removeNode])]
                    partList' = if (refNodeList == []) then partList else (refPart, Nothing, refNodeList):partList

                    edgesRemoved = foldl' (\x (y,z) -> Graph.delLEdge y x) gr edgeList
                    partsAdded = graphAddList partList' edgesRemoved
                in partsAdded)

        f = (\gr node edgemap ->
                let reuse = Graph.indeg graph node
                    costToStore = (nodeCost graph node) + (listCost ["def","0"]) + (reuse - 1) * (listCost ["ref","0"])
                    costToIgnore = reuse * (nodeCost graph node)
                in if (costToStore >= costToIgnore)
                   then gr
                   else s gr node edgemap)

    in foldl' (\g n -> f g n (fromJust (Map.lookup n umap))) graph singleNodes



genPart :: [String] -> Bool -> GraphPart
genPart labels hasInput =
    let nodeList = zip [1..] labels
        edgeFunc = (\edges nodes ->
            if (nodes == [] || (tail nodes) == [])
            then edges
            else let newEdge = (fst (nodes!!1), fst (nodes!!0), (1,1))
                 in edgeFunc (newEdge:edges) (tail nodes))
        edgeList = edgeFunc [] nodeList
        input = if (hasInput) then Just (1,1) else Nothing
        output = Just (length labels, 1)
    in graphPart nodeList edgeList input output



next :: Int -> PGraph -> [String]
next num graph =
    let nodeList = filter (isNumber . snd) (Graph.labNodes graph)
        numList = nub . (map (read . snd)) $ nodeList
        f x y = if (x `elem` y) then f (x + 1) y else x
        g x y = if (x == 0) then y else g (x - 1) (f 0 (y ++ numList) : y)
    in map show (g num [])



removeUnused :: PGraph -> [Node] -> PGraph
removeUnused graph nodeList =
    let unused = filter (\x -> Graph.indeg graph x == 0 && x `notElem` nodeList) (Graph.nodes graph)
    in if (unused == [])
       then graph
       else removeUnused (Graph.delNodes unused graph) nodeList



resolve :: PGraph -> [Node] -> PGraph
resolve graph nodeList =
    let liveGraph = removeUnused graph nodeList
        umap = usageMap graph nodeList (Set.fromList (Graph.nodes liveGraph))
        singlesDone = singleCommands liveGraph umap nodeList
        multisDone = multiCommandsSimple singlesDone umap nodeList
    in multisDone



writeGraph :: PGraph -> Node -> [String]
writeGraph graph node =
    let label = fromJust (Graph.lab graph node)
        argList = [1 .. (Graph.outdeg graph node)]
        f s a = let arg = getArg graph node a
                in if (isNothing arg)
                    then s
                    else (writeGraph graph (fromJust arg)) ++ s
    in foldl' f [label] argList



write :: PGraph -> Node -> [String]
write graph node =
    writeGraph (resolve graph [node]) node



writeAll :: PGraph -> [Node] -> [String]
writeAll graph nodeList =
    let ordered = orderNodes graph nodeList
        resolved = resolve graph ordered
        f g n = if (n == [])
                then []
                else (writeGraph g (head n)) ++ (f g (tail n))
    in f resolved ordered


-- metric relates to minimum amount of work done not-on-top of the stack


doWriteProof :: PGraph -> [String]
doWriteProof graph =
    let initList = filter (\x -> Graph.indeg graph x == 0) (Graph.nodes graph)
    in writeAll graph initList

