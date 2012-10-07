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



reuse :: PGraph -> Node -> Int
reuse graph node =
    let labels = map snd (Graph.lpre graph node)
        f = (\x y -> length (filter (\z -> fst y == fst z) x))
        reuseList = map (f labels) labels
    in maximum reuseList



next :: Int -> PGraph -> [String]
next num graph =
    let nodeList = filter (isNumber . snd) (Graph.labNodes graph)
        numList = nub . (map (read . snd)) $ nodeList
        f = (\x y -> if (x `elem` y) then f (x + 1) y else x)
        g = (\x y -> if (x == 0) then y else g (x - 1) (f 0 (y ++ numList) : y))
    in map show (g num [])



subGraph :: Gr a b -> Node -> [Node]
subGraph graph node =
    let sucList = nub (Graph.suc graph node)
    in nub (node : (foldl' (++) [] (map (subGraph graph) sucList)))



orderNodes :: PGraph -> [Node] -> [Node]
orderNodes graph nodeList = nodeList
--placeholder



removeOverlap :: PGraph -> Node -> [Node] -> [Node]
removeOverlap graph node list =
    let nubFunc = (\x y -> (getArg graph node x) == (getArg graph node y))
    in nubBy nubFunc list



--rightmostEdge :: Gr String (Int,Int) -> LEdge (Int,Int) -> Bool
--rightmostEdge graph edge =




--crossEdge :: Gr String (Int,Int) -> LEdge (Int,Int) -> Bool



--trace :: Gr String (Int,Int) -> Node -> [Node] -> Maybe (Int,Node,Int)
--trace graph node history =
--    let tr = (\g n curnode curarg hist ->
--            if (curarg > output g n)
--            then Nothing
--            else let argToFollow = if (n == curnode) then curarg else 1
--                     outputsFromArg = filter (\(x,y) -> snd y == argToFollow) (Graph.lpre g curnode)
--
--            in case outputFromCurArg of
--                    0 -> 
--            if (filter (\(x,y) -> snd y == curarg) (Graph.lpre g n) > 1)
--            if (curarg > output g n)
--            then Nothing
--            else if (n == curnode)
--                 then let curnode' = filter (\(x,y) -> ) (Graph.lpre g n)
--                      in tr g n curnode' curarg
--                 else )
--
--        f = (\g n curarg h ->
--            if (curarg > output g n)
--            then Nothing
--            else let hist = if 
--                     place = tr g n n hist
--                 in if (isJust place)
--                    then place
--                    else f g n (curarg+1) hist
--
--    in f graph node 1 history

-- higher numbers -> deeper in the stack

--multiCommands :: Gr String (Int,Int) -> [Node] -> (Gr String (Int,Int), [Node])
--multiCommands graph nodeList =
--    let f = (\g n p ->
--            if ((output g n) <= 1)
--            then g
--            else let changed = do (argToUseDict, place, placeArg) <- trace g n
--                                  let edgesToRemove = 
--                                      edgesRemoved = foldl' (\x y -> Graph.delLEdge y x) g edgesToRemove
--                                      defSubGraph = 
--                                      edgesToRef = 
--                                      new = 
--                                      refsToAdd = 
--                                  return (foldl' insertSubGraph edgesRemoved refsToAdd)
--                 in if (isNothing changed) then g else fromJust changed
--
--        multiOutputNodes = filter (\x -> output graph x > 1) (Graph.nodes graph)
--        bestCase = foldl' f graph multiOutputNodes
--
--    in



multiCommandsSimple :: PGraph -> [Node] -> PGraph
multiCommandsSimple graph nodeList =
    let r = (\g n p -> let g' = if ((nodeOutput g n) <= 1)
                                then g
                                else let ou = nodeOutput g n
                                         index = next ou g
                                         new = Graph.newNodes (5 * ou + 2) g -- 3 for num/def/pop, 2 for num/ref, per output plus an extra num/ref
                                         (defNew,refNew) = splitAt (3 * ou + 2) new

                                         edgeCheck x y = compare (snd . thd3 $ x) (snd . thd3 $ y)

                                         oldEdge = maximumBy edgeCheck (filter (\x -> fst3 x == p) (Graph.inn g n))
                                         toConvert = delete oldEdge (Graph.inn g n)

                                         defNodeGen = (\i j x lim -> if (x >= lim)
                                                                     then []
                                                                     else [(j!!(x*3), i!!x), (j!!(x*3+1), "def"),
                                                                          (j!!(x*3+2), "pop")] ++ (defNodeGen i j (x+1) lim))
                                         defNodes = (defNodeGen index defNew 0 ou) ++ [(defNew!!(3*ou), index!!((snd . thd3 $ oldEdge)-1)), (defNew!!(3*ou+1), "ref")]
                                         defEdgeGen = (\x b -> let x' = [(fst b, fst . snd $ x, (1,1))] ++ (fst x)
                                                               in (x',b))
                                         defEdges = [(p, (fst . last $ defNodes), thd3 oldEdge), ((fst . head $ defNodes), n, (1,1))] ++
                                                    (fst (foldl' defEdgeGen ([], head defNodes) (tail defNodes)))
                                         defAdded = (Graph.insEdges defEdges) . (Graph.insNodes defNodes) . (Graph.delLEdge oldEdge) $ g

                                         refGen = (\i lab -> [(i!!(2*(lab-1)), index!!(lab-1)), (i!!(2*(lab-1)+1), "ref")])
                                         refNodes = map (refGen refNew) [1 .. (ou)]
                                         refEdges = map (\[x,y] -> (fst y, fst x,(1,1))) refNodes
                                         refAdded = (Graph.insEdges refEdges) . (Graph.insNodes (concat refNodes)) $ defAdded

                                         convertEdge = (\g e -> let new = (fst3 e, fst . last $ (refNodes!!(snd . thd3 $ e)), thd3 e)
                                                                in (Graph.insEdge new) . (Graph.delLEdge e) $ g)

                                         done = foldl' convertEdge refAdded toConvert
                                     in done
                       in f g' n)

        f = (\g n -> let argList = reverse $ [1 .. (Graph.outdeg g n)]
                     in foldl' (\x y -> r x (fromJust (getArg x n y)) n) g argList)

    in foldl' f graph nodeList



singleCommands :: PGraph -> UsageMap -> [Node] -> PGraph
singleCommands graph usemap nodeList =
    let singleNodes = filter (\x -> nodeOutput graph x == 1 && Graph.indeg graph x > 1) (Graph.nodes graph)
        umap = Map.filterWithKey (\n _ -> n `elem` singleNodes) usemap

        s = (\graph node edgemap ->
                let index = head (next 1 graph)
                    edgeList = Map.toList edgemap

                    defEdge = fst (minimumBy useSort edgeList)
                    removeEdge = fst (maximumBy useSort edgeList)
                    refEdgeList = filter (\x -> x /= defEdge && x /= removeEdge) (map fst edgeList)

                    defPart = genPart [index, "def"] True
                    refPart = genPart [index, "ref"] False
                    removePart = genPart [index, "remove"] False

                    defNode = (fst3 defEdge, fst . thd3 $ defEdge)
                    removeNode = (fst3 removeEdge, fst . thd3 $ removeEdge)
                    refNodeList = map (\x -> (fst3 x, fst . thd3 $ x)) refEdgeList

                    partList = [(defPart, Just (node, 1), [defNode]), (removePart, Nothing, [removeNode])]
                    partList' = if (refNodeList == []) then partList else (refPart, Nothing, refNodeList):partList

                    edgesRemoved = foldl' (\x (y,z) -> Graph.delLEdge y x) graph edgeList
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
    in foldl' (\g f -> f g umap nodeList) liveGraph [singleCommands]



writeGraph :: PGraph -> Node -> [String]
writeGraph graph node =
    let label = fromJust (Graph.lab graph node)
        argList = [1 .. (Graph.outdeg graph node)]
        f = (\s a -> let arg = getArg graph node a
                     in if (isNothing arg)
                        then s
                        else (writeGraph graph (fromJust arg)) ++ s)
    in foldl' f [label] argList



write :: PGraph -> Node -> [String]
write graph node =
    writeGraph (resolve graph [node]) node



writeAll :: PGraph -> [Node] -> [String]
writeAll graph nodeList =
    let ordered = orderNodes graph nodeList
        resolved = resolve graph ordered
        f = (\g n -> if (n == [])
                     then []
                     else (writeGraph g (head n)) ++ (f g (tail n)))
    in f resolved ordered


-- metric relates to minimum amount of work done not-on-top of the stack


doWriteProof :: PGraph -> [String]
doWriteProof graph =
    let initList = filter (\x -> Graph.indeg graph x == 0) (Graph.nodes graph)
    in writeAll graph initList

