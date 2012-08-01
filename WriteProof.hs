module WriteProof (
    write,
    writeAll,
    doWriteProof,
    singleCommands,

    ) where



import Data.Maybe
import Data.Graph.Inductive.Graph( LNode, LEdge, Node, Edge, (&) )
import qualified Data.Graph.Inductive.Graph as Graph
import Data.Graph.Inductive.Tree
import Data.Map( Map, (!) )
import qualified Data.Map as Map
import Data.List
import Stack( Stack, at, (<:>) )
import qualified Stack as Stack
import Parse( isNumber, fst3, snd3, thd3 )



output :: Gr String (Int,Int) -> Node -> Int
output graph node =
    let label = fromJust (Graph.lab graph node)
    in case label of
           "thm" -> 0
           "pop" -> 0
           "defineConst" -> 2
           "defineTypeOp" -> 5
           x -> 1



reuse :: Gr String (Int,Int) -> Node -> Int
reuse graph node =
    let labels = map snd (Graph.lpre graph node)
        f = (\x y -> length (filter (\z -> fst y == fst z) x))
        reuseList = map (f labels) labels
    in maximum reuseList



cost :: Gr String (Int,Int) -> Node -> Int
cost graph node =
    length (subGraph graph node)



next :: Int -> Gr String (Int,Int) -> [String]
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



orderNodes :: Gr String (Int,Int) -> [Node] -> [Node]
orderNodes graph nodeList = nodeList
--placeholder



removeOverlap :: Gr String (Int,Int) -> Node -> [Node] -> [Node]
removeOverlap graph node list =
    let nubFunc = (\x y -> (getArg graph node x) == (getArg graph node y))
    in nubBy nubFunc list



--rightmostEdge :: Gr String (Int,Int) -> LEdge (Int,Int) -> Bool
--rightmostEdge graph edge =




--crossEdge :: Gr String (Int,Int) -> LEdge (Int,Int) -> Bool



--multiCommands :: Gr String (Int,Int) -> [Node] -> Gr String (Int,Int)
--multiCommands graph nodeList =
--    let trace = (\g n cn ca ->)
--
--        r = (\g n p -> let g' = if ((output g n) <= 1)
--                                then g
--                                else let (argToUseDict, (place, placeArg)) = trace g n n 1
--                                         edgesToRemove = 
--                                         edgesRemoved = foldl' (\x y -> Graph.delLEdge y x) g edgesToRemove
--                                         defSubGraph = 
--                                         edgesToRef = 
--                                         new = 
--                                         refsToAdd = 
--                                         done = foldl' insertSubGraph edgesRemoved refsToAdd
--                                     in done
--                       in f g' n) 
--
--        f = (\g n -> let argList = (removeOverlap g) . reverse $ [1 .. (Graph.outdeg g n)]
--                     in foldl' (\x y -> r x (getArg x n y) n) g argList)
--
--    in foldl' f graph nodeList



multiCommandsSimple :: Gr String (Int,Int) -> [Node] -> Gr String (Int,Int)
multiCommandsSimple graph nodeList =
    let r = (\g n p -> let g' = if ((output g n) <= 1)
                                then g
                                else let ou = output g n
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
                     in foldl' (\x y -> r x (getArg x n y) n) g argList)

    in foldl' f graph nodeList



singleCommands :: Gr String (Int,Int) -> [Node] -> Gr String (Int,Int)
singleCommands graph nodeList =
    let r = (\g n p -> let g' = if (((output g n) /= 1) || ((Graph.indeg g n) == 1) || ((cost g n) < 3) || ((cost g n) == 3 && (Graph.indeg g n) < 3))
                                then g
                                else let index = head (next 1 g)
                                         new = Graph.newNodes 4 g  -- 2 new nodes for def and 2 new nodes for ref

                                         oldEdge = head $ (filter (\x -> fst3 x == p) (Graph.inn g n))
                                         defNodes = [(new!!0, "def"), (new!!1, index)]
                                         defEdges = [(p, fst (defNodes!!0), (fst . thd3 $ oldEdge, 1)),
                                                     (fst (defNodes!!0), fst (defNodes!!1), (1,1)),
                                                     (fst (defNodes!!1), n, (1,1))]
                                         defAdded = (Graph.insEdges defEdges) . (Graph.insNodes defNodes) . (Graph.delLEdge oldEdge) $ g

                                         refNodes = [(new!!2, "ref"), (new!!3, index)]
                                         refEdge = (fst (refNodes!!0), fst (refNodes!!1), (1,1))
                                         refAdded = (Graph.insEdge refEdge) . (Graph.insNodes refNodes) $ defAdded
                                         convertEdge = (\g e -> let new = (fst3 e, fst (refNodes!!0), thd3 e)
                                                                in (Graph.insEdge new) . (Graph.delLEdge e) $ g)
                                         done = foldl' convertEdge refAdded (filter (\x -> fst3 x /= fst (defNodes!!1)) (Graph.inn refAdded n))
                                     in done
                       in f g' n)

        f = (\g n -> let argList = reverse $ [1 .. (Graph.outdeg g n)]
                     in foldl' (\x y -> r x (getArg x n y) n) g argList)

    in foldl' f graph nodeList



removeUnused :: Gr String (Int,Int) -> [Node] -> Gr String (Int,Int)
removeUnused graph nodeList =
    let unused = filter (\x -> Graph.indeg graph x == 0 && x `notElem` nodeList) (Graph.nodes graph)
    in if (unused == [])
       then graph
       else removeUnused (Graph.delNodes unused graph) nodeList



resolve :: Gr String (Int,Int) -> [Node] -> Gr String (Int,Int)
resolve graph nodeList =
    foldl' (\g f -> f g nodeList) graph [removeUnused, singleCommands, multiCommandsSimple]



getArg :: Gr String (Int,Int) -> Node -> Int -> Node
getArg graph node arg =
    snd3 . head $ (filter (\x -> (fst . thd3 $ x) == arg) (Graph.out graph node))



writeGraph :: Gr String (Int,Int) -> Node -> [String]
writeGraph graph node =
    let label = fromJust (Graph.lab graph node)
        argList = [1 .. (Graph.outdeg graph node)]
    in foldl' (\s a -> (writeGraph graph (getArg graph node a)) ++ s) [label] argList



write :: Gr String (Int,Int) -> Node -> [String]
write graph node =
    writeGraph (resolve graph [node]) node



writeAll :: Gr String (Int,Int) -> [Node] -> [String]
writeAll graph nodeList =
    let ordered = orderNodes graph nodeList
        graph' = resolve graph ordered
        f = (\g n -> if (n == [])
                     then []
                     else (writeGraph g (head n)) ++ (f g (tail n)))
    in f graph' ordered


-- metric relates to minimum amount of work done not-on-top of the stack


doWriteProof :: Gr String (Int,Int) -> [String]
doWriteProof graph =
    let initList = filter (\x -> Graph.indeg graph x == 0) (Graph.nodes graph)
    in writeAll graph initList

