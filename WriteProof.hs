module WriteProof (
    write,
    writeAll,
    doWriteProof,
    singleCommands
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
import Parse( isNumber )



output :: Gr String (Int,Int) -> Node -> Int
output graph node =
    let label = fromJust (Graph.lab graph node)
    in case label of
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



next :: Gr String (Int,Int) -> String
next graph =
    let nodeList = filter (isNumber . snd) (Graph.labNodes graph)
        numList = nub . (map (read . snd)) $ nodeList
        f = (\x y -> if (x `elem` y) then f (x + 1) y else x)
    in show (f 0 numList)



subGraph :: Gr a b -> Node -> [Node]
subGraph graph node =
    let sucList = nub (Graph.suc graph node)
    in nub (node : (foldl' (++) [] (map (subGraph graph) sucList)))



fst3 :: (a,b,c) -> a
fst3 (a,_,_) = a

snd3 :: (a,b,c) -> b
snd3 (_,b,_) = b

thd3 :: (a,b,c) -> c
thd3 (_,_,c) = c



orderNodes :: Gr String (Int,Int) -> [Node] -> [Node]
orderNodes graph nodeList = nodeList
--placeholder



multiCommands :: Gr String (Int,Int) -> [Node] -> Gr String (Int,Int)
multiCommands graph nodeList = graph
--placeholder



singleCommands :: Gr String (Int,Int) -> [Node] -> Gr String (Int,Int)
singleCommands graph nodeList =
    let r = (\g n p -> let g' = if (((output g n) /= 1) || ((Graph.indeg g n) == 1) || ((cost g n) < 3) || ((cost g n) == 3 && (Graph.indeg g n) < 3))
                                then g
                                else let index = next g --show . length . (nubBy (\x y -> snd x == snd y)) $ (filter (isNumber . snd) (Graph.labNodes g))
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

        f = (\g n -> let argList = reverse [1 .. (Graph.outdeg g n)]
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
    foldl' (\g f -> f g nodeList) graph [removeUnused, singleCommands, multiCommands]



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

