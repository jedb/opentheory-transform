import System( getArgs )
import Text.Printf
import Parse
import ProofGraph
import WriteProof
import Semantic
import Stack

import Data.Maybe
import Data.Graph.Inductive.Graph( Node, LNode, Edge, LEdge )
import qualified Data.Graph.Inductive.Graph as Graph



fst4 :: (a,b,c,d) -> a
fst4 (a,_,_,_) = a



toThms :: PGraph -> [Node] -> [String]
toThms graph nodeList =
    let hyp = (\g n -> let edge = filter (\x -> (fst . thd3 $ x) == 2) (Graph.out g n)
                           node = snd3 . head $ edge
                       in write g node)

        con = (\g n -> let edge = filter (\x -> (fst . thd3 $ x) == 1) (Graph.out g n)
                           node = snd3 . head $ edge
                       in write g node)

        f = (\x -> (show . fst $ x) ++ ".  [" ++ show ((fst4 . fromJust $ (eval (hyp graph (snd x)))) `at` 0) ++ 
                   "] |- " ++ show ((fst4 . fromJust $ (eval (con graph (snd x)))) `at` 0))

    in map f (zip [1..] nodeList)



main = do
    args <- getArgs
    list <- getLines $ head args
    let graph = doGraphGen (map (stripReturn) list)
        initList = filter (\x -> Graph.indeg graph x == 0) (Graph.nodes graph)
        theorems = toThms graph initList
    output theorems
