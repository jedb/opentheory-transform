module ProofGraph (
    PGraph,
    doGraphGen
    ) where



import Data.Maybe
import Data.List
import Data.Set( Set )
import qualified Data.Set as Set
import Data.Map( Map, (!) )
import qualified Data.Map as Map

import Data.Graph.Inductive.Graph( LNode, LEdge, (&) )
import qualified Data.Graph.Inductive.Graph as Graph
import Data.Graph.Inductive.Tree

import Stack( Stack, at, (<:>) )
import qualified Stack as Stack
import Parse( isNumber, isName )
import GraphPart( checkDupe )



type PGraph = Gr String (Int,Int)
type PStack = Stack (Int, LNode String)
type PMap = Map Int (Int, LNode String)


data CommandIO = IO { args :: Int
                    , results :: Int }


argMap :: String -> CommandIO
argMap "absTerm" = IO 2 1
argMap "absThm" = IO 2 1
argMap "appTerm" = IO 2 1
argMap "appThm" = IO 2 1
argMap "assume" = IO 1 1
argMap "axiom" = IO 2 1
argMap "betaConv" = IO 1 1
argMap "cons" = IO 2 1
argMap "const" = IO 1 1
argMap "constTerm" = IO 2 1
argMap "deductAntisym" = IO 2 1
argMap "defineConst" = IO 2 2
argMap "defineTypeOp" = IO 5 5
argMap "eqMp" = IO 2 1
argMap "nil" = IO 0 1
argMap "opType" = IO 2 1
argMap "refl" = IO 1 1
argMap "subst" = IO 2 1
argMap "thm" = IO 3 0
argMap "typeOp" = IO 1 1
argMap "var" = IO 2 1
argMap "varTerm" = IO 1 1
argMap "varType" = IO 1 1
argMap x | (isName x) = IO 0 1



process :: String -> CommandIO -> PGraph -> PStack -> (PGraph, PStack)
process str io graph stack =
    let argList = map (\x -> fromJust (stack `at` x)) [0..((args io) - 1)]
        nextNum = head (Graph.newNodes 1 graph)
        node = (nextNum, str)
        edgeList = map (\x -> (nextNum, (fst . snd . snd $ x), (fst x, fst . snd $ x))) (zip [1..(args io)] argList)
        graph' = (Graph.insEdges edgeList) . (Graph.insNode node) $ graph
        nodeList = map (\x -> (x, node)) [1..(results io)]
        stack' = foldr (<:>) (Stack.pop (args io) stack) nodeList
    in (graph', stack')



parse :: (PGraph,PStack,PMap) -> String -> (PGraph,PStack,PMap)
parse gs@(graph,stack,dictionary) str =
    case str of
        "def" -> let num = fst . fromJust $ stack `at` 0
                     node = fromJust $ stack `at` 1
                     dictionary' = Map.insert num node dictionary
                     stack' = Stack.pop 1 stack
                 in (graph, stack', dictionary')

        "ref" -> let num = fst . fromJust $ stack `at` 0
                     node = fromJust (Map.lookup num dictionary)
                     stack' = node <:> (Stack.pop 1 stack)
                 in (graph, stack', dictionary)

        "remove" -> let num = fst . fromJust $ stack `at` 0
                        node = fromJust (Map.lookup num dictionary)
                        stack' = node <:> (Stack.pop 1 stack)
                        dictionary' = Map.delete num dictionary
                    in (graph, stack', dictionary')

        "pop" -> (graph, (Stack.pop 1 stack), dictionary)

        '#':rest -> gs

        n | (isNumber n) -> let node = (read n, (0,""))
                                stack' = node <:> stack
                            in (graph, stack', dictionary)

        x -> let (graph', stack') = process x (argMap x) graph stack
             in (graph', stack', dictionary)
        


doGraphGen :: [String] -> PGraph
doGraphGen list =
    let graph = Graph.empty
        stack = Stack.empty
        dictionary = Map.empty
        result = foldl' parse (graph,stack,dictionary) list
    in case result of (g,s,d) -> checkDupe g

