import System( getArgs )
import Data.Maybe
import Data.List
import Data.Set( Set )
import qualified Data.Set as Set
import Data.Map( Map, (!) )
import qualified Data.Map as Map
import ProofGraph
import qualified ProofGraph as Graph
import Stack( Stack, at, (<:>) )
import qualified Stack as Stack
import Parse



type GraphState = (Graph String,
	               Stack (Node String),
	               Map Int (Node String))



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
argMap "pop" = IO 1 0
argMap "refl" = IO 1 1
argMap "subst" = IO 2 1
argMap "thm" = IO 3 0
argMap "typeOp" = IO 1 1
argMap "var" = IO 2 1
argMap "varTerm" = IO 1 1
argMap "varType" = IO 1 1
argMap x | (isNumber x || isName x) = IO 0 1



process :: String -> CommandIO -> Graph String -> Stack (Node String) -> (Graph String, Stack (Node String))
process str io graph stack =
	let argList = map (\x -> fromJust (stack `at` x)) [0..((args io) - 1)]
	    nodeList = replicate (results io) (Node str (Set.fromList argList))
	    stack' = foldr (<:>) (Stack.pop (args io) stack) nodeList
	    graph' = Graph.insert (head nodeList) graph
    in (graph',stack')



parse :: GraphState -> String -> GraphState
parse gs@(graph,stack,dictionary) str =
	case str of
		"def" -> let num = read . contents . fromJust $ stack `at` 0
		             node = fromJust $ stack `at` 1
		             dictionary' = Map.insert num node dictionary
		             stack' = Stack.pop 1 stack
		         in (graph, stack', dictionary')

		"ref" -> let num = read . contents . fromJust $ stack `at` 0
		             node = fromJust (Map.lookup num dictionary)
		             stack' = node <:> stack
		         in (graph, stack', dictionary)

		"remove" -> let num = read . contents . fromJust $ stack `at` 0
		                node = fromJust (Map.lookup num dictionary)
		                stack' = node <:> stack
		                dictionary' = Map.delete num dictionary
		            in (graph, stack', dictionary')

		'#':rest -> gs

		x -> let (graph', stack') = process x (argMap x) graph stack
			 in (graph', stack', dictionary)
		


doGraphGen :: [String] -> GraphState
doGraphGen list =
	let graph = Graph.empty
	    stack = Stack.empty
	    dictionary = Map.empty
	in foldl' (parse) (graph,stack,dictionary) list



main = do
	args <- getArgs
	list <- getLines $ head args
	let result = doGraphGen (map (stripReturn) list)
	print $ show result

