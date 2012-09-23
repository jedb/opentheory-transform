import System.Environment( getArgs )
import Text.Printf
import Library.Parse
import Library.ProofGraph
import Library.GraphPart
import Library.Theorem
import Library.Term
import Library.TypeVar
import Library.Generator
import Library.TermNet( TermNet )
import qualified Library.TermNet as TermNet
import Data.Graph.Inductive.Graph( Node, LNode )
import qualified Data.Graph.Inductive.Graph as Graph
import Data.Map( Map )
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List
import Data.Maybe



data Step = Step { from :: Node
                 , to :: Node
                 , sub :: Substitution }



alreadyMatchedElem :: (Ord a, Eq b) => a -> b -> Map a b -> Bool
alreadyMatchedElem x y xymap =
    Map.member x xymap && Map.lookup x xymap /= Just y



createSubst :: Theorem -> Theorem -> Maybe Substitution
createSubst one two =
    let f = (\x y tymap varmap ->
                case (x,y) of
                    (TVar (Var n ty), term) ->
                        if (alreadyMatchedElem (Var n ty) term varmap)
                        then Nothing
                        else let varmap' = if (Map.lookup (Var n ty) varmap == Nothing)
                                           then Map.insert (Var n ty) term varmap
                                           else varmap
                             in Just (tymap, varmap')

                    (TConst a1 ty1, TConst a2 ty2) ->
                        if (alreadyMatchedElem (typeVar ty1) ty2 tymap || a1 /= a2 || (ty1 /= ty2 && not (isTypeVar ty1)))
                        then Nothing
                        else let tymap' = if (ty1 /= ty2)
                                          then Map.insert (typeVar ty1) ty2 tymap
                                          else tymap
                             in Just (tymap', varmap)

                    (TApp a1 b1, TApp a2 b2) ->
                        do (tymap', varmap') <- f a1 a2 tymap varmap
                           f b1 b2 tymap' varmap'

                    (TAbs a1 b1, TAbs a2 b2) ->
                        do (tymap', varmap') <- f a1 a2 tymap varmap
                           f b1 b2 tymap' varmap'

                    (other, combination) -> Nothing)

        g = (\x y tymap varmap ->
                let (hyp1,hyp2) = (Set.toList . thmHyp $ x, Set.toList . thmHyp $ y)
                in if (hyp1 == [] && hyp2 == [])
                   then f (thmCon x) (thmCon y) tymap varmap
                   else do (tymap', varmap') <- f (head hyp1) (head hyp2) tymap varmap
                           g (Theorem (Set.fromList . tail $ hyp1) (thmCon x))
                             (Theorem (Set.fromList . tail $ hyp2) (thmCon y))
                             tymap' varmap')

        maps = g one two Map.empty Map.empty

    in if ((Set.size . thmHyp $ one) /= (Set.size . thmHyp $ two) || isNothing maps)
       then Nothing
       else do (t,v) <- maps
               return (Map.toList t, Map.toList v)



checkNode :: PGraph -> TermNet -> Node -> (TermNet, Maybe (GraphPart, GraphPart), Maybe (Node,Node))
checkNode graph termnet node =
    let theorem = TermNet.genThm graph node
        possibles = TermNet.matchThm termnet theorem 

        canBeSubbed = (\inn out ->
            if (inn == [])
            then out
            else let sub = createSubst theorem (fst . head $ inn)
                     out' = if (isNothing sub)
                            then out
                            else (Step node (snd . head $ inn) (fromJust sub)) : out
                 in canBeSubbed (tail inn) out')

        maxPositiveBenefit = (\list ->
            let sortVal = (\x -> let added = fst . snd $ x
                                     removed = snd . snd $ x
                                     inn = from . fst $ x
                                     out = to . fst $ x
                                     outNodes = map (\(x,(y,z)) -> (x,y)) (Graph.lpre graph out)
                                 in (size removed) - (addedSize added (Just (inn,1)) outNodes graph) - (overlap added removed))
                sorted = sortBy (\x y -> compare (sortVal x) (sortVal y)) list
                check = head sorted
            in if (sortVal check > 0) then (Just (snd check), Just (from . fst $ check, to . fst $ check)) else (Nothing, Nothing))

        (step, between) = maxPositiveBenefit . (map (\x -> (x, stepGen graph x))) $ (canBeSubbed possibles [])
        termnet' = TermNet.addThm termnet theorem node

    in (termnet', step, between)



stepGen :: PGraph -> Step -> (GraphPart, GraphPart)
stepGen graph step =
    let unusedNodes = findUnused graph (Graph.suc graph (to step))
        unusedLabNodes = filter (\(x,y) -> x `elem` unusedNodes) (Graph.labNodes graph)
        unused = graphPart unusedLabNodes [] Nothing Nothing

        base = doGraphGen (substitutionGen . sub $ step)
        num = head (Graph.newNodes 1 base)
        node = (num, "subst")
        cons = head (filter (\x -> Graph.indeg base x == 0) (Graph.nodes base))
        edge = (num, cons, (2,1))

        substGraph = (Graph.insEdge edge) . (Graph.insNode node) $ base
        subst = makeGraphPart substGraph (Just (num,1)) (Just (num,1))

    in (subst, unused)



findUnused :: PGraph -> [Node] -> [Node]
findUnused graph nodeList =
    let nextLayer = concat . (map (Graph.suc graph)) $ nodeList
        unused = filter (\x -> all (\y -> y `elem` nodeList) (Graph.pre graph x)) nextLayer
    in nodeList ++ (findUnused graph unused)



doMeaningSubst :: PGraph -> PGraph
doMeaningSubst graph =
    let f = (\g termnet nodeList ->
                if (nodeList == [])
                then g
                else let working = head nodeList
                         (termnet', step, between) = checkNode g termnet working
                         (g', nodeList') = if (isNothing step || isNothing between)
                                           then (g, nodeList)
                                           else let (subst, unused) = fromJust step
                                                    (from, to) = fromJust between
                                                    toNodes = map (\(x,(y,z)) -> (x,y)) (Graph.lpre g to)
                                                in ((graphAdd subst (Just (from, 1)) toNodes) . (graphDel unused) $ g,
                                                    nodeList \\ (map (\(x,y) -> x) (nodes unused)))
                     in f g' termnet' nodeList')

    in f graph TermNet.empty (Graph.nodes graph)



main = do
    args <- getArgs
    list <- getLines (head args)
    let graph = doGraphGen (map stripReturn list)
        result = doMeaningSubst graph
    printf $ (show result) ++ "\n"
