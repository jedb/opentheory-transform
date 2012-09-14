import System.Environment( getArgs )
import Text.Printf
import Library.Parse
import Library.ProofGraph
import Library.GraphPart
import Library.TermNet( TermNet )
import qualified Library.TermNet as TermNet
import qualified Data.Graph.Inductive.Graph as Graph



data Step = Step { from :: Node
                 , to :: Node
                 , sub :: Substitution }



createSubst :: Theorem -> Theorem -> Maybe Substitution
createSubst one two =
    let alreadyMatched = (\x y m -> (Map.member x m || y `elem` Map.elems m) && Map.lookup x m /= Just y)

        f = (\x y tymap varmap ->
                case (x,y) of
                    (TConst a1 ty1, TConst a2 ty2) ->
                        if (alreadyMatch ty1 ty2 tymap || a1 /= a2)
                        then Nothing
                        else let tymap' = if (ty1 == ty2) then tymap else Map.insert ty1 ty2 tymap
                             in Just (tymap', varmap')

                    (TApp a1 b1, TApp a2 b2) ->
                        do (tymap', varmap') <- f a1 a2 tymap varmap
                           f b1 b2 tymap' varmap'

                    (TAbs (TVar (Var n1 ty1)) b1, TAbs (TVar (Var n2 ty2)) b2) ->
                        if (alreadyMatched ty1 ty2 tymap || alreadyMatched n1 n2 varmap)
                        then Nothing
                        else let tymap' = if (ty1 == ty2) then tymap else Map.insert ty1 ty2 tymap
                                 varmap' = if (n1 == n2) then varmap else Map.insert n1 n2 varmap
                             in f b1 b2 tymap' varmap'

                    (TVar (Var n1 ty1), TVar (Var n2 ty2)) ->
                        if (alreadyMatched ty1 ty2 tymap || alreadyMatched n1 n2 varmap)
                        then Nothing
                        else let tymap' = if (ty1 == ty2) then tymap else Map.insert ty1 ty2 tymap
                                 varmap' = if (n1 == n2) then varmap else Map.insert n1 n2 varmap
                             in Just (tymap', varmap')

                    (other, combination) -> Nothing)

        g = (\x y tymap varmap ->
                let (hyp1,hyp2) = (Set.fromList . thmHyp $ x, Set.fromList . thmHyp $ y)
                    if (hyp1 == [])
                    then f (thmCon x) (thmCon y) tymap varmap
                    else do (tymap', varmap') <- f (head hyp1) (head hyp2) tymap varmap
                            g (Theorem (Set.fromList . tail $ hyp1) (thmCon x))
                              (Theorem (Set.fromList . tail $ hyp2) (thmCon y))
                              tymap' varmap')

        maps = g one two Map.empty Map.empty

    in if (Set.size . thmHyp $ one /= Set.size . thmHyp $ two || isNothing maps)
       then Nothing
       else do (t,v) <- maps
               return (Map.toList t, Map.toList v)



checkNode :: PGraph -> TermNet -> Node -> (TermNet, Maybe (GraphPart, GraphPart))
checkNode graph termnet node =
    let (termnet', list) = TermNet.addThm termnet graph node
        (working, possibles) = partition (\x -> snd x == node) list

        canBeSubbed = (\inn out ->
            if (inn == [])
            then out
            else let sub = createSubst (fst working) (fst . head $ inn)
                     out' = if (isNothing sub)
                            then out
                            else (Step (snd working) (snd . head $ inn) (fromJust sub)) : out
                 in canBeSubbed (tail inn) out'

        maxPositiveBenefit = (\list ->
            let sortVal = (\x -> (size . snd x) - (addedSize (fst x) graph) - (overlap (fst x) (snd x)))
                sorted = sortBy (\x y -> compare (sortVal x) (sortVal y)) list
                check = head sorted
            in if (sortVal check > 0) then check else Nothing)

        step = maxPositiveBenefit . (map (stepGen graph)) $ (canBeSubbed possibles [])

    in (termnet', step)



stepGen :: PGraph -> Step -> (GraphPart, GraphPart)
stepGen graph step =
    let unused = graphPart (findUnused graph (Graph.suc graph (to step))) [] Nothing Nothing

        base = doGraphGen (substitutionGen . sub $ step)
        num = Graph.newNodes 1 base
        node = (num, "subst")
        cons = head (filter (\x -> Graph.indeg base x == 0) (Graph.nodes base))
        edge = (num, cons, (2,1))

        substGraph = (Graph.insEdge edge) . (Graph.insNode node) $ base
        subst = makeGraphPart substGraph (Just (node,1)) (Just (node,1))

    in (subst, unused)



findUnused :: PGraph -> [Node] -> [Node]
findUnused graph nodeList =
    let nextLayer = concat . (map (Graph.suc graph)) $ nodeList
        unused = filter (\x -> all (\y -> y `elem` nodeList) (Graph.pre graph x)) nextLayer
    in nodeList ++ (findUnused graph usused)



doMeaningSubst :: PGraph -> PGraph
doMeaningSubst graph =
    let f = (\g termnet nodeList ->
                if (nodeList == [])
                then g
                else let working = head nodeList
                         (termnet', step) = checkNode g termnet working
                         (g', nodeList') = if (isNothing step)
                                           then (g, nodeList)
                                           else let (subst, unused) = fromJust step
                                                in ((graphAdd subst () ()) . (graphDel unused) $ g, nodeList \\ (nodes unused))
                     in f g' termnet' nodeList')

    in f graph TermNet.empty (Graph.nodes graph)



main = do
    args <- getArgs
    list <- getLines (head args)
    let graph = doGraphGen (map stripReturn list)
        result = doMeaningSubst graph
    printf $ (show result) ++ "\n"
