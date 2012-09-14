module Library.TermNet(
    TermNet,

    empty,
    getLeafList,
    getBranchList,

    termToTermString,
    thmToTermString,

    addThm,
    addThmFromNode
    ) where



import Data.Maybe
import Data.List
import qualified Data.Set as Set
import Data.Graph.Inductive.Graph( Node )
import qualified Data.Graph.Inductive.Graph as Graph
import Library.ProofGraph
import Library.WriteProof
import Library.Object
import Library.Theorem
import Library.Term
import Library.Parse
import Library.Semantic
import Library.Stack( Stack, at, (<:>) )
import qualified Library.Stack as Stack



data TermNet = Leaf [(Theorem, Node)] | Branch [(String, TermNet)] deriving (Eq, Show)



empty :: TermNet
empty = Branch []



isLeaf :: TermNet -> Bool
isLeaf (Leaf _) = True
isLeaf _ = False



isBranch :: TermNet -> Bool
isBranch (Branch _) = True
isBranch _ = False



getLeafList :: TermNet -> Maybe [(Theorem, Node)]
getLeafList net =
    case net of
        Leaf list -> Just list
        Branch list -> Nothing



getBranchList :: TermNet -> Maybe [(String, TermNet)]
getBranchList net =
    case net of 
        Leaf list -> Nothing
        Branch list -> Just list



genThm :: PGraph -> Node -> Theorem
genThm graph node =
    let gen = (\g n num -> let edge = filter (\x -> (fst . thd3 $ x) == num) (Graph.out g n)
                               node = (snd3 . head $ edge)
                               listing = write g node
                           in fromJust (((\(a,_,_,_) -> a) . fromJust $ (eval listing)) `at` 0))
        hypList = map (fromJust . objTerm) (fromJust . objList $ (gen graph node 2))
        con = fromJust . objTerm $ (gen graph node 1)
    in Theorem (Set.fromList hypList) con



termToTermString :: Term -> [String]
termToTermString term =
    case term of
        (TConst _ _) ->
            ["const"]

        (TApp func arg) ->
            ["app"] ++ (termToTermString func) ++ (termToTermString arg)

        (TAbs var body) ->
            ["abs"] ++ (termToTermString body)

        (TVar var) ->
            ["var"]



thmToTermString :: Theorem -> [String]
thmToTermString theorem =
    let hypList = Set.toList (thmHyp theorem)
        f = (\soFar hyp -> soFar ++ ["hyp"] ++ (termToTermString hyp))
    in (foldl' f [] hypList) ++ ["con"] ++ (termToTermString . thmCon $ theorem)



addThm :: TermNet -> Theorem -> Node -> (TermNet, [(Theorem, Node)])
addThm net theorem node =
    let stringThm = thmToTermString theorem
        add = (\net list ->
                let branchList = fromJust . getBranchList $ net
                in case list of
                        [] ->
                            (net, [])

                        x:[] ->
                            let (check, rest) = partition (\(y,z) -> y == x && isLeaf z) branchList
                            in if (check == [])
                                then (Branch ((x,(Leaf [(theorem,node)])):rest), [(theorem,node)])
                                else let leaf = snd . head $ check
                                         leafList = fromJust . getLeafList $ leaf
                                     in --if ((theorem,node) `elem` leafList)
                                        --then (Branch ((x,leaf):rest), leafList)
                                        --else
                                        (Branch ((x,(Leaf ((theorem,node):leafList))):rest), (theorem,node):leafList)

                        x:xs ->
                            let (check, rest) = partition (\(y,z) -> y == x && isBranch z) branchList
                            in if (check == [])
                                then let (net', resultList) = add empty xs
                                     in (Branch ((x,net'):rest), resultList)
                                else let nextStepDown = snd . head $ check
                                         (net', resultList) = add nextStepDown xs
                                     in (Branch ((head check):rest), resultList))

    in add net stringThm



addThmFromNode :: TermNet -> PGraph -> Node -> (TermNet, [(Theorem, Node)])
addThmFromNode net graph node =
    let theorem = genThm graph node
    in addThm net theorem node


