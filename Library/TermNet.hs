module Library.TermNet(
    TermNet,

    empty,
    getLeafList,
    getBranchList,

    genThm,
    termToTermString,
    thmToTermString,

    addThm,
    addThmFromNode,
    matchThm
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



data TermNet = Leaf [(Theorem, Node)] | Branch [(String, TermNet)]
    deriving (Eq, Show)



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
    let gen g n num = 
            let edge = filter (\x -> (fst . thd3 $ x) == num) (Graph.out g n)
                node = (snd3 . head $ edge)
                listing = write g node
            in fromJust (((\(a,_,_,_) -> a) . fromJust $ (eval listing)) `at` 0)

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
            ["abs", "var"] ++ (termToTermString body)

        (TVar var) ->
            ["var"]



thmToTermString :: Theorem -> [String]
thmToTermString theorem =
    let hypList = Set.toList (thmHyp theorem)
        f soFar hyp = soFar ++ ["hyp"] ++ (termToTermString hyp)
    in (foldl' f [] hypList) ++ ["con"] ++ (termToTermString . thmCon $ theorem)



addThm :: TermNet -> Theorem -> Node -> TermNet
addThm net theorem node =
    addThmImp net (theorem,node) (thmToTermString theorem)



addThmFromNode :: TermNet -> PGraph -> Node -> TermNet
addThmFromNode net graph node =
    let theorem = genThm graph node
    in addThmImp net (theorem,node) (thmToTermString theorem)



addThmImp :: TermNet -> (Theorem,Node) -> [String] -> TermNet
addThmImp (Branch branchList) item (x:[]) =
    let (sameKey, rest) = partition (\(y,z) -> y == x && isLeaf z) branchList
    in if (sameKey == [])
       then let leaf' = Leaf [item]
            in Branch ((x,leaf'):rest)
       else let leaf = snd . head $ sameKey
                leafList = fromJust . getLeafList $ leaf
            in if (item `elem` leafList)
               then Branch branchList
               else let leaf' = Leaf (item:leafList)
                    in Branch ((x,leaf'):rest)

addThmImp (Branch branchList) item (x:xs) =
    let (sameKey, rest) = partition (\(y,z) -> y == x) branchList
    in if (sameKey == [])
       then let net' = addThmImp empty item xs
            in Branch ((x,net'):rest)
       else let nextStepDown = snd . head $ sameKey
                net' = addThmImp nextStepDown item xs
            in Branch ((x,net'):rest)



matchThm :: TermNet -> Theorem -> [(Theorem,Node)]
matchThm net theorem =
    let hyp = Set.toList (thmHyp theorem)
        con = thmCon theorem
        (curPrefix, curTerm) = if (hyp == [])
                               then ("con", con)
                               else ("hyp", head hyp)

        r = do a <- matchImp curPrefix net
               let b = matchTermImp curTerm a
                   (branches, leaves) = partition (\x -> isBranch x) b

               c <- if (hyp == [])
                    then getLeafList (foldl' unify (Leaf []) leaves)
                    else let theorem' = Theorem (Set.fromList (tail hyp)) con
                         in return (matchThm (foldl' unify empty branches) theorem')
               return c

    in if (isNothing r) then [] else fromJust r



matchImp :: String -> TermNet -> Maybe TermNet
matchImp key net =
    do list <- getBranchList net
       let result = filter (\(x,y) -> x == key) list
       r <- if (result == []) then Nothing else Just (snd . head $ result)
       return r



matchTermImp :: Term -> TermNet -> [TermNet]
matchTermImp term net =
    let list = getBranchList net
        var = matchImp "var" net
        result =
            case term of
                (TConst c ty) -> 
                    do a <- matchImp "const" net
                       return [a]

                (TApp f x) ->
                    do a <- matchImp "app" net
                       let b = matchTermImp f a
                       return (concat (map (matchTermImp x) b))

                (TAbs v x) ->
                    do a <- matchImp "abs" net
                       b <- matchImp "var" a
                       return (matchTermImp x b)

                (TVar v) -> Nothing --don't need to do anything because variables are already taken care of

        var' = if (isNothing var) then [] else [fromJust var]
        result' = if (isNothing result) then [] else fromJust result

    in var' ++ result'



unify :: TermNet -> TermNet -> TermNet
unify (Branch a) (Branch b) = Branch (a ++ b)
unify (Leaf a) (Leaf b) = Leaf (a ++ b)

