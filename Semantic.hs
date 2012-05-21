import System( getArgs )
import Data.List
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Map as Map
import TypeVar
import Term
import Theorem
import Object
import Parse
import qualified Command as Com


data Stack = Stack { stackList :: [Object] }
data Dictionary = Dictionary { dictionMap :: Map.Map Int Object }
data Assumptions = Assumptions { assumeSet :: Set.Set Theorem }
data Theorems = Theorems { theoremSet :: Set.Set Theorem }


instance Show Stack where
    show a   =   "Stack:\n" ++ intercalate "\n" (map (show) (stackList a)) ++ "\n\n"

instance Show Dictionary where
    show a   =   "Dictionary:\n" ++ intercalate "\n" (map (show) (Map.toList . dictionMap $ a)) ++ "\n\n"

instance Show Assumptions where
    show a   =   "Assumptions:\n" ++ intercalate "\n" (map (show) (Set.toList . assumeSet $ a)) ++ "\n\n"

instance Show Theorems where
    show a   =   "Theorems:\n" ++ intercalate "\n" (map (show) (Set.toList . theoremSet $ a)) ++ "\n\n"


data ArticleLine = Comment { commentString :: String }
                 | Command { commandFunc :: ((Stack,Dictionary,Assumptions,Theorems) -> Maybe (Stack,Dictionary,Assumptions,Theorems)) }



parse :: String -> ArticleLine
parse "absTerm" = Command absTerm
parse "absThm" = Command absThm
parse "appTerm" = Command appTerm
parse "appThm" = Command appThm
parse "assume" = Command assume
parse "axiom" = Command axiom
parse "betaConv" = Command betaConv
parse "cons" = Command cons
parse "const" = Command constant
parse "constTerm" = Command constTerm
parse "deductAntisym" = Command deductAntisym
parse "def" = Command def
parse "defineConst" = Command defineConst
parse "defineTypeOp" = Command defineTypeOp
parse "eqMp" = Command eqMp
parse "nil" = Command nil
parse "opType" = Command opType
parse "pop" = Command pop
parse "ref" = Command ref
parse "refl" = Command refl
parse "remove" = Command remove
parse "subst" = Command subst
parse "thm" = Command thm
parse "typeOp" = Command typeOp
parse "var" = Command var
parse "varTerm" = Command varTerm
parse "varType" = Command varType
parse s@('#':rest) = Comment s
parse s@('"':rest) = Command (name s)
parse n = Command (number n)



name :: String -> ((Stack,Dictionary,Assumptions,Theorems) -> Maybe (Stack,Dictionary,Assumptions,Theorems))
name str = \(s,d,a,t) ->
               let n = Com.name str
                   s' = Stack $ ObjName (fromMaybe nullName n) : (stackList s)
               in if (isNothing n)
                  then Nothing
                  else Just (s',d,a,t)


number :: String -> ((Stack,Dictionary,Assumptions,Theorems) -> Maybe (Stack,Dictionary,Assumptions,Theorems))
number n = \(s,d,a,t) ->
               let num = Com.number $ n
                   s' = Stack $ ObjNum (fromMaybe nullNumber num) : (stackList s)
               in if (isNothing num)
                  then Nothing
                  else Just (s',d,a,t)


absTerm :: (Stack,Dictionary,Assumptions,Theorems) -> Maybe (Stack,Dictionary,Assumptions,Theorems)
absTerm (s,d,a,t) =
    let stack = stackList s
        term = ObjTerm $ Com.absTerm (objTerm $ stack!!0) (objVar $ stack!!1)
        s' = Stack $ term : (drop 2 stack)
    in Just (s',d,a,t)


absThm :: (Stack,Dictionary,Assumptions,Theorems) -> Maybe (Stack,Dictionary,Assumptions,Theorems)
absThm (s,d,a,t) =
    let stack = stackList s
        thm = Com.absThm (objThm $ stack!!0) (objVar $ stack!!1)
        s' = Stack $ (ObjThm $ fromMaybe nullThm thm) : (drop 2 stack)
    in if (isNothing thm)
       then Nothing
       else Just (s',d,a,t)


appTerm :: (Stack,Dictionary,Assumptions,Theorems) -> Maybe (Stack,Dictionary,Assumptions,Theorems)
appTerm (s,d,a,t) =
    let stack = stackList s
        term = ObjTerm $ Com.appTerm (objTerm $ stack!!0) (objTerm $ stack!!1)
        s' = Stack $ term : (drop 2 stack)
    in Just (s',d,a,t)


appThm :: (Stack,Dictionary,Assumptions,Theorems) -> Maybe (Stack,Dictionary,Assumptions,Theorems)
appThm (s,d,a,t) =
    let stack = stackList s
        thm = ObjThm $ Com.appThm (objThm $ stack!!0) (objThm $ stack!!1)
        s' = Stack $ thm : (drop 2 stack)
    in Just (s',d,a,t)


assume :: (Stack,Dictionary,Assumptions,Theorems) -> Maybe (Stack,Dictionary,Assumptions,Theorems)
assume (s,d,a,t) =
    let stack= stackList s
        thm = Com.assume (objTerm $ stack!!0)
        s' = Stack $ (ObjThm $ fromMaybe nullThm thm) : (tail stack)
    in if (isNothing thm)
       then Nothing
       else Just (s',d,a,t)


axiom :: (Stack,Dictionary,Assumptions,Theorems) -> Maybe (Stack,Dictionary,Assumptions,Theorems)
axiom (s,d,a,t) =
    let stack = stackList s
        assumptions = assumeSet a
        thm = Com.axiom (objTerm $ stack!!0) (map (objTerm) . objList $ stack!!1)
        s' = Stack $ (ObjThm $ fromMaybe nullThm thm) : (drop 2 stack)
        a' = Assumptions $ Set.insert (fromMaybe nullThm thm) assumptions
    in if (isNothing thm)
       then Nothing
       else Just (s',d,a',t)


betaConv :: (Stack,Dictionary,Assumptions,Theorems) -> Maybe (Stack,Dictionary,Assumptions,Theorems)
betaConv (s,d,a,t) =
    let stack = stackList s
        thm = ObjThm $ Com.betaConv (objTerm $ stack!!0)
        s' = Stack $ thm : (tail stack)
    in Just (s',d,a,t)


cons :: (Stack,Dictionary,Assumptions,Theorems) -> Maybe (Stack,Dictionary,Assumptions,Theorems)
cons (s,d,a,t) =
    let stack = stackList s
        newList = ObjList $ (stack!!1) : (objList $ stack!!0)
        s' = Stack $ newList : (drop 2 stack)
    in Just (s',d,a,t)


constant :: (Stack,Dictionary,Assumptions,Theorems) -> Maybe (Stack,Dictionary,Assumptions,Theorems)
constant (s,d,a,t) =
    let stack = stackList s
        constant = ObjConst $ Com.constant (objName $ stack!!0)
        s' = Stack $ constant : (tail stack)
    in Just (s',d,a,t)


constTerm :: (Stack,Dictionary,Assumptions,Theorems) -> Maybe (Stack,Dictionary,Assumptions,Theorems)
constTerm (s,d,a,t) =
    let stack = stackList s
        term = ObjTerm $ Com.constTerm (objType $ stack!!0) (objConst $ stack!!1)
        s' = Stack $ term : (drop 2 stack)
    in Just (s',d,a,t)


deductAntisym :: (Stack,Dictionary,Assumptions,Theorems) -> Maybe (Stack,Dictionary,Assumptions,Theorems)
deductAntisym (s,d,a,t) =
    let stack = stackList s
        thm = ObjThm $ Com.deductAntisym (objThm $ stack!!0) (objThm $ stack!!1)
        s' = Stack $ thm : (drop 2 stack)
    in Just (s',d,a,t)


def :: (Stack,Dictionary,Assumptions,Theorems) -> Maybe (Stack,Dictionary,Assumptions,Theorems)
def (s,d,a,t) =
    let stack = stackList s
        d' = Dictionary $ Map.insert (objNum $ stack!!0) (stack!!1) (dictionMap d)
        s' = Stack $ tail stack
    in Just (s',d',a,t)


defineConst :: (Stack,Dictionary,Assumptions,Theorems) -> Maybe (Stack,Dictionary,Assumptions,Theorems)
defineConst (s,d,a,t) =
    let stack = stackList s
        result = Com.defineConst (objTerm $ stack!!0) (objName $ stack!!1)
        (thm, constant) = fromMaybe (nullThm, nullConst) result
        s' = Stack $ (ObjThm thm) : (ObjConst constant) : (drop 2 stack)
    in if (isNothing result)
       then Nothing
       else Just (s',d,a,t)


defineTypeOp :: (Stack,Dictionary,Assumptions,Theorems) -> Maybe (Stack,Dictionary,Assumptions,Theorems)
defineTypeOp (s,d,a,t) =
    let stack = stackList s
        result = Com.defineTypeOp (objThm $ stack!!0) (map (objName) . objList $ stack!!1)
                                  (objName $ stack!!2) (objName $ stack!!3) (objName $ stack!!4)
        (rthm, athm, rep, abst, n) = fromMaybe (nullThm, nullThm, nullConst, nullConst, nullTyOp) result
        s' = Stack $ (ObjThm rthm) : (ObjThm athm) : (ObjConst rep) : (ObjConst abst) : (ObjTyOp n) : (drop 5 stack)
    in if (isNothing result)
       then Nothing
       else Just (s',d,a,t)


eqMp :: (Stack,Dictionary,Assumptions,Theorems) -> Maybe (Stack,Dictionary,Assumptions,Theorems)
eqMp (s,d,a,t) =
    let stack = stackList s
        thm = Com.eqMp (objThm $ stack!!0) (objThm $ stack!!1)
        s' = Stack $ (ObjThm $ fromMaybe nullThm thm) : (drop 2 stack)
    in if (isNothing thm)
       then Nothing
       else Just (s',d,a,t)


nil :: (Stack,Dictionary,Assumptions,Theorems) -> Maybe (Stack,Dictionary,Assumptions,Theorems)
nil (s,d,a,t) = Just (Stack $ ObjList [] : (stackList s), d, a, t)


opType :: (Stack,Dictionary,Assumptions,Theorems) -> Maybe (Stack,Dictionary,Assumptions,Theorems)
opType (s,d,a,t) =
    let stack = stackList s
        newType = ObjType $ Com.opType (map (objType) . objList $ stack!!0) (objTyOp $ stack!!1)
        s' = Stack $ newType : (drop 2 stack)
    in Just (s',d,a,t)


pop :: (Stack,Dictionary,Assumptions,Theorems) -> Maybe (Stack,Dictionary,Assumptions,Theorems)
pop (s,d,a,t) = Just (Stack $ tail (stackList s),d,a,t)


ref :: (Stack,Dictionary,Assumptions,Theorems) -> Maybe (Stack,Dictionary,Assumptions,Theorems)
ref (s,d,a,t) =
    let stack = stackList s
        dictionary = dictionMap d
        object = dictionary Map.! (objNum $ stack!!0)
        s' = Stack $ object : tail stack
    in Just (s',d,a,t)


refl :: (Stack,Dictionary,Assumptions,Theorems) -> Maybe (Stack,Dictionary,Assumptions,Theorems)
refl (s,d,a,t) =
    let stack = stackList s
        thm = ObjThm $ Com.refl (objTerm $ stack!!0)
        s' = Stack $ thm : (tail stack)
    in Just (s',d,a,t)


remove :: (Stack,Dictionary,Assumptions,Theorems) -> Maybe (Stack,Dictionary,Assumptions,Theorems)
remove (s,d,a,t) =
    let stack = stackList s
        dictionary = dictionMap d
        object = dictionary Map.! (objNum $ stack!!0)
        s' = Stack $ object : tail stack
        d' = Dictionary $ Map.delete (objNum $ stack!!0) dictionary
    in Just (s',d',a,t)


subst :: (Stack,Dictionary,Assumptions,Theorems) -> Maybe (Stack,Dictionary,Assumptions,Theorems)
subst (s,d,a,t) =
    let stack = stackList s
        thm = ObjThm $ Com.subst (objThm $ stack!!0) (objList $ stack!!1)
        s' = Stack $ thm : (drop 2 stack)
    in Just (s',d,a,t)


thm :: (Stack,Dictionary,Assumptions,Theorems) -> Maybe (Stack,Dictionary,Assumptions,Theorems)
thm (s,d,a,t) =
    let stack = stackList s
        thmSet = theoremSet t
        thm = Com.thm (objTerm $ stack!!0) (map (objTerm) . objList $ stack!!1) (objThm $ stack!!2)
        s' = Stack $ (drop 3 stack)
        t' = Theorems $ Set.insert (fromMaybe nullThm thm) thmSet
    in if (isNothing thm)
       then Nothing
       else Just (s',d,a,t')


typeOp :: (Stack,Dictionary,Assumptions,Theorems) -> Maybe (Stack,Dictionary,Assumptions,Theorems)
typeOp (s,d,a,t) =
    let stack = stackList s
        typeOp = ObjTyOp $ Com.typeOp (objName $ stack!!0)
        s' = Stack $ typeOp : (tail stack)
    in Just (s',d,a,t)


var :: (Stack,Dictionary,Assumptions,Theorems) -> Maybe (Stack,Dictionary,Assumptions,Theorems)
var (s,d,a,t) =
    let stack = stackList s
        v = Com.var (objType $ stack!!0) (objName $ stack!!1)
        s' = Stack $ (ObjVar $ fromMaybe nullVar v) : (drop 2 stack)
    in if (isNothing v)
       then Nothing
       else Just (s',d,a,t)


varTerm :: (Stack,Dictionary,Assumptions,Theorems) -> Maybe (Stack,Dictionary,Assumptions,Theorems)
varTerm (s,d,a,t) =
    let stack = stackList s
        term = ObjTerm $ Com.varTerm (objVar $ stack!!0)
        s' = Stack $ term : (tail stack)
    in Just (s',d,a,t)


varType :: (Stack,Dictionary,Assumptions,Theorems) -> Maybe (Stack,Dictionary,Assumptions,Theorems)
varType (s,d,a,t) =
    let stack = stackList s
        newType = Com.varType (objName $ stack!!0)
        s' = Stack $ (ObjType $ fromMaybe nullType newType) : (tail stack)
    in if (isNothing newType)
       then Nothing
       else Just (s',d,a,t)



doSemanticCheck :: [String] -> Maybe (Stack,Dictionary,Assumptions,Theorems)
doSemanticCheck =
    let s = Stack []
        d = Dictionary Map.empty
        a = Assumptions Set.empty
        t = Theorems Set.empty
        op = (\x y -> case x of (Nothing) -> Nothing
                                (Just w) -> case y of (Comment _) -> x
                                                      (Command z) -> z w)
    in (foldl' (op) (Just (s,d,a,t))) . (map (parse))
    -- important to use foldl here so commands get applied in the correct order


main = do
      args <- getArgs
      list <- getLines $ head args
      result <- return $ doSemanticCheck (map (stripReturn) list)
      print $ result
