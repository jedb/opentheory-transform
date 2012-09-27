module Library.Semantic (
    MachineState,
    machineToString,
    eval,
    doSemanticCheck
    ) where



import Data.List
import Data.Maybe
import Data.Set( Set )
import qualified Data.Set as Set
import Data.Map( Map, (!) )
import qualified Data.Map as Map
import Library.TypeVar
import Library.Term
import Library.Theorem
import Library.Object
import Library.Parse
import Library.Stack( Stack, at, (<:>) )
import qualified Library.Stack as Stack
import qualified Library.Command as Com



type MachineState = Maybe (Stack Object,
                           Map Int Object, --dictionary
                           Set Theorem, --assumptions
                           Set Theorem) --theorems


machineToString :: MachineState -> Maybe String
machineToString x =
    do (s,d,a,t) <- x
       let s' = show s
           d' = "Dictionary:\n" ++ intercalate "\n" (map (show) (Map.toList d)) ++ "\n\n"
           a' = "Assumptions:\n" ++ intercalate "\n" (map (show) (Set.toList a)) ++ "\n\n"
           t' = "Theorems:\n" ++ intercalate "\n" (map (show) (Set.toList t)) ++ "\n\n"
       return (s' ++ d' ++ a' ++ t')


data ArticleLine = Comment { commentString :: String }
                 | Command { commandFunc :: (MachineState -> MachineState) }



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



name :: String -> (MachineState -> MachineState)
name str = \x ->
    do (s,d,a,t) <- x
       n <- Com.name str
       let s' = (ObjName n) <:> s
       return (s',d,a,t)


number :: String -> (MachineState -> MachineState)
number n = \x ->
    do (s,d,a,t) <- x
       num <- Com.number n
       let s' = (ObjNum num) <:> s
       return (s',d,a,t)


absTerm :: MachineState -> MachineState
absTerm x =
    do (s,d,a,t) <- x
       te <- (s `at` 0) >>= objTerm; v <- (s `at` 1) >>= objVar
       let term = Com.absTerm te v
           s' = (ObjTerm term) <:> (Stack.pop 2 s)
       return (s',d,a,t)


absThm :: MachineState -> MachineState
absThm x =
    do (s,d,a,t) <- x
       th <- (s `at` 0) >>= objThm; v <- (s `at` 1) >>= objVar
       thm <- Com.absThm th v
       let s' = (ObjThm thm) <:> (Stack.pop 2 s)
       return (s',d,a,t)


appTerm :: MachineState -> MachineState
appTerm x =
    do (s,d,a,t) <- x
       f <- (s `at` 0) >>= objTerm; x <- (s `at` 1) >>= objTerm
       let term = Com.appTerm f x
           s' = (ObjTerm term) <:> (Stack.pop 2 s)
       return (s',d,a,t)


appThm :: MachineState -> MachineState
appThm x =
    do (s,d,a,t) <- x
       t1 <- (s `at` 0) >>= objThm; t2 <- (s `at` 1) >>= objThm
       let thm = Com.appThm t1 t2
           s' = (ObjThm thm) <:> (Stack.pop 2 s)
       return (s',d,a,t)


assume :: MachineState -> MachineState
assume x =
    do (s,d,a,t) <- x
       te <- (s `at` 0) >>= objTerm
       thm <- Com.assume te
       let s' = (ObjThm thm) <:> (Stack.pop 1 s)
       return (s',d,a,t)


axiom :: MachineState -> MachineState
axiom x =
    do (s,d,a,t) <- x
       te <- (s `at` 0) >>= objTerm; l <- (s `at` 1) >>= objList
       thm <- Com.axiom te (mapMaybe objTerm l)
       let s' = (ObjThm thm) <:> (Stack.pop 2 s)
           a' = Set.insert thm a
       return (s',d,a',t)


betaConv :: MachineState -> MachineState
betaConv x =
    do (s,d,a,t) <- x
       te <- (s `at` 0) >>= objTerm
       let thm = Com.betaConv te
           s' = (ObjThm thm) <:> (Stack.pop 1 s)
       return (s',d,a,t)


cons :: MachineState -> MachineState
cons x =
    do (s,d,a,t) <- x
       l <- (s `at` 0) >>= objList; h <- (s `at` 1)
       let s' = (ObjList $ h : l) <:> (Stack.pop 2 s)
       return (s',d,a,t)


constant :: MachineState -> MachineState
constant x =
    do (s,d,a,t) <- x
       n <- (s `at` 0) >>= objName
       let constant = Com.constant n
           s' = (ObjConst constant) <:> (Stack.pop 1 s)
       return (s',d,a,t)


constTerm :: MachineState -> MachineState
constTerm x =
    do (s,d,a,t) <- x
       ty <- (s `at` 0) >>= objType; c <- (s `at` 1) >>= objConst
       let term = Com.constTerm ty c
           s' = (ObjTerm term) <:> (Stack.pop 2 s)
       return (s',d,a,t)


deductAntisym :: MachineState -> MachineState
deductAntisym x =
    do (s,d,a,t) <- x
       t1 <- (s `at` 0) >>= objThm; t2 <- (s `at` 1) >>= objThm
       let thm = Com.deductAntisym t1 t2
           s' = (ObjThm thm) <:> (Stack.pop 2 s)
       return (s',d,a,t)


def :: MachineState -> MachineState
def x =
    do (s,d,a,t) <- x
       num <- (s `at` 0) >>= objNum; obj <- (s `at` 1)
       let d' = Map.insert num obj d
           s' = Stack.pop 1 s
       return (s',d',a,t)


defineConst :: MachineState -> MachineState
defineConst x =
    do (s,d,a,t) <- x
       te <- (s `at` 0) >>= objTerm; n <- (s `at` 1) >>= objName
       (thm, constant) <- Com.defineConst te n
       let s' = (ObjThm thm) <:> (ObjConst constant) <:> (Stack.pop 2 s)
       return (s',d,a,t)


defineTypeOp :: MachineState -> MachineState
defineTypeOp x =
    do (s,d,a,t) <- x
       th <- (s `at` 0) >>= objThm; l <- (s `at` 1) >>= objList; r <- (s `at` 2) >>= objName
       ab <- (s `at` 3) >>= objName; y <- (s `at` 4) >>= objName
       (rthm, athm, rep, abst, n) <- Com.defineTypeOp th (mapMaybe objName l) r ab y
       let s' = (ObjThm rthm) <:> (ObjThm athm) <:> (ObjConst rep) <:> (ObjConst abst) <:> (ObjTyOp n) <:> (Stack.pop 5 s)
       return (s',d,a,t)


eqMp :: MachineState -> MachineState
eqMp x =
    do (s,d,a,t) <- x
       t1 <- (s `at` 0) >>= objThm; t2 <- (s `at` 1) >>= objThm
       thm <- Com.eqMp t1 t2
       let s' = (ObjThm thm) <:> (Stack.pop 2 s)
       return (s',d,a,t)


nil :: MachineState -> MachineState
nil x =
    do (s,d,a,t) <- x
       return (ObjList [] <:> s, d, a, t)


opType :: MachineState -> MachineState
opType x =
    do (s,d,a,t) <- x
       l <- (s `at` 0) >>= objList; to <- (s `at` 1) >>= objTyOp
       let newType = Com.opType (mapMaybe objType l) to
           s' = (ObjType newType) <:> (Stack.pop 2 s)
       return (s',d,a,t)


pop :: MachineState -> MachineState
pop x = 
    do (s,d,a,t) <- x
       return ((Stack.pop 1 s),d,a,t)


ref :: MachineState -> MachineState
ref x =
    do (s,d,a,t) <- x
       n <- (s `at` 0) >>= objNum
       let object = d ! n
           s' = object <:> (Stack.pop 1 s)
       return (s',d,a,t)


refl :: MachineState -> MachineState
refl x =
    do (s,d,a,t) <- x
       te <- (s `at` 0) >>= objTerm
       let thm = Com.refl te
           s' = (ObjThm thm) <:> (Stack.pop 1 s)
       return (s',d,a,t)


remove :: MachineState -> MachineState
remove x =
    do (s,d,a,t) <- x
       n <- (s `at` 0) >>= objNum
       let object = d ! n
           s' = object <:> (Stack.pop 1 s)
           d' = Map.delete n d
       return (s',d',a,t)


subst :: MachineState -> MachineState
subst x =
    do (s,d,a,t) <- x
       th <- (s `at` 0) >>= objThm; l <- (s `at` 1) >>= objList
       thm <- Com.subst th l
       let s' = (ObjThm thm) <:> (Stack.pop 2 s)
       return (s',d,a,t)


thm :: MachineState -> MachineState
thm x =
    do (s,d,a,t) <- x
       te <- (s `at` 0) >>= objTerm; l <- (s `at` 1) >>= objList; th <- (s `at` 2) >>= objThm
       thm <- Com.thm te (mapMaybe objTerm l) th
       let s' = Stack.pop 3 s
           t' = Set.insert thm t
       return (s',d,a,t')


typeOp :: MachineState -> MachineState
typeOp x =
    do (s,d,a,t) <- x
       n <- (s `at` 0) >>= objName
       let typeOp = Com.typeOp n
           s' = (ObjTyOp typeOp) <:> (Stack.pop 1 s)
       return (s',d,a,t)


var :: MachineState -> MachineState
var x =
    do (s,d,a,t) <- x
       ty <- (s `at` 0) >>= objType; n <- (s `at` 1) >>= objName
       v <- Com.var ty n
       let s' = (ObjVar v) <:> (Stack.pop 2 s)
       return (s',d,a,t)


varTerm :: MachineState -> MachineState
varTerm x =
    do (s,d,a,t) <- x
       v <- (s `at` 0) >>= objVar
       let term = Com.varTerm v
           s' = (ObjTerm term) <:> (Stack.pop 1 s)
       return (s',d,a,t)


varType :: MachineState -> MachineState
varType x =
    do (s,d,a,t) <- x
       n <- (s `at` 0) >>= objName
       newType <- Com.varType n
       let s' = (ObjType newType) <:> (Stack.pop 1 s)
       return (s',d,a,t)



eval :: [String] -> MachineState
eval list =
    let s = Stack.empty
        d = Map.empty
        a = Set.empty
        t = Set.empty
        op = (\x y -> case y of (Comment _) -> x
                                (Command z) -> z x)

        -- important to use foldl here so commands get applied in the correct order
        result = (foldl' (op) (Just (s,d,a,t))) . (map (parse)) $ list

    in result



doSemanticCheck :: [String] -> String
doSemanticCheck list =
    case (machineToString (eval list)) of
           Just x -> x
           Nothing -> "Error\n"

