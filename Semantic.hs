module Main (main) where


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
import Stack( Stack, at, (<:>) )
import qualified Stack as Stack
import qualified Command as Com


data Dictionary = Dictionary { dictionMap :: Map.Map Int Object }
data Assumptions = Assumptions { assumeSet :: Set.Set Theorem }
data Theorems = Theorems { theoremSet :: Set.Set Theorem }


instance Show Dictionary where
    show a   =   "Dictionary:\n" ++ intercalate "\n" (map (show) (Map.toList . dictionMap $ a)) ++ "\n\n"

instance Show Assumptions where
    show a   =   "Assumptions:\n" ++ intercalate "\n" (map (show) (Set.toList . assumeSet $ a)) ++ "\n\n"

instance Show Theorems where
    show a   =   "Theorems:\n" ++ intercalate "\n" (map (show) (Set.toList . theoremSet $ a)) ++ "\n\n"



data ArticleLine = Comment { commentString :: String }
                 | Command { commandFunc :: ((Stack Object,Dictionary,Assumptions,Theorems) -> Maybe (Stack Object,Dictionary,Assumptions,Theorems)) }



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



name :: String -> ((Stack Object,Dictionary,Assumptions,Theorems) -> Maybe (Stack Object,Dictionary,Assumptions,Theorems))
name str = \(s,d,a,t) ->
               do n <- Com.name str
                  let s' = (ObjName n) <:> s
                  return (s',d,a,t)


number :: String -> ((Stack Object,Dictionary,Assumptions,Theorems) -> Maybe (Stack Object,Dictionary,Assumptions,Theorems))
number n = \(s,d,a,t) ->
               do num <- Com.number n
                  let s' = (ObjNum num) <:> s
                  return (s',d,a,t)


absTerm :: (Stack Object,Dictionary,Assumptions,Theorems) -> Maybe (Stack Object,Dictionary,Assumptions,Theorems)
absTerm (s,d,a,t) =
    do te <- (s `at` 0) >>= objTerm; v <- (s `at` 1) >>= objVar
       let term = Com.absTerm te v
           s' = (ObjTerm term) <:> (Stack.pop 2 s)
       return (s',d,a,t)


absThm :: (Stack Object,Dictionary,Assumptions,Theorems) -> Maybe (Stack Object,Dictionary,Assumptions,Theorems)
absThm (s,d,a,t) =
    do th <- (s `at` 0) >>= objThm; v <- (s `at` 1) >>= objVar
       thm <- Com.absThm th v
       let s' = (ObjThm thm) <:> (Stack.pop 2 s)
       return (s',d,a,t)


appTerm :: (Stack Object,Dictionary,Assumptions,Theorems) -> Maybe (Stack Object,Dictionary,Assumptions,Theorems)
appTerm (s,d,a,t) =
    do f <- (s `at` 0) >>= objTerm; x <- (s `at` 1) >>= objTerm
       let term = Com.appTerm f x
           s' = (ObjTerm term) <:> (Stack.pop 2 s)
       return (s',d,a,t)


appThm :: (Stack Object,Dictionary,Assumptions,Theorems) -> Maybe (Stack Object,Dictionary,Assumptions,Theorems)
appThm (s,d,a,t) =
    do t1 <- (s  `at` 0) >>= objThm; t2 <- (s `at` 1) >>= objThm
       let thm = Com.appThm t1 t2
           s' = (ObjThm thm) <:> (Stack.pop 2 s)
       return (s',d,a,t)


assume :: (Stack Object,Dictionary,Assumptions,Theorems) -> Maybe (Stack Object,Dictionary,Assumptions,Theorems)
assume (s,d,a,t) =
    do te <- (s `at` 0) >>= objTerm
       thm <- Com.assume te
       let s' = (ObjThm thm) <:> (Stack.pop 1 s)
       return (s',d,a,t)


axiom :: (Stack Object,Dictionary,Assumptions,Theorems) -> Maybe (Stack Object,Dictionary,Assumptions,Theorems)
axiom (s,d,a,t) =
    do te <- (s `at` 0) >>= objTerm; l <- (s `at` 1) >>= objList
       thm <- Com.axiom te (mapMaybe objTerm l)
       let s' = (ObjThm thm) <:> (Stack.pop 2 s)
           a' = Assumptions $ Set.insert thm (assumeSet a)
       return (s',d,a',t)


betaConv :: (Stack Object,Dictionary,Assumptions,Theorems) -> Maybe (Stack Object,Dictionary,Assumptions,Theorems)
betaConv (s,d,a,t) =
    do te <- (s `at` 0) >>= objTerm
       let thm = Com.betaConv te
           s' = (ObjThm thm) <:> (Stack.pop 1 s)
       return (s',d,a,t)


cons :: (Stack Object,Dictionary,Assumptions,Theorems) -> Maybe (Stack Object,Dictionary,Assumptions,Theorems)
cons (s,d,a,t) =
    do l <- (s `at` 0) >>= objList; h <- (s `at` 1)
       let newList = h : l
           s' = (ObjList newList) <:> (Stack.pop 2 s)
       return (s',d,a,t)


constant :: (Stack Object,Dictionary,Assumptions,Theorems) -> Maybe (Stack Object,Dictionary,Assumptions,Theorems)
constant (s,d,a,t) =
    do n <- (s `at` 0) >>= objName
       let constant = Com.constant n
           s' = (ObjConst constant) <:> (Stack.pop 1 s)
       return (s',d,a,t)


constTerm :: (Stack Object,Dictionary,Assumptions,Theorems) -> Maybe (Stack Object,Dictionary,Assumptions,Theorems)
constTerm (s,d,a,t) =
    do ty <- (s `at` 0) >>= objType; c <- (s `at` 1) >>= objConst
       let term = Com.constTerm ty c
           s' = (ObjTerm term) <:> (Stack.pop 2 s)
       return (s',d,a,t)


deductAntisym :: (Stack Object,Dictionary,Assumptions,Theorems) -> Maybe (Stack Object,Dictionary,Assumptions,Theorems)
deductAntisym (s,d,a,t) =
    do t1 <- (s `at` 0) >>= objThm; t2 <- (s `at` 1) >>= objThm
       let thm = Com.deductAntisym t1 t2
           s' = (ObjThm thm) <:> (Stack.pop 2 s)
       return (s',d,a,t)


def :: (Stack Object,Dictionary,Assumptions,Theorems) -> Maybe (Stack Object,Dictionary,Assumptions,Theorems)
def (s,d,a,t) =
    do n <- (s `at` 0) >>= objNum; h <- (s `at` 1)
       let d' = Dictionary $ Map.insert n h (dictionMap d)
           s' = (Stack.pop 1 s)
       return (s',d',a,t)


defineConst :: (Stack Object,Dictionary,Assumptions,Theorems) -> Maybe (Stack Object,Dictionary,Assumptions,Theorems)
defineConst (s,d,a,t) =
    do te <- (s `at` 0) >>= objTerm; n <- (s `at` 1) >>= objName
       (thm, constant) <- Com.defineConst te n
       let s' = (ObjThm thm) <:> (ObjConst constant) <:> (Stack.pop 2 s)
       return (s',d,a,t)


defineTypeOp :: (Stack Object,Dictionary,Assumptions,Theorems) -> Maybe (Stack Object,Dictionary,Assumptions,Theorems)
defineTypeOp (s,d,a,t) =
    do th <- (s `at` 0) >>= objThm; l <- (s `at` 1) >>= objList; r <- (s `at` 2) >>= objName
       ab <- (s `at` 3) >>= objName; y <- (s `at` 4) >>= objName
       (rthm, athm, rep, abst, n) <- Com.defineTypeOp th (mapMaybe objName l) r ab y
       let s' = (ObjThm rthm) <:> (ObjThm athm) <:> (ObjConst rep) <:> (ObjConst abst) <:> (ObjTyOp n) <:> (Stack.pop 5 s)
       return (s',d,a,t)


eqMp :: (Stack Object,Dictionary,Assumptions,Theorems) -> Maybe (Stack Object,Dictionary,Assumptions,Theorems)
eqMp (s,d,a,t) =
    do t1 <- (s `at` 0) >>= objThm; t2 <- (s `at` 1) >>= objThm
       thm <- Com.eqMp t1 t2
       let s' = (ObjThm thm) <:> (Stack.pop 2 s)
       return (s',d,a,t)


nil :: (Stack Object,Dictionary,Assumptions,Theorems) -> Maybe (Stack Object,Dictionary,Assumptions,Theorems)
nil (s,d,a,t) = Just (ObjList [] <:> s, d, a, t)


opType :: (Stack Object,Dictionary,Assumptions,Theorems) -> Maybe (Stack Object,Dictionary,Assumptions,Theorems)
opType (s,d,a,t) =
    do l <- (s `at` 0) >>= objList; to <- (s `at` 1) >>= objTyOp
       let newType = Com.opType (mapMaybe objType l) to
           s' = (ObjType newType) <:> (Stack.pop 2 s)
       return (s',d,a,t)


pop :: (Stack Object,Dictionary,Assumptions,Theorems) -> Maybe (Stack Object,Dictionary,Assumptions,Theorems)
pop (s,d,a,t) = Just ((Stack.pop 1 s),d,a,t)


ref :: (Stack Object,Dictionary,Assumptions,Theorems) -> Maybe (Stack Object,Dictionary,Assumptions,Theorems)
ref (s,d,a,t) =
    do n <- (s `at` 0) >>= objNum
       let object = (dictionMap d) Map.! n
           s' = object <:> (Stack.pop 1 s)
       return (s',d,a,t)


refl :: (Stack Object,Dictionary,Assumptions,Theorems) -> Maybe (Stack Object,Dictionary,Assumptions,Theorems)
refl (s,d,a,t) =
    do te <- (s `at` 0) >>= objTerm
       let thm = Com.refl te
           s' = (ObjThm thm) <:> (Stack.pop 1 s)
       return (s',d,a,t)


remove :: (Stack Object,Dictionary,Assumptions,Theorems) -> Maybe (Stack Object,Dictionary,Assumptions,Theorems)
remove (s,d,a,t) =
    do n <- (s `at` 0) >>= objNum
       let object = (dictionMap d) Map.! n
           s' = object <:> (Stack.pop 1 s)
           d' = Dictionary $ Map.delete n (dictionMap d)
       return (s',d',a,t)


subst :: (Stack Object,Dictionary,Assumptions,Theorems) -> Maybe (Stack Object,Dictionary,Assumptions,Theorems)
subst (s,d,a,t) =
    do th <- (s `at` 0) >>= objThm; l <- (s `at` 1) >>= objList
       thm <- Com.subst th l
       let s' = (ObjThm thm) <:> (Stack.pop 2 s)
       return (s',d,a,t)


thm :: (Stack Object,Dictionary,Assumptions,Theorems) -> Maybe (Stack Object,Dictionary,Assumptions,Theorems)
thm (s,d,a,t) =
    do te <- (s `at` 0) >>= objTerm; l <- (s `at` 1) >>= objList; th <- (s `at` 2) >>= objThm
       thm <- Com.thm te (mapMaybe objTerm l) th
       let s' = Stack.pop 3 s
           t' = Theorems $ Set.insert thm (theoremSet t)
       return (s',d,a,t')


typeOp :: (Stack Object,Dictionary,Assumptions,Theorems) -> Maybe (Stack Object,Dictionary,Assumptions,Theorems)
typeOp (s,d,a,t) =
    do n <- (s `at` 0) >>= objName
       let typeOp = Com.typeOp n
           s' = (ObjTyOp typeOp) <:> (Stack.pop 1 s)
       return (s',d,a,t)


var :: (Stack Object,Dictionary,Assumptions,Theorems) -> Maybe (Stack Object,Dictionary,Assumptions,Theorems)
var (s,d,a,t) =
    do ty <- (s `at` 0) >>= objType; n <- (s `at` 1) >>= objName
       v <- Com.var ty n
       let s' = (ObjVar v) <:> (Stack.pop 2 s)
       return (s',d,a,t)


varTerm :: (Stack Object,Dictionary,Assumptions,Theorems) -> Maybe (Stack Object,Dictionary,Assumptions,Theorems)
varTerm (s,d,a,t) =
    do v <- (s `at` 0) >>= objVar
       let term = Com.varTerm v
           s' = (ObjTerm term) <:> (Stack.pop 1 s)
       return (s',d,a,t)


varType :: (Stack Object,Dictionary,Assumptions,Theorems) -> Maybe (Stack Object,Dictionary,Assumptions,Theorems)
varType (s,d,a,t) =
    do n <- (s `at` 0) >>= objName
       newType <- Com.varType n
       let s' = (ObjType newType) <:> (Stack.pop 1 s)
       return (s',d,a,t)



doSemanticCheck :: [String] -> Maybe (Stack Object,Dictionary,Assumptions,Theorems)
doSemanticCheck =
    let s = Stack.empty
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
