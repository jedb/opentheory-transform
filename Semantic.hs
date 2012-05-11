import Control.Monad( liftM )
import System( getArgs )
import Data.List
import TypeVar
import Term
import Theorem
import Object



data Stack = Stack { stackList :: [Object] }
data Dictionary = Dictionary { dictionMap :: [(Int,Object)] }
data Assumptions = Assumptions { assumeList :: [Object] }
data Theorems = Theorems { theoremList :: [Object] }


instance Show Stack where
    show a   =   "Stack:\n" ++ intercalate "\n" (map (show) (stackList a)) ++ "\n\n"

instance Show Dictionary where
    show a   =   "Dictionary:\n" ++ intercalate "\n" (map (show) (dictionMap a)) ++ "\n\n"

instance Show Assumptions where
    show a   =   "Assumptions:\n" ++ intercalate "\n" (map (show) (assumeList a)) ++ "\n\n"

instance Show Theorems where
    show a   =   "Theorems:\n" ++ intercalate "\n" (map (show) (theoremList a)) ++ "\n\n"


data ArticleLine = Comment { commentString :: String }
                 | Command { commandFunc :: ((Stack,Dictionary,Assumptions,Theorems)->(Stack,Dictionary,Assumptions,Theorems)) }



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



name :: String -> ((Stack,Dictionary,Assumptions,Theorems) -> (Stack,Dictionary,Assumptions,Theorems))
name str = \(s,d,a,t) ->
               let unQuoted = init . tail $ str
                   wordList = words . (map (\x -> if (x == '.') then ' ' else x)) $ unQuoted
                   name = Name (init wordList) (last wordList)
                   s' = Stack $ ObjName name : (stackList s)
               in (s',d,a,t)


number :: String -> ((Stack,Dictionary,Assumptions,Theorems) -> (Stack,Dictionary,Assumptions,Theorems))
number n = \(stack,d,a,t) -> (Stack $ ObjNum (read n) : (stackList stack), d, a, t)


absTerm :: (Stack,Dictionary,Assumptions,Theorems) -> (Stack,Dictionary,Assumptions,Theorems)
absTerm (s,d,a,t) =
    let stack = stackList s
        op = (\x y -> TAbs (TVar y) x)
        newTerm = ObjTerm $ op (objTerm $ stack!!0) (objVar $ stack!!1)
        s' = Stack $ newTerm : (drop 2 stack)
    in (s',d,a,t)


-- need to add guards to check that the variable is not free in the hypothesis
absThm :: (Stack,Dictionary,Assumptions,Theorems) -> (Stack,Dictionary,Assumptions,Theorems)
absThm (s,d,a,t) =
    let stack = stackList s
        op = (\x y -> Theorem (thmHyp x)
                              (mkEquals (TAbs (TVar y) (getlhs . thmCon $ x))
                                        (TAbs (TVar y) (getrhs . thmCon $ x))))
        theorem = ObjThm $ op (objThm $ stack!!0) (objVar $ stack!!1)
        s' = Stack $ theorem : (drop 2 stack)
    in (s',d,a,t)


appTerm :: (Stack,Dictionary,Assumptions,Theorems) -> (Stack,Dictionary,Assumptions,Theorems)
appTerm (s,d,a,t) =
    let stack = stackList s
        op = (\x y -> TApp y x)
        newTerm = ObjTerm $ op (objTerm $ stack!!0) (objTerm $ stack!!1)
        s' = Stack $ newTerm : (drop 2 stack)
    in (s',d,a,t)


appThm :: (Stack,Dictionary,Assumptions,Theorems) -> (Stack,Dictionary,Assumptions,Theorems)
appThm (s,d,a,t) =
    let stack = stackList s
        op = (\x y -> Theorem (union (thmHyp x) (thmHyp y))
                              (mkEquals (TApp (getlhs . thmCon $ y) (getlhs . thmCon $ x))
                                        (TApp (getrhs . thmCon $ y) (getrhs . thmCon $ x))))
        theorem = ObjThm $ op (objThm $ stack!!0) (objThm $ stack!!1)
        s' = Stack $ theorem : (drop 2 stack)
    in (s',d,a,t)


assume :: (Stack,Dictionary,Assumptions,Theorems) -> (Stack,Dictionary,Assumptions,Theorems)
assume (s,d,a,t) =
    let stack = stackList s
        op = (\x -> Theorem [x] x)
        theorem = ObjThm $ op (objTerm $ stack!!0)
        s' = Stack $ theorem : (tail stack)
    in (s',d,a,t)


-- need to add guarding for all terms being of type bool
axiom :: (Stack,Dictionary,Assumptions,Theorems) -> (Stack,Dictionary,Assumptions,Theorems)
axiom (s,d,a,t) =
    let stack = stackList s
        assumptions = assumeList a
        op = (\x y -> Theorem y x)
        theorem = ObjThm $ op (objTerm $ stack!!0) (map (objTerm) . objList $ stack!!1)
        s' = Stack $ theorem : (drop 2 stack)
        a' = Assumptions $ theorem : assumptions
    in (s',d,a',t)


betaConv :: (Stack,Dictionary,Assumptions,Theorems) -> (Stack,Dictionary,Assumptions,Theorems)
betaConv (s,d,a,t) =
    let stack = stackList s
        op = (\x -> Theorem [] (mkEquals x
                                         (substitute ([], [(tVar . tAbsVar . tAppLeft $ x, tAppRight $ x)])
                                                     (tAbsTerm . tAppLeft $ x))))
        theorem = ObjThm $ op (objTerm $ stack!!0)
        s' = Stack $ theorem : (tail stack)
    in (s',d,a,t)


cons :: (Stack,Dictionary,Assumptions,Theorems) -> (Stack,Dictionary,Assumptions,Theorems)
cons (s,d,a,t) =
    let stack = stackList s
        op = (\x y -> y : x)
        newList = ObjList $ op (objList $ stack!!0) (stack!!1)
        s' = Stack $ newList : (drop 2 stack)
    in (s',d,a,t)


constant :: (Stack,Dictionary,Assumptions,Theorems) -> (Stack,Dictionary,Assumptions,Theorems)
constant (s,d,a,t) =
    let stack = stackList s
        op = (\x -> Const x)
        constant = ObjConst $ op (objName $ stack!!0)
        s' = Stack $ constant : (tail stack)
    in (s',d,a,t)


constTerm :: (Stack,Dictionary,Assumptions,Theorems) -> (Stack,Dictionary,Assumptions,Theorems)
constTerm (s,d,a,t) =
    let stack = stackList s
        op = (\x y -> TConst y x)
        newType = ObjTerm $ op (objType $ stack!!0) (objConst $ stack!!1)
        s' = Stack $ newType : (drop 2 stack)
    in (s',d,a,t)


deductAntisym :: (Stack,Dictionary,Assumptions,Theorems) -> (Stack,Dictionary,Assumptions,Theorems)
deductAntisym (s,d,a,t) =
    let stack = stackList s
        op = (\x y -> Theorem (union ((thmHyp $ y) \\ [(thmCon $ x)])
                                     ((thmHyp $ x) \\ [(thmCon $ y)]))
                              (mkEquals (thmCon $ y) (thmCon $ x)))
        theorem = ObjThm $ op (objThm $ stack!!0) (objThm $ stack!!1)
        s' = Stack $ theorem : (drop 2 stack)
    in (s',d,a,t)


def :: (Stack,Dictionary,Assumptions,Theorems) -> (Stack,Dictionary,Assumptions,Theorems)
def (s,d,a,t) =
    let stack = stackList s
        dictionary = dictionMap d
        newEntry = ((objNum $ stack!!0), (stack!!1))
        cleanDict = filter ((/=) (objNum $ stack!!0) . fst) dictionary
        d' = Dictionary $ newEntry : cleanDict
        s' = Stack $ tail stack
    in (s',d',a,t)


defineConst :: (Stack,Dictionary,Assumptions,Theorems) -> (Stack,Dictionary,Assumptions,Theorems)
defineConst (s,d,a,t) =
    let stack = stackList s
        op1 = (\x -> Const x)
        op2 = (\x y -> Theorem [] (mkEquals x y))
        constant = ObjConst $ op1 (objName $ stack!!1)
        constTerm = TConst (objConst $ constant) (typeOf (objTerm $ stack!!0))
        theorem = ObjThm $ op2 constTerm (objTerm $ stack!!0)
        s' = Stack $ theorem : constant : (drop 2 stack)
    in (s',d,a,t)


defineTypeOp :: (Stack,Dictionary,Assumptions,Theorems) -> (Stack,Dictionary,Assumptions,Theorems)
defineTypeOp (s,d,a,t) =
    let stack = stackList s
        oldthm = objThm $ stack!!0
        namelist = map (objName) . objList $ stack!!1
        rep = Const . objName $ stack!!2
        abst = Const . objName $ stack!!3
        n = TypeOp . objName $ stack!!4
        rtype = typeOf . tAppRight . thmCon $ oldthm
        atype = AType (map (\x -> TypeVar x) namelist) n
        r' = TVar (Var (Name [] "r'") rtype)
        a' = TVar (Var (Name [] "a'") atype)
        reptype = typeFunc atype rtype
        abstype = typeFunc rtype atype
        repTerm = TConst rep reptype
        absTerm = TConst abst abstype
        rthm = Theorem [] (mkEquals (TApp (tAppLeft . thmCon $ oldthm) r')
                                    (mkEquals (TApp repTerm (TApp absTerm r')) r'))
        athm = Theorem [] (mkEquals (TApp absTerm (TApp repTerm a')) a')
        s' = Stack $ (ObjThm rthm) : (ObjThm athm) : (ObjConst rep) : (ObjConst abst) : (ObjTyOp n) : (drop 5 stack)
    in (s',d,a,t)


eqMp :: (Stack,Dictionary,Assumptions,Theorems) -> (Stack,Dictionary,Assumptions,Theorems)
eqMp (s,d,a,t) =
    let stack = stackList s
        op = (\x y -> if (thmCon x == (getlhs (thmCon y))) then
                      Theorem (union (thmHyp x) (thmHyp y))
                              (getrhs (thmCon y))
                      else error "Theorem consequents not alpha equivalent in eqMp")
        theorem = ObjThm $ op (objThm $ stack!!0) (objThm $ stack!!1)
        s' = Stack $ theorem : (drop 2 stack)
    in (s',d,a,t)


nil :: (Stack,Dictionary,Assumptions,Theorems) -> (Stack,Dictionary,Assumptions,Theorems)
nil (s,d,a,t) = (Stack $ ObjList [] : (stackList s), d, a, t)


opType :: (Stack,Dictionary,Assumptions,Theorems) -> (Stack,Dictionary,Assumptions,Theorems)
opType (s,d,a,t) =
    let stack = stackList s
        op = (\x y -> AType x y)
        newType = ObjType $ op (map (objType) . objList $ stack!!0) (objTyOp $ stack!!1)
        s' = Stack $ newType : (drop 2 stack)
    in (s',d,a,t)


pop :: (Stack,Dictionary,Assumptions,Theorems) -> (Stack,Dictionary,Assumptions,Theorems)
pop (s,d,a,t) = (Stack $ tail (stackList s),d,a,t)


ref :: (Stack,Dictionary,Assumptions,Theorems) -> (Stack,Dictionary,Assumptions,Theorems)
ref (s,d,a,t) =
    let stack = stackList s
        dictionary = dictionMap d
        entry = filter (((==) (objNum $ stack!!0)) . fst) $ dictionary
        object = snd . head $ entry
        s' = Stack $ object : tail stack
    in (s',d,a,t)


refl :: (Stack,Dictionary,Assumptions,Theorems) -> (Stack,Dictionary,Assumptions,Theorems)
refl (s,d,a,t) =
    let stack = stackList s
        op = (\x -> Theorem [] (mkEquals x x))
        theorem = ObjThm $ op (objTerm $ stack!!0)
        s' = Stack $ theorem : (tail stack)
    in (s',d,a,t)


remove :: (Stack,Dictionary,Assumptions,Theorems) -> (Stack,Dictionary,Assumptions,Theorems)
remove (s,d,a,t) =
    let stack = stackList s
        dictionary = dictionMap d
        entry = filter (((==) (objNum $ stack!!0)) . fst) $ dictionary
        object = snd . head $ entry
        s' = Stack $ object : tail stack
        d' = Dictionary $ dictionary \\ entry
    in (s',d',a,t)


subst :: (Stack,Dictionary,Assumptions,Theorems) -> (Stack,Dictionary,Assumptions,Theorems)
subst (s,d,a,t) =
    let stack = stackList s
        op = (\x y -> Theorem (map (substitute y) (thmHyp x))
                              (substitute y (thmCon x)))
        substitution =
            (\x -> let list = (map (map objList)) . (map objList) . objList $ x
                       f = (\g h x -> (g . head $ x, h . last $ x))
                   in f (map (f objName objType)) (map (f objVar objTerm)) list)

        theorem = ObjThm $ op (objThm $ stack!!0) (substitution $ stack!!1)
        s' = Stack $ theorem : (drop 2 stack)
    in (s',d,a,t)


thm :: (Stack,Dictionary,Assumptions,Theorems) -> (Stack,Dictionary,Assumptions,Theorems)
thm (s,d,a,t) =
    let stack = stackList s
        theorems = theoremList t
        op = (\x y z -> Theorem (alphaConvertList (thmHyp z) y)
                                (alphaConvert (thmCon z) x))
        theorem = ObjThm $ op (objTerm $ stack!!0) (map (objTerm) . objList $ stack!!1) (objThm $ stack!!2)
        s' = Stack $ drop 3 stack
        t' = Theorems $ union theorems [theorem]
    in (s',d,a,t')


typeOp :: (Stack,Dictionary,Assumptions,Theorems) -> (Stack,Dictionary,Assumptions,Theorems)
typeOp (s,d,a,t) =
    let stack = stackList s
        op = (\x -> TypeOp x)
        typeOp = ObjTyOp $ op (objName $ stack!!0)
        s' = Stack $ typeOp : (tail stack)
    in (s',d,a,t)


var :: (Stack,Dictionary,Assumptions,Theorems) -> (Stack,Dictionary,Assumptions,Theorems)
var (s,d,a,t) =
    let stack = stackList s
        op = (\x y -> Var y x)
        var = ObjVar $ op (objType $ stack!!0) (objName $ stack!!1)
        s' = Stack $ var : (drop 2 stack)
    in (s',d,a,t)


varTerm :: (Stack,Dictionary,Assumptions,Theorems) -> (Stack,Dictionary,Assumptions,Theorems)
varTerm (s,d,a,t) =
    let stack = stackList s
        op = (\x -> TVar x)
        term = ObjTerm $ op (objVar $ stack!!0)
        s' = Stack $ term : (tail stack)
    in (s',d,a,t)


varType :: (Stack,Dictionary,Assumptions,Theorems) -> (Stack,Dictionary,Assumptions,Theorems)
varType (s,d,a,t) =
    let stack = stackList s
        op = (\x -> TypeVar x)
        newType = ObjType $ op (objName $ stack!!0)
        s' = Stack $ newType : (tail stack)
    in (s',d,a,t)



doSemanticCheck :: [String] -> (Stack,Dictionary,Assumptions,Theorems)
doSemanticCheck =
    let s = Stack []
        d = Dictionary []
        a = Assumptions []
        t = Theorems []
        op = (\x y -> case y of (Comment _) -> x
                                (Command z) -> z x)
    in (foldl' (op) (s,d,a,t)) . (map (parse))
    -- important to use foldl here so commands get applied in the correct order


getLines :: FilePath -> IO [String]
getLines = liftM lines . readFile


stripReturn :: String -> String
stripReturn s = if (last s == '\r') then init s else s


main = do
      args <- getArgs
      list <- getLines $ head args
      result <- return $ doSemanticCheck (map (stripReturn) list)
      print $ result
