import Control.Monad( liftM )
import System( getArgs )
import Data.List


data Object = ObjNum { objNum :: Number }
            | ObjName { objName :: Name }
            | ObjList { objList :: List }
            | ObjTyOp { objTyOp :: TypeOp }
            | ObjType { objType :: Type }
            | ObjConst { objConst :: Const }
            | ObjVar { objVar :: Var }
            | ObjTerm { objTerm :: Term }
            | ObjThm { objThm :: Theorem }
            | ObjSub { objSub :: Substitution } deriving (Eq)

type Number = Int

data Name = Name { nameSpace :: [String]
                 , nameId :: String } deriving (Eq)

type List = [Object]

data TypeOp = TypeOp { tyOp :: Name } deriving (Eq)

data Type = TypeVar { typeVar :: Name }
          | AType { aType :: [Type]
                  , aTypeOp :: TypeOp } deriving (Eq)

data Const = Const { constName :: Name } deriving (Eq)

data Var = Var { varName :: Name
               , varTy :: Type } deriving (Eq)

data Term = TVar { tVar :: Var }
          | TConst { tConst :: Const
                   , tConstType :: Type }
          | TApp { tAppLeft :: Term
                 , tAppRight :: Term }
          | TAbs { tAbsVar :: Term
                 , tAbsTerm :: Term }

data Theorem = Theorem { thmHyp :: [Term]
                       , thmCon :: Term } deriving (Eq)


instance Eq Term where
    a == b   =   a `alphaEquiv` b


type Stack = [Object]
type Dictionary = [(Int,Object)]
type Assumptions = [Object]
type Theorems = [Object]


type Substitution = ([(Type,Type)],[(Var,Term)])


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
name str = \(stack,d,a,t) -> (ObjName (Name [] str) : stack, d, a, t)


number :: String -> ((Stack,Dictionary,Assumptions,Theorems) -> (Stack,Dictionary,Assumptions,Theorems))
number n = \(stack,d,a,t) -> (ObjNum (read n) : stack, d, a, t)


absTerm :: (Stack,Dictionary,Assumptions,Theorems) -> (Stack,Dictionary,Assumptions,Theorems)
absTerm (stack,d,a,t) =
    let op = (\x y -> TAbs (TVar y) x)
        newTerm = ObjTerm $ op (objTerm $ stack!!0) (objVar $ stack!!1)
        stack' = newTerm : (drop 2 stack)
    in (stack',d,a,t)


-- need to add guards to check that the variable is not free in the hypothesis
absThm :: (Stack,Dictionary,Assumptions,Theorems) -> (Stack,Dictionary,Assumptions,Theorems)
absThm (stack,d,a,t) =
    let op = (\x y -> Theorem (thmHyp x)
                              (mkEquals (TAbs (TVar y) (getlhs . thmCon $ x))
                                        (TAbs (TVar y) (getrhs . thmCon $ x))))
        theorem = ObjThm (op (objThm $ stack!!0) (objVar $ stack!!1))
        stack' = theorem : (drop 2 stack)
    in (stack',d,a,t)


appTerm :: (Stack,Dictionary,Assumptions,Theorems) -> (Stack,Dictionary,Assumptions,Theorems)
appTerm (stack,d,a,t) =
    let op = (\x y -> TApp y x)
        newTerm = ObjTerm $ op (objTerm $ stack!!0) (objTerm $ stack!!1)
        stack' = newTerm : (drop 2 stack)
    in (stack',d,a,t)


appThm :: (Stack,Dictionary,Assumptions,Theorems) -> (Stack,Dictionary,Assumptions,Theorems)
appThm (stack,d,a,t) =
    let op = (\x y -> Theorem (union (thmHyp x) (thmHyp y))
                              (mkEquals (TApp (getlhs . thmCon $ y) (getlhs . thmCon $ x))
                                        (TApp (getrhs . thmCon $ y) (getrhs . thmCon $ x))))
        theorem = ObjThm $ op (objThm $ stack!!0) (objThm $ stack!!1)
        stack' = theorem : (drop 2 stack)
    in (stack',d,a,t)


assume :: (Stack,Dictionary,Assumptions,Theorems) -> (Stack,Dictionary,Assumptions,Theorems)
assume (stack,d,a,t) =
    let op = (\x -> Theorem [x] x)
        theorem = ObjThm $ op (objTerm $ stack!!0)
        stack' = theorem : (tail stack)
    in (stack',d,a,t)


-- need to add guarding for all terms being of type bool
axiom :: (Stack,Dictionary,Assumptions,Theorems) -> (Stack,Dictionary,Assumptions,Theorems)
axiom (stack,d,assumptions,t) =
    let op = (\x y -> Theorem y x)
        theorem = ObjThm $ op (objTerm $ stack!!0) (map (objTerm) . objList $ stack!!1)
        stack' = theorem : (drop 2 stack)
        assumptions' = theorem : assumptions
    in (stack', d, assumptions', t)


betaConv :: (Stack,Dictionary,Assumptions,Theorems) -> (Stack,Dictionary,Assumptions,Theorems)
betaConv (stack,d,a,t) =
    let op = (\x -> Theorem [] (mkEquals x
                                         (substitute ([], [(tVar . tAbsVar . tAppLeft $ x, tAppRight $ x)])
                                                     (tAbsTerm . tAppLeft $ x))))
        theorem = ObjThm $ op (objTerm $ stack!!0)
        stack' = theorem : (tail stack)
    in (stack',d,a,t)


cons :: (Stack,Dictionary,Assumptions,Theorems) -> (Stack,Dictionary,Assumptions,Theorems)
cons (stack,d,a,t) =
    let op = (\x y -> y : x)
        newList = ObjList $ op (objList $ stack!!0) (stack!!1)
        stack' = newList : (drop 2 stack)
    in (stack',d,a,t)


constant :: (Stack,Dictionary,Assumptions,Theorems) -> (Stack,Dictionary,Assumptions,Theorems)
constant (stack,d,a,t) =
    let op = (\x -> Const x)
        constant = ObjConst $ op (objName $ stack!!0)
        stack' = constant : (tail stack)
    in (stack',d,a,t)


constTerm :: (Stack,Dictionary,Assumptions,Theorems) -> (Stack,Dictionary,Assumptions,Theorems)
constTerm (stack,d,a,t) =
    let op = (\x y -> TConst y x)
        newType = ObjTerm $ op (objType $ stack!!0) (objConst $ stack!!1)
        stack' = newType : (drop 2 stack)
    in (stack',d,a,t)


deductAntisym :: (Stack,Dictionary,Assumptions,Theorems) -> (Stack,Dictionary,Assumptions,Theorems)
deductAntisym (stack,d,a,t) =
    let op = (\x y -> Theorem (union ((thmHyp $ y) \\ [(thmCon $ x)])
                                     ((thmHyp $ x) \\ [(thmCon $ y)]))
                              (mkEquals (thmCon $ y) (thmCon $ x)))
        theorem = ObjThm $ op (objThm $ stack!!0) (objThm $ stack!!1)
        stack' = theorem : (drop 2 stack)
    in (stack',d,a,t)


def :: (Stack,Dictionary,Assumptions,Theorems) -> (Stack,Dictionary,Assumptions,Theorems)
def (stack,dictionary,a,t) =
    let newEntry = ((objNum $ stack!!0), (stack!!1))
        cleanDict = filter ((/=) (objNum $ stack!!0) . fst) dictionary
        dictionary' = newEntry : cleanDict
        stack' = tail stack
    in (stack',dictionary',a,t)


defineConst :: (Stack,Dictionary,Assumptions,Theorems) -> (Stack,Dictionary,Assumptions,Theorems)
defineConst (stack,d,a,t) =
    let op1 = (\x -> Const x)
        op2 = (\x y -> Theorem [] (mkEquals x y))
        constant = ObjConst $ op1 (objName $ stack!!1)
        constTerm = TConst (objConst $ constant) (typeOf (objTerm $ stack!!0))
        theorem = ObjThm $ op2 constTerm (objTerm $ stack!!0)
        stack' = theorem : constant : (drop 2 stack)
    in (stack',d,a,t)


defineTypeOp :: (Stack,Dictionary,Assumptions,Theorems) -> (Stack,Dictionary,Assumptions,Theorems)
defineTypeOp (stack,d,a,t) =
    let oldthm = objThm $ stack!!0
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
        stack' = (ObjThm rthm) : (ObjThm athm) : (ObjConst rep) : (ObjConst abst) : (ObjTyOp n) : (drop 5 stack)
    in (stack',d,a,t)


eqMp :: (Stack,Dictionary,Assumptions,Theorems) -> (Stack,Dictionary,Assumptions,Theorems)
eqMp (stack,d,a,t) =
    let op = (\x y -> if (thmCon x == (getlhs (thmCon y))) then
                      Theorem (union (thmHyp x) (thmHyp y))
                              (getrhs (thmCon y))
                      else error "Theorem consequents not alpha equivalent in eqMp")
        theorem = ObjThm $ op (objThm $ stack!!0) (objThm $ stack!!1)
        stack' = theorem : (drop 2 stack)
    in (stack',d,a,t)


nil :: (Stack,Dictionary,Assumptions,Theorems) -> (Stack,Dictionary,Assumptions,Theorems)
nil (stack,d,a,t) = (ObjList [] : stack, d, a, t)


opType :: (Stack,Dictionary,Assumptions,Theorems) -> (Stack,Dictionary,Assumptions,Theorems)
opType (stack,d,a,t) =
    let op = (\x y -> AType x y)
        newType = ObjType $ op (map (objType) . objList $ stack!!0) (objTyOp $ stack!!1)
        stack' = newType : (drop 2 stack)
    in (stack',d,a,t)


pop :: (Stack,Dictionary,Assumptions,Theorems) -> (Stack,Dictionary,Assumptions,Theorems)
pop (stack,d,a,t) = (tail stack,d,a,t)


ref :: (Stack,Dictionary,Assumptions,Theorems) -> (Stack,Dictionary,Assumptions,Theorems)
ref (stack,dictionary,a,t) =
    let entry = filter (((==) (objNum $ stack!!0)) . fst) $ dictionary
        object = snd . head $ entry
        stack' = object : tail stack
    in (stack',dictionary,a,t)


refl :: (Stack,Dictionary,Assumptions,Theorems) -> (Stack,Dictionary,Assumptions,Theorems)
refl (stack,d,a,t) =
    let op = (\x -> Theorem [] (mkEquals x x))
        theorem = ObjThm $ op (objTerm $ stack!!0)
        stack' = theorem : (tail stack)
    in (stack',d,a,t)


remove :: (Stack,Dictionary,Assumptions,Theorems) -> (Stack,Dictionary,Assumptions,Theorems)
remove (stack,dictionary,a,t) =
    let entry = filter (((==) (objNum $ stack!!0)) . fst) $ dictionary
        object = snd . head $ entry
        stack' = object : tail stack
        dictionary' = dictionary \\ entry
    in (stack',dictionary',a,t)


subst :: (Stack,Dictionary,Assumptions,Theorems) -> (Stack,Dictionary,Assumptions,Theorems)
subst (stack,d,a,t) =
    let op = (\x y -> Theorem (map (substitute y) (thmHyp x))
                              (substitute y (thmCon x)))
        theorem = ObjThm $ op (objThm $ stack!!0) (objSub $ stack!!1)
        stack' = theorem : (drop 2 stack)
    in (stack',d,a,t)


thm :: (Stack,Dictionary,Assumptions,Theorems) -> (Stack,Dictionary,Assumptions,Theorems)
thm (stack,d,a,theorems) =
    let op = (\x y z -> Theorem (alphaConvertList (thmHyp z) y)
                                (alphaConvert (thmCon z) x))
        theorem = ObjThm $ op (objTerm $ stack!!0) (map (objTerm) . objList $ stack!!1) (objThm $ stack!!2)
        stack' = drop 3 stack
        theorems' = union theorems [theorem]
    in (stack', d, a, theorems')


typeOp :: (Stack,Dictionary,Assumptions,Theorems) -> (Stack,Dictionary,Assumptions,Theorems)
typeOp (stack,d,a,t) =
    let op = (\x -> TypeOp x)
        typeOp = ObjTyOp $ op (objName $ stack!!0)
        stack' = typeOp : (tail stack)
    in (stack',d,a,t)


var :: (Stack,Dictionary,Assumptions,Theorems) -> (Stack,Dictionary,Assumptions,Theorems)
var (stack,d,a,t) =
    let op = (\x y -> Var y x)
        var = ObjVar $ op (objType $ stack!!0) (objName $ stack!!1)
        stack' = var : (drop 2 stack)
    in (stack',d,a,t)


varTerm :: (Stack,Dictionary,Assumptions,Theorems) -> (Stack,Dictionary,Assumptions,Theorems)
varTerm (stack,d,a,t) =
    let op = (\x -> TVar x)
        term = ObjTerm $ op (objVar $ stack!!0)
        stack' = term : (tail stack)
    in (stack',d,a,t)


varType :: (Stack,Dictionary,Assumptions,Theorems) -> (Stack,Dictionary,Assumptions,Theorems)
varType (stack,d,a,t) =
    let op = (\x -> TypeVar x)
        newType = ObjType $ op (objName $ stack!!0)
        stack' = newType : (tail stack)
    in (stack',d,a,t)




alphaEquiv :: Term -> Term -> Bool
alphaEquiv a b =
    let equiv = \term1 term2 var1 var2 ->
            case (term1,term2,var1,var2) of
                (TConst a1 b1, TConst a2 b2, _, _) ->
                        a1 == a2 &&
                        b1 == b2
                (TApp a1 b1, TApp a2 b2, v1, v2) ->
                        equiv a1 a2 v1 v2 &&
                        equiv b1 b2 v1 v2
                (TAbs (TVar (Var n1 t1)) b1, TAbs (TVar (Var n2 t2)) b2, v1, v2) ->
                        t1 == t2 &&
                        equiv b1 b2 (Var n1 t1) (Var n2 t2) &&
                        equiv b1 b2 v1 v2
                (TVar a1, TVar a2, v1, v2) ->
                        a1 == a2 ||
                        (a1 == v1 && a2 == v2)
                (_, _, _, _) ->
                        False
        dummy = Var (Name [] "") (TypeVar (Name [] ""))
    in equiv a b dummy dummy


alphaConvert :: Term -> Term -> Term
alphaConvert (TConst a ty) (TConst _ _) = TConst a ty
alphaConvert (TApp a1 b1) (TApp a2 b2) = TApp (alphaConvert a1 a2) (alphaConvert b1 b2)
alphaConvert (TVar v) (TVar _) = TVar v
alphaConvert (TAbs v1 a) (TAbs v2 b) = substitute ([],[(tVar v1,v2)]) (TAbs v1 (alphaConvert a b))


alphaConvertList :: [Term] -> [Term] -> [Term]
alphaConvertList a b = map (\x -> alphaConvert (fst x) (snd x)) (zip a b)


substitute :: Substitution -> Term -> Term
substitute (tymap,vmap) term =
    let typesub =
            (\x y ->
                case y of
                    (TConst a ty) -> if (ty == fst x)
                                     then TConst a (snd x)
                                     else TConst a ty
                    (TApp a b) -> TApp (typesub x a) (typesub x b)
                    (TAbs v a) -> TAbs v (typesub x a)
                    (TVar v) -> TVar v)
        varsub =
            (\x y ->
                case y of
                    (TConst a ty) -> TConst a ty
                    (TApp a b) -> TApp (varsub x a) (varsub x b)
                    (TVar v) -> if (v == fst x)
                                then snd x
                                else TVar v
                    (TAbs v a) -> let safe = rename (TAbs v a) (union [(fst x)] (containsVars (snd x)))
                                  in case safe of
                                         (TAbs m n) -> TAbs m (varsub x n))
        tydone = foldl' (\x y -> typesub y x) term tymap
        vdone = foldl' (\x y -> varsub y x) tydone vmap
    in vdone


containsVars :: Term -> [Var]
containsVars t =
    let f = (\term list ->
            case term of
                (TConst a b) -> list
                (TApp a b) -> union list ((f a list) ++ (f b list))
                (TVar a) -> union list [a]
                (TAbs a b) -> union list ([tVar a] ++ (f b list)))
    in f t []


rename :: Term -> [Var] -> Term
rename (TAbs (TVar v) t) varlist =
    let doRename =
            (\x y z -> case x of
                           (TAbs (TVar a) b) -> if (a == y)
                                         then TAbs (TVar z) (doRename b y z)
                                         else TAbs (TVar a) (doRename b y z)
                           (TConst a b) -> TConst a b
                           (TApp a b) -> TApp (doRename a y z) (doRename b y z)
                           (TVar a) -> if (a == y)
                                         then TVar z
                                         else TVar a)
        findSafe =
            (\x y -> if (x `elem` y)
                     then case x of
                              (Var a b) ->
                                  case a of
                                      (Name c d) -> findSafe (Var (Name c (d ++ "'")) b) y
                     else x)
    in if (v `elem` varlist)
       then doRename (TAbs (TVar v) t) v (findSafe v varlist)
       else TAbs (TVar v) t




typeOf :: Term -> Type
typeOf (TConst c ty) = ty


mkEquals :: Term -> Term -> Term
mkEquals lhs rhs =
    let eqConst = TConst (Const (Name [] "=")) (mkEqualsType (typeOf lhs))
    in TApp (TApp eqConst lhs) rhs


mkEqualsType :: Type -> Type
mkEqualsType ty = typeFunc (AType [] (TypeOp (Name [] "bool"))) (typeFunc ty ty)


getlhs :: Term -> Term
getlhs (TApp (TApp eq lhs) _) =
    if (isEq eq) then lhs else error "Tried to get lhs from a non-eq term"


getrhs :: Term -> Term
getrhs (TApp (TApp eq _) rhs) =
    if (isEq eq) then rhs else error "Tried to get rhs from a non-eq term"


isEq :: Term -> Bool
isEq (TApp (TApp (TConst (Const (Name [] "=")) _) _) _) = True
isEq _ = False


typeFunc :: Type -> Type -> Type
typeFunc ty1 ty2 = AType [ty1,ty2] (TypeOp (Name [] "->"))




doSemanticCheck :: [String] -> (Stack,Dictionary,Assumptions,Theorems)
doSemanticCheck =
    let s = [] :: Stack
        d = [] :: Dictionary
        a = [] :: Assumptions
        t = [] :: Theorems
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
      print $ "Result OK"
