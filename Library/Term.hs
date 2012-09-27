module Library.Term (
    Term(..),
    Substitution,

    alphaEquiv,
    alphaConvert,
    alphaConvertList,
    substitute,
    boundVars,
    freeVars,
    rename,
    typeOf,
    typeVarsInTerm,
    mkEquals,
    isEq,
    getlhs,
    getrhs
    ) where



import Data.List
import qualified Data.Set as Set
import qualified Data.Map as Map
import Library.TypeVar



data Term = TVar { tVar :: Var }
          | TConst { tConst :: Const
                   , tConstType :: Type }
          | TApp { tAppLeft :: Term
                 , tAppRight :: Term }
          | TAbs { tAbsVar :: Term
                 , tAbsTerm :: Term } deriving (Ord)

type Substitution = ( [(Name,Type)], [(Var,Term)] )


instance Show Term where
    show (TVar a)       =   (show a)
    show (TConst a _)   =   show a 
    show (TApp (TApp eq lhs) rhs)
            | isEq eq   =   "(" ++ (show lhs) ++ " = " ++ (show rhs) ++ ")"
    show (TApp a b)     =   "(" ++ (show a) ++ " " ++ (show b) ++ ")"
    show (TAbs a b)     =   "(\\" ++ (show a) ++ " -> " ++ (show b) ++ ")"


instance Eq Term where
    a == b   =   a `alphaEquiv` b



alphaEquiv :: Term -> Term -> Bool
alphaEquiv a b =
    let equiv = \term1 term2 varmap1 varmap2 depth ->
            case (term1,term2) of
                (TConst a1 b1, TConst a2 b2) ->
                    a1 == a2 --&& b1 == b2

                (TApp a1 b1, TApp a2 b2) ->
                    equiv a1 a2 varmap1 varmap2 depth &&
                    equiv b1 b2 varmap1 varmap2 depth

                (TAbs (TVar (Var name1 type1)) b1, TAbs (TVar (Var name2 type2)) b2) ->
                    --type1 == type2 &&
                    equiv b1 b2 newmap1 newmap2 (depth+1)
                    where newmap1 = Map.insert (Var name1 type1) depth varmap1
                          newmap2 = Map.insert (Var name2 type2) depth varmap2

                (TVar (Var name1 type1), TVar (Var name2 type2)) ->
                    (name1 == name2 && Map.notMember (Var name1 type1) varmap1  && Map.notMember (Var name2 type2) varmap2) ||
                    Map.lookup (Var name1 type1) varmap1 == Map.lookup (Var name2 type2) varmap2

                (_,_) -> False
    in equiv a b Map.empty Map.empty 0


alphaConvert :: Term -> Term -> Term
alphaConvert (TConst a ty) (TConst _ _) = TConst a ty
alphaConvert (TApp a1 b1) (TApp a2 b2) = TApp (alphaConvert a1 a2) (alphaConvert b1 b2)
alphaConvert (TVar _) (TVar v) = TVar v
alphaConvert (TAbs v1 a) (TAbs v2 b) = substitute ([],[(tVar v1,v2)]) (TAbs v1 (alphaConvert a b))


alphaConvertList :: [Term] -> [Term] -> [Term]
alphaConvertList a b = map (\x -> alphaConvert (fst x) (snd x)) (zip a b)


substitute :: Substitution -> Term -> Term
substitute (tymap,vmap) term =
    let typesub =
            (\x y ->
                case y of
                    (TConst a ty) -> if (ty == (TypeVar . fst $ x))
                                     then TConst a (snd x)
                                     else TConst a ty
                    (TApp a b) -> TApp (typesub x a) (typesub x b)
                    (TAbs (TVar (Var n ty)) a) -> if (ty == (TypeVar . fst $ x))
                                                  then TAbs (TVar (Var n (snd x))) (typesub x a)
                                                  else TAbs (TVar (Var n ty)) (typesub x a)
                    (TVar (Var n ty)) -> if (ty == (TypeVar . fst $ x))
                                         then TVar (Var n (snd x))
                                         else TVar (Var n ty))
        varsub =
            (\x y ->
                case y of
                    (TConst a ty) -> TConst a ty
                    (TApp a b) -> TApp (varsub x a) (varsub x b)
                    (TVar v) -> if (v == (fst x))
                                then snd x
                                else TVar v
                    (TAbs v a) -> let safe = rename (TAbs v a) (Set.insert (fst x) (freeVars . snd $ x))
                                  in case safe of
                                         (TAbs m n) -> TAbs m (varsub x n))
        tydone = foldl' (\x y -> typesub y x) term tymap
        vdone = foldl' (\x y -> varsub y x) tydone vmap
    in vdone


boundVars :: Term -> Set.Set Var
boundVars (TConst a b) = Set.empty
boundVars (TApp a b) = Set.union (boundVars a) (boundVars b)
boundVars (TVar a) = Set.empty
boundVars (TAbs a b) = Set.insert (tVar a) (boundVars b)


freeVars :: Term -> Set.Set Var
freeVars (TConst a b) = Set.empty
freeVars (TApp a b) = Set.union (freeVars a) (freeVars b)
freeVars (TVar a) = Set.singleton a
freeVars (TAbs a b) = Set.delete (tVar a) (freeVars b)


rename :: Term -> Set.Set Var -> Term
rename (TAbs (TVar v) t) vars =
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
            (\x y -> if (Set.member x y)
                     then case x of
                              (Var a b) ->
                                  case a of
                                      (Name c d) -> findSafe (Var (Name c (d ++ "'")) b) y
                     else x)
    in if (Set.member v vars)
       then doRename (TAbs (TVar v) t) v (findSafe v vars)
       else TAbs (TVar v) t


typeOf :: Term -> Type
typeOf (TConst _ ty) = ty
typeOf (TVar v) = varTy v
typeOf (TAbs v t) = typeFunc (typeOf v) (typeOf t)
typeOf (TApp f _) = 
    -- type of f is of the form [[a,b], "->"]
    last . aType . typeOf $ f


typeVarsInTerm :: Term -> Set.Set Type
typeVarsInTerm (TConst _ ty) = typeVarsInType ty
typeVarsInTerm (TVar v) = typeVarsInType . varTy $ v
typeVarsInTerm (TAbs v t) = Set.union (typeVarsInType . varTy . tVar $ v) (typeVarsInTerm t)
typeVarsInTerm (TApp f x) = Set.union (typeVarsInTerm f) (typeVarsInTerm x)


mkEquals :: Term -> Term -> Term
mkEquals lhs rhs =
    let eqConst = TConst (Const (Name [] "=")) (mkEqualsType (typeOf lhs))
    in TApp (TApp eqConst lhs) rhs


getlhs :: Term -> Term
getlhs (TApp (TApp eq lhs) _)
    | (isEq eq) = lhs


getrhs :: Term -> Term
getrhs (TApp (TApp eq _) rhs)
    | (isEq eq) = rhs


isEq :: Term -> Bool
isEq (TConst (Const (Name [] "=")) _) = True
isEq _ = False
