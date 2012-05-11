module Term (
    Term(..),

    alphaEquiv,
    alphaConvert,
    alphaConvertList,
    substitute,
    containsVars,
    rename,
    typeOf,
    mkEquals,
    isEq,
    getlhs,
    getrhs
    ) where



import Data.List
import TypeVar



data Term = TVar { tVar :: Var }
          | TConst { tConst :: Const
                   , tConstType :: Type }
          | TApp { tAppLeft :: Term
                 , tAppRight :: Term }
          | TAbs { tAbsVar :: Term
                 , tAbsTerm :: Term }

type Substitution = ( [(Name,Type)], [(Var,Term)] )


instance Show Term where
    show (TVar a)       =   show a
    show (TConst a _)   =   show a
    show (TApp (TApp eq lhs) rhs)
            | isEq eq   =   "(" ++ (show lhs) ++ " = " ++ (show rhs) ++ ")"
    show (TApp a b)     =   "(" ++ (show a) ++ " " ++ (show b) ++ ")"
    show (TAbs a b)     =   "(\\" ++ (show a) ++ " -> " ++ (show b) ++ ")"

instance Eq Term where
    a == b   =   a `alphaEquiv` b



alphaEquiv :: Term -> Term -> Bool
alphaEquiv a b =
    let equiv = \term1 term2 varmap lambdaDepth ->
            case (term1,term2) of
                (TConst a1 b1, TConst a2 b2) ->
                    a1 == a2 && b1 == b2

                (TApp a1 b1, TApp a2 b2) ->
                    equiv a1 a2 varmap lambdaDepth &&
                    equiv b1 b2 varmap lambdaDepth

                (TAbs (TVar (Var name1 type1)) b1, TAbs (TVar (Var name2 type2)) b2) ->
                    type1 == type2 &&
                    equiv b1 b2 newmap (lambdaDepth + 1)
                    where newmap = (lambdaDepth + 1, ((Var name1 type1),(Var name2 type2))) : varmap

                (TVar a1, TVar a2) ->
                    -- the order of the pair is important
                    (lambdaDepth, (a1,a2)) `elem` varmap ||
                    not ((lambdaDepth, (a1,a2)) `elem` varmap) && a1 == a2

                (_,_) -> False
    in equiv a b [] 0


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
                    (TConst a ty) -> if (ty == (TypeVar . fst $ x))
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
                    (TVar v) -> if (v == (fst x))
                                then snd x
                                else TVar v
                    (TAbs v a) -> let safe = rename (TAbs v a) (union [fst x] (containsVars . snd $ x))
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
typeOf (TVar v) = varTy v
typeOf (TAbs v t) = typeFunc (typeOf v) (typeOf t)
typeOf (TApp f _) = 
    -- type of f is of the form [[a,b], "->"]
    last . aType . typeOf $ f


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
