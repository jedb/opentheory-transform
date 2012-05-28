module Command (
	name,
	number,
	absTerm,
	absThm,
	appTerm,
	appThm,
	assume,
	axiom,
	betaConv,
	constant,
	constTerm,
	deductAntisym,
	defineConst,
	defineTypeOp,
	eqMp,
	opType,
	refl,
	subst,
	thm,
	typeOp,
	var,
	varTerm,
	varType
    ) where

-- deliberately not included:
-- cons, nil, def, ref, remove, pop

-- all functions here deal exclusively with arguments
-- and results from/to the stack

import Data.List
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Map as Map
import TypeVar
import Term
import Theorem
import Object
import Parse


name :: String -> Maybe Name
name str =
	if (not . isName $ str)
	then Nothing
	else let wordlist = (separateBy '.') . removeEscChars . removeQuotes $ str
	         name = Name (init wordlist) (last wordlist)
	     in Just name


number :: String -> Maybe Number
number num =
	if (not . isNumber $ num)
	then Nothing
	else Just (read num)


absTerm :: Term -> Var -> Term
absTerm term var =
	TAbs (TVar var) term


absThm :: Theorem -> Var -> Maybe Theorem
absThm thm var =
	if (Set.member (TVar var) (thmHyp thm))
	then Nothing
	else Just (Theorem (thmHyp thm) (mkEquals (TAbs (TVar var) (getlhs . thmCon $ thm))
		                                      (TAbs (TVar var) (getrhs . thmCon $ thm))))


appTerm :: Term -> Term -> Term
appTerm term1 term2 =
	TApp term2 term1


appThm :: Theorem -> Theorem -> Theorem
appThm thm1 thm2 =
	Theorem (Set.union (thmHyp thm1) (thmHyp thm2))
	        (mkEquals (TApp (getlhs . thmCon $ thm2) (getlhs . thmCon $ thm1))
                      (TApp (getrhs . thmCon $ thm2) (getrhs . thmCon $ thm1)))


assume :: Term -> Maybe Theorem
assume term =
	if (typeOf term /= typeBool)
	then Nothing
	else Just (Theorem (Set.singleton term) term)


axiom :: Term -> [Term] -> Maybe Theorem
axiom term termlist =
	if (not (all ((== typeBool) . typeOf) termlist))
	then Nothing
	else Just (Theorem (Set.fromList termlist) term)


betaConv :: Term -> Theorem
betaConv term =
	Theorem (Set.empty)
            (mkEquals term
                      (substitute ([], [(tVar . tAbsVar . tAppLeft $ term, tAppRight $ term)])
                                  (tAbsTerm . tAppLeft $ term)))


constant :: Name -> Const
constant name =
	Const name


constTerm :: Type -> Const -> Term
constTerm ty c =
	TConst c ty


deductAntisym :: Theorem -> Theorem -> Theorem
deductAntisym x y =
	Theorem (Set.union (Set.delete (thmCon $ x) (thmHyp $ y))
                       (Set.delete (thmCon $ y) (thmHyp $ x)))
            (mkEquals (thmCon $ y) (thmCon $ x))


defineConst :: Term -> Name -> Maybe (Theorem, Const)
defineConst term name =
	if (freeVars term /= Set.empty || typeVarsInTerm term /= typeVarsInType (typeOf term))
	then Nothing
	else let constant = Const name
	         constTerm = TConst constant (typeOf term)
	         theorem = Theorem Set.empty (mkEquals constTerm term)
	     in Just (theorem, constant)


defineTypeOp :: Theorem -> [Name] -> Name -> Name -> Name -> Maybe (Theorem, Theorem, Const, Const, TypeOp)
defineTypeOp thm namelist r a n =
	if ((typeVarsInTerm . tAppLeft . thmCon $ thm) /= (Set.fromList . map (TypeVar) $ namelist) ||
		(length namelist) /= (length . nub $ namelist))
	then Nothing
	else let rep = Const r
	         abst = Const a
	         op = TypeOp n
	         rtype = typeOf . tAppRight . thmCon $ thm
	         atype = AType (map (TypeVar) namelist) op
	         r' = TVar (Var (Name [] "r'") rtype)
	         a' = TVar (Var (Name [] "a'") atype)
	         reptype = typeFunc atype rtype
	         abstype = typeFunc rtype atype
	         repTerm = TConst rep reptype
	         absTerm = TConst abst abstype
	         rthm = Theorem Set.empty 
	                        (mkEquals (TApp (tAppLeft . thmCon $ thm) r')
	         	                      (mkEquals (TApp repTerm (TApp absTerm r')) r'))
	         athm = Theorem Set.empty
	                        (mkEquals (TApp absTerm (TApp repTerm a')) a')
	     in Just (rthm, athm, rep, abst, op)


eqMp :: Theorem -> Theorem -> Maybe Theorem
eqMp thm1 thm2 =
	if (thmCon thm1 /= (getlhs . thmCon $ thm2))
	then Nothing
	else Just (Theorem (Set.union (thmHyp thm1) (thmHyp thm2))
	                   (getrhs . thmCon $ thm2))


opType :: [Type] -> TypeOp -> Type
opType typelist tyOp =
	AType typelist tyOp


refl :: Term -> Theorem
refl term =
	Theorem Set.empty (mkEquals term term)


subst :: Theorem -> [Object] -> Maybe Theorem
subst thm list =
	do s <- makeSubst list
	   return (Theorem (Set.map (substitute s) (thmHyp thm))
                       (substitute s (thmCon thm)))


thm :: Term -> [Term] -> Theorem -> Maybe Theorem
thm term termlist oldthm =
	if ((term /= thmCon oldthm) || (Set.fromList termlist /= thmHyp oldthm))
	then Nothing
	else Just (Theorem (Set.fromList (alphaConvertList (Set.toList . thmHyp $ oldthm) termlist))
                       (alphaConvert (thmCon oldthm) term))


typeOp :: Name -> TypeOp
typeOp name =
	TypeOp name


var :: Type -> Name -> Maybe Var
var ty name =
	if (nameSpace name /= [])
	then Nothing
	else Just (Var name ty)


varTerm :: Var -> Term
varTerm var =
	TVar var


varType :: Name -> Maybe Type
varType name =
	if (nameSpace name /= [])
	then Nothing
	else Just (TypeVar name)


