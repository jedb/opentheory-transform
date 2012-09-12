module Library.Command (
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

import Control.Monad
import Data.List
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Map as Map
import Library.TypeVar
import Library.Term
import Library.Theorem
import Library.Object
import Library.Parse


name :: String -> Maybe Name
name str =
	do guard (isName str)
	   let wordlist = (separateBy '.') . removeEscChars . removeQuotes $ str
	   return (Name (init wordlist) (last wordlist))


number :: String -> Maybe Number
number num =
	do guard (isNumber num)
	   return (read num)


absTerm :: Term -> Var -> Term
absTerm term var =
	TAbs (TVar var) term


absThm :: Theorem -> Var -> Maybe Theorem
absThm thm var =
	do guard (not (Set.member (TVar var) (thmHyp thm)))
	   return (Theorem (thmHyp thm) (mkEquals (TAbs (TVar var) (getlhs . thmCon $ thm))
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
	do guard (typeOf term == typeBool)
	   return (Theorem (Set.singleton term) term)


axiom :: Term -> [Term] -> Maybe Theorem
axiom term termlist =
	do guard (all ((== typeBool) . typeOf) termlist)
	   return (Theorem (Set.fromList termlist) term)


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
	do guard ((freeVars term == Set.empty) && (typeVarsInTerm term == typeVarsInType (typeOf term)))
	   let constant = Const name
	       constTerm = TConst constant (typeOf term)
	       theorem = Theorem Set.empty (mkEquals constTerm term)
	   return (theorem, constant)


defineTypeOp :: Theorem -> [Name] -> Name -> Name -> Name -> Maybe (Theorem, Theorem, Const, Const, TypeOp)
defineTypeOp thm namelist r a n =
	do guard ((typeVarsInTerm . tAppLeft . thmCon $ thm) == (Set.fromList . map (TypeVar) $ namelist) &&
		      (length namelist) == (length . nub $ namelist))
	   let rep = Const r
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
	   return (rthm, athm, rep, abst, op)


eqMp :: Theorem -> Theorem -> Maybe Theorem
eqMp thm1 thm2 =
	do guard (thmCon thm1 == (getlhs . thmCon $ thm2))
	   return (Theorem (Set.union (thmHyp thm1) (thmHyp thm2))
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
	do guard ((term == thmCon oldthm) && (Set.fromList termlist == thmHyp oldthm))
	   return (Theorem (Set.fromList (alphaConvertList (Set.toList . thmHyp $ oldthm) termlist))
	   	               (alphaConvert (thmCon oldthm) term))


typeOp :: Name -> TypeOp
typeOp name =
	TypeOp name


var :: Type -> Name -> Maybe Var
var ty name =
    do guard ((nameSpace name) == [])
       return (Var name ty)


varTerm :: Var -> Term
varTerm var =
	TVar var


varType :: Name -> Maybe Type
varType name =
    do guard ((nameSpace name) == [])
       return (TypeVar name)


