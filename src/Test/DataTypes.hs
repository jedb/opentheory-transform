module Test.DataTypes(
    stdName,
    stdType,
    stdConst,
    stdConstTerm,
    stdTypeVarName,
    altTypeVarName,
    stdTypeVar,
    altTypeVar,
    stdVar,
    stdVarTerm,
    altVar,
    altVarTerm,
    stdAbsTerm,
    stdAppTerm
    ) where



import Library.TypeVar
import Library.Term



stdName :: String -> Name
stdName s = Name [] s 

stdType :: Type
stdType = AType [] (TypeOp (stdName "atype"))

stdConst :: Const
stdConst = Const (stdName "const")

stdConstTerm :: Term
stdConstTerm = TConst stdConst stdType

stdTypeVarName :: Name
stdTypeVarName = stdName "typevar"

altTypeVarName :: Name
altTypeVarName = stdName "alttypevar"

stdTypeVar :: Type
stdTypeVar = TypeVar stdTypeVarName

altTypeVar :: Type
altTypeVar = TypeVar altTypeVarName

stdVar :: String -> Var
stdVar s = Var (stdName s) stdTypeVar

stdVarTerm :: String -> Term
stdVarTerm s = TVar (stdVar s)

altVar :: String -> Var
altVar s = Var (stdName s) altTypeVar

altVarTerm :: String -> Term
altVarTerm s = TVar (altVar s)

stdAbsTerm :: String -> Term
stdAbsTerm s = TAbs (stdVarTerm s) stdConstTerm

stdAppTerm :: String -> Term
stdAppTerm s = TApp (stdAbsTerm s) stdConstTerm



