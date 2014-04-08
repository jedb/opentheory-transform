module Library.TypeVar (
	Number,

	Name(..),

	TypeOp(..),

	Type(..),

	Const(..),

	Var(..),

	mkEqualsType,
	typeFunc,
	typeBool,
	typeVarsInType,
	isTypeVar,
	typeVarSub
	) where



import Data.List
import Data.Maybe
import qualified Data.Set as Set
import Data.Map( Map, (!) )
import qualified Data.Map as Map



type Number = Int

data Name = Name { nameSpace :: [String]
                 , nameId :: String } deriving (Eq, Ord)

data TypeOp = TypeOp { tyOp :: Name } deriving (Eq, Ord)

data Type = TypeVar { typeVar :: Name }
          | AType { aType :: [Type]
                  , aTypeOp :: TypeOp } deriving (Eq, Ord)

data Const = Const { constName :: Name } deriving (Eq, Ord)

data Var = Var { varName :: Name
               , varTy :: Type } deriving (Eq, Ord)



instance Show Name where
    show a   =   intercalate "." (nameSpace a ++ [nameId a])

instance Show TypeOp where
    show a   =   "typeOp " ++ (show $ tyOp a)

instance Show Type where
    show (TypeVar tyVar)   =   "V " ++ (show tyVar)
    show (AType [] (TypeOp (Name [] "bool"))) = "bool"
    show (AType [d,r] (TypeOp (Name [] "->"))) = "(" ++ show d ++ " -> " ++ show r ++ ")"
    show (AType list typeOp) =   "type " ++ (show $ tyOp typeOp) ++ " " ++ (show list)

instance Show Const where
    show (Const a)   =   show a

instance Show Var where
    show (Var a _)   =   show a



mkEqualsType :: Type -> Type
mkEqualsType ty = typeFunc ty (typeFunc ty typeBool)


typeFunc :: Type -> Type -> Type
typeFunc ty1 ty2 = AType [ty1,ty2] (TypeOp (Name [] "->"))


typeBool :: Type
typeBool = AType [] (TypeOp (Name [] "bool"))


typeVarsInType :: Type -> Set.Set Type
typeVarsInType (TypeVar t) = Set.singleton (TypeVar t)
typeVarsInType (AType list _) = Set.unions . (map typeVarsInType) $ list


isTypeVar :: Type -> Bool
isTypeVar (TypeVar _) = True
isTypeVar _ = False


typeVarSub :: Map Name Type -> Type -> Type
typeVarSub m (TypeVar a) = 
	if (Map.member a m)
	then fromJust (Map.lookup a m)
	else TypeVar a
typeVarSub m (AType list op) =
	AType (map (typeVarSub m) list) op

