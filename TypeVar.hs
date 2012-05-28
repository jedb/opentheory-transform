module TypeVar (
	Number,

	Name(..),

	TypeOp(..),

	Type(..),

	Const(..),

	Var(..),

	mkEqualsType,
	typeFunc,
	typeBool,
	typeVarsInType
	) where



import Data.List
import qualified Data.Set as Set



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
    show (TypeVar tyVar)   =   "typeVar " ++ (show tyVar)
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
