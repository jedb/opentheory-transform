module Object (
	Object(..),

	List
	) where



import Data.List
import TypeVar
import Term
import Theorem



data Object = ObjNum { objNum :: Number }
            | ObjName { objName :: Name }
            | ObjList { objList :: List }
            | ObjTyOp { objTyOp :: TypeOp }
            | ObjType { objType :: Type }
            | ObjConst { objConst :: Const }
            | ObjVar { objVar :: Var }
            | ObjTerm { objTerm :: Term }
            | ObjThm { objThm :: Theorem } deriving (Eq)

type List = [Object]



instance Show Object where
    show (ObjNum a)    =   show a
    show (ObjName a)   =   show a
    show (ObjList a)   =   show a
    show (ObjTyOp a)   =   show a
    show (ObjType a)   =   show a
    show (ObjConst a)  =   show a
    show (ObjVar a)    =   show a
    show (ObjTerm a)   =   show a
    show (ObjThm a)    =   show a
