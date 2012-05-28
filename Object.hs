module Object (
	Object(..),
    objNum,
    objName,
    objList,
    objTyOp,
    objType,
    objConst,
    objVar,
    objTerm,
    objThm,

	List,

    makeSubst
	) where



import Data.Maybe
import Data.List
import TypeVar
import Term
import Theorem



data Object = ObjNum Number
            | ObjName Name
            | ObjList List
            | ObjTyOp TypeOp
            | ObjType Type
            | ObjConst Const
            | ObjVar Var
            | ObjTerm Term
            | ObjThm Theorem deriving (Eq, Ord)

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



makeSubst :: [Object] -> Maybe Substitution
makeSubst l =
    let list = (map (mapMaybe objList)) . (mapMaybe objList) $ l
        f = (\g h x -> (g . head $ x, h . last $ x))
        check = f (map (f objName objType)) (map (f objVar objTerm)) list
        g = all (\x -> (isJust . fst $ x) && (isJust . snd $ x))
        h = (\x -> (fromJust . fst $ x, fromJust . snd $ x))
    in if ((g . fst $ check) && (g . snd $ check))
       then Just (map h (fst check), map h (snd check))
       else Nothing


objNum :: Object -> Maybe Number
objNum obj =
    case (obj) of
        (ObjNum n) -> Just n
        (_) -> Nothing


objName :: Object -> Maybe Name
objName obj =
    case (obj) of
        (ObjName n) -> Just n
        (_) -> Nothing


objList :: Object -> Maybe List
objList obj =
    case (obj) of
        (ObjList l) -> Just l
        (_) -> Nothing


objTyOp :: Object -> Maybe TypeOp
objTyOp obj =
    case (obj) of
        (ObjTyOp tyop) -> Just tyop
        (_) -> Nothing


objType :: Object -> Maybe Type
objType obj =
    case (obj) of
        (ObjType ty) -> Just ty
        (_) -> Nothing


objConst :: Object -> Maybe Const
objConst obj =
    case (obj) of
        (ObjConst c) -> Just c
        (_) -> Nothing


objVar :: Object -> Maybe Var
objVar obj =
    case (obj) of
        (ObjVar var) -> Just var
        (_) -> Nothing


objTerm :: Object -> Maybe Term
objTerm obj =
    case (obj) of
        (ObjTerm term) -> Just term
        (_) -> Nothing


objThm :: Object -> Maybe Theorem
objThm obj =
    case (obj) of
        (ObjThm thm) -> Just thm
        (_) -> Nothing

