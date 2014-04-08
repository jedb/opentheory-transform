module Library.Generator (
    listGen,
    substitutionGen,
    termGen,
    varGen,
    typeGen,
    typeOpGen,
    constGen,
    nameGen
    ) where


import Data.List
import Library.Term
import Library.TypeVar



listGen :: (a -> [String]) -> [a] -> [String]
listGen f list =
    concat (map f list) ++ ["nil"] ++ replicate (length list) "cons"



substitutionGen :: Substitution -> [String]
substitutionGen sub =
    let varTermList = listGen varTermPair (snd sub)
        nameTypeList = listGen nameTypePair (fst sub)
    in nameTypeList ++ varTermList ++ ["nil", "cons", "cons"]



varTermPair :: (Var, Term) -> [String]
varTermPair (var, term) =
    (varGen var) ++ (termGen term) ++ ["nil", "cons", "cons"]



nameTypePair :: (Name, Type) -> [String]
nameTypePair (name, ty) =
    (nameGen name) ++ (typeGen ty) ++ ["nil", "cons", "cons"]



termGen :: Term -> [String]
termGen (TVar v) = (varGen v) ++ ["varTerm"]
termGen (TConst c ty) = (constGen c) ++ (typeGen ty) ++ ["constTerm"]
termGen (TApp h x) = (termGen h) ++ (termGen x) ++ ["appTerm"]
termGen (TAbs x t) = (termGen x) ++ (termGen t) ++ ["absTerm"]



varGen :: Var -> [String]
varGen var =
    (nameGen . varName $ var) ++ (typeGen . varTy $ var) ++ ["var"]



typeGen :: Type -> [String]
typeGen (TypeVar v) = (nameGen v) ++ ["varType"]
typeGen (AType ty op) = 
    let list = listGen typeGen ty
    in (typeOpGen op) ++ list ++ ["opType"]



typeOpGen :: TypeOp -> [String]
typeOpGen op = 
    (nameGen . tyOp $ op) ++ ["typeOp"]



constGen :: Const -> [String]
constGen c =
    (nameGen . constName $ c) ++ ["const"]



nameGen :: Name -> [String]
nameGen name =
    ["\"" ++ intercalate "." (nameSpace name ++ [nameId name]) ++ "\""]

