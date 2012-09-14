import Library.Parse
import Library.Generator
import Library.Term
import Library.TypeVar


main = do
	let s = substitutionGen ([],[])
	let t = substitutionGen ( [(Name [] "tyvar", AType [] (TypeOp (Name [] "atype")))], [] )
	output t
