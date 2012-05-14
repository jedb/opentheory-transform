import System( getArgs )
import Text.Printf


scan :: String -> String
scan s = if (s == "absTerm" ||
	         s == "absThm" ||
	         s == "appTerm" ||
	         s == "appThm" ||
	         s == "assume" ||
	         s == "axiom" ||
	         s == "betaConv" ||
	         s == "cons" ||
	         s == "const" ||
	         s == "constTerm" ||
	         s == "deductAntisym" ||
	         s == "def" ||
	         s == "defineConst" ||
	         s == "defineTypeOp" ||
	         s == "eqMp" ||
	         s == "nil" ||
	         s == "opType" ||
	         s == "pop" ||
	         s == "ref" ||
	         s == "refl" ||
	         s == "remove" ||
	         s == "subst" ||
	         s == "thm" ||
	         s == "typeOp" ||
	         s == "var" ||
	         s == "varTerm" ||
	         s == "varType" ||
	         isComment(s) ||
	         isNumber(s) ||
	         isName(s))
         then s
         else error $ "Invalid input " ++ s


doScan :: [String] -> [String]
doScan = map (scan . stripReturn)


main = do
	args <- getArgs
	list <- getLines $ head args
	plist <- return $ doScan list
	printf $ if (list == plist) then "Scan OK\n" else "Syntax error\n"

