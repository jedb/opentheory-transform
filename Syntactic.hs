import Control.Monad( liftM )
import System( getArgs )
import Text.Printf
import qualified Data.Char as Char


getLines :: FilePath -> IO [String]
getLines = liftM lines . readFile


stripReturn :: String -> String
stripReturn s = if (last s == '\r') then init s else s


isComment :: String -> Bool
isComment = (==) '#' . head


isNumber :: String -> Bool
isNumber ('0':[]) = True
isNumber ('-':ns)
         | (ns /= [] && head ns /= '0') = isNumber ns
isNumber n = all (Char.isNumber) n


isName :: String -> Bool
isName s = all ((==) '"') [head s, last s]


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

