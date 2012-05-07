import Control.Monad( liftM )
import System( getArgs )
import Text.Printf


getLines :: FilePath -> IO [String]
getLines = liftM lines . readFile


stripReturn :: String -> String
stripReturn s = if (last s == '\r') then init s else s


isComment :: String -> Bool
isComment = (==) '#' . head


isNumber :: String -> Bool
isNumber ('0':[]) = True
isNumber ('-':ns)
         | ns /= [] && head ns /= '0' = isNumber ns
isNumber n = null . filter (not . isDigit) $ n


isDigit :: Char -> Bool
isDigit '0' = True
isDigit '1' = True
isDigit '2' = True
isDigit '3' = True
isDigit '4' = True
isDigit '5' = True
isDigit '6' = True
isDigit '7' = True
isDigit '8' = True
isDigit '9' = True
isDigit _ = False


isName :: String -> Bool
isName s = foldr (&&) True $ map ((==) '"') $ [head s, last s]


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

