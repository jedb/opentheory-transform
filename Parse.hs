import Control.Monad( liftM )
import qualified Data.Char as Char


getLines :: FilePath -> IO [String]
getLines = liftM lines . readFile


stripReturn :: String -> String
stripReturn s = if (last s == '\r') then init s else s


removeEscChars :: String -> String
removeEscChars [] = []
removeEscChars (x:[]) = [x]
removeEscChars x = if (head x == '\\')
                   then (x!!1) : (removeEscChars . (drop 2) $ x)
                   else (head x) : (removeEscChars . tail $ x)


removeQuotes :: String -> String
removeQuotes = init . tail


separateBy :: String -> Char -> [String]
separateBy char list =
	let f = (\x -> if (x == char)
		           then ' '
		           else x)
	in words . (map f) $ list


isComment :: String -> Bool
isComment = (==) '#' . head


isNumber :: String -> Bool
isNumber ('0':[]) = True
isNumber ('-':ns)
         | (ns /= [] && head ns /= '0') = isNumber ns
isNumber n = all (Char.isNumber) n


isName :: String -> Bool
isName s = all ((==) '"') [head s, last s]
