module Library.Parse (
	getLines,
	stripReturn,
	removeEscChars,
	removeQuotes,
	separateBy,
	isComment,
	isNumber,
	isName,
	output,
	fst3,
	snd3,
	thd3
	) where

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


separateBy :: Char -> String -> [String]
separateBy char list =
	let f x = if (x == char) then ' ' else x
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


output :: [String] -> IO ()
output [] = return ()
output list = do
	putStrLn (head list)
	output (tail list)


fst3 :: (a,b,c) -> a
fst3 (a,_,_) = a

snd3 :: (a,b,c) -> b
snd3 (_,b,_) = b

thd3 :: (a,b,c) -> c
thd3 (_,_,c) = c
