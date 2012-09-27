import System.Environment( getArgs )
import Text.Printf
import Library.Parse
import Library.Semantic
import Library.Stack



main = do
    args <- getArgs
    listA <- getLines (args!!0)
    listB <- getLines (args!!1)
    let resultA = eval (map (stripReturn) listA)
        resultB = eval (map (stripReturn) listB)
        output = if (resultA == resultB)
    	         then "Articles identical\n"
    	         else "Differences detected\n"
    printf output
