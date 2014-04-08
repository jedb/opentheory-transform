import System.Environment( getArgs )
import Text.Printf
import Library.Parse
import Library.Semantic
import qualified Library.Stack as Stack
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe


main = do
    args <- getArgs
    listA <- getLines (args!!0)
    listB <- getLines (args!!1)

    let result = do resultA <- eval (map stripReturn listA)
                    resultB <- eval (map stripReturn listB)
                    let (sA,dA,aA,tA) = resultA
                        (sB,dB,aB,tB) = resultB

                        sA_diff = Stack.diff sA sB
                        sB_diff = Stack.diff sB sA

                        dA_diff = dA Map.\\ dB
                        dB_diff = dB Map.\\ dA

                        aA_diff = aA Set.\\ aB
                        aB_diff = aB Set.\\ aA

                        tA_diff = tA Set.\\ tB
                        tB_diff = tB Set.\\ tA
                    return (Just (sA_diff,dA_diff,aA_diff,tA_diff),
                    	    Just (sB_diff,dB_diff,aB_diff,tB_diff))
    
        output = if (isNothing result)
                 then "Error in article files\n"
                 else let (diff_A, diff_B) = fromJust result
                      in if (diff_A == diff_B)
                      	 then "Articles identical\n"
                      	 else (args!!0) ++ " has:\n" ++ (fromJust . machineToString $ diff_A) ++ "\n" ++
                      	      (args!!1) ++ " has:\n" ++ (fromJust . machineToString $ diff_B) ++ "\n"

    printf output
