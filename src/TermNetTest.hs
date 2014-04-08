import Library.Theorem
import Library.Term
import qualified Library.TermNet as Net
import Test.DataTypes
import qualified Data.Set as Set
import Data.List



main = do
    let thm1 = Theorem (Set.empty) stdConstTerm
        thm2 = Theorem (Set.empty) (stdVarTerm "b")
        thm3 = Theorem (Set.empty) (stdVarTerm "c")
        thm4 = Theorem (Set.empty) (stdAbsTerm "h")

        net1 = Net.addThm Net.empty thm1 0
        net2 = Net.addThm net1 thm2 1
        net3 = Net.addThm net2 thm3 2
        net4 = Net.addThm net3 thm4 3

        match = Net.matchThm net4 thm4
 

    putStrLn (show net4)
    putStrLn ""
    putStrLn (show match)
