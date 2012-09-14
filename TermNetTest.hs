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

        (net1, matches1) = Net.addThm Net.empty thm1 0
        (net2, matches2) = Net.addThm net1 thm2 1
        (net3, matches3) = Net.addThm net2 thm3 2


    putStrLn (show net3)
    putStrLn ""
    putStrLn (show matches3)
    putStrLn (intercalate " " . Net.thmToTermString $ thm1)
    putStrLn (intercalate " " . Net.thmToTermString $ thm2)
    putStrLn (intercalate " " . Net.thmToTermString $ thm3)
