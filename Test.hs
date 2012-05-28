import Test.HUnit
import Command
import TypeVar
import Term
import Theorem
import qualified Data.Set as Set



stdName :: String -> Name
stdName s = Name [] s 

stdConst :: Const
stdConst = Const (stdName "const")

stdTypeVar :: Type
stdTypeVar = TypeVar (stdName "typevar")

stdVar :: String -> Var
stdVar s = Var (stdName s) stdTypeVar

stdVarTerm :: String -> Term
stdVarTerm s = TVar (stdVar s)

altVarTerm :: String -> Term
altVarTerm s = TVar (Var (stdName s) (TypeVar (stdName "alttypevar")))




name1 = TestCase (assertEqual "for (name \"abc\")" 
	                          (Just (Name [] "abc"))
	                          (name "\"abc\""))

name2 = TestCase (assertEqual "for (name \"first.second.third\")" 
	                          (Just (Name ["first","second"] "third"))
	                          (name "\"first.second.third\""))

name3 = TestCase (assertEqual "for (name \"firs\\t.se\\cond.\\t\\h\\ird\")"
	                          (Just (Name ["first","second"] "third"))
	                          (name "\"firs\\t.se\\cond.\\t\\h\\ird\""))

name4 = TestCase (assertEqual "for (name abc)" Nothing (name "abc"))



number1 = TestCase (assertEqual "for (number \"90\")" (Just 90) (number "90"))
number2 = TestCase (assertEqual "for (number \"0\")" (Just 0) (number "0"))
number3 = TestCase (assertEqual "for (number \"-1\")" (Just (-1)) (number "-1"))
number4 = TestCase (assertEqual "for (number \"-0\")" Nothing (number "-0"))
number5 = TestCase (assertEqual "for (number \"1.2\")" Nothing (number "1.2"))



assume1 = TestCase (assertEqual "for (assume (TConst ...)"
	                            (Just (Theorem (Set.singleton (TConst stdConst typeBool))
	                             	           (TConst stdConst typeBool)))
	                            (assume (TConst stdConst typeBool)))

assume2 = TestCase (assertEqual "for (assume (TConst ...) --with wrong type"
                                Nothing
	                            (assume (TConst stdConst stdTypeVar)))



axiom1 = TestCase (assertEqual "for (axiom (TConst ...) [])"
	                           (Just (Theorem Set.empty (TConst stdConst stdTypeVar)))
	                           (axiom (TConst stdConst stdTypeVar) []))

axiom2 = TestCase (assertEqual "for (axiom (TConst ...) [term1, term2] --term1 has wrong type"
	                           Nothing
	                           (axiom (TConst stdConst stdTypeVar)
	                           	      [(TConst stdConst stdTypeVar),(TConst stdConst typeBool)]))



alphaEquiv1 = TestCase (assertEqual "for ((\\xy -> x) `alphaEquiv` (\\yx -> x))"
	                                False
	                                (alphaEquiv (TAbs (stdVarTerm "x") (TAbs (stdVarTerm "y") (stdVarTerm "x"))) 
	                                	        (TAbs (stdVarTerm "y") (TAbs (stdVarTerm "x") (stdVarTerm "x")))))

alphaEquiv2 = TestCase (assertEqual "for ((\\xy -> x) `alphaEquiv` (\\xy -> x))"
	                                True
	                                (alphaEquiv (TAbs (stdVarTerm "x") (TAbs (stdVarTerm "y") (stdVarTerm "x")))
	                                	        (TAbs (stdVarTerm "x") (TAbs (stdVarTerm "y") (stdVarTerm "x")))))

alphaEquiv3 = TestCase (assertEqual "for ((\\xyx -> x) `alphaEquiv` (\\yxx -> x))"
	                                True
	                                (alphaEquiv (TAbs (stdVarTerm "x") (TAbs (stdVarTerm "y") (TAbs (stdVarTerm "x") (stdVarTerm "x"))))
	                                	        (TAbs (stdVarTerm "y") (TAbs (stdVarTerm "x") (TAbs (stdVarTerm "x") (stdVarTerm "x"))))))

alphaEquiv4 = TestCase (assertEqual "for ((\\xyz -> y) `alphaEquiv` (\\zyx -> y))"
	                                True
	                                (alphaEquiv (TAbs (stdVarTerm "x") (TAbs (stdVarTerm "y") (TAbs (stdVarTerm "z") (stdVarTerm "y"))))
	                                	        (TAbs (stdVarTerm "z") (TAbs (stdVarTerm "y") (TAbs (stdVarTerm "x") (stdVarTerm "y"))))))

alphaEquiv5 = TestCase (assertEqual "for ((\\x -> x) `alphaEquiv` (\\x -> x)) --x of lhs is different type to x of rhs"
	                                False
	                                (alphaEquiv (TAbs (stdVarTerm "x") (stdVarTerm "x"))
	                                	        (TAbs (altVarTerm "x") (altVarTerm "x"))))

alphaEquiv6 = TestCase (assertEqual "for ((TAbs ...) `alphaEquiv` (TConst ...))"
	                                False
	                                (alphaEquiv (TAbs (stdVarTerm "x") (stdVarTerm "x"))
	                                	        (TConst stdConst typeBool)))

main =
	runTestTT $ TestList [name1,name2,name3,name4,
	                      number1,number2,number3,number4,number5,
	                      assume1,assume2,
	                      axiom1,axiom2,
	                      alphaEquiv1,alphaEquiv2,alphaEquiv3,alphaEquiv4,alphaEquiv5,alphaEquiv6]

