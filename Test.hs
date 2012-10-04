import Test.HUnit
import Library.Command
import Library.TypeVar
import Library.Term
import Library.Theorem
import Test.DataTypes
import qualified Data.Set as Set



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



substitute1 = TestCase (assertEqual "for (substitute ([],[(x',a)]) (TVar x'))"
	                                (stdConstTerm)
	                                (substitute ([],[((stdVar "x'"),stdConstTerm)]) (stdVarTerm "x'")))

substitute2 = TestCase (assertEqual "for (substitute ([],[(x',a)]) (TVar y'))"
	                                (stdVarTerm "y'")
	                                (substitute ([],[((stdVar "x'"),stdConstTerm)]) (stdVarTerm "y'")))

substitute3 = TestCase (assertEqual "for (substitute ([],[(x',a)]) (TApp (TVar x') (TVar x')))"
	                                (TApp stdConstTerm stdConstTerm)
	                                (substitute ([],[((stdVar "x'"),stdConstTerm)]) (TApp (stdVarTerm "x'") (stdVarTerm "x'"))))

substitute4 = TestCase (assertEqual "for (substitute ([],[(x',a)]) (\\y' -> x'))"
	                                (TAbs (stdVarTerm "y'") stdConstTerm)
	                                (substitute ([],[((stdVar "x'"),stdConstTerm)]) (TAbs (stdVarTerm "y'") (stdVarTerm "x'"))))

substitute5 = TestCase (assertEqual "for (substitute ([],[(x',y')]) (\\y' -> x'))"
	                                (TAbs (stdVarTerm "y''") (stdVarTerm "y'"))
	                                (substitute ([],[((stdVar "x'"),(stdVarTerm "y'"))]) (TAbs (stdVarTerm "y'") (stdVarTerm "x'"))))

substitute6 = TestCase (assertEqual "for (substitute ([(tx',ta)],[]) (z' with typevar tx'))"
	                                (TVar (Var (stdName "z'") stdType))
	                                (substitute ([(stdTypeVarName,stdType)],[]) (stdVarTerm "z'")))

substitute7 = TestCase (assertEqual "for (substitute ([(tx',ta)],[(y',b)]) (\\z' -> y')) --z' has type tx'"
	                                (TAbs (TVar (Var (stdName "z'") stdType)) stdConstTerm)
	                                (substitute ([(stdTypeVarName,stdType)],[((altVar "y'"),stdConstTerm)]) (TAbs (stdVarTerm "z'") (altVarTerm "y'"))))

substitute8 = TestCase (assertEqual "for (substitute ([],[(x',y'),(y',z')]) (x'))"
	                                (stdVarTerm "z'")
	                                (substitute ([],[((stdVar "x'"),(stdVarTerm "y'")),
	                                	             ((stdVar "y'"),(stdVarTerm "z'"))]) (stdVarTerm "x'")))

substitute9 = TestCase (assertEqual "for (substitute ([(tx',ty'),(ty',ta)],[]) (z' with typevar tx'))"
	                                (TVar (Var (stdName "z'") stdType))
	                                (substitute ([(stdTypeVarName,altTypeVar),(altTypeVarName,stdType)],[]) (TVar (Var (stdName "z'") altTypeVar))))


main =
	do putStrLn "Command.name"
	   runTestTT $ TestList [name1,name2,name3,name4]
	   putStrLn "Command.number"
	   runTestTT $ TestList [number1,number2,number3,number4,number5]
	   putStrLn "Command.assume"
	   runTestTT $ TestList [assume1,assume2]
	   putStrLn "Command.axiom"
	   runTestTT $ TestList [axiom1,axiom2]
	   putStrLn "Term.alphaEquiv"
	   runTestTT $ TestList [alphaEquiv1,alphaEquiv2,alphaEquiv3,alphaEquiv4,alphaEquiv5,alphaEquiv6]
	   putStrLn "Term.substitute"
	   runTestTT $ TestList [substitute1,substitute2,substitute3,substitute4,substitute5,substitute6,substitute7,substitute8,substitute9]

