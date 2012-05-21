import Test.HUnit
import Command
import TypeVar


nametest1 = TestCase (assertEqual "for (name \"abc\")" 
	                              (Just (Name [] "abc"))
	                              (name "\"abc\""))

nametest2 = TestCase (assertEqual "for (name \"first.second.third\")" 
	                              (Just (Name ["first","second"] "third"))
	                              (name "\"first.second.third\""))

nametest3 = TestCase (assertEqual "for (name \"firs\\t.se\\cond.\\t\\h\\ird\")"
	                              (Just (Name ["first","second"] "third"))
	                              (name "\"firs\\t.se\\cond.\\t\\h\\ird\""))

nametest4 = TestCase (assertEqual "for (name abc)"
	                              Nothing
	                              (name "abc"))

main =
	runTestTT $ TestList [nametest1,nametest2,nametest3,nametest4]
