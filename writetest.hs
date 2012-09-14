import Library.WriteProof
import Library.ProofGraph


list = [-- create a var term, define a constant to it
        "\"const\"","\"x\"","const","\"t1\"","typeOp","nil","opType","1","def","constTerm","0","def","defineConst",
        
        -- hypothesis list for theorem
        "nil",

        -- equals name
        "\"=\"","const",

        -- equals type
        "\"->\"","typeOp","1","ref","\"->\"","typeOp","1","ref","\"bool\"","typeOp","nil","opType","nil","cons","cons","opType",
        "nil","cons","cons","opType",

        -- equals term
        "constTerm",

        -- the constant
        "\"const\"","const","1","remove","constTerm",

        -- construct the equation of constant = variable
        "appTerm",
        "0","remove",
        "appTerm",

        "thm","pop"]

graph = doGraphGen list

fst3 :: (a,b,c) -> a
fst3 (a,_,_) = a

snd3 :: (a,b,c) -> b
snd3 (_,b,_) = b

thd3 :: (a,b,c) -> c
thd3 (_,_,c) = c

edgeCheck :: (Ord d) => (a,b,(c,d)) -> (e,f,(g,d)) -> Ordering
edgeCheck = (\a b -> compare (snd . thd3 $ a) (snd . thd3 $ b))
