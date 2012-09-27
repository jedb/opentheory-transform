module Library.Stack (
	Stack,
    empty,
    at,
    pop,
    (<:>),
    size
    ) where


import Data.List


data Stack a = Stack [a] deriving (Eq)


instance Show a => Show (Stack a) where
    show (Stack x)   =   "Stack:\n" ++ intercalate "\n" (map (show) x) ++ "\n\n"


infixr 9 <:>


empty :: Stack a
empty = Stack []


at :: Stack a -> Int -> Maybe a
at (Stack list) index =
    if (index < length list && index >= 0)
    then Just (list!!index)
    else Nothing


pop :: Int -> Stack a -> Stack a
pop n (Stack list) = Stack (drop n list)


(<:>) :: a -> Stack a -> Stack a
x <:> (Stack list) = Stack (x : list)


size :: Stack a -> Int
size (Stack list) = length list
