module Theorem (
	Theorem(..),
	) where



import Data.List
import TypeVar
import Term



data Theorem = Theorem { thmHyp :: [Term]
                       , thmCon :: Term } deriving (Eq)



instance Show Theorem where
    show a   =   (show . thmHyp $ a) ++ " |- " ++ (show . thmCon $ a)
