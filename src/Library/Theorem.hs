module Library.Theorem (
	Theorem(..),
	) where



import qualified Data.Set as Set
import Library.TypeVar
import Library.Term



data Theorem = Theorem { thmHyp :: Set.Set Term
                       , thmCon :: Term } deriving (Eq, Ord)



instance Show Theorem where
    show a   =   (show . Set.toList . thmHyp $ a) ++ " |- " ++ (show . thmCon $ a)
