module ProofGraph (
	Node(..),
	Graph(..),

	empty,
	insert,
	delete,
	union
	) where

import Data.Set( Set )
import qualified Data.Set as Set
import qualified Data.List as List



data Node a = Node { contents :: a
                   , successors :: Set (Node a) } deriving (Eq, Ord)

data Graph a = Graph { nodes :: Set (Node a) }



instance (Show a, Eq a) => Show (Node a) where
	show x   =   let header = show . contents $ x
	                 footer = if ((successors x) == Set.empty)
	                 	      then "\n"
	                 	      else " -> " ++ List.intercalate ", " (map (show . contents) (Set.toList . successors $ x)) ++ "\n"
	             in header ++ footer

instance (Show a, Eq a) => Show (Graph a) where
	show x   =   "Graph: \n" ++ List.foldl' (++) [] (map show (Set.toList . nodes $ x)) ++ "\n"


empty :: Graph a
empty = Graph Set.empty


insert :: Ord a => Node a -> Graph a -> Graph a
insert node graph = Graph (Set.insert node (nodes graph))


delete :: Ord a => Node a -> Graph a -> Graph a
delete node graph = Graph (Set.delete node (nodes graph))


union :: Ord a => Set (Node a) -> Graph a -> Graph a
union nodeSet graph = Graph (Set.union nodeSet (nodes graph))
