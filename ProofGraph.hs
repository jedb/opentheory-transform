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



data Node a = Node { contents :: a
                   , successors :: Set (Node a) } deriving (Eq, Ord, Show)

data Graph a = Graph { nodes :: Set (Node a) } deriving (Show)



empty :: Graph a
empty = Graph Set.empty


insert :: Ord a => Node a -> Graph a -> Graph a
insert node graph = Graph (Set.insert node (nodes graph))


delete :: Ord a => Node a -> Graph a -> Graph a
delete node graph = Graph (Set.delete node (nodes graph))


union :: Ord a => Set (Node a) -> Graph a -> Graph a
union nodeSet graph = Graph (Set.union nodeSet (nodes graph))
