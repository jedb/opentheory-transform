module TermNet(
	TermNet,
	empty,
	addThm
	) where



import ProofGraph
import Object
import Theorem



data TermNet = Empty | Leaf Object Node | Branch String [TermNet]



empty :: TermNet
empty = Empty



addThm :: TermNet -> PGraph -> Node -> (TermNet, [(Theorem, Node)])
addThm net graph node =


