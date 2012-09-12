module Library.TermNet(
	TermNet,
	empty,
	addThm
	) where



import Library.ProofGraph
import Library.Object
import Library.Theorem



data TermNet = Empty | Leaf Object Node | Branch String [TermNet]



empty :: TermNet
empty = Empty



addThm :: TermNet -> PGraph -> Node -> (TermNet, [(Theorem, Node)])
addThm net graph node =


