all:
	ghc --make ./SemanticMain.hs -o ./Semantic
	ghc --make ./ProofGraphMain.hs -o ./ProofGraph
	ghc --make ./WriteProofMain.hs -o ./WriteProof
	ghc --make ./Delete.hs -o ./Delete
	ghc --make ./Concat.hs -o ./Concat
	ghc --make ./ListThm.hs -o ./ListThm
	ghc --make ./Syntactic.hs -o ./Syntactic
