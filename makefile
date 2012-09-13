
OUTPUTDIR = ./bin



all: semantic syntactic proofgraph writeproof delete concat listthm meaningsubst unittest

clean:
	find . -name '*.hi' -delete
	find . -name '*.o' -delete


semantic:
	ghc --make ./SemanticMain.hs -o ${OUTPUTDIR}/Semantic

syntactic:
	ghc --make ./Syntactic.hs -o ${OUTPUTDIR}/Syntactic

proofgraph:
	ghc --make ./ProofGraphMain.hs -o ${OUTPUTDIR}/ProofGraph

writeproof:
	ghc --make ./WriteProofMain.hs -o ${OUTPUTDIR}/WriteProof

delete:
	ghc --make ./Delete.hs -o ${OUTPUTDIR}/Delete

concat:
	ghc --make ./Concat.hs -o ${OUTPUTDIR}/Concat

listthm:
	ghc --make ./ListThm.hs -o ${OUTPUTDIR}/ListThm

meaningsubst:
	ghc --make ./MeaningSubst.hs -o ${OUTPUTDIR}/MeaningSubst

unittest:
	ghc --make ./Test.hs -o ${OUTPUTDIR}/UnitTest
