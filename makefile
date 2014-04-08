
OUTPUTDIR = bin
SOURCEDIR = src



all: semantic syntactic proofgraph writeproof delete concat listthm meaningsubst unittest compare

clean:
	find . -name '*.hi' -delete
	find . -name '*.o' -delete


semantic:
	cd ${SOURCEDIR}; \
	ghc --make SemanticMain.hs -o ../${OUTPUTDIR}/Semantic

syntactic:
	cd ${SOURCEDIR}; \
	ghc --make Syntactic.hs -o ../${OUTPUTDIR}/Syntactic

proofgraph:
	cd ${SOURCEDIR}; \
	ghc --make ProofGraphMain.hs -o ../${OUTPUTDIR}/ProofGraph

writeproof:
	cd ${SOURCEDIR}; \
	ghc --make WriteProofMain.hs -o ../${OUTPUTDIR}/WriteProof

delete:
	cd ${SOURCEDIR}; \
	ghc --make Delete.hs -o ../${OUTPUTDIR}/Delete

concat:
	cd ${SOURCEDIR}; \
	ghc --make Concat.hs -o ../${OUTPUTDIR}/Concat

listthm:
	cd ${SOURCEDIR}; \
	ghc --make ListThm.hs -o ../${OUTPUTDIR}/ListThm

meaningsubst:
	cd ${SOURCEDIR}; \
	ghc --make MeaningSubst.hs -o ../${OUTPUTDIR}/MeaningSubst

unittest:
	cd ${SOURCEDIR}; \
	ghc --make Test.hs -o ../${OUTPUTDIR}/UnitTest

compare:
	cd ${SOURCEDIR}; \
	ghc --make Compare.hs -o ../${OUTPUTDIR}/Compare

