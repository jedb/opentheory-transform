

Semantic <input file>

Evaluates a proof trace, and shows the contents of the Dictionary, Assumptions, Stack and Theorems when done.




Syntactic <input file>

Checks a proof trace for syntax errors.




ProofGraph <input file>

Constructs a DAG dependency graph of the virtual machine commands used in a given proof trace.




WriteProof <input file> > <output file>

Reads a proof trace in, converts it to a DAG a la ProofGraph, then converts it back into a linear proof trace.




Delete <input file> [<num#1>, <num#2>, <num3>] > <output file>

Removes all theorems of specified numbers from a proof trace. Best used in conjunction with ListThm, so you can
actually know what theorems are what. Generates a simplified proof trace as output.




Concat <input #1> <input#2> <input #2> > <output file>

Joins two proof traces together, converts it to a DAG and back again, then outputs the result.




ListThm <input file>

Displays numbered theorems from a proof trace. Best used in conjunction with Delete so you know what theorems you
are deleting.


