
OUTPUTDIR=build
COMPILER_ARGS=-outputdir $(OUTPUTDIR)

build: day07p2
test: testp2
run: runp2

day07p1: day07p1.hs
	ghc -o $(OUTPUTDIR)/day07p1 $(COMPILER_ARGS) day07p1.hs

day07p2: day07p2.hs
	ghc -o $(OUTPUTDIR)/day07p2 $(COMPILER_ARGS) day07p2.hs

testp1: day07p1
	cat test_part1.txt | $(OUTPUTDIR)/day07p1 

runp1: day07p1
	cat input.txt | $(OUTPUTDIR)/day07p1

testp2: day07p2
	cat test_part2.txt | $(OUTPUTDIR)/day07p2 

runp2: day07p2
	cat input.txt | $(OUTPUTDIR)/day07p2

