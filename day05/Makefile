
OUTPUTDIR=build
COMPILER_ARGS=-outputdir $(OUTPUTDIR)

build: day05p2
test: testp2
run: runp2

day05p1: day05p1.hs
	ghc -o $(OUTPUTDIR)/day05p1 $(COMPILER_ARGS) day05p1.hs

day05p2: day05p2.hs
	ghc -o $(OUTPUTDIR)/day05p2 $(COMPILER_ARGS) day05p2.hs

testp1: day05p1
	cat test_part1.txt | $(OUTPUTDIR)/day05p1 

runp1: day05p1
	cat input.txt | $(OUTPUTDIR)/day05p1

testp2: day05p2
	cat test_part2.txt | $(OUTPUTDIR)/day05p2 

runp2: day05p2
	cat input.txt | $(OUTPUTDIR)/day05p2

