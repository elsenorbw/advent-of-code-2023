
OUTPUTDIR=build
COMPILER_ARGS=-outputdir $(OUTPUTDIR)

build: day03p2
test: testp2
run: runp2

day03p1: day03p1.hs
	ghc -o $(OUTPUTDIR)/day03p1 $(COMPILER_ARGS) day03p1.hs

day03p2: day03p2.hs
	ghc -o $(OUTPUTDIR)/day03p2 $(COMPILER_ARGS) day03p2.hs

testp1: day03p1
	cat test_part1.txt | $(OUTPUTDIR)/day03p1 

runp1: day03p1
	cat input.txt | $(OUTPUTDIR)/day03p1

testp2: day03p2
	cat test_part2.txt | $(OUTPUTDIR)/day03p2 

runp2: day03p2
	cat input.txt | $(OUTPUTDIR)/day03p2

