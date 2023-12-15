
OUTPUTDIR=build
COMPILER_ARGS=-outputdir $(OUTPUTDIR)

build: day13p2
test: testp2
run: runp2

day13p1: day13p1.hs
	ghc -o $(OUTPUTDIR)/day13p1 $(COMPILER_ARGS) day13p1.hs

day13p2: day13p2.hs
	ghc -o $(OUTPUTDIR)/day13p2 $(COMPILER_ARGS) day13p2.hs

testp1: day13p1
	cat test_part1.txt | $(OUTPUTDIR)/day13p1 

runp1: day13p1
	cat input.txt | $(OUTPUTDIR)/day13p1

testp2: day13p2
	cat test_part2.txt | $(OUTPUTDIR)/day13p2 

runp2: day13p2
	cat input.txt | $(OUTPUTDIR)/day13p2

