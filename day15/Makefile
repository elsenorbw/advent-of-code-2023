
OUTPUTDIR=build
COMPILER_ARGS=-outputdir $(OUTPUTDIR)

build: day15p2
test: testp2
run: runp2

day15p1: day15p1.hs
	ghc -o $(OUTPUTDIR)/day15p1 $(COMPILER_ARGS) day15p1.hs

day15p2: day15p2.hs
	ghc -o $(OUTPUTDIR)/day15p2 $(COMPILER_ARGS) day15p2.hs

testp1: day15p1
	cat test_part1.txt | $(OUTPUTDIR)/day15p1 

runp1: day15p1
	cat input.txt | $(OUTPUTDIR)/day15p1

testp2: day15p2
	cat test_part2.txt | $(OUTPUTDIR)/day15p2 

runp2: day15p2
	cat input.txt | $(OUTPUTDIR)/day15p2

