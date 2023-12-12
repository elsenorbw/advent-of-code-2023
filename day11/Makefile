
OUTPUTDIR=build
COMPILER_ARGS=-outputdir $(OUTPUTDIR)

build: day11p2
test: testp2
run: runp2

day11p1: day11p1.hs
	ghc -o $(OUTPUTDIR)/day11p1 $(COMPILER_ARGS) day11p1.hs

day11p2: day11p2.hs
	ghc -o $(OUTPUTDIR)/day11p2 $(COMPILER_ARGS) day11p2.hs

testp1: day11p1
	cat test_part1.txt | $(OUTPUTDIR)/day11p1 

runp1: day11p1
	cat input.txt | $(OUTPUTDIR)/day11p1

testp2: day11p2
	cat test_part2.txt | $(OUTPUTDIR)/day11p2 

runp2: day11p2
	cat input.txt | $(OUTPUTDIR)/day11p2

