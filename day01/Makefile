
OUTPUTDIR=build
COMPILER_ARGS=-outputdir $(OUTPUTDIR)

build: day01p1
test: testp1
run: runp1

day01p1: day01p1.hs
	ghc -o $(OUTPUTDIR)/day01p1 $(COMPILER_ARGS) day01p1.hs

day01p2: day01p2.hs
	ghc -o $(OUTPUTDIR)/day01p2 $(COMPILER_ARGS) day01p2.hs

testp1: day01p1
	cat test_part1.txt | $(OUTPUTDIR)/day01p1 

runp1: day01p1
	cat input.txt | $(OUTPUTDIR)/day01p1

testp2: day01p2
	cat test_part2.txt | $(OUTPUTDIR)/day01p2 

runp2: day01p2
	cat input.txt | $(OUTPUTDIR)/day01p2

