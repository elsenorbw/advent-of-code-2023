
OUTPUTDIR=build
COMPILER_ARGS=-outputdir $(OUTPUTDIR)

build: day06p2
test: testp2
run: runp2

day06p1: day06p1.hs
	ghc -o $(OUTPUTDIR)/day06p1 $(COMPILER_ARGS) day06p1.hs

day06p2: day06p2.hs
	ghc -o $(OUTPUTDIR)/day06p2 $(COMPILER_ARGS) day06p2.hs

testp1: day06p1
	cat test_part1.txt | $(OUTPUTDIR)/day06p1 

runp1: day06p1
	cat input.txt | $(OUTPUTDIR)/day06p1

testp2: day06p2
	cat test_part2.txt | $(OUTPUTDIR)/day06p2 

runp2: day06p2
	cat input.txt | $(OUTPUTDIR)/day06p2

