
OUTPUTDIR=build
COMPILER_ARGS=-outputdir $(OUTPUTDIR)

build: day16p2
test: testp2
run: runp2

day16p1: day16p1.hs
	ghc -o $(OUTPUTDIR)/day16p1 $(COMPILER_ARGS) day16p1.hs

day16p2: day16p2.hs
	ghc -o $(OUTPUTDIR)/day16p2 $(COMPILER_ARGS) day16p2.hs

testp1: day16p1
	cat test_part1.txt | $(OUTPUTDIR)/day16p1 

runp1: day16p1
	cat input.txt | $(OUTPUTDIR)/day16p1

testp2: day16p2
	cat test_part2.txt | $(OUTPUTDIR)/day16p2 

runp2: day16p2
	cat input.txt | $(OUTPUTDIR)/day16p2

