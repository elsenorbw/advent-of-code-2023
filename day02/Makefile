
OUTPUTDIR=build
COMPILER_ARGS=-outputdir $(OUTPUTDIR)

build: day02p2
test: testp2
run: runp2

day02p1: day02p1.hs
	ghc -o $(OUTPUTDIR)/day02p1 $(COMPILER_ARGS) day02p1.hs

day02p2: day02p2.hs
	ghc -o $(OUTPUTDIR)/day02p2 $(COMPILER_ARGS) day02p2.hs

testp1: day02p1
	cat test_part1.txt | $(OUTPUTDIR)/day02p1 

runp1: day02p1
	cat input.txt | $(OUTPUTDIR)/day02p1

testp2: day02p2
	cat test_part2.txt | $(OUTPUTDIR)/day02p2 

runp2: day02p2
	cat input.txt | $(OUTPUTDIR)/day02p2

