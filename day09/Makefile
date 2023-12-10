
OUTPUTDIR=build
COMPILER_ARGS=-outputdir $(OUTPUTDIR)

build: day09p2
test: testp2
run: runp2

day09p1: day09p1.hs
	ghc -o $(OUTPUTDIR)/day09p1 $(COMPILER_ARGS) day09p1.hs

day09p2: day09p2.hs
	ghc -o $(OUTPUTDIR)/day09p2 $(COMPILER_ARGS) day09p2.hs

testp1: day09p1
	cat test_part1.txt | $(OUTPUTDIR)/day09p1 

runp1: day09p1
	cat input.txt | $(OUTPUTDIR)/day09p1

testp2: day09p2
	cat test_part2.txt | $(OUTPUTDIR)/day09p2 

runp2: day09p2
	cat input.txt | $(OUTPUTDIR)/day09p2

