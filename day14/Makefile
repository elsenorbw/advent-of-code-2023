
OUTPUTDIR=build
COMPILER_ARGS=-outputdir $(OUTPUTDIR)

build: day14p2
test: testp2
run: runp2

day14p1: day14p1.hs
	ghc -o $(OUTPUTDIR)/day14p1 $(COMPILER_ARGS) day14p1.hs

day14p2: day14p2.hs
	ghc -o $(OUTPUTDIR)/day14p2 $(COMPILER_ARGS) day14p2.hs

testp1: day14p1
	cat test_part1.txt | $(OUTPUTDIR)/day14p1 

runp1: day14p1
	cat input.txt | $(OUTPUTDIR)/day14p1

testp2: day14p2
	cat test_part2.txt | $(OUTPUTDIR)/day14p2 

runp2: day14p2
	cat input.txt | $(OUTPUTDIR)/day14p2

