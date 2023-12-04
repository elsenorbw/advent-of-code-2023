
OUTPUTDIR=build
COMPILER_ARGS=-outputdir $(OUTPUTDIR)

build: day04p2
test: testp2
run: runp2

day04p1: day04p1.hs
	ghc -o $(OUTPUTDIR)/day04p1 $(COMPILER_ARGS) day04p1.hs

day04p2: day04p2.hs
	ghc -o $(OUTPUTDIR)/day04p2 $(COMPILER_ARGS) day04p2.hs

testp1: day04p1
	cat test_part1.txt | $(OUTPUTDIR)/day04p1 

runp1: day04p1
	cat input.txt | $(OUTPUTDIR)/day04p1

testp2: day04p2
	cat test_part2.txt | $(OUTPUTDIR)/day04p2 

runp2: day04p2
	cat input.txt | $(OUTPUTDIR)/day04p2

