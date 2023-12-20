
OUTPUTDIR=build
COMPILER_ARGS=-outputdir $(OUTPUTDIR)

build: day19p2
test: testp2
run: runp2

day19p1: day19p1.hs
	ghc -o $(OUTPUTDIR)/day19p1 $(COMPILER_ARGS) day19p1.hs

day19p2: day19p2.hs
	ghc -o $(OUTPUTDIR)/day19p2 $(COMPILER_ARGS) day19p2.hs

testp1: day19p1
	cat test_part1.txt | $(OUTPUTDIR)/day19p1 

runp1: day19p1
	cat input.txt | $(OUTPUTDIR)/day19p1

testp2: day19p2
	cat test_part2.txt | $(OUTPUTDIR)/day19p2 

runp2: day19p2
	cat input.txt | $(OUTPUTDIR)/day19p2

