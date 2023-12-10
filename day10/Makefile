
OUTPUTDIR=build
COMPILER_ARGS=-outputdir $(OUTPUTDIR)

build: day10p2
test: testp2 testp2b testp2c
run: runp2

day10p1: day10p1.hs
	ghc -o $(OUTPUTDIR)/day10p1 $(COMPILER_ARGS) day10p1.hs

day10p2: day10p2.hs
	ghc -o $(OUTPUTDIR)/day10p2 $(COMPILER_ARGS) day10p2.hs

day10p2-expander: day10p2-expander.hs
	ghc -o $(OUTPUTDIR)/day10p2-expander $(COMPILER_ARGS) day10p2-expander.hs

testp1: day10p1
	cat test_part1.txt | $(OUTPUTDIR)/day10p1 

testp1b: day10p1
	cat test_part1b.txt | $(OUTPUTDIR)/day10p1 


runp1: day10p1
	cat input.txt | $(OUTPUTDIR)/day10p1

testp2: day10p2-expander
	cat test_part2.txt | $(OUTPUTDIR)/day10p2-expander

testp2b: day10p2-expander
	cat test_part2b.txt | $(OUTPUTDIR)/day10p2-expander 

testp2c: day10p2-expander
	cat test_part2c.txt | $(OUTPUTDIR)/day10p2-expander 

runp2: day10p2-expander
	cat input.txt | $(OUTPUTDIR)/day10p2-expander

