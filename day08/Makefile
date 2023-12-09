
OUTPUTDIR=build
COMPILER_ARGS=-outputdir $(OUTPUTDIR)

build: day08p2-lcm
test: testp2-lcm
run: runp2-lcm

day08p1: day08p1.hs
	ghc -o $(OUTPUTDIR)/day08p1 $(COMPILER_ARGS) day08p1.hs

day08p2: day08p2.hs
	ghc -o $(OUTPUTDIR)/day08p2 $(COMPILER_ARGS) day08p2.hs

day08p2-lcm: day08p2-lcm.hs
	ghc -o $(OUTPUTDIR)/day08p2-lcm $(COMPILER_ARGS) day08p2-lcm.hs

testp1: day08p1
	echo "Test part1"
	cat test_part1.txt | $(OUTPUTDIR)/day08p1 
	echo "Test part1b"
	cat test_part1b.txt | $(OUTPUTDIR)/day08p1 

runp1: day08p1
	cat input.txt | $(OUTPUTDIR)/day08p1

testp2: day08p2
	cat test_part2.txt | $(OUTPUTDIR)/day08p2 

runp2: day08p2
	cat input.txt | $(OUTPUTDIR)/day08p2

testp2-lcm: day08p2-lcm
	cat test_part2.txt | $(OUTPUTDIR)/day08p2-lcm 

runp2-lcm: day08p2-lcm
	cat input.txt | $(OUTPUTDIR)/day08p2-lcm


