
OUTPUTDIR=build
COMPILER_ARGS=-outputdir $(OUTPUTDIR)

build: dayXXp1
test: testp1
run: runp1

dayXXp1: dayXXp1.hs
	ghc -o $(OUTPUTDIR)/dayXXp1 $(COMPILER_ARGS) dayXXp1.hs

dayXXp2: dayXXp2.hs
	ghc -o $(OUTPUTDIR)/dayXXp2 $(COMPILER_ARGS) dayXXp2.hs

testp1: dayXXp1
	cat test_part1.txt | $(OUTPUTDIR)/dayXXp1 

runp1: dayXXp1
	cat input.txt | $(OUTPUTDIR)/dayXXp1

testp2: dayXXp2
	cat test_part2.txt | $(OUTPUTDIR)/dayXXp2 

runp2: dayXXp2
	cat input.txt | $(OUTPUTDIR)/dayXXp2

