GHC = ghc
GHC_OPTS = -O2 -threaded -eventlog -rtsopts

.PHONY: all main Main GenSamples gensamples points view

all: Main GenSamples points test

clean:
	rm -rvf *.hi */*.hi *.o */*.o *.exe */*.exe Main GenSamples

Main main: Main.hs Makefile
	$(GHC) $(GHC_OPTS) Main.hs

GenSamples gensamples: GenSamples.hs Makefile
	$(GHC) $(GHC_OPTS) GenSamples.hs

points: GenSamples
	./GenSamples -n 100000 -o "sample_points.txt"

test: Main points
	./main -k 10 -a 1.1 -m 5 -o centers.txt sample_points.txt;						       \
	gnuplot -e 'set terminal png; plot "sample_points.txt" with points 1, "centers.txt" with points 2' >points.png
