GHC = ghc
GHC_OPTS = -O2 -threaded -rtsopts

.PHONY: all main Main kclustering KClustering GenSamples gensamples points view

all: Main Benchmark GenSamples

clean:
	rm -rvf */*.hi */*.o */*.exe Main Benchmark GenSamples

Main main: Main.hs
	$(GHC) Main.hs

Benchmark benchmark: Benchmark.hs
	$(GHC) Benchmark.hs

GenSamples gensamples: GenSamples.hs
	$(GHC) GenSamples.hs

points: GenSamples
	./GenSamples 2 100 100 sample_points.txt

view: points
	gnuplot -e 'set terminal png; plot "points"' >points.png; eog points.png
