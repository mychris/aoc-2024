
all build:
	dune build --release

run exec:
	dune exec --release aoc-2024

fmt:
	dune fmt

clean:
	dune clean

.PHONY: all build run exec fmt clean
