
all:
	dune build

run:
	dune exec aoc-2024

fmt:
	dune fmt

clean:
	dune clean

.PHONY: all run fmt clean
