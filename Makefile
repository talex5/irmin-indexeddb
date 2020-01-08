.PHONY: all clean

build:
	dune build _build/default/test/test.bc.js

clean:
	dune clean
