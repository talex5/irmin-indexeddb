.PHONY: all clean

lib_and_test: build
	js_of_ocaml +cstruct/cstruct.js helpers.js _build/default/test/test.bc

build:
	dune build @all

clean:
	dune clean
