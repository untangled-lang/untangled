all: toplevel.native

toplevel.native: toplevel.ml ast.ml parser.mly scanner.mll
	ocamlbuild -r toplevel.native

.PHONY: clean toplevel.native

clean:
	rm tests/out/*
