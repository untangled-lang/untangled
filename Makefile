all: untangled.native

untangled.native: untangled.ml ast.ml parser.mly scanner.mll
	ocamlbuild -r untangled.native

.PHONY: clean untangled.native

clean:
	find tests | grep ".output$$" | xargs rm
