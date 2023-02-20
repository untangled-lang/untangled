.PHONY: clean untangled.native

untangled.native: src/untangled.ml src/ast.ml src/parser.mly src/scanner.mll
	ocamlbuild -r src/untangled.native

clean:
	find tests | grep ".output$$" | xargs rm
