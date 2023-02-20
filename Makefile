.PHONY: clean untangled.native test

untangled.native: src/untangled.ml src/ast.ml src/parser.mly src/scanner.mll
	ocamlbuild -r src/untangled.native

clean:
	find tests | grep ".output$$" | xargs rm
	rm -rf _build
	rm untangled.native

test: clean untangled.native
	python tests
