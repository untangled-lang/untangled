.PHONY: clean untangled.exe test vscode-extension

untangled.exe: src/untangled.ml src/ast.ml src/parser.mly src/scanner.mll
	dune build

clean:
	find tests | grep ".output$$" | xargs rm
	dune clean
	-rm -rf vscode-extension/untangled-*.vsix

test: clean untangled.exe
	python3 tests

vscode-extension:
	cd vscode-extension && yes | npx vsce package && code --install-extension untangled-*.vsix
