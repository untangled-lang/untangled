CC := clang
LIB = src/c

.PHONY: untangled.exe test clean-untangled
.PHONY: clean-vscode vscode-extension
.PHONY: docs-dev docs-build docs-pdf clean-docs
.PHONY: default clean

default: untangled.exe
clean: clean-untangled clean-vscode clean-docs


# Untangled language

$(LIB)/%.o: src/c/%.cpp
	$(CC) $< -o $@

untangled.exe: src/untangled.ml src/ast.ml src/parser.mly src/scanner.mll src/sast.ml src/c/libraries.o
	dune build

clean-tests:
	find tests | grep "\\.output$$" | xargs rm -f
	find tests | grep "\\.ll$$" | xargs rm -f
	find tests | grep "\\.s$$" | xargs rm -f
	find tests/e2e | grep "\\.unt$$" | sed s/\\.unt$$// | xargs rm -f

clean-untangled: clean-tests
	dune clean
	rm -f untangled.exe

test: clean-untangled untangled.exe
	python3 tests

test-ast: clean-untangled untangled.exe
	python3 tests --filter "ast/*"

test-semant: clean-untangled untangled.exe
	python3 tests --filter "semant/*"

test-e2e: clean-untangled untangled.exe
	python3 tests --filter "e2e/*"


# VSCode extension

vscode-extension:
	cd vscode-extension && yes | npx vsce package && code --install-extension untangled-*.vsix

clean-vscode:
	-rm -rf vscode-extension/untangled-*.vsix


# Docs

docs/node_modules: docs/package.json
	cd docs && npm install
	# Update mtime to avoid make rebuilding unnecessarily
	touch -m docs/node_modules

docs-dev:
	cd docs && npm run dev

docs-build: docs/node_modules
	cd docs && npm run build

docs-pdf: docs/node_modules
	cd docs && npm run pdf

docs: docs-build docs-pdf

clean-docs:
	-rm -rf docs/node_modules
	-rm -rf docs/dist
	-rm -f docs/export/*.pdf
