.PHONY: untangled.exe test clean-untangled
.PHONY: clean-vscode
.PHONY: docs-dev docs-build docs-pdf clean-docs
.PHONY: default clean

default: untangled.exe
clean: clean-untangled clean-vscode clean-docs


# Untangled language

untangled.exe: src/untangled.ml src/ast.ml src/parser.mly src/scanner.mll src/sast.ml
	dune build

clean-untangled:
	find tests | grep ".output$$" | xargs rm
	dune clean

test: clean-untangled untangled.exe
	python3 tests

test-ast: clean-untangled untangled.exe
	python3 tests --step ast

test-semants: clean-untangled untangled.exe
	python3 tests --step sast

test-llvm: clean-untangled untangled.exe
	python3 tests --step llvm


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
	-rm docs/untangled.pdf
