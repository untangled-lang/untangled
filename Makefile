.PHONY: untangled.exe test clean-untangled
.PHONY: clean-vscode
.PHONY: docs-pdf docs-build clean-docs
.PHONY: default clean

default: untangled.exe
clean: clean-untangled clean-vscode clean-docs


# Untangled language

untangled.exe: src/untangled.ml src/ast.ml src/parser.mly src/scanner.mll
	dune build

clean-untangled:
	find tests | grep ".output$$" | xargs rm
	dune clean

test: clean-untangled untangled.exe
	python3 tests


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

docs-pdf: docs/node_modules
	cd docs && npm run pdf

docs-build: docs/node_modules
	cd docs && npm run build

docs: docs-build docs-pdf

clean-docs:
	-rm -rf docs/node_modules
	-rm -rf docs/dist
	-rm docs/untangled.pdf
