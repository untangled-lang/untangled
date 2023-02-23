# Untangled

Luke Taylor (luke.taylor@tufts.edu),
Nick Doan (hdoan02@tufts.edu),
Caleb Ledi (cledi01@tufts.edu),
Duru Ugurlu (duru.ugurlu@tufts.edu),
Chloe Lam (clam08@tufts.edu)


## Instructions
First, install required dependencies:
```sh
opam install dune
opam install llvm.14.0.6 # or whatever version of llvm you have installed
```
*(Note that [Dune](https://github.com/ocaml/dune) supersedes [Ocamlbuild](https://github.com/ocaml/ocamlbuild);
the Ocamlbuild README has recommended to use Dune instead for several years.)*

After installing dependencies, you will be able to:
- Run `make` to build the main `untangled.exe` executable.
- Run `make test` to run the full series of automated tests. See [Testing](#testing) for
  additional options. Requires Python 3.
- Run `make vscode-extension` to build and install the VS Code extension for Untangled syntax
  highlighting. Requires VS Code, a recent version of Node.js, and the `code` command in your PATH.
- Run `make docs` to generate a PDF of the Language Reference Manual. The generated PDF appears at
  `docs/untangled.pdf`. Requires a recent version Node.js.


## Testing
You can invoke the test script directly using `python3 tests` to pass additional command line
arguments. Note that in this case, it is your responsibility to run `make` first to ensure the
executable is up to date.
- `python3 tests --tests test1 test2 ...` runs only the specified tests
- `python3 tests --test-groups group1 group2 ...` limits tests to those in the specified groups
- `python3 tests --record-ground-truths` writes new `.gt` ground truths for all tests
- `python3 tests --record-ground-truths test1 test2 ...` writes new `.gt` ground truths for the
  specified tests


## Documentation
The MDX file at `docs/src/main.mdx` (MDX is Markdown with support for React components) is the
source file for the Language Reference Manual. You can edit it like a normal markdown file. You can
also define additional react components in `docs/src/components`, and then import/use them in the
MDX file.
