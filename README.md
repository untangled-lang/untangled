# Untangled

Luke Taylor (luke.taylor@tufts.edu),
Nick Doan (hdoan02@tufts.edu),
Caleb Ledi (cledi01@tufts.edu),
Duru Uğurlu (duru.ugurlu@tufts.edu),
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
- Run `./untangled.exe < path/to/program.unt -o exe_name` to compile an Untangled program to an
  executable named `exe_name`.

You will also be able to:
- Run `make test` to run the full series of automated tests. See [Testing](#testing) for
  additional options. Requires Python 3.
- Run `make vscode-extension` to build and install the VS Code extension for Untangled syntax
  highlighting. Requires VS Code, a recent version of Node.js, and the `code` command in your PATH.
- Run `make docs` to generate a PDF of the Language Reference Manual. The generated PDF appears at
  `docs/untangled.pdf`. Requires a recent version Node.js.
- Run `make test-ast` to run all ast tests inside of the tests/ast folder
- Run `make test-semants` to run all tests inside of the tests/semantics folder
- Run `make test-e2e` to run all tests inside of the tests/e2e folder
- Run `make clean` to remove artifacts of all of these



## Testing

### How-to

You can invoke the test script directly using `python3 tests` to pass additional command line
arguments. Note that in this case, it is your responsibility to run `make` first to ensure the
executable is up to date.

- `python3 tests --tests test1 test2 ...` runs only the specified tests
- `python3 tests --test-groups group1 group2 ...` limits tests to those in the specified groups
- `python3 tests --record-ground-truths` writes new `.gt` ground truths for all tests
- `python3 tests --record-ground-truths test1 test2 ...` writes new `.gt` ground truths for the
  specified tests
- `python3 tests --steps {ast|sast|e2e}` specifies which step of the compiler to test


### Methodology

Tests are written as `.unt` files nested inside the [`tests`](/tests) folder. Within the `/tests`
folder, there are subfolders for different “levels” of tests; each level tests a progressively
further stage of compiler execution.
- [`tests/ast`](/tests/ast) - tests that check the pretty-printed abstract syntax trees of several
  Untangled programs against ground truths
- [`test/sast`](/tests/sast) - tests that check the pretty-printed *semantically-checked* ASTs of
  several Untangled programs against ground truths (these are ASTs with additional semantic
  information added in)
- [`test/e2e`](/tests/e2e) - end-to-end tests that compile, link, and execute full Untangled
  programs, capturing what they write to `stdout` and comparing against ground truths.

A given “level” of tests may optionally also be subdivided further into “test groups,” which have
no special function other than to organize test files and to make it easier to run only a subset of
tests at a time.

For each set of tests, comparing the output against a gold standard allows us to ensure that the
compiler behaves correctly. For the end-to-end tests, we intentionally write programs such that the
printed output is sufficient to assess the correctness of a given test run.


### End-to-end tests

The following end-to-end tests are currently included:

1. `tests/e2e/hello-world.unt` - a simple program which prints the string "Hello, World!" from the
   main thread

## Documentation
The MDX file at `docs/src/main.mdx` (MDX is Markdown with support for React components) is the
source file for the Language Reference Manual. You can edit it like a normal markdown file. You can
also define additional react components in `docs/src/components`, and then import/use them in the
MDX file.

While writing, you can run `npm run dev` from inside the `docs` folder to start a local server that
will render a preview of the LRM, and auto-refresh when you make changes.

Running `make docs` from the project root will automatically generate a PDF of the LRM.
