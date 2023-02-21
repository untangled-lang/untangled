# Untangled

## Instructions
- Run `make` to build the `untangled.native` executable.
- Run `make test` to run the full series of automated tests (requires a current version of Python).
  See [Testing](#testing) for additional options.
- Run `make clean` to remove all build artifacts.
- Run `make vscode-extension` to build and install the VS Code extension for Untangled syntax
  highlighting (requires VS Code, a recent version of Node, and `code` in your PATH).


## Testing
You can invoke the test script directly using `python tests` to pass additional command line
arguments. Note that in this case, it is your responsibility to run `make` first to ensure the
executable is up to date.
- `python tests --tests test_1 test_2 ...` runs only the specified tests
- `python tests --test-groups group_1 group_2` limits tests to those in the specified groups
- `python tests --record-ground-truths` writes new `.gt` ground truths for all tests
- `python tests --record-ground-truths test_1 test_2 ...` writes new `.gt` ground truths for the
  specified tests
