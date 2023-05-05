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
- Run `make test-semant` to run all tests inside of the tests/semant folder
- Run `make test-e2e` to run all tests inside of the tests/e2e folder
- Run `make clean` to remove artifacts of all of these



## Testing

### How-to

You can invoke the test script directly using `python3 tests` to pass additional command line
arguments. Note that in this case, it is your responsibility to run `make` first to ensure the
executable is up to date.

- `python3 tests --filter e2e/real-programs/factorial e2e/real-programs/string-of-int ...` runs only
  the specified tests
  - Use wildcards like `python3 tests --filter "ast/*" "e2e/real-programs/*" ...` to run all the
    tests inside a folder (including nested subfolders)
- `python3 tests --record-ground-truths` writes new `.gt` ground truths for all tests
  - `python3 tests --record-ground-truths test1 pattern2 ...` writes new `.gt` ground truths for the
    specified tests (following the filter syntax above).


### Methodology

Tests are written as `.unt` files nested inside the [`tests`](/tests) folder. Within the `/tests`
folder, there are subfolders for different “levels” of tests; each level tests a progressively
further stage of compiler execution.
- [`tests/ast`](/tests/ast) - tests that check the pretty-printed abstract syntax trees of several
  Untangled programs against ground truths
- [`test/sast`](/tests/sast) - tests that check the pretty-printed *semantically-checked* ASTs of
  several Untangled programs against ground truths (these are ASTs with additional semantic
  information added in)
- [`test/e2e`](/tests/e2e) - end-to-end (integration) tests that compile, link, and execute full
  Untangled programs if there are no semantic errors, capturing what they write to `stdout` and comparing against ground truths.

A given “level” of tests may optionally also be subdivided further into “test groups,” which have
no special function other than to organize test files and to make it easier to run only a subset of
tests at a time.

For each set of tests, comparing the output against a gold standard allows us to ensure that the
compiler behaves correctly. For the end-to-end tests, we intentionally write programs such that the
printed output is sufficient to assess the correctness of a given test run.


### Integration tests

The following integration tests are currently included:

#### Basics
1. `tests/e2e/basics/hello-world.unt` - a simple program which prints the string "Hello, World!"
    from the main thread
2. `tests/e2e/basics/reassign.unt` - a simple program that reassigns the value of a variable and
    prints the new value of the variable
3. `tests/e2e/basics/scope.unt` - a simple program that has multiple variable declarations that
    shadow each other. It prints the variable at difernt moments in time to verify that the
    appropriate variable shadows.
4. `tests/e2e/basics/variable-declaration.unt` - a simple program that declares and initializes a
    variable and then prints it

#### Binop
1.  `tests/e2e/binop/string-concat-var.unt` - a simple program that declares and initializes
     multiple strings, concatenates them and  prints the result.
2.  `tests/e2e/binop/concat.unt` - a simple program that concatenates to string literals and prints
     them.
3.  `tests/e2e/binop/string_equality.unt` - a simple program that compares strings of multiple
     different types against each other and prints specific output according to the result.

#### Builtin
1. `tests/e2e/builtin/string_of.unt` - a simple program that converts variables of multiple types
   (bool, float, int) to strings and prints the result

#### Control-Flow
1. `tests/e2e/control-flow/functions.unt` - a simple program that calls a function in different
    ways and and generates output both inside the function and based on the return value of the
    function.
2. `tests/e2e/control-flow/if.unt` - a simple program that has if statements with boolean literals
    inside of them. According to the predicate passed to the if statements, different values are
    printed out.
3. `tests/e2e/control-flow/while.unt` - a simple program that concatenates 'X' to an empty string
    in a loop. The final string is printed in order to test the accuracy of the number of
    iterations.

#### Failing (These are the semantically failing tests)
##### Declaration
1. `tests/e2e/failing/declaration/parent.unt` - a simple program that tries to declare "parent" which is a protected keyword
2. `tests/e2e/failing/declaration/scope.unt` - a simple program that tries to redeclare a variable in the same scope
3. `tests/e2e/failing/declaration/self.unt` - a simple program that tries to declare "self" which is a protected keyword

##### Receive
1. `tests/e2e/failing/receive/non-exhaustive.unt` - a simple program that does not pattern-match
    exhaustively on the receive statement
2. `tests/e2e/failing/receive/pattern-out-of-scope.unt` - a simple program that tries to access a
    variable declared in a specific receive case is not available elsewhere
3. `tests/e2e/failing/receive/redclaration.unt` - checks that a semantic error is raised if the
    same identifier is used in a pattern

#### Real Programs

1. `tests/e2e/real-programs/string-of-int.unt` - an actual program that takes the integer 213 and
    prints the string equivalent of said integer
2. `tests/e2e/real-programs/factorial.unt` - a program to compute the 3! and prints 6

#### Threading

1. `tests/e2e/threading/basic-spawn.unt` - spawns a thread, which prints the string "HELLO"
2. `tests/e2e/threading/send-and-receive.unt` - spawns a thread and send to child thread literals
    primitives `string`, `int`, `boolean`, `float`, and `tuple`. The child thread that it receives
    the message from the message queue by printing out the received value in order.
3. `tests/e2e/threading/send-child-int.unt` - spawns a thread and send the child an integer. The
    child receives the integer and prints it
3. `tests/e2e/threading/send-child-float.unt` - spawns a thread and send the child a float. The
    child receives the integer and prints it
4. `tests/e2e/threading/send-child-tuple.unt` - spawns a thread and send the child a tuple. The
    child receives the tuple and prints it
5. `tests/e2e/threading/send-parent.unt` - spawns a child thread which sends a
  message to its parent. The parent receives the message and prints it


## Documentation

The MDX file at `docs/src/main.mdx` (MDX is Markdown with support for React components) is the
source file for the Language Reference Manual. You can edit it like a normal markdown file. You can
also define additional react components in `docs/src/components`, and then import/use them in the
MDX file.

While writing, you can run `npm run dev` from inside the `docs` folder to start a local server that
will render a preview of the LRM, and auto-refresh when you make changes.

Running `make docs` from the project root will automatically generate a PDF of the LRM.
