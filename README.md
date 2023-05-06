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
  Untangled programs if there are no semantic errors, capturing what they write to `stdout` and
  comparing against ground truths.

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
    appropriate variable shadows
4. `tests/e2e/basics/variable-declaration.unt` - a simple program that declares and initializes a
    variable and then prints it
5. `tests/e2e/basics/exit.unt` - tests that `exit()` terminates the program with status is 0
6. `tests/e2e/basics/failure-nonzero-exit.unt` - tests that `exit()` terminates the program with
  status 1

#### Binop

1.  `tests/e2e/binop/string-concat-var.unt` - a simple program that declares and initializes
     multiple strings, concatenates them and  prints the result
2.  `tests/e2e/binop/concat.unt` - a simple program that concatenates to string literals and prints
     them
3.  `tests/e2e/binop/string_equality.unt` - a simple program that compares strings of multiple
     different types against each other and prints specific output according to the result
4. `tests/e2e/binop/bool-binop.unt` - tests binary operations on boolean values
5. `tests/e2e/binop/failure-int-pow.unt` - tests that power operation fails when the base is 0 and
    the exponent is negative
6. `tests/e2e/binop/failure-float-pow.unt` - tests the power operation on float types
7. `tests/e2e/binop/int-pow.unt` - tests the power operation on integer types
8. `tests/e2e/binop/address/array-comparison` - tests equality for array types
9. `tests/e2e/binop/address/semaphore-comparison` - tests equality for array types
10. `tests/e2e/binop/address/thread-comparison` - tests equality for thread types
11. `tests/e2e/binop/address/tuple-comparison` - tests equality for tuple types


#### Builtin

1. `tests/e2e/builtin/bool_of_string.unt` - tests boolean to string conversion
2. `tests/e2e/builtin/failure-bool_of_string.unt` - tests that the program raises an assertion if
   the argument is not a boolean value
3. `tests/e2e/builtin/failure-float-of-string.unt` - tests that the program raises an assertion if
    the argument can't be converted from a float to a string
4. `tests/e2e/builtin/failure-int-of-string.unt` - tests that the program raises an assertion if
    the argument can't be converted from an integer to a string
5. `tests/e2e/builtin/float_to_int.unt` - tests converting a float type to an integer type
6. `tests/e2e/builtin/int_to_float.unt` - tests converting an integer type to a float type
7. `tests/e2e/builtin/make_semaphore.unt` - tests the builtin function `make_semaphore` successfully
    initializes a semaphore
8. `number_of_string` - tests builtin functions to convert integer and float types to string
10. `tests/e2e/builtin/string_of.unt` - a simple program that converts variables of multiple types
    (bool, float, int) to strings and prints the result

#### Control-Flow

1. `tests/e2e/control-flow/for-loop/unt` - tests that for loops execute properly by printing in
    the for loop's body
2. `tests/e2e/control-flow/functions.unt` - a simple program that calls a function in different
    ways and and generates output both inside the function and based on the return value of the
    function.
3. `tests/e2e/control-flow/if.unt` - a simple program that has if statements with boolean literals
    inside of them. According to the predicate passed to the if statements, different values are
    printed out.
4. `tests/e2e/control-flow/while.unt` - a simple program that concatenates 'X' to an empty string
    in a loop. The final string is printed in order to test the accuracy of the number of
    iterations.

#### Failing (These are the semantically failing tests)

##### Declaration
1. `tests/e2e/failing/declaration/failure-arent.unt` - a simple program that tries to declare
    "parent" which is a protected keyword
2. `tests/e2e/failing/declaration/failure-self.unt` - a simple program that tries to declare “self”
    which is a protected keyword

##### Receive

1. `tests/e2e/failing/receive/failure-non-exhaustive.unt` - a simple program that does not
    pattern-match exhaustively on the receive statement
2. `tests/e2e/failing/receive/failure-pattern-out-of-scope.unt` - a simple program that tries to
    access a variable declared in a specific receive case is not available elsewhere
3. `tests/e2e/failing/receive/failure-redclaration.unt` - checks that a semantic error is raised if
    the
    same identifier is used in a pattern

#### Functions

1. `tests/e2e/function/array-arg.unt` - tests that functions can receive array as arguments and
    ensure that array arguments are passed by reference
2. `tests/e2e/function/clash-name.unt` - tests that ensure functions can share the same
    identifier with threads
3. `tests/e2e/function/factorial.unt` - a factorial implementation to test recursion
4. `tests/e2e/function/fib.unt` - a fibonacci calculator to test recursion
5. `tests/e2e/function/return-non-primitives.unt` - tests that arrays and tuples are returned as
    references
6. `tests/e2ed/send-thread.unt` - sends a thread handler as a function argument and receive a
    message from the function
7. `tests/e2e/tuple-arg.unt` - tests that ensure functions can receive tuple as arguments and ensure
    that tuples are immutable

#### Non Primitives

1. `tests/e2e/non-primitives/array-extract.unt` - tests index and assignment operations
2. `tests/e2e/non-primitives/array-literal.unt` - tests array literal are correctly constructed
    by printing its content
3. `tests/e2e/non-primitives/failure-array-oob.unt` - tests that an out of bound array indexing
    raises an exception
4. `tests/e2e/non-primitives/sem-op.unt` - tests that semaphores can properly incremented and
    decremented without deadlocking the program
5. `tests/e2e/non-primitives/tuple-unpack.unt` - tests that a tuple can be unpacked


#### “Real programs”

1. `tests/e2e/real-programs/lame-merge-sort.unt` - a merge sort implementation using a recursive
    function
2. `tests/e2e/real-programs/lrm-program.unt` - tests the program proposed in the LRM that spawns 2
    threads, each of which computes the sum concurrently
3. `tests/e2e/real-programs/merge-sort.unt` - a merge sort implementation using `thread_def`
    instances. The implementation is similar to a recursive function, except that the computations
    are done using `thread_def` instances
4. `tests/e2e/real-programs/proposal-primes.unt` - tests the Mersenne prime checker from the LRM
    using `thread_def` instances
4. `tests/e2e/real-programs/string-of-int.unt` - implements a function to “stringify” an integer
   value in pure Untangled (i.e. without using the built-in `string_of_int` function)

#### Threading

1. `tests/e2e/threading/basic-spawn.unt` - spawns a thread, which prints the string "HELLO"
2. `tests/e2e/threading/send-and-receive.unt` - spawns a thread and send to child thread literals
   primitives `string`, `int`, `boolean`, `float`, and `tuple`. The child thread that it receives
   the message from the message queue by printing out the received value in order.
3. `tests/e2e/threading/send-child-float.unt` - spawns a thread and send the child a float. The
   child receives the integer and prints it
4. `tests/e2e/threading/send-child-tuple.unt` - spawns a thread and send the child a tuple. The
   child receives the tuple and prints it
5. `tests/e2e/threading/send-parent.unt` - spawns a child thread which sends a
   message to its parent. The parent receives the message and prints it
6. `tests/e2e/threading/array-variable-send-receive.unt` - Spawns a thread and sends 3 different
   representations of arrays. Checks to see that all three match the correct receive case
7. `tests/e2e/threading/end-thread.unt` - Checks whether the end function in a thread will only end
   its corresponding thread's execution.
8. j`tests/e2e/threading/exit-from-thread.unt` - Checks that an exit function will end the execution
   of the entire program.
9. `tests/e2e/threading/mutex-test.unt` - Checks that the semaphore actually locks by changing the
   order of a print sequence.
10. `tests/e2e/threading/mutexed-array.unt` - Checks that asemaphore can be used toeliminate data
   races. (matched with `tests/e2e/threading/unmutexed-array.unt`)
11. `tests/e2e/threading/nested-receive.unt` - Checks whether it is possible to nest receive
   statements.
12. `tests/e2e/threading/self-send-self.unt` -Checs that a thread can send a message to itself.
13. `tests/e2e/threading/send-array-tuple-second.unt` - Checks pattern matching on a tuple that
   contains an array
14. `tests/e2e/threading/send-child-array.unt` - Checks that a child thread can safely send an array
   containing value to its parent
15. `tests/e2e/threading/send-child-int.unt` - spawns a thread and send the child an integer. The
   child receives the integer and prints it
16. `tests/e2e/threading/send-child-string.unt` - Checks that sending a string does work
17. `tests/e2e/threading/send-other.unt` - Checks that sending threads works properly
18. `tests/e2e/threading/send-self.unt` - Checks that sending the self thread works as expected
19. `tests/e2e/threading/send-semaphore.unt` - Checks that sending a semaphore works as expected and
   sends a reference that can be used frommultiple places
20. `tests/e2e/threading/tuple-variable-send-receive.unt` - Checks that all 3 different ways of
   sending tuples works with pattern matching
21. `tests/e2e/threading/undefine-values.unt` - Checks that undefined values do not break our
   pattern matching algorithm
22. `tests/e2e/threading/unmutexed-array.unt` - Replicates the data race conditions (corresponds to
    the `tests/e2e/threading/mutexed-array.unt`)
23. `tests/e2e/threading/wildcard-pattern.unt` - Tests that the wilcard patterns within tuples work.

#### Unop
1. `tests/e2e/unop/unop.unt` - Performs unary operations and prints their results to ensure they
    match the expected result


## Documentation

The MDX file at `docs/src/main.mdx` (MDX is Markdown with support for React components) is the
source file for the Language Reference Manual. You can edit it like a normal markdown file. You can
also define additional react components in `docs/src/components`, and then import/use them in the
MDX file.

While writing, you can run `npm run dev` from inside the `docs` folder to start a local server that
will render a preview of the LRM, and auto-refresh when you make changes.

Running `make docs` from the project root will automatically generate a PDF of the LRM.
