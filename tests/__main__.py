#!/usr/bin/python3

# TODO: nicer diff output

import os
from os import path
import subprocess
import glob
from argparse import ArgumentParser

PWD = os.getcwd()
BINARY = "untangled.exe"
TESTS_DIR = f"{PWD}/tests"
num_succeeded = 0
num_failed = 0


class Format:
    """Defines common ANSI escape codes for formatted terminal output"""
    green = "\033[32m"
    red = "\033[91m"
    yellow = "\033[93m"
    dim = "\033[90m"
    reset = "\033[0m"
    reset_color = "\033[39m"
    bold = "\033[1m"
    cursor_up = "\033[1A"


def report_status(diff_exit_code, test_display, test_exit_code, expect_fail):
    """Record the status of a test given its exit code"""
    global num_succeeded, num_failed

    if test_exit_code != 0 and not expect_fail:
        print(
            f"{Format.cursor_up}" if diff_exit_code else ""
            f"{Format.red}{Format.bold}\u2717{Format.reset} "
            f"Test {test_display} "
            f"{Format.red}failed with status {test_exit_code}{Format.reset}"
        )
        num_failed += 1
    elif test_exit_code == 0 and expect_fail:
        print(
            f"{Format.cursor_up}"
            f"{Format.red}{Format.bold}\u2717{Format.reset} "
            f"Test {test_display} "
            f"{Format.red}succeeded, but was expected to fail{Format.reset}"
        )
        num_failed += 1
    elif diff_exit_code != 0:
        print(
            f"{Format.red}{Format.bold}\u2717{Format.reset} "
            f"Test {test_display} "
            f"{Format.red}did not match ground-truth output{Format.reset}"
            "\n"
        )
        num_failed += 1
    else:
        print(
            f"{Format.cursor_up}"
            f"{Format.green}{Format.bold}\u2713{Format.reset} "
            f"Test {test_display} "
            f"{Format.green}passed{Format.reset}         "
        )
        num_succeeded += 1


# PARSE ARGS ==================================================================


class CompilerStep:
    def __init__(self, name, compiler_arg):
        self.name = name
        self.compiler_arg = compiler_arg


# Different “phases”/”steps” of the compiler pipeline
# We have a separate folder of tests for each of these steps
# For each step, the compiler has a different flag for output that can
# interrogate that specific step.
COMPILER_STEPS = {
    "ast": CompilerStep("abstract syntax tree", "--print-ast"),
    "semant": CompilerStep("semantic", "--print-sast"),
    "e2e": CompilerStep("end-to-end", None),
}

parser = ArgumentParser()
parser.add_argument("-f", "--filter", nargs="*",
                    help="Limits tests to those with the specified prefix",
                    default=[])
parser.add_argument("-gt", "--record-ground-truths", nargs="*", required=False,
                    help="Regenerate ground truths from test outputs for the specified tests. Leave list blank to regenerate all.",  # noqa: E501
                    default=None)
args = parser.parse_args()

# Set flags based on the command line arguments we get
TESTS_FILTERS = args.filter
REGENERATE_GTS = args.record_ground_truths
REGENERATE_ALL_GTS = REGENERATE_GTS is not None and len(REGENERATE_GTS) == 0


def test_matches_filter(test_id, filters=TESTS_FILTERS):
    if not filters:
        return False

    for filter in filters:
        if test_id == filter:
            return True
        elif filter.endswith("/*") and test_id.startswith(filter[:-1]):
            return True
    return False


# RUN TESTS ===================================================================


num_tests = 0

# The top-level groups inside the tests directory represent tests for different
# compiler stages, matching the COMPILER_STEPS map above.
for step_name in COMPILER_STEPS:
    step_dir = path.join(TESTS_DIR, step_name)
    step = COMPILER_STEPS[step_name]

    step_display = f"{Format.bold}{step.name}{Format.reset} tests"

    # Identify tests to run
    test_files = [
        y for x in os.walk(step_dir)
        for y in glob.glob(os.path.join(x[0], '*.unt'))
    ]
    test_ids = [
        path.splitext(path.relpath(test_file, TESTS_DIR))[0]
        for test_file in test_files
    ]
    # Apply filter
    test_ids_filtered = [
        test_id
        for test_id in test_ids
        if test_matches_filter(test_id)
    ] if TESTS_FILTERS else test_ids
    num_tests += len(test_ids_filtered)

    # “announce” this new compiler step, if we are going to run any tests here
    if (len(test_ids_filtered) > 0):
        step_display = f"{Format.bold}{step.name}{Format.reset} tests"
        term_width = min(os.get_terminal_size().columns, 100)
        text_length = 17 + len(step.name)
        num_dashes = (term_width - text_length) // 2
        line = "—" * num_dashes
        print(f"\n{line} running {step_display} {line}\n")

    # Run the tests for this step
    for test in test_ids_filtered:
        input_path = path.join(TESTS_DIR, f"{test}.unt")
        output_path = path.join(TESTS_DIR, f"{test}.output")
        ground_truth_path = path.join(TESTS_DIR, f"{test}.gt")
        expected_failure = path.basename(test).startswith("failure-") \
            or "failure" in test.split(os.sep)
        # Terminal-formatted test name
        test_display = f"{Format.bold}{test}{Format.reset}" \
            if not expected_failure else \
            f"{Format.bold}{path.split(test)[0]}/{Format.reset}{Format.red}failure-{Format.reset}{Format.bold}{path.basename(test)[8:]}{Format.reset}"  # noqa: E501
        print(f"Running test {test_display}...")

        # Steps that pass an argument to the compiler just compare the stdout
        # from the compiler to a ground truth.
        if step.compiler_arg:
            # Run the test
            exit_code = os.system(
                f"./{BINARY} {step.compiler_arg}"
                f" < {input_path}"
                f" > {output_path} 2>&1"
            )
        # The e2e step passes no argument to the compiler. It compiles a
        # program and then executes that program, comparing the final output
        # of the compiled program to a ground truth.
        else:
            exe_path = path.join(TESTS_DIR, test)
            # Compile the program
            compile_return_code = os.system(
                f"./{BINARY}"
                f" < {input_path}"
                f" -o {exe_path} > {output_path} 2>&1"
            )
            if path.exists(exe_path):
                # Run the generated program
                exit_code = os.system(f"{exe_path} >> {output_path} 2>&1") \
                    + compile_return_code
            else:
                exit_code = compile_return_code

        # Record a new ground truth for this test if that was requested
        if REGENERATE_ALL_GTS or test_matches_filter(test, REGENERATE_GTS):
            subprocess.run(
                ["cp", output_path, ground_truth_path]
            ).check_returncode()
            print(
                f"{Format.cursor_up}{Format.yellow}! Overwriting ground"
                f" truth for test {Format.bold}{test}{Format.reset}"
            )
            num_tests -= 1
            continue

        # Compare the output of the test to the ground truth
        if not path.isfile(ground_truth_path):
            print(
                f"{Format.cursor_up}{Format.yellow}! "
                "Could not find ground truth output for test "
                f"{test_display}{Format.yellow}; "
                f"skipping checks{Format.reset}"
            )
            continue
        diff_exit_code = subprocess.run(
            ["diff", ground_truth_path, output_path],
        ).returncode

        # Report the results of this test
        report_status(
            diff_exit_code,
            test_display,
            exit_code,
            expected_failure,
        )


# Report final summary of results of all tests
print()
print(
    f"{Format.red if num_failed > 0 else Format.green}{Format.bold}"
    f"{num_succeeded}/{num_tests} tests succeeded",
    end=Format.reset
)
if num_failed + num_succeeded != num_tests:
    print(
        f"{Format.yellow} "
        f"(diff skipped for {num_tests - num_succeeded - num_failed} tests)",
        end=Format.reset
    )
print("\n")

exit(1 if num_failed > 0 else 0)
