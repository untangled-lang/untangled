#!/usr/bin/python3

# TODO: nicer diff output
# TODO: options for end-to-end testing by re-parsing pretty printed AST output
#       and ensuring it’s the same as the previous output

import os
import subprocess
import glob
import sys
from argparse import ArgumentParser


PWD = os.getcwd()
BINARY = "untangled.native"
TESTS_DIR = f"{PWD}/tests"
num_succeeded = 0
num_failed = 0


class Format:
    """Defines common ANSI escape codes for formatted terminal output"""
    green = "\033[32m"
    red = "\033[91m"
    yellow = "\033[93m"
    dim = "\033[2m"
    reset = "\033[0m"
    bold = "\033[1m"
    cursor_up = "\033[1A"


def report_status(exit_code, test_name):
    """Record the status of a test given its exit code"""
    global num_succeeded, num_failed
    if exit_code == 0:
        print(
            f"{Format.cursor_up}"
            f"{Format.green}{Format.bold}\u2713{Format.reset} "
            f"Test {Format.bold}{test_name}{Format.reset} "
            f"{Format.green}succeeded{Format.reset}         "
        )
        num_succeeded += 1
    else:
        print(
            f"{Format.red}{Format.bold}\u2717{Format.reset} "
            f"Test {Format.bold}{test_name}{Format.reset} "
            f"{Format.red}failed with status {exit_code}{Format.reset}"
            "\n"
        )
        num_failed += 1


# PARSE ARGS ==================================================================


parser = ArgumentParser()
group = parser.add_mutually_exclusive_group(required=False)
group.add_argument("-t", "--tests", nargs="*", help="Run specified tests")
group.add_argument("-gt", "--ground_truths", nargs="*", required=False,
                   help="Regenerate specified ground truths from test outputs. Leave list blank to regenerate all")  # noqa: E501
group.add_argument("-tg", "--test_groups", nargs="*", required=False,
                   help="Runs test groups supplied as arguments, specify none to run all")  # noqa: E501
args = parser.parse_args()


REGENERATE_GTS = set(args.ground_truths) if args.ground_truths is not None else None  # noqa: E501
TEST_GROUPS_FILTER = set(args.test_groups) if args.test_groups is not None else set()  # noqa: E501
TESTS_FILTER = set(args.tests) if args.tests is not None else None


# RUN TESTS ===================================================================


test_groups = glob.glob(f"{TESTS_DIR}/*/")
print()
if TESTS_FILTER is not None:
    num_test_groups = len(TEST_GROUPS_FILTER or test_groups)
    plural = "s" if num_test_groups != 1 else ""
    print(f"Executing {num_test_groups} test group{plural}...")
else:
    print(f"Executing specified test(s)...")
print()


num_tests = 0
for test_group_path in sorted(test_groups):
    test_group_name = os.path.basename(os.path.normpath(test_group_path))
    if len(TEST_GROUPS_FILTER) and test_group_name not in TEST_GROUPS_FILTER:
        continue
    input_files = glob.glob(os.path.join(test_group_path, "*.unt"))
    if TESTS_FILTER is None:
        num_tests += len(input_files)

    for input_file_path in sorted(input_files):
        test_name = os.path.splitext(os.path.basename(input_file_path))[0]
        if TESTS_FILTER is not None:
            if test_name in TESTS_FILTER:
                num_tests += 1
            else:
                continue
        full_test_name = f"{test_group_name}/{test_name}"
        output_path = os.path.join(test_group_path, f"{test_name}.output")
        ground_truth_path = os.path.join(test_group_path, f"{test_name}.gt")

        if REGENERATE_GTS is not None:
            if len(REGENERATE_GTS) == 0 or test_name in REGENERATE_GTS:
                print(f"Generating GTs for {Format.bold}{full_test_name}{Format.reset}...")  # noqa: E501
                os.system(f"./{BINARY} < {input_file_path} > {ground_truth_path} 2>&1")  # noqa: E501
            continue

        print(f"Running test {Format.bold}{full_test_name}{Format.reset}...")
        # Run the test
        os.system(f"./{BINARY} < {input_file_path} > {output_path} 2>&1")

        # Invoke diff against the ground truth output
        if not os.path.isfile(ground_truth_path):
            print(f"{Format.cursor_up}{Format.yellow}WARNING: Could not find ground truth AST output for test {Format.bold}{full_test_name}{Format.reset}{Format.yellow}; skipping diff{Format.reset}")  # noqa: E501
            continue
        diff_exit_code = subprocess.run(
            ["diff", ground_truth_path, output_path],
        ).returncode
        report_status(diff_exit_code, full_test_name)


# Report results of tests

print()
if num_failed > 0:
    print(f"{Format.red + Format.bold}{num_succeeded}/{num_tests} tests succeeded{Format.reset}", end="")  # noqa: E501
else:
    print(f"{Format.green + Format.bold}{num_succeeded}/{num_tests} tests succeeded{Format.reset}", end="")  # noqa: E501
print(f" {Format.yellow}(diff skipped for {num_tests - num_succeeded - num_failed} tests){Format.reset}" if num_failed + num_succeeded != num_tests else "")  # noqa: E501
exit(1 if num_failed > 0 else 0)
