#!/usr/bin/python3

# TODO: nicer diff output
# TODO: options for running a single test / test group
# TODO: options for regenerating ground truth
# TODO: options for end-to-end testing by re-parsing pretty printed AST output and ensuring it’s the same as the previous output

import os
import subprocess
import glob
import sys
from argparse import ArgumentParser

PWD = os.getcwd()

BINARY = f"untangled.native"
TESTS_DIR = f"{PWD}/tests"

from argparse import ArgumentParser

parser = ArgumentParser()
parser.add_argument("-gt", "--ground_truths", nargs="*", required=False, 
                    help="Regenerates ground truths, specify test names to regenerate specific ones, specify no test names to regenerate all")
args = parser.parse_args()

REGENERATE_GTS = set(args.ground_truths) if args.ground_truths != None else None

class Format:
    """Defines common ANSI escape codes for formatted terminal output"""
    green = "\033[32m"
    red = "\033[91m"
    yellow = "\033[93m"
    dim = "\033[2m"
    reset = "\033[0m"
    bold = "\033[1m"
    cursor_up = "\033[1A"


num_succeeded = 0
num_failed = 0
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


# Run all the tests defined in the “tests/” directory

test_groups = glob.glob(f"{TESTS_DIR}/*/")
print()
print(f"Executing {len(test_groups)} test group{'s' if len(test_groups) != 1 else ''}...")
print()


num_tests = 0
for test_group_path in sorted(test_groups):
    test_group_name = os.path.basename(os.path.normpath(test_group_path))
    input_files = glob.glob(os.path.join(test_group_path, "*.unt"))
    num_tests += len(input_files)

    for input_file_path in sorted(input_files):
        test_name = os.path.splitext(os.path.basename(input_file_path))[0]
        full_test_name = f"{test_group_name}/{test_name}"
        output_ast_file_path = os.path.join(test_group_path, f"{test_name}.output")
        ground_truth_ast_file_path = os.path.join(test_group_path, f"{test_name}.gt")

        if REGENERATE_GTS != None:
          if len(REGENERATE_GTS) == 0 or test_name in REGENERATE_GTS:
            print(f"Generating GTs for {Format.bold}{full_test_name}{Format.reset}...")
            os.system(f"./{BINARY} < {input_file_path} > {ground_truth_ast_file_path} 2>&1")
          continue


        print(f"Running test {Format.bold}{full_test_name}{Format.reset}...")
        # Run the test
        os.system(f"./{BINARY} < {input_file_path} > {output_ast_file_path} 2>&1")


        # Invoke diff against the ground truth output
        if not os.path.isfile(ground_truth_ast_file_path):
            print(f"{Format.cursor_up}{Format.yellow}WARNING: Could not find ground truth AST output for test {Format.bold}{full_test_name}{Format.reset}{Format.yellow}; skipping diff{Format.reset}")
            continue
        diff_exit_code = subprocess.run(["diff", ground_truth_ast_file_path, output_ast_file_path]).returncode
        report_status(diff_exit_code, full_test_name)


# Report results of tests

print()
if num_failed > 0:
    print(f"{Format.red + Format.bold}{num_succeeded}/{num_tests} tests succeeded{Format.reset}", end="")
else:
    print(f"{Format.green + Format.bold}{num_succeeded}/{num_tests} tests succeeded{Format.reset}", end="")
print(f" {Format.yellow}(diff skipped for {num_tests - num_succeeded - num_failed} tests){Format.reset}" if num_failed + num_succeeded != num_tests else "")
exit(1 if num_failed > 0 else 0)
