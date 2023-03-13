#!/usr/bin/python3

# TODO: nicer diff output
# TODO: options for end-to-end testing by re-parsing pretty printed AST output
#       and ensuring it’s the same as the previous output

import os
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
    bold = "\033[1m"
    cursor_up = "\033[1A"


class EverythingSet:
    """A collection that contains everything"""
    def __contains__(self, item):
        return True


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

actionMapper = { "ast": "-a", "sast": "-s", "llvm": "-l", "compile": "-c" }

parser = ArgumentParser()
group = parser.add_mutually_exclusive_group(required=False)
group.add_argument("-t", "--tests", nargs="*",
                   help="Limits tests to those with the specified names.")
group.add_argument("-tg", "--test-groups", nargs="*", required=False,
                   help="Limits tests to those in the specified test groups.")
group.add_argument("-gt", "--record-ground-truths", nargs="*", required=False,
                   help="Regenerate ground truths from test outputs for the specified tests. Leave list blank to regenerate all.")  # noqa: E501
parser.add_argument("-a", "--action", choices=["ast", "sast", "llvm", "compile"],
                    default="compile", required=False, help="Specify compiler's stopping point. Leave blank for full compilation")
args = parser.parse_args()

ACTION = actionMapper[args.action]
TEST_GROUPS_FILTER = args.test_groups
TESTS_FILTER = args.tests
REGENERATE_GTS = args.record_ground_truths
# If -gt is passed without specific tests, we will regenerate all ground truths
if REGENERATE_GTS is not None and len(REGENERATE_GTS) == 0:
    REGENERATE_GTS = EverythingSet()


# RUN TESTS ===================================================================


test_group_folders = glob.glob(f"{TESTS_DIR}/*/")
print()
if TESTS_FILTER:  # don’t know which groups we’re running if we filter by test
    print(f"Executing specified test(s)...")
else:
    num_test_groups = len(TEST_GROUPS_FILTER or test_group_folders)
    plural = "s" if num_test_groups != 1 else ""
    print(f"Executing {num_test_groups} test group{plural}...")
print()


num_tests = 0
for test_group_path in sorted(test_group_folders):
    group_name = os.path.basename(os.path.normpath(test_group_path))
    # Skip test groups that aren’t in the filter
    if TEST_GROUPS_FILTER is not None and group_name not in TEST_GROUPS_FILTER:
        continue

    input_files = glob.glob(os.path.join(test_group_path, "*.unt"))
    for input_file_path in sorted(input_files):
        test_name = os.path.splitext(os.path.basename(input_file_path))[0]
        # Skip tests that aren’t in the filter
        if TESTS_FILTER is not None and test_name not in TESTS_FILTER:
            continue

        num_tests += 1
        full_test_name = f"{group_name}/{test_name}"
        output_path = os.path.join(test_group_path, f"{test_name}.output")
        ground_truth_path = os.path.join(test_group_path, f"{test_name}.gt")

        # Run the test
        print(f"Running test {Format.bold}{full_test_name}{Format.reset}...")
        os.system(f"./{BINARY} {ACTION} < {input_file_path} > {output_path} 2>&1")
        # Record a new ground truth if requested
        if REGENERATE_GTS and test_name in REGENERATE_GTS:
            subprocess.run(
                ["cp", output_path, ground_truth_path]
            ).check_returncode()
            print(
                f"{Format.cursor_up}{Format.yellow}! "
                "Overwriting ground truth for test "
                f"{Format.bold}{full_test_name}{Format.reset}\n"
            )

        # Invoke diff against the ground truth output
        if not os.path.isfile(ground_truth_path):
            print(
                f"{Format.cursor_up}{Format.yellow}! "
                "Could not find ground truth AST output for test "
                f"{Format.bold}{full_test_name}{Format.reset}{Format.yellow}; "
                f"skipping diff{Format.reset}"
            )
            continue
        diff_exit_code = subprocess.run(
            ["diff", ground_truth_path, output_path],
        ).returncode
        report_status(diff_exit_code, full_test_name)


# Report results of tests

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
