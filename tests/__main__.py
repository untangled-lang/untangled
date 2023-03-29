#!/usr/bin/python3

# TODO: nicer diff output
# TODO: options for end-to-end testing by re-parsing pretty printed AST output
#       and ensuring it’s the same as the previous output

import os
import subprocess
import glob
from argparse import ArgumentParser

STEPS = [
    "ast", "sast", "e2e"
]
PWD = os.getcwd()
BINARY = "untangled.exe"
TESTS_DIR = f"{PWD}/tests"
COMPILER_STEPS_DIR = list(map(lambda step: f"{TESTS_DIR}/{step}", STEPS))
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


def get_current_folders(path):
    all_paths = map(lambda x: os.path.join(path, x), os.listdir(path))
    folders = list(filter(lambda x: os.path.isdir(x), all_paths))
    return folders


# PARSE ARGS ==================================================================

ACTION_MAPPER = {"ast": "-a", "sast": "-s", "e2e": "-l", "compile": "-c"}

parser = ArgumentParser()
group = parser.add_mutually_exclusive_group(required=False)
group.add_argument("-t", "--tests", nargs="*",
                   help="Limits tests to those with the specified names.", default=[])
group.add_argument("-tg", "--test-groups", nargs="*", required=False,
                   help="Limits tests to those in the specified test groups.", default=[])
group.add_argument("-gt", "--record-ground-truths", nargs="*", required=False,
                   help="Regenerate ground truths from test outputs for the specified tests. Leave list blank to regenerate all.")  # noqa: E501
parser.add_argument(
    "-s", "--step", nargs="*", choices=STEPS, default=[], required=False, help="Specify which compiler's step to test. Leave blank to run all steps"
)
parser.add_argument("-l", "--link", nargs="*", help="Link executable with other .o files", required=False, default=[])
args = parser.parse_args()

TEST_GROUPS_FILTER = args.test_groups
TESTS_FILTER = args.tests
REGENERATE_GTS = args.record_ground_truths
TEST_STEPS_FILTER = args.step
OBJECT_FILES = args.link

if len(TESTS_FILTER) == 0:
    TESTS_FILTER = EverythingSet()

if len(TEST_GROUPS_FILTER) == 0:
    TEST_GROUPS_FILTER = EverythingSet()

# If -gt is passed without specific tests, we will regenerate all ground truths
if REGENERATE_GTS is not None and len(REGENERATE_GTS) == 0:
    REGENERATE_GTS = EverythingSet()

if len(TEST_STEPS_FILTER) == 0:
    TEST_STEPS_FILTER = EverythingSet()

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

for step_dir in COMPILER_STEPS_DIR:
    compiler_step = os.path.basename(os.path.normpath(step_dir))
    test_groups = get_current_folders(step_dir)
    test_groups.append(step_dir)
    ocaml_driver_option = ACTION_MAPPER.get(compiler_step)

    # Skip random folder
    if ocaml_driver_option is None:
        continue

    # Skip if compiler step isn't in the filter
    if compiler_step not in TEST_STEPS_FILTER:
        print(f"Skipping {compiler_step}")
        continue

    while len(test_groups) > 0:
        test_group_path = test_groups.pop(0)
        group_name = os.path.basename(os.path.normpath(test_group_path))
        print(group_name)

        # Skip test groups that aren’t in the filter
        if group_name not in TEST_GROUPS_FILTER:
            continue

        # Add nested test folder
        test_groups += get_current_folders(test_group_path)
        input_files = glob.glob(os.path.join(test_group_path, "*.unt"))
        for input_file_path in sorted(input_files):
            test_name = os.path.splitext(os.path.basename(input_file_path))[0]
            if test_name not in TESTS_FILTER:
                continue

            num_tests += 1
            full_test_name = f"{group_name}/{test_name}"
            output_path = os.path.join(test_group_path, f"{test_name}.output")
            ground_truth_path = os.path.join(test_group_path, f"{test_name}.gt")

            print(f"Running test {Format.bold}{full_test_name}{Format.reset}...")

            if compiler_step != "e2e":
                # Run the test
                os.system(
                    f"./{BINARY} {ocaml_driver_option} < {input_file_path} > {output_path} 2>&1"
                )

            else:
                ll_path = os.path.join(test_group_path, f"{test_name}.ll")
                s_path = os.path.join(test_group_path, f"{test_name}.s")
                exe_path = os.path.join(test_group_path, f"{test_name}")
                os.system(
                    f"./{BINARY} {ocaml_driver_option} < {input_file_path} > {ll_path} 2>&1"
                )
                os.system(f"llc {ll_path} > {s_path}")
                os.system(f"clang -o {exe_path} {s_path}")
                os.system(f"{exe_path} > {output_path}")

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
