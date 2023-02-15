#!/usr/bin/python3

import os
import subprocess
import pathlib
import re


PWD = os.getcwd()

BINARY = f"toplevel.native"
ENTRY = f"{PWD}/tests"
GROUND_TRUTH = f"{ENTRY}/gt"
INPUT = f"{ENTRY}/in"
OUTPUT = f"{ENTRY}/out"

def status(rc):
  green = "\033[92m"
  endc = "\033[0m"
  fail = '\033[91m'
  if rc == 0:
    print(f"{green}Exit with status {rc}{endc}")
  else:
    print(f"{fail}Exit with status {rc}{endc}")

def format(input):
    fullname = re.search(r"([\w+\-\w+]+\.in$)", input).group(0)
    filename = fullname.split(".in")[0]
    out = f"{OUTPUT}/{filename}.out"
    gt = f"{GROUND_TRUTH}/{filename}.gt"
    return [input, out, gt]

def configure():
  os.system("make")
  if not os.path.isfile(BINARY):
    raise ValueError(f"{BINARY} does not exist")

def main():
  inputFiles = [str(f) for f in pathlib.Path(INPUT).glob("*")]
  print(f"Executing tests {len(inputFiles)}\n{'*' * 50}")

  for file in inputFiles:
    print(f"\033[1mTest file: {file}\033[0m")
    input, out, gt = format(file)

    if not os.path.isfile(gt):
      raise ValueError(f"{gt} does not exist")
      
    os.system(f"./{BINARY} < {input} > {out}")
    rc = subprocess.run(["diff", gt, out]).returncode
    status(rc)

    print(f"\033[1mDone\033[0m\n{'*' * 50}")




if __name__ == "__main__":
  configure()
  main()
