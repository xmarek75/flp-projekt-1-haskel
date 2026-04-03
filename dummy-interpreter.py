#!/usr/bin/env python3

# This is a simple "dummy interpreter" script that can be used for experimenting with the SOL26
# testing tool. It behaves similarly to the SOL26 interpreter. It has two CLI parameters:
# required --source and optional --input.
#
# The script will read the entire "source" file. Then it will read its first line and parse it
# as an integer. If it is non-zero, it exits with this integer as the exit code. Otherwise, it
# will a) copy the contents of the file given as --input to its stdout; or b) copy the rest of
# the source file to its stdout if --input is not given.

import sys
import argparse
from pathlib import Path

arg_parser = argparse.ArgumentParser()
arg_parser.add_argument(
    "-s",
    "--source",
    type=Path,
    required=True
)
arg_parser.add_argument(
    "-i",
    "--input",
    type=Path,
    required=False
)

try:
    args = arg_parser.parse_args()
except SystemExit:
    # Invalid arguments -> exit code 10
    exit(10)

source_file: Path = args.source
input_file: Path = args.input

if not source_file.is_file():
    # Source file does not exist or is not a file
    exit(11)
if input_file is not None and not input_file.is_file():
    # Input file does not exist or is not a file.
    exit(11)

with source_file.open() as sf:
    line = sf.readline()
    rest_src = sf.read()

try:
    ret_code = int(line)
except ValueError:
    # First line is not an integer -> exit code 20
    exit(20)

if ret_code != 0:
    exit(ret_code)

if input_file is not None:
    with input_file.open() as inf:
        for line in inf:
            sys.stdout.write(line)
else:
    sys.stdout.write(rest_src)
