#!/usr/bin/env python3

# This is a simple "dummy parser" script that can be used for experimenting with the SOL26
# testing tool. It behaves similarly to the SOL2XML parser, i.e. it expects something on the
# stdin and then outputs something on the stdout.
#
# Specifically, the script expects an integer in the first line. If it is not 0, the script
# uses the integer as its exit code. Otherwise, it copies all the other lines from the input
# to its output.

import sys

line = sys.stdin.readline()
try:
    ret_code = int(line)
except ValueError:
    # If the first line is not an integer, end with exit code 10.
    exit(10)

if ret_code != 0:
    exit(ret_code)

for line in sys.stdin:
    sys.stdout.write(line)
