#!/usr/bin/env python3

import subprocess
import sys
import os

args = sys.argv[1:]

# Make sure args is non-empty
if not args:
    print("Error: no arguments provided for rustc")
    sys.exit(1)

# Call rustc with the arguments
try:
    rustc_args = ["rustc"] + args
    subprocess.run(rustc_args, check=True)
except subprocess.CalledProcessError as e:
    sys.exit(e.returncode)

#is_test_run = os.getenv("UITEST_TEST_RUN", "False") == "True"
#if is_test_run:
    # In real-life use-cases, you might do something here