#!/usr/bin/env python3

import re
import sys
import os

def clear_terminal():
    if os.name == "posix":  # Linux and macOS
        os.system("clear")
    elif os.name == "nt":  # Windows
        os.system("cls")

pattern = r'(Compiling .*\n((Checking .*\n)?error: .*))'
input_text = sys.stdin.read()

clear_terminal()

matches = re.findall(pattern, input_text)

for match in matches:
    print(match)
    remaining_lines = input_text.split(match, 1)[1]
    print(remaining_lines)

