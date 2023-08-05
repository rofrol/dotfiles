#!/usr/bin/env python3

import re
import sys

pattern = r'(Compiling .*\nerror: .*)'
input_text = sys.stdin.read()

matches = re.findall(pattern, input_text)

for match in matches:
    print(match)
    remaining_lines = input_text.split(match, 1)[1]
    print(remaining_lines)
