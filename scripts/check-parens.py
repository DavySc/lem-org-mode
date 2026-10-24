#!/usr/bin/env python3
"""Report top-level forms whose parentheses do not balance: python3 scripts/check-parens.py FILE...

A line that starts with ( in column 0 while a form is still open means the previous form lacks closing
parentheses; a ) that closes more than was opened is reported at its line."""
import sys

def scan(path):
    depth = 0
    in_string = False
    escape = False
    start_line = None
    i_line = 0
    with open(path, encoding='utf-8') as f:
        lines = f.read().split('\n')
    block_comment = 0
    problems = []
    for number, line in enumerate(lines, 1):
        if not in_string and block_comment == 0 and depth > 0 and line.startswith('(') :
            problems.append((start_line, number, 'form starting at line %d is not closed before line %d' % (start_line, number)))
            depth = 0
        k = 0
        while k < len(line):
            c = line[k]
            if in_string:
                if escape:
                    escape = False
                elif c == '\\':
                    escape = True
                elif c == '"':
                    in_string = False
            elif block_comment:
                if line.startswith('|#', k):
                    block_comment -= 1; k += 1
            else:
                if c == ';':
                    break
                if line.startswith('#|', k):
                    block_comment += 1; k += 1
                elif c == '#' and line.startswith('#\\', k):
                    k += 2  # character literal: skip the next char
                elif c == '"':
                    in_string = True
                elif c == '(':
                    if depth == 0:
                        start_line = number
                    depth += 1
                elif c == ')':
                    depth -= 1
                    if depth < 0:
                        problems.append((number, number, 'extra ) at line %d' % number))
                        depth = 0
            k += 1
    if depth > 0:
        problems.append((start_line, len(lines), 'form starting at line %d is not closed at the end of the file' % start_line))
    return problems

status = 0
for path in sys.argv[1:]:
    for first, last, message in scan(path):
        print('%s: %s' % (path, message))
        status = 1
sys.exit(status)
