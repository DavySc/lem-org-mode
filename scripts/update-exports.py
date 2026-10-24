#!/usr/bin/env python3
"""Add every define-command and every *org-...* variable of the source files to the :export list of org-mode.lisp.

    python3 scripts/update-exports.py

Run it after adding commands or options; it only adds symbols, never removes them."""
import re, glob, os
root = os.path.join(os.path.dirname(os.path.abspath(__file__)), '..')
files = [os.path.join(root, f) for f in ['org-mode.lisp'] + sorted(set(os.path.basename(p) for p in glob.glob(os.path.join(root, '*.lisp'))) - {'org-mode.lisp'})]
src = ''.join(open(f).read() for f in files)
commands = set(re.findall(r'^\((?:define-command|define-agenda-entry-command|define-meta-command|define-heading-motion) \(?([a-z0-9*+-]+)', src, re.M))
variables = set(re.findall(r'^\(defvar (\*org-[a-z0-9-]+\*)', src, re.M))
path = os.path.join(root, 'org-mode.lisp')
text = open(path).read()
block = re.search(r'\(defpackage :lem-org-mode.*?\(:export(.*?)\)\)\n\(in-package', text, re.S)
exported = {e.lower() for e in re.findall(r':([^\s()]+)', block.group(1))}
new = sorted((commands | variables) - exported)
if new:
    marker = block.group(1).rstrip()
    text = text.replace(marker, marker + ''.join('\n           :%s' % n for n in new), 1)
    open(path, 'w').write(text)
print('exported %d new symbol(s)' % len(new))
