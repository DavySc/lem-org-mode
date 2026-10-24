# AGENTS.md

Guide for AI agents and contributors changing this repo. It is a Lem extension written in Common Lisp (SBCL).

## Layout

All files are in package `lem-org-mode`, loaded in the order of `lem-org-mode.asd`. Command logic lives in functions that take a point or a buffer (`org-todo-change`, `org-archive-subtree`, `org-refile`, `export-run` ...); the `define-command` forms are thin wrappers that prompt. Keep it that way so tests can call them on a plain buffer.

| File | Holds |
|---|---|
| `org-mode.lisp` | package and exports, highlighting, keymap, headings, subtrees, tables, plain lists, tags / priority / planning / properties, clocking, SQL blocks, `C-c C-c` |
| `timestamp.lisp` | dates, timestamp parse / format / shift, repeaters, typed dates (`date-input-parse`) |
| `tasks-model.lisp`, `tasks.lisp` | TODO sequences, log entries, repeat rules, statistics cookies; `org-todo-change`, notes, effort |
| `planning.lisp` | timestamps in the text (`C-c .`, Shift arrows), ranges |
| `structure.lisp` | folding, navigation, copy / cut / paste, archive, sort, sparse trees, property inheritance, todo dependencies |
| `capture.lisp` | capture templates and targets, date trees, refile |
| `links.lisp` | links, opening / storing / inserting, IDs, targets, abbreviations, footnotes |
| `tables.lisp` | `#+TBLFM:` spreadsheet, sums, CSV, text to table |
| `dblocks.lisp` | dynamic blocks: clock table, column view |
| `editing.lisp` | structure templates, list editing, drawers, COMMENT, timers, RET |
| `agenda.lisp` | entry parsing, matcher, views (day / week / month / year, todo, match, search, stuck), filters, the agenda buffer and its commands |
| `babel-model.lisp`, `babel.lisp` | Babel: header arguments, named elements, inline blocks, data, results, noweb, tangle; runtime, buffer edits, commands |
| `export.lisp`, `export-more.lisp`, `publish.lisp` | export pipeline (options, includes, macros, sections, inline markup, HTML, Markdown), ASCII / LaTeX / Org / iCalendar, subtree export, projects |
| `tests/*.lisp` | Rove tests (`lem-org-mode/tests`), one file per area; `main.lisp` has the helpers (`make-org-buffer`, `lines`, `line-point`, `text-of`) |
| `tests/prompt-tests.lisp` | commands that prompt, driven by canned answers (`with-answers` encapsulates `prompt-for-string`) |
| `scripts/pure-test.lisp`, `scripts/pure/*.lisp` | tests of the pure functions with plain SBCL (see below); the interpreters it finds are run too |
| `scripts/smoke.lisp`, `scripts/smoke.d/*.lisp` | the same behaviours checked inside a real Lem without Rove; add a `smoke-check` for each new behaviour |
| `scripts/headless/` | `setup.sh` builds a Lem without a GUI; `run.sh` runs pure, Rove and smoke tests with it |
| `scripts/update-exports.py` | adds every new `define-command` and `*org-...*` variable to the `:export` list of `org-mode.lisp` |
| `scripts/check-parens.py` | finds the top-level form whose parentheses do not balance (the reader's message is unhelpful) |

## Verify your change

1. `scripts/headless/run.sh pure` (or `sbcl --script scripts/pure-test.lisp`) runs in seconds without Lem and must print `0 failures`. Pure functions live between `;;; BEGIN pure …` and `;;; END pure …` markers; they use only CL (plus `editor-error`, which the script stubs, and cl-ppcre if it can be loaded). **Everything the pure code needs must be inside a marked region, `defvar`s included**, or the test script reports an unbound variable. Put new logic there and add a `check` in `scripts/pure/NN-area.lisp` (a new file in that directory is picked up automatically; new source files go into the file list at the top of `pure-test.lisp`).
2. Anything that touches a buffer must run in a real Lem. Without a GUI: `scripts/headless/setup.sh` once (about a minute; needs git, make, python3, sbcl and github.com), then `scripts/headless/run.sh` (all three layers, about 20 seconds) or `run.sh rove smoke`. The Rove tests cover buffer glue, prompts (canned answers), files in `/tmp`, and the interpreters they find. In a window instead: `lem -q --eval "(load \"<repo>/scripts/smoke.lisp\")"`, then read `scripts/smoke.log`: every line must be `PASS` and the last line `DONE`. Kill Lem afterwards; it stays open.
3. After adding commands run `python3 scripts/update-exports.py`. Test files are in a different package: use `lem-org-mode::name` for internals (a plain `lem-org-mode:name` on a non-exported symbol aborts the whole file at read time; `run.sh rove` prints the compiler's message for that).
4. Do not report something as tested unless you ran it. What the headless run does not cover: key handling in a window, `switch-to-buffer` and other window commands (`org-switch-to-buffer` calls `*org-switch-function*`, which the tests replace by "make it current"), rendering and colours, `lem-vi-mode`, and the real prompt (the tests replace `prompt-for-string`, so completion and history are untested). The GUI exe is not needed for any of this.

## Traps found so far

- **Define keys after the mode.** `define-key *org-mode-keymap*` before `define-major-mode` fails at load with an unbound variable.
- **Temporary points do not follow edits.** After `insert-string` at a `:temporary` point, the point stays put, so two inserts land in the wrong order. Build one string and insert once, or use a live point kind.
- **SBCL package locks.** Do not define or intern symbols named like CL or `lem` symbols (e.g. a macro called `step`). In test scripts, reading `lem:some-symbol` that is not exported aborts the whole file silently.
- **Names that do not exist:** `lem:find-mode-from-name` (use `get-file-mode`). `indent-line-and-complete-symbol` is not exported from `lem/language-mode`; the exported `indent` command is.
- **The GUI exe prints nothing to stdout.** Log to a file, and wrap a top-level `handler-case` around `load` when debugging, otherwise errors appear only inside the window.
- **PowerShell quoting.** `lem.exe -e "(load \"…\")"` breaks in PowerShell (`\"` is not an escape). Use bash, or `Start-Process -ArgumentList '-q','-e','"(load \"…\")"'`.
- Under `lem-vi-mode`, normal-state keys can shadow mode keymaps. Check with `lem-core:lookup-keybind` inside the running editor.

- **Do not define functions named like `lem` exports.** `use :lem` imports them and SBCL's package lock rejects the redefinition when the file loads (`blank-line-p` once broke the whole mode). `scripts/headless/run.sh smoke` catches it; the pure tests cannot.
- **`message` needs a frame.** Headless, it raises an unbound-variable error. The Rove tests add an `:around` method on `lem-core::show-message` that collects messages.
- **Do not filter blank lines out of output** (`grep -v '^$'`) when you print a buffer's text: you will read missing blank lines that are there.
- **Plan edits as data.** `result-edit` returns `(:insert N lines)`, `(:replace S COUNT lines)` or `(:delete A B)` in 1-based line numbers; `babel-apply-edit` carries them out with `replace-lines`, which edits only the differing span so that points keep their place.

- **Live sessions** (`babel-session-eval`): a server script per family in `*babel-session-servers*` reads `mode<TAB>file<TAB>marker` lines; the block's program goes in a temp file. Rove tests start real interpreters, so kill them with `babel-sessions-kill-all`.

## Conventions

Follow Lem's own style (see Lem's `contract.yml`): `defpackage` first and named after the file, `(loop :for … :do …)` with colons, kebab-case names, docstrings on commands and exported symbols, `editor-error` for user-facing errors, prefer `lem` exported symbols over `lem::` internals. Keep changes small and focused.

- **`format` and `~@[`.** A `~@[...~]` with more than one argument inside takes the first argument as the test and leaves it for the clause only when it is true; when it is false the *next* argument is eaten by the directive after it. Build optional pieces as separate strings (see `html-table`) instead of one big `format`.
- **Do not name functions like Lem's**: `write-file` (in tests too), `blank-line-p`, `step` ... all violate package locks. The loading error is not shown by `run.sh pure`.
- **`call-command` and `newline` need a frame.** Commands that insert text should insert directly (`insert-character`) so that they work headless.
- **Temporary points do not follow deletions.** After `delete-subtree`, find the anchor again (`find-heading-line`) instead of reusing a temporary point.
- **A key cannot be both a command and a prefix.** `C-c a` is `org-agenda`, so there is no `C-c a a`.
- **Planning goes right under the heading** (SCHEDULED, DEADLINE and CLOSED on one line, before the drawer) in the tests as in Org; otherwise the agenda does not see it.

## Roadmap

Done and tested headless (see the README for each): tables with formulas, plain lists and checkboxes, TODO sequences with logging, repeaters, cookies, dependencies, archiving; timestamps and clocking with clock tables; the agenda (spans, grid, repeaters, ranges, matcher, search, stuck projects, filters, custom commands, actions; the window part is untested); capture with date trees and refile; links, IDs, targets, footnotes; sparse trees, sort, copy / cut / paste, folding; Babel on par with Org's core including inline blocks, indexes, the Library of Babel and 25 languages (lua and R ran; powershell did not); export to HTML / Markdown / text / LaTeX / Org / iCalendar with options, includes, macros, footnotes, TOC and numbering; publishing projects with sitemap and a timestamp cache.

Next:

1. `:stdin`; live sessions beyond sh / python / js / ruby (add a server script to `*babel-session-servers*`).
2. Column view as an editable buffer, habits, attachments.
3. Speed keys, countdown timers, `org-protocol`.
4. ODT export; detangling.
5. Highlighting that follows a buffer's own `#+TODO:` keywords.
