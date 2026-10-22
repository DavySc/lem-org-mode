# AGENTS.md

Guide for AI agents and contributors changing this repo. It is a Lem extension written in Common Lisp (SBCL).

## Layout

- `org-mode.lisp`: the core of the mode (package `lem-org-mode`): highlighting, headings, tables, lists, tags, planning, SQL blocks. `agenda.lisp`, `export.lisp` and `babel.lisp` (same package, loaded in that order after it) hold the agenda, HTML / Markdown export and the other Babel languages with tangling. Command logic lives in functions that take a point (`org-cycle-todo`, `org-shift-subtree`, `org-move-subtree`, …); the `define-command` forms are thin wrappers. Keep it that way so tests can call them on a plain buffer.
- `tests/main.lisp`: Rove tests (`lem-org-mode/tests`).
- `scripts/smoke.lisp`: runs inside a real Lem and checks the same behaviours without Rove. Add a `smoke-check` for each new behaviour.
- `scripts/pure-test.lisp`: tests the pure functions with plain SBCL, no Lem needed (see below).

## Verify your change

0. `sbcl --script scripts/pure-test.lisp` runs in seconds without Lem and must print `0 failures`. Pure functions live between `;;; BEGIN pure …` and `;;; END pure …` markers in the four source files; they use only CL (plus `editor-error`, which the script stubs). Put new logic there and add a `check` for it. It does not cover anything that touches a buffer, a window or a process.
1. Paren balance is not enough. Run the smoke test in a real Lem (a built `lem.exe`, or any Lem with `lem/core` loadable):
   `lem -q --eval "(load \"<repo>/scripts/smoke.lisp\")"`, then read `scripts/smoke.log`. Every line must be `PASS` and the last line `DONE`. Kill Lem afterwards; it stays open.
2. Do not report something as tested unless you ran it. State what you did not check (key handling in a window, Tab folding, rendering).

## Traps found so far

- **Define keys after the mode.** `define-key *org-mode-keymap*` before `define-major-mode` fails at load with an unbound variable.
- **Temporary points do not follow edits.** After `insert-string` at a `:temporary` point, the point stays put, so two inserts land in the wrong order. Build one string and insert once, or use a live point kind.
- **SBCL package locks.** Do not define or intern symbols named like CL or `lem` symbols (e.g. a macro called `step`). In test scripts, reading `lem:some-symbol` that is not exported aborts the whole file silently.
- **Names that do not exist:** `lem:find-mode-from-name` (use `get-file-mode`). `indent-line-and-complete-symbol` is not exported from `lem/language-mode`; the exported `indent` command is.
- **The GUI exe prints nothing to stdout.** Log to a file, and wrap a top-level `handler-case` around `load` when debugging, otherwise errors appear only inside the window.
- **PowerShell quoting.** `lem.exe -e "(load \"…\")"` breaks in PowerShell (`\"` is not an escape). Use bash, or `Start-Process -ArgumentList '-q','-e','"(load \"…\")"'`.
- Under `lem-vi-mode`, normal-state keys can shadow mode keymaps. Check with `lem-core:lookup-keybind` inside the running editor.

## Conventions

Follow Lem's own style (see Lem's `contract.yml`): `defpackage` first and named after the file, `(loop :for … :do …)` with colons, kebab-case names, docstrings on commands and exported symbols, `editor-error` for user-facing errors, prefer `lem` exported symbols over `lem::` internals. Keep changes small and focused.

## Roadmap

Run the smoke test after pulling: the buffer glue of everything below was written without a Lem to run it in (only the pure functions are tested here), and the first real run already found a cursor bug.

Done: tables, plain lists and checkboxes, priorities, tags, properties, SCHEDULED/DEADLINE/CLOSED, SQL / shell / python / node / ruby / perl / lua / R / powershell / lisp blocks with `:var`, `:results`, `:dir`, `:cmdline`, `:tangle`, agenda (week, todo list, tag search), HTML and Markdown export.

Next:

1. Statistics cookies `[1/3]` and `[50%]` on headings and items.
2. Skip `*` lines inside src blocks in `org-heading-level`.
3. Recurring timestamps (`+1w`) in the agenda; `S-Right` / `S-Left` TODO cycling.
4. Capture templates and refile.
5. `:results value`, `#+NAME:` / `:var` references to named tables and blocks.
