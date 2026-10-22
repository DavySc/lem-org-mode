# AGENTS.md

Guide for AI agents and contributors changing this repo. It is a Lem extension written in Common Lisp (SBCL).

## Layout

- `org-mode.lisp`: the core of the mode (package `lem-org-mode`): highlighting, headings, tables, lists, tags, planning, SQL blocks. `agenda.lisp`, `babel-model.lisp`, `export.lisp` and `babel.lisp` (same package, loaded in that order after it) hold the agenda; the pure Babel model (header arguments and inheritance, named elements, data, results, noweb, tangle plan); HTML / Markdown export; and the Babel runtime and commands (interpreters, results in the buffer, tangling, source edit buffers). Command logic lives in functions that take a point (`org-cycle-todo`, `org-shift-subtree`, `org-move-subtree`, …); the `define-command` forms are thin wrappers. Keep it that way so tests can call them on a plain buffer.
- `tests/main.lisp`: Rove tests (`lem-org-mode/tests`).
- `scripts/smoke.lisp`: runs inside a real Lem and checks the same behaviours without Rove. Add a `smoke-check` for each new behaviour.
- `scripts/pure-test.lisp`: tests the pure functions with plain SBCL, no Lem needed (see below). It also runs the real interpreters it finds (python, node, ruby, perl, php, lua, R, sbcl, bash, gcc, g++, go, java).
- `scripts/headless/`: `setup.sh` builds a Lem without a GUI; `run.sh` runs the pure tests, the Rove tests and the smoke test with it. This is how to test glue code when no GUI is at hand.

## Verify your change

1. `scripts/headless/run.sh pure` (or `sbcl --script scripts/pure-test.lisp`) runs in seconds without Lem and must print `0 failures`. Pure functions live between `;;; BEGIN pure …` and `;;; END pure …` markers in the five source files; they use only CL (plus `editor-error`, which the script stubs). Put new logic there and add a `check` for it.
2. Anything that touches a buffer must run in a real Lem. Without a GUI: `scripts/headless/setup.sh` once (about a minute; needs git, make, python3, sbcl and github.com), then `scripts/headless/run.sh rove smoke`. The Rove tests (`tests/main.lisp`) cover buffer glue and run the interpreters they find; the smoke test checks the same in the form the GUI user runs. In a window instead: `lem -q --eval "(load \"<repo>/scripts/smoke.lisp\")"`, then read `scripts/smoke.log`: every line must be `PASS` and the last line `DONE`. Kill Lem afterwards; it stays open.
3. Do not report something as tested unless you ran it. What the headless run does not cover: key handling in a window, `switch-to-buffer` and other window commands (agenda, expand and source-edit buffers are tested up to the point where they are shown), Tab folding, rendering, `prompt-for-string`, and `lem-vi-mode`.

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

## Conventions

Follow Lem's own style (see Lem's `contract.yml`): `defpackage` first and named after the file, `(loop :for … :do …)` with colons, kebab-case names, docstrings on commands and exported symbols, `editor-error` for user-facing errors, prefer `lem` exported symbols over `lem::` internals. Keep changes small and focused.

## Roadmap

Done and tested headless: tables, plain lists and checkboxes, priorities, tags, properties, SCHEDULED/DEADLINE/CLOSED, agenda (week, todo list, tag search; the window part is untested), HTML and Markdown export (inherited `:exports`, noweb), SQL blocks (engines untested against real databases), and Babel on par with Org's core: header-argument inheritance, named blocks / tables / lists / results as `:var`, calls, `:results` shapes and handling, `:cache`, emulated `:session`, noweb, `:post`, tangle with shebang / mode / comments / mkdirp, source edit buffers, 16 languages (lua and R ran; powershell did not).

Next:

1. Statistics cookies `[1/3]` and `[50%]` on headings and items.
2. Skip `*` lines inside src blocks in `org-heading-level`.
3. Inline `src_lang{}` and `call_name()`; `:var` indexes `name[1,2]`; the Library of Babel.
4. Recurring timestamps (`+1w`) in the agenda; `S-Right` / `S-Left` TODO cycling.
5. Capture templates and refile.
