# lem-org-mode

A small Org mode for the [Lem](https://github.com/lem-project/lem) editor. Work in progress: a core subset of Emacs Org, built in phases.

## What works

- **Highlighting** for headings (per level), TODO/DONE keywords, `#+KEYWORD:` lines, comments, `#+begin_…`/`#+end_…` blocks, tables, list items and checkboxes, links, and `*bold*`, `/italic/`, `=code=`, `~code~`.
- **Folding:** `Tab` on a heading folds or unfolds its subtree.
- **Subtree editing:**

| Key | Command | Does |
|---|---|---|
| `Tab` | `org-cycle` | fold / unfold the subtree at a heading |
| `C-c C-t` | `org-todo` | cycle none → TODO → DONE → none |
| `M-Left` / `M-Right` | `org-promote-subtree` / `org-demote-subtree` | change the level of a heading and its subtree |
| `M-Up` / `M-Down` | `org-move-subtree-up` / `org-move-subtree-down` | swap with the previous or next sibling |
| `M-Return` | `org-insert-heading` | new heading after the current subtree |

Customise the TODO keywords with `lem-org-mode:*org-todo-keywords*` (default `("TODO" "DONE")`).

## Not yet

Tables (alignment, cell navigation), plain lists and checkbox toggling, tags and properties, Babel `#+begin_src` execution (the plan is SQL against Postgres, SQL Server and Db2 for i through their CLI tools), agenda and export. Heading detection does not skip `*` lines inside src blocks. Under `lem-vi-mode`, normal-state keys can shadow the keys above; use `M-x` or add normal-state bindings.

## Install

You need a Lem checkout or build. Load the extension from your `init.lisp` (after enabling vi-mode if you use it):

```lisp
(asdf:load-asd #p"/path/to/lem-org-mode/lem-org-mode.asd")
(asdf:load-system :lem-org-mode)
```

Open any `.org` file; the modeline shows `Org`.

## Test

Rove tests in `tests/main.lisp` (`lem-org-mode/tests`) run with Lem's usual tooling. A prebuilt `lem.exe` does not include Rove, so there is also a smoke test that runs inside Lem and writes PASS/FAIL lines to `scripts/smoke.log`:

```
lem -q --eval "(load \"/path/to/lem-org-mode/scripts/smoke.lisp\")"
```

See `AGENTS.md` for details on working on this code.

## License

MIT, see `LICENSE`.
