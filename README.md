# lem-org-mode

A small Org mode for the [Lem](https://github.com/lem-project/lem) editor. Work in progress: a core subset of Emacs Org, built in phases.

## What works

- **Highlighting** for headings (per level), TODO/DONE keywords, `#+KEYWORD:` lines, comments, `#+begin_…`/`#+end_…` blocks, tables, list items and checkboxes, links, and `*bold*`, `/italic/`, `=code=`, `~code~`.
- **Folding:** `Tab` on a heading folds or unfolds its subtree.
- **Subtree editing, tables, lists, tags, planning and SQL blocks**, keys below. Keys that depend on context act on the table, list item or heading at point.

| Key | Command | Does |
|---|---|---|
| `Tab` | `org-cycle` | in a table: realign and go to the next cell (new row at the end); on a heading: fold / unfold |
| `Shift-Tab` | `org-table-previous-cell` | previous table cell |
| `C-c C-t` | `org-todo` | cycle none → TODO → DONE → none; DONE adds `CLOSED: [date]`, leaving it removes it |
| `M-Left` / `M-Right` | `org-meta-left` / `-right` | table: move column; otherwise promote / demote the subtree |
| `M-Up` / `M-Down` | `org-meta-up` / `-down` | table: move row; otherwise move the subtree up / down |
| `M-Shift-Left` / `-Right` | `org-table-delete-column` / `org-table-insert-column` | delete / insert a column |
| `M-Shift-Up` / `-Down` | `org-table-delete-row` / `org-table-insert-row` | delete the row / insert a row above |
| `C-c -` | `org-table-insert-hline` | horizontal line under the current table row |
| `M-Return` | `org-meta-return` | on a list item: new item (numbers are renumbered, `[ ]` is kept); otherwise new heading after the subtree |
| `C-c C-c` | `org-ctrl-c-ctrl-c` | table: realign; list item: toggle checkbox; `sql` source block: run it |
| `Shift-Up` / `Shift-Down` | `org-priority-up` / `-down` | cycle the `[#A]`..`[#C]` priority of the heading |
| `C-c C-q` | `org-set-tags-command` | set tags (`work:home`), right-aligned at column 77 (`*org-tags-column*`) |
| `C-c C-s` / `C-c C-d` | `org-schedule` / `org-deadline` | `SCHEDULED:` / `DEADLINE:` line; enter `2026-11-03`, `+3d`, `+1w` or nothing for today |
| `C-c C-x p` | `org-set-property-command` | set a property in the `:PROPERTIES:` drawer |

Numeric table columns are right-aligned. Customise the TODO keywords with `lem-org-mode:*org-todo-keywords*` (default `("TODO" "DONE")`).

### SQL blocks

```org
#+begin_src sql :engine postgres :db mydb :host localhost :user me
select id, name from customers;
#+end_src
```

`C-c C-c` in the block runs it and writes a `#+RESULTS:` table under it (replacing an older one). Engines:

| `:engine` | Runs | Header arguments |
|---|---|---|
| `postgres` (default, `*org-sql-default-engine*`) | `psql --csv` | `:db :host :port :user`; password via `~/.pgpass` or `PGPASSWORD` |
| `mssql` | `sqlcmd` | `:db :host :port`, `:user` and `:password`, or Windows authentication (`-E`) when no user |
| `db2i` | `isql` (unixODBC) | `:db` is the ODBC data source name, optional `:user :password` |

The client must be on `PATH`. Do not commit passwords in header arguments.

## Not yet

Statistics cookies (`[1/3]`), tag inheritance and completion, agenda views, export, `#+begin_src` languages other than `sql`, and a Db2 for i route that does not need ODBC. Heading detection does not skip `*` lines inside src blocks. Table column widths count characters, so wide (CJK) text misaligns. Under `lem-vi-mode`, normal-state keys can shadow the keys above; use `M-x` or add normal-state bindings.

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
