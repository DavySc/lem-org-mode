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
| `C-c C-x C-i` | `org-clock-in-command` | start a clock on the entry: adds `CLOCK: [date time]` to its `:LOGBOOK:` drawer; stops a clock running elsewhere |
| `C-c C-x C-o` | `org-clock-out-command` | stop the running clock, write `--[end] =>  1:30` and show the duration |
| `C-c C-x C-q` | `org-clock-cancel-command` | drop the running clock without recording it |
| `C-c a a` / `t` / `m` | `org-agenda-week` / `-todo-list` / `-tags` | week agenda / global TODO list / entries with a tag (see Agenda) |
| `C-c C-e h` / `m` | `org-export-html` / `org-export-markdown` | export the buffer to `.html` / `.md` next to the file |
| `C-c C-v t` | `org-babel-tangle` | write blocks with `:tangle` to files |

Marking an entry DONE with `C-c C-t` also stops its running clock.

Numeric table columns are right-aligned. Customise the TODO keywords with `lem-org-mode:*org-todo-keywords*` (default `("TODO" "DONE")`).

### Agenda

`C-c a a` shows the next `*org-agenda-span*` days (default 7): items `SCHEDULED` or `DEADLINE` on each day, overdue scheduled items (`Sched. 2x:`) and deadlines due within `*org-deadline-warning-days*` (`In 3 d.:`) on today. `C-c a t` lists every open TODO, highest `[#A]` priority first; `C-c a m` lists entries with a tag. `Return` on a line jumps to the entry, `q` closes the agenda.

It reads the current buffer. To read several files set, in `init.lisp`, `(setf lem-org-mode:*org-agenda-files* '("~/org/work.org" "~/org/home/"))`; directories contribute their `.org` files (files are read from disk, so save first). Repeaters (`+1w`) and time ranges are not understood; a time such as `<2026-10-22 Thu 14:30>` is shown as `14:30 Sched.:`.

### Export

`C-c C-e h` writes `file.html` and `C-c C-e m` writes `file.md` next to the buffer's file (or opens a buffer when it has none). Supported: `#+TITLE:` / `#+AUTHOR:`, headings with TODO, priority and tags, planning lines, paragraphs, nested and numbered lists with checkboxes, tables (a rule after the first rows makes them the header), `src` / `example` / `quote` / `center` / `verse` blocks, `: ` example lines, `#+begin_export html|md` passthrough, links, `*bold* /italic/ _underline_ =code= ~code~ +strike+`. `:PROPERTIES:` drawers and `#+begin_comment` / `# ` comments are dropped. `:exports code` (default) shows the block, `results` its result, `both` both, `none` neither. No footnotes, LaTeX or images.

### Source blocks (Babel)

`C-c C-c` inside a block runs it and writes the result under `#+RESULTS:`, replacing an older result. Header arguments: `:var name=value` (repeatable; numbers are passed as numbers, anything else, or `"quoted text"`, as a string), `:results output|table|silent` (default `output`, shown as `: ` lines; `table` splits the output into cells at tabs, or at blanks when there are none), `:dir`, `:cmdline`, `:exports`, `:tangle yes|no|file` (used by `C-c C-v t`).

| Language | Interpreter run on a temporary file |
|---|---|
| `sh` / `bash`, `zsh` | `bash`, `zsh` |
| `python` | `python3` |
| `javascript` / `js` | `node` |
| `ruby`, `perl`, `lua` | `ruby`, `perl`, `lua` |
| `r` | `Rscript` |
| `powershell` / `pwsh` | `pwsh -NoProfile -File` |
| `lisp` / `sbcl` | `sbcl --script` |
| `sql` | see below |

The interpreters must be on `PATH`. On Windows the defaults are `sh`, `python` and `powershell -NoProfile -ExecutionPolicy Bypass -File` instead of `bash`, `python3` and `pwsh`, because `bash` and `python3` there are often the WSL launcher and the Microsoft Store stub. To change one, edit `lem-org-mode:*org-babel-languages*`, for example python on Windows: `(setf (getf (cdr (assoc '("python" "py" "python3") lem-org-mode:*org-babel-languages* :test #'equal)) :argv) '("python"))`. Only `:results output` is supported (what the program prints), not `value`; there are no sessions.

#### SQL blocks

```org
#+begin_src sql :engine postgres :db mydb :host localhost :user me
select id, name from customers;
#+end_src
```

Results are a table with a rule under the header row. Engines:

| `:engine` | Runs | Header arguments |
|---|---|---|
| `postgres` (default, `*org-sql-default-engine*`) | `psql --csv` | `:db :host :port :user`; password via `~/.pgpass` or `PGPASSWORD` |
| `mssql` | `sqlcmd` | `:db :host :port`, `:user` and `:password`, or Windows authentication (`-E`) when no user. `:trust-cert yes` passes `-C` for servers with a self-signed certificate. `:timeout <seconds>` bounds login and query (default `*org-sql-timeout*`, 60); it is not a hard cap on an unreachable host, and Lem waits while `sqlcmd` runs |
| `db2i` | `isql` (unixODBC) | `:db` is the ODBC data source name, optional `:user :password` |

The client must be on `PATH`. Do not commit passwords in header arguments.

## Not yet

Statistics cookies (`[1/3]`), tag inheritance and completion, recurring timestamps, clocking, capture and refile, `:results value`, Babel sessions, footnotes and LaTeX in export, and a Db2 for i route that does not need ODBC. Heading detection in the editing commands does not skip `*` lines inside src blocks (the agenda and export do). Table column widths count characters, so wide (CJK) text misaligns. Under `lem-vi-mode`, normal-state keys can shadow the keys above; use `M-x` or add normal-state bindings.

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
