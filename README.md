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
| `C-c a a` / `t` / `m` | `org-agenda-week` / `-todo-list` / `-tags` | week agenda / global TODO list / entries with a tag (see Agenda) |
| `C-c C-e h` / `m` | `org-export-html` / `org-export-markdown` | export the buffer to `.html` / `.md` next to the file |
| `C-c C-v t` | `org-babel-tangle` | write blocks with `:tangle` to files |

Numeric table columns are right-aligned. Customise the TODO keywords with `lem-org-mode:*org-todo-keywords*` (default `("TODO" "DONE")`).

### Agenda

`C-c a a` shows the next `*org-agenda-span*` days (default 7): items `SCHEDULED` or `DEADLINE` on each day, overdue scheduled items (`Sched. 2x:`) and deadlines due within `*org-deadline-warning-days*` (`In 3 d.:`) on today. `C-c a t` lists every open TODO, highest `[#A]` priority first; `C-c a m` lists entries with a tag. `Return` on a line jumps to the entry, `q` closes the agenda.

It reads the current buffer. To read several files set, in `init.lisp`, `(setf lem-org-mode:*org-agenda-files* '("~/org/work.org" "~/org/home/"))`; directories contribute their `.org` files (files are read from disk, so save first). Repeaters (`+1w`) and time ranges are not understood; a time such as `<2026-10-22 Thu 14:30>` is shown as `14:30 Sched.:`.

### Export

`C-c C-e h` writes `file.html` and `C-c C-e m` writes `file.md` next to the buffer's file (or opens a buffer when it has none). Supported: `#+TITLE:` / `#+AUTHOR:`, headings with TODO, priority and tags, planning lines, paragraphs, nested and numbered lists with checkboxes, tables (a rule after the first rows makes them the header), `src` / `example` / `quote` / `center` / `verse` blocks, `: ` example lines, `#+begin_export html|md` passthrough, links, `*bold* /italic/ _underline_ =code= ~code~ +strike+`. `:PROPERTIES:` drawers and `#+begin_comment` / `# ` comments are dropped. `:exports code` (default) shows the block, `results` its result, `both` both, `none` neither. No footnotes, LaTeX or images.

### Source blocks (Babel / literate programming)

Everything here follows Org's header-argument model. `C-c C-c` (or `C-c C-v e`) runs the block or `#+CALL:` line at point.

```org
#+PROPERTY: header-args:python :results value
#+NAME: squares
#+begin_src python :var n=4
return [[i, i * i] for i in range(n)]
#+end_src

#+begin_src sh :var table=squares :results output
echo "$table"
#+end_src

#+CALL: squares(n=10)
```

| Key | Command | Does |
|---|---|---|
| `C-c C-c`, `C-c C-v e` | `org-babel-execute-src-block` | run the block / `#+CALL:` at point |
| `C-c C-v b` / `s` | `org-babel-execute-buffer` / `-subtree` | run every block under the buffer / heading, top to bottom |
| `C-c C-v n` / `p` | `org-babel-next-src-block` / `-previous-` | move between blocks |
| `C-c C-v g` | `org-babel-goto-named-src-block` | jump to a `#+NAME:`d block, table, list or result |
| `C-c C-v v` | `org-babel-expand-src-block` | show the program that would run (variables, noweb, wrappers) |
| `C-c C-v k` | `org-babel-remove-result` | delete the block's result |
| `C-c C-v I` | `org-babel-info` | language and effective header arguments |
| `C-c C-v t` | `org-babel-tangle` | write blocks with `:tangle` to their files |
| `C-c '` | `org-edit-src-block` | edit the body in its own buffer in the language's mode; `C-c '` writes back, `C-c C-k` discards |

**Header arguments** are inherited, later wins: `*org-babel-default-header-args*`, then `#+PROPERTY: header-args` and `header-args:LANG` lines, then `:header-args:` / `:header-args:LANG:` in the `:PROPERTIES:` drawers of the enclosing headings, then the block. `:var` accumulates by name; `:results` tokens replace by category.

| Argument | Meaning |
|---|---|
| `:var x=value`, `:var x=name`, `:var x=name(a=1, 2)` | a number, `"a string"`, or a reference to a `#+NAME:`d table, list, example, `#+RESULTS: name`, or source block (which is run). Tables, lists and values become native lists / arrays / vectors in python, js, ruby, perl, php, lua, R, powershell and lisp, and tab-separated text in shell. In `sql`, `$x` in the query is replaced. |
| `:colnames yes\|no`, `:hlines yes\|no`, `:rownames yes` | a table's header row (taken off the input when followed by a rule, and put back on a table result), horizontal lines, first column |
| `:results value\|output` | what to collect (default `value`, Org's default): the value the block returns — `return` in python and js, the last expression in ruby / perl / R / lisp / powershell, the output of a shell block — or everything printed |
| `:results table\|list\|scalar\|verbatim\|file` | the shape; lists and tables are detected from the value by default |
| `:results raw\|code\|drawer\|html\|latex\|org` and `:wrap [name]` | how to wrap it |
| `:results replace\|append\|prepend\|silent` | what to do with an older result |
| `:file out.png :file-desc "Plot"` | with `:results file`, the result is a link `[[file:out.png]]` (the code writes the file) |
| `:session name` | later blocks with the same session and language see the variables of earlier ones. Emulated: the earlier blocks are run again, with their output discarded, so side effects repeat. `M-x org-babel-reset-sessions` forgets them. |
| `:cache yes` | the result carries a hash (`#+RESULTS[hash]:`); an unchanged block is not run again |
| `:eval never\|no\|query` | refuse to run, or ask first (`*org-babel-confirm-evaluate*` asks for every block) |
| `:noweb yes\|tangle\|no-export\|strip-export\|eval\|query\|no` | expand `<<name>>` by the body of the block with that `#+NAME:`, or by all blocks with `:noweb-ref name` (joined by `:noweb-sep`, default newline); indentation is kept; `<<name(1)>>` runs the block |
| `:prologue "..."`, `:epilogue "..."`, `:dir`, `:cmdline`, `:post name(data=*this*)` | text around the body, working directory, extra arguments for the program, post-processing by another block |
| `:exports code\|results\|both\|none` | what export shows (also from inherited arguments) |
| `:tangle yes\|no\|file`, `:mkdirp yes`, `:shebang "#!/bin/sh"`, `:tangle-mode (identity #o755)`, `:padline no`, `:comments link\|org\|both` | tangling: `yes` names the file after the Org file; `:shebang` makes the file executable; `:comments` adds a comment with a link back to the Org heading |

`#+CALL: name[inside header args](arguments)[end header args]` runs a named block with other variables and writes `#+RESULTS: name` under the call line. `#+NAME:` also names tables, lists and example blocks for `:var`.

| Language | Run with |
|---|---|
| `sh` / `bash`, `zsh` | `bash`, `zsh` |
| `python` | `python3` |
| `javascript` / `js` | `node` |
| `ruby`, `perl`, `php`, `lua` | `ruby`, `perl`, `php`, `lua` |
| `r` | `Rscript` |
| `powershell` / `pwsh` | `pwsh -NoProfile -File` |
| `lisp` / `sbcl` | `sbcl --script` |
| `c`, `cpp` | `gcc` / `g++`, then the program; a block without `main` is wrapped in one, as in Org |
| `go`, `java`, `haskell` | `go run`, `java` (single-file mode), `runghc` — output only, no `:var` |
| `sql` | see below |

The interpreters must be on `PATH`; change one in `lem-org-mode:*org-babel-languages*` (for example python on Windows: `(setf (getf (cdr (assoc '("python" "py" "python3") lem-org-mode:*org-babel-languages* :test #'equal)) :argv) '("python"))`). There is no timeout: a block that never ends blocks Lem.

Not supported: inline `src_python{}` / `call_name()`, the Library of Babel, `:var` indexes (`name[1,2]`) and references to other files, real interpreter sessions, `:stdin`, running blocks as part of export (run `C-c C-v b` first; export uses the results in the buffer), detangling.

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
| `mssql` | `sqlcmd` | `:db :host :port`, `:user` and `:password`, or Windows authentication (`-E`) when no user |
| `db2i` | `isql` (unixODBC) | `:db` is the ODBC data source name, optional `:user :password` |

The client must be on `PATH`. Do not commit passwords in header arguments.

## Not yet

Statistics cookies (`[1/3]`), tag inheritance and completion, recurring timestamps, clocking, capture and refile, footnotes and LaTeX in export, the Babel items listed above, and a Db2 for i route that does not need ODBC. Heading detection in the editing commands does not skip `*` lines inside src blocks (the agenda and export do). Table column widths count characters, so wide (CJK) text misaligns. Under `lem-vi-mode`, normal-state keys can shadow the keys above; use `M-x` or add normal-state bindings.

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
