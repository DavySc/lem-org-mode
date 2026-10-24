# lem-org-mode

An Org mode for the [Lem](https://github.com/lem-project/lem) editor, written in Common Lisp. It covers the parts of Emacs Org that people use every day: outlines, tasks with logging and repeaters, the agenda, capture and refile, links, tables with formulas, literate programming (Babel), clocking, and export and publishing to HTML, Markdown, plain text, LaTeX, Org and iCalendar.

Everything is keyed like Emacs Org (`C-c C-t`, `C-c a`, `C-c c`, `C-c C-e` ...). Keys that depend on context act on the table, list item, timestamp or heading at point. What the headless tests cannot reach (key handling in a window, rendering, prompts) is listed in `AGENTS.md`.

## Install

You need a Lem checkout or build. Load the extension from your `init.lisp` (after enabling vi-mode if you use it):

```lisp
(asdf:load-asd #p"/path/to/lem-org-mode/lem-org-mode.asd")
(asdf:load-system :lem-org-mode)
```

Open any `.org` file; the modeline shows `Org`. A typical setup:

```lisp
(setf lem-org-mode:*org-agenda-files* '("~/org/"))
(setf lem-org-mode:*org-todo-keywords* '("TODO" "NEXT" "WAIT(w@/!)" "|" "DONE" "CANCELED(c@)"))
(setf lem-org-mode:*org-default-notes-file* "~/org/inbox.org")
(setf lem-org-mode:*org-capture-templates*
      '(("t" "Task" entry (file+headline "~/org/inbox.org" "Tasks") "* TODO %?\n  %U\n  %a")
        ("j" "Journal" entry (file+datetree "~/org/journal.org") "* %<%H:%M> %?" :empty-lines 1)))
(setf lem-org-mode:*org-refile-targets* '((:current . (:maxlevel 2)) ("~/org/projects.org" . (:maxlevel 2))))
(setf lem-org-mode:*org-publish-project-alist*
      '(("site" :base-directory "~/org/" :publishing-directory "~/public_html/" :recursive t
         :publishing-function org-html-publish-to-html :auto-sitemap t)))
```

## Keys at a glance

| Area | Keys |
|---|---|
| Outline | `Tab` fold / cycle a heading (folded, children, subtree) · `Shift-Tab` cycle the whole buffer · `M-Left/Right` promote / demote · `M-Up/Down` move subtree · `M-Return` new heading or item · `M-Shift-Return` new TODO heading · `C-c C-n/p/f/b/u` move between headings · `C-c C-j` go to a heading · `C-c *` heading ↔ text · `C-c ^` sort |
| Tasks | `C-c C-t` cycle state · `C-c C-x t` choose a state · `Shift-Left/Right` previous / next state · `C-c ,` or `Shift-Up/Down` priority · `C-c C-q` tags · `C-c C-z` note · `C-c C-x e` effort · `C-c #` update `[1/3]` cookies |
| Planning | `C-c C-s` schedule · `C-c C-d` deadline · `C-c .` / `C-c !` timestamp · `Shift-arrows` on a timestamp change it · `C-c C-y` length of a range |
| Clock | `C-c C-x C-i/o/q/j` in / out / cancel / go to · `C-c C-x C-r` clock table |
| Agenda | `C-c a` dispatcher (`a` week, `d` day, `t` todo list, `T` state, `m`/`M` match, `s` search, `#` stuck projects, custom keys) · `M-x org-agenda-file-to-front` / `org-remove-file` add or remove the file of the buffer |
| Capture | `C-c c` capture · in the capture buffer `C-c C-c` finish, `C-c C-k` abort, `C-c C-w` refile · `C-c C-w` refile · `C-c M-w` refile copy · `C-c C-x j` jump to a target |
| Subtrees | `C-c C-x M-w` copy · `C-c C-x C-w` cut · `C-c C-x C-y` paste · `C-c C-x C-s` archive · `C-c C-x a` ARCHIVE tag · `C-c C-x A` archive sibling · `C-c /` sparse tree |
| Links | `C-c C-o` open · `C-c l` store · `C-c C-l` insert / edit · `C-c C-x C-n/p` next / previous link · `C-c C-x f` footnote · `C-c C-x i` ID |
| Tables | `Tab`, `Shift-Tab`, `Return` move · `M-arrows`, `M-Shift-arrows` move / insert / delete rows and columns · `C-c -` rule · `C-c C-c` realign / recompute · `C-c =` column formula · `C-c *` recompute · `C-c +` sum · `C-c |` text → table |
| Lists | `M-Left/Right` outdent / indent · `M-Up/Down` move item · `C-c -` change bullet · `C-c C-c` toggle checkbox |
| Blocks | `C-c C-,` insert a block · `<s` `Tab` expand a template · `C-c '` edit source · `C-c C-x d` drawer · `C-c ;` COMMENT |
| Babel | `C-c C-c` run · `C-c C-v` prefix (see below) |
| Dynamic blocks | `C-c C-c` on `#+BEGIN:` · `C-c C-x C-u` update all |
| Export | `C-c C-e h/m/a/l/o/i` html / markdown / ascii / latex / org / ics · `C-c C-e s` subtree · `C-c C-e e` dispatcher (`hs`, `hb`, `hB`, `hv` ...) · `C-c C-e P` publish |
| Timers | `C-c C-x 0` start · `C-c C-x ,` pause / continue · `C-c C-x _` stop · `C-c C-x .` insert · `C-c C-x -` item |

Customise with `lem-org-mode:` variables, all documented where they are defined (`M-x describe-variable`, or read the `defvar`s at the top of each file).

## Tasks

`*org-todo-keywords*` is one sequence (`("TODO" "DONE")`, or `("TODO" "NEXT" "|" "DONE" "CANCELED")`) or a list of sequences. A buffer may declare its own with `#+TODO: TODO(t) NEXT(n!) | DONE(d@/!)`, `#+SEQ_TODO:` and `#+TYP_TODO:`. `(t)` is a fast-selection key; `!` logs a time stamp and `@` asks for a note, before the `/` on entering the state and after it on leaving. Several done states are fine; `C-c C-t` cycles each sequence.

Changing a state adds `CLOSED: [...]` (`*org-log-done*`; `:note` also asks for a closing note), writes log lines into the `:LOGBOOK:` drawer (`- State "DONE"       from "TODO"       [2026-10-22 Thu 10:00]`, with the note under it; `*org-log-into-drawer*` nil puts them under the heading), stops a running clock, and updates statistics cookies.

**Repeaters.** `SCHEDULED: <2026-10-22 Thu +1w>` (also `++1w` and `.+1w`): finishing the task moves every repeating timestamp on, sets the state back (to `REPEAT_TO_STATE`, `*org-todo-repeat-to-state*`, or the sequence's first keyword), logs the completion and sets `LAST_REPEAT`. `RESET_CHECK_BOXES` clears checkboxes.

**Cookies.** `[/]`, `[%]`, `[1/3]` and `[33%]` on a heading count its child TODOs (or, without any, its checkboxes); on a list item they count its sub-items. Items with sub-items get `[-]`/`[X]` themselves. They update when you change a state or toggle a box; `C-c #` recomputes the buffer.

**Dependencies.** With `*org-enforce-todo-dependencies*` an entry with open children, or under a parent with `ORDERED`, cannot be finished. `C-c C-x o` toggles `ORDERED`.

**Archiving.** `C-c C-x C-s` moves the subtree to `*org-archive-location*` (`%s_archive::`, or `#+ARCHIVE:` / the `ARCHIVE` property), adding `ARCHIVE_TIME`, `ARCHIVE_FILE`, `ARCHIVE_CATEGORY`, `ARCHIVE_TODO`, `ARCHIVE_OLPATH` and `ARCHIVE_ITAGS`. Subtrees tagged `ARCHIVE` stay out of the agenda and export (their heading is kept).

Properties are read with inheritance (`org-entry-get`); `#+PRIORITIES` is replaced by `*org-priority-highest*`, `-lowest` and `-default`.

## Timestamps and planning

`C-c .` asks for a date the way Org does: `+3d`, `-2w`, `++1m`, `fri`, `tomorrow`, `2026-10-22`, `10-22`, `3/15`, `feb 3`, `28`, with a time anywhere (`9:00`, `14:30-15:30`, `3pm`); on an existing timestamp it replaces it. `Shift-Up/Down` changes the year, month, day, hour or minute under the cursor (minutes in steps of five); `Shift-Left/Right` moves a day. Timestamps can carry a repeater (`+1w`) and a warning (`-3d` on deadlines, a delay on scheduled dates). `C-c C-y` on `<a>--<b>` writes `=> 2d 3:30`.

Clocking is as before: `C-c C-x C-i` starts, `C-c C-x C-o` stops (`CLOCK: [..]--[..] =>  1:30` in `:LOGBOOK:`), finishing the task stops it, `C-c C-c` on a CLOCK line repairs its duration. A clock table:

```org
#+BEGIN: clocktable :scope file :maxlevel 2 :block thisweek
#+END:
```

`:scope file|subtree|agenda`, `:block today|yesterday|thisweek|lastweek|thismonth|lastmonth|thisyear|lastyear|2026|2026-10|2026-W43|2026-10-22`, or `:tstart`/`:tend`, `:tags`, `:link t`. `C-c C-c` on the `#+BEGIN:` line fills it. `#+BEGIN: columnview :format "%ITEM %TODO %Effort{:}"` (or a `#+COLUMNS:` line) makes a table of properties with summaries; add writers for your own blocks in `*org-dblock-writers*`.

## Agenda

`C-c a` asks what to show; the commands are also available directly (`org-agenda-week`, `-day`, `-todo-list`, `-tags`, `-tags-todo`, `-todo-keyword`, `-search`, `-list-stuck-projects`).

- **Day / week / fortnight / month / year** (`*org-agenda-span*`, `v d w t m y` in the agenda): SCHEDULED items, DEADLINEs and plain `<timestamps>` and ranges `<a>--<b>` (`(2/3):`), repeaters on every day they fall, overdue items (`Sched. 2x:`, `3 d. ago:`) and approaching deadlines (`In 3 d.:`, per-deadline `-3d` or `*org-deadline-warning-days*`) on today, timed entries first with the time column (`10:00-11:00`) and a time grid (`*org-agenda-grid-times*`). Weeks start on Monday (`*org-agenda-start-on-weekday*`). `l` also shows closed entries and clocked time. Finished entries are hidden (`*org-agenda-skip-done*`); `ARCHIVE` trees are skipped.
- **Match** (`m`, `M` for open tasks only): `+work-home`, `work|home`, `PRIORITY="A"`, `Effort<"1:00"`, `LEVEL=2`, `CATEGORY="x"`, `ITEM="..."`, `SCHEDULED<="<today>"`, `DEADLINE<"<+1w>"`, `{regexp}` on tags, any property, and after `/` a todo part: `/TODO|NEXT`, `/-DONE`, `/!` (open tasks).
- **Search** (`s`): words, `-word`, `"a phrase"`, `{regexp}` over headings and text. **Stuck projects** (`#`, `*org-stuck-projects*`).
- **Navigation and actions in the agenda:** `f`/`b` later / earlier, `.` today, `Return`/`Tab` visit, `n`/`p`, `r`/`g` refresh, `/` filter (`+tag -tag =regexp <30 >1:00`), `|` clear, `t` state, `+`/`-` priority, `:` tags, `C-c C-s`/`C-c C-d` plan, `z` note, `I`/`O` clock, `e` effort, `C-x C-s` save the files, `q` quit.
- **Custom commands** in `*org-agenda-custom-commands*`:

```lisp
(setf lem-org-mode:*org-agenda-custom-commands*
      '(("n" "Next actions" tags-todo "NEXT")
        ("w" "Work week" agenda "" (:span :week :files ("~/org/work.org")))
        ("d" "Day and todos" ((agenda "" (:span :day)) (alltodo "")))))
```

Categories come from `#+CATEGORY:`, the `CATEGORY` property (inherited) or the file name; tags are inherited, with `#+FILETAGS:` (`*org-tags-exclude-from-inheritance*`). Open buffers are read in preference to files on disk.

## Capture and refile

`C-c c` offers the templates of `*org-capture-templates*`: `(KEY DESCRIPTION TYPE TARGET TEMPLATE . OPTIONS)`. Types: `entry`, `item`, `checkitem`, `table-line`, `plain`. Targets: `(file F)`, `(file+headline F "H")`, `(file+olp F "A" "B")`, `(file+datetree F)`, `(file+olp+datetree F "A")`, `(file+regexp F "re")`, `(function fn)`. Options: `:prepend t`, `:empty-lines n`, `:immediate-finish t`, `:jump-to-captured t`. A date tree is `* 2026` / `** 2026-10 October` / `*** 2026-10-22 Thursday`, kept in order.

Template escapes: `%?` cursor, `%t %T %u %U` times (`%^t` asks), `%<%Y-%m-%d>` any time format, `%^{Prompt|default|choice}` and `%\1` to reuse it, `%^g` tags, `%a` link to where you were (`C-c l` first, or the buffer's place), `%i` region, `%c` kill ring, `%f %F` file, `%n` user. `org-capture-string` captures without a prompt for scripts.

`C-c C-w` refiles the subtree to a heading of `*org-refile-targets*` (`(:current . (:maxlevel 2))`, `("file.org" . (:maxlevel 1 :tag "project"))`), `C-c M-w` copies it, `C-c C-x j` visits a target. `*org-reverse-note-order*` puts new entries first.

## Links, targets and footnotes

`[[target][description]]`, `<https://...>` and bare URLs. `C-c C-o` opens: URLs and non-text files with the desktop (`*org-url-open-function*`, `*org-file-apps*`), `file:x.org::*Heading`, `::#custom-id`, `::42`, `::/regexp/`, `id:UUID` (searched in open buffers, the agenda files and `org-id-update-id-locations`), `*Heading`, `#custom-id`, `<<dedicated>>` targets, `#+NAME:`s and plain text, `shell:` after a question. On a timestamp it shows that day's agenda; on a heading tag, the entries with the tag; on a footnote reference, the definition. `C-c l` stores a link (to the heading, or `id:` with `*org-id-link-to-org-use-id*`), `C-c C-l` inserts it. `#+LINK: gh https://github.com/%s` and `*org-link-abbrev-alist*` define abbreviations (`[[gh:lem-project/lem]]`).

Footnotes: `[fn:1]`, `[fn:name]`, `[fn::inline text]`; `C-c C-x f` creates one (definition under `* Footnotes`) or jumps between reference and definition.

## Tables

Alignment, rows and columns as before (numeric columns are right-aligned). **Formulas** in a `#+TBLFM:` line after the table: `$3=$1*$2;%.2f::@>$1=vsum(@I..@>>)`. Fields: `$2`, `@3`, `@2$3`, `@-1`, `@>`/`$>` last, `@<`/`$<` first, ranges `@2$1..@5$1`, `@I..@II` between rules. Functions: `vsum vmean vmax vmin vcount vprod vmedian`, `if`, `abs sqrt round floor ceiling exp ln log10 sin cos tan min max`, comparisons and `&& ||`. `C-c C-c` in the table or on the formula line recomputes; `C-c =` adds a column formula. `C-c |` turns text (tabs, commas, blanks) into a table; `M-x org-table-export-command` and `org-table-import` write and read CSV and TSV; `C-c ^` sorts by the column.

## Lists, blocks and structure

Plain lists: `M-Left/Right` indent / outdent an item with its sub-items, `M-Up/Down` move it (ordered lists renumber), `C-c -` cycles `- + 1. 1)`. `C-c C-,` wraps the region in (or inserts) a quote / src / example / center / verse / comment / export block; `<s` + `Tab` and the other keys of `*org-structure-template-alist*` do the same. Headings inside blocks are text (editing commands skip them). `C-c ;` COMMENTs a subtree. Timers: `C-c C-x 0` starts a relative timer, `C-c C-x -` inserts `- 0:12:34 :: `.

## Source blocks (Babel / literate programming)

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
| `:prologue "..."`, `:epilogue "..."`, `:dir`, `:cmdline`, `:post name(data=*this*)` | text around the body, working directory (relative to the Org file), extra arguments for the program, post-processing by another block |
| `:file-ext png`, `:output-dir img`, `:sep ";"` | a block that writes a file gets its name from `#+NAME:` (or a hash) plus the extension, below `:output-dir`; `:sep` splits a text result into table cells |
| `#+header: :results output` | header arguments on their own lines before a block; they win over the `#+begin_src` line |
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
| `rust`, `typescript`, `awk`, `julia`, `octave`, `scheme` (guile), `ocaml`, `fortran`, `tcl` | `rustc`, `ts-node`, `awk -f`, `julia`, `octave`, `guile`, `ocaml`, `gfortran`, `tclsh` — output only, no `:var` |
| `dot`, `gnuplot`, `mermaid` | `dot -Tpng`, `gnuplot`, `mmdc`: need `:file out.png`; the result is the file link |
| `sql`, `sqlite` | see below (`sqlite` uses `sqlite3 -csv`, `:db file.db`) |

The interpreters must be on `PATH`; change one in `lem-org-mode:*org-babel-languages*` (for example python on Windows: `(setf (getf (cdr (assoc '("python" "py" "python3") lem-org-mode:*org-babel-languages* :test #'equal)) :argv) '("python"))`). There is no timeout: a block that never ends blocks Lem.

**Inline blocks and calls.** `src_python[:var x=3]{return x * 2}` and `call_squares(n=4)` run with `C-c C-c` on them and put ` {{{results(=8=)}}}` after the element; `:exports results` (the inline default) shows only the result in an export.

**References.** `:var x=data[1,2]` (a cell), `data[*,0]` (a column), `data[0:2]` (a range), `data[-1]` (counted from the end); `:var x=other.org:name` takes the named element of another Org file.

**Library of Babel.** `C-c C-v i` adds the named blocks of the buffer to the library, and `*org-babel-library-files*` lists files whose named blocks are always available; `#+CALL:` and `:var` find them in every buffer. `C-c C-v d` splits the block at point, `C-c C-v o` jumps to its result.

Not supported: real interpreter sessions, `:stdin`, running blocks as part of export (run `C-c C-v b` first; export uses the results in the buffer), detangling.

### SQL blocks

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

## Export and publishing

`C-c C-e h` HTML, `m` Markdown, `a` plain text, `l` LaTeX, `o` Org (includes, macros and filters applied), `i` iCalendar; `C-c C-e s` the subtree at point (its heading is the title, `EXPORT_FILE_NAME`/`EXPORT_*` properties apply); `C-c C-e e` takes keys such as `hs` (subtree), `hb` (body only), `hB` (into a buffer), `hv` (open afterwards). Files are written next to the Org file.

Handled: `#+TITLE/AUTHOR/DATE/EMAIL/LANGUAGE/DESCRIPTION/KEYWORDS`, `#+OPTIONS:` (`toc:t|N num:t|N H:3 ^:{} tags todo pri p:t e f author date email title creator - * | timestamp tasks stat`), `#+SETUPFILE:`, `#+INCLUDE: "f.org" :lines "5-10" :minlevel 2` (also `src lang`, `example`, `quote`), `#+MACRO:` and `{{{name(args)}}}` with `{{{title}}} {{{author}}} {{{date(fmt)}}} {{{time(fmt)}}} {{{input-file}}} {{{modification-time(fmt)}}} {{{keyword(K)}}} {{{results(...)}}}`, `#+EXCLUDE_TAGS:` (default `noexport`), `#+SELECT_TAGS:`, COMMENT and `ARCHIVE` subtrees, `#+TOC: headlines 2`, nested sections with numbers and ids (`CUSTOM_ID`, `UNNUMBERED`), headings deeper than `H` as lists, `#+CAPTION:`, `#+NAME:`, `#+ATTR_HTML: :width 300 :alt ...` for tables and images (with `Figure n:` / `Table n:` / `Listing n:`), description lists, footnotes, `\alpha` entities (about a hundred), `x_{1}` and `x^{2}` (`^:t` also accepts `x_1`), `$x$` / `\(x\)` / `\[x\]` (MathJax in HTML, `*org-export-mathjax*`), `--`, `---`, `...`, `\\` line breaks, `#+HTML:` / `#+LATEX:` / `#+begin_export`, `@@html:...@@`, `src_lang{code}`, timestamps, links between exported Org files (`file:b.org` becomes `b.html`), images, `[[*Heading]]`, `[[#id]]`, `<<targets>>`. `:exports code|results|both|none` on source blocks as before. Drawers, comments and `:PROPERTIES:` are not exported. `*org-export-html-head*`, `*org-export-with-toc*`, `*org-export-with-section-numbers*`, `*org-export-headline-levels*`, `*org-latex-class*`, `*org-latex-packages*`, `*org-ascii-text-width*` set defaults.

Org's `^:t` is `^:{}` here by default so that `snake_case` stays text.

**Publishing.** `*org-publish-project-alist*` (see Install) with `:base-directory :base-extension :publishing-directory :recursive :exclude :include :publishing-function :components :auto-sitemap :sitemap-title :sitemap-filename :sitemap-style :sitemap-sort-files :with-toc :section-numbers :headline-levels :html-head ... :language :author`. Publishing functions: `org-html-publish-to-html`, `org-md-publish-to-md`, `org-ascii-publish-to-txt`, `org-latex-publish-to-latex`, `org-icalendar-publish-to-ics`, `org-publish-attachment` (copy). Only changed files are published (a record is kept in `*org-publish-timestamp-directory*`); `M-x org-publish` takes a project name (`name!` forces everything), `all`, or use `org-publish-all`, `org-publish-current-file`, `org-publish-current-project`.

## Not yet

Real interpreter sessions for Babel, column view editing, habits, attachments, `org-protocol`, diary sexps, Org mode's countdown timers, speed keys, the ODT backend, and table formulas in Lisp. Table column widths count characters, so wide (CJK) text misaligns. Under `lem-vi-mode`, `Tab` is taken over in normal state (Org buffers get `org-cycle`, others keep the vi jump) and all other keys above work in normal and insert state, except `Shift-Up` / `Shift-Down` (and `Shift-Left/Right`) which insert state binds to movement; use `M-x org-shift-up` there. `C-c a a/t/m` became `C-c a` plus the key, as in Org.

## Test

Three layers, all runnable in a terminal (see `AGENTS.md`):

```
scripts/headless/setup.sh          # once: builds a Lem without a window (needs git, make, python3, sbcl, github.com)
scripts/headless/run.sh            # pure functions, Rove tests, smoke test
scripts/headless/run.sh pure       # only the pure functions: no Lem needed, seconds
```

A prebuilt `lem.exe` does not include Rove, so the smoke test also runs inside a window and writes PASS/FAIL lines to `scripts/smoke.log`:

```
lem -q --eval "(load \"/path/to/lem-org-mode/scripts/smoke.lisp\")"
```

## License

MIT, see `LICENSE`.
