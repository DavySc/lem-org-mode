;;; Tests for the pure functions of lem-org-mode. They need only SBCL, not Lem:
;;;
;;;   sbcl --script scripts/pure-test.lisp
;;;
;;; The code between ";;; BEGIN pure ..." and ";;; END pure ..." in the source files is read into a
;;; scratch package (with `editor-error' stubbed) and checked. Exit status 1 when a check fails.

(require :asdf)
(defpackage :org-pure-test (:use :cl))
(in-package :org-pure-test)

(define-condition editor-failure (error) ((message :initarg :message :reader failure-message)))
(defun editor-error (control &rest arguments)
  (error 'editor-failure :message (apply #'format nil control arguments)))

(defvar *root* (merge-pathnames "../" (make-pathname :name nil :type nil :defaults *load-truename*)))

(defun pure-regions (file)
  (with-open-file (in (merge-pathnames file *root*) :external-format :utf-8)
    (let ((inside nil) (text (make-string-output-stream)))
      (loop :for line := (read-line in nil) :while line
            :do (cond ((and (>= (length line) 14) (string= ";;; BEGIN pure" line :end2 14)) (setf inside t))
                      ((and (>= (length line) 12) (string= ";;; END pure" line :end2 12)) (setf inside nil))
                      (inside (write-line line text))))
      (get-output-stream-string text))))

(dolist (file '("org-mode.lisp" "agenda.lisp" "babel-model.lisp" "export.lisp" "babel.lisp"))
  (with-input-from-string (in (pure-regions file))
    (loop :for form := (read in nil :eof) :until (eq form :eof)
          :do (let ((*error-output* (make-broadcast-stream))) (eval form)))))

(defvar *failures* 0)
(defmacro check (name expected form)
  `(let ((actual (handler-case ,form
                   (editor-failure (e) (list :editor-error (failure-message e)))
                   (error (e) (list :lisp-error (princ-to-string e))))))
     (if (equal actual ,expected)
         (format t "PASS ~A~%" ,name)
         (progn (incf *failures*) (format t "FAIL ~A~%  expected ~S~%  got      ~S~%" ,name ,expected actual)))))
(defmacro check-error (name form)
  `(check ,name :error (handler-case (progn ,form :no-error) (editor-failure () :error))))

(defvar *kw* '("TODO" "DONE"))
(defun contains (string part) (and (search part string) t))

;;; tables
(defvar *table* (mapcar #'table-split-line '("| Name | Qty |" "|-|" "| apple | 3 |" "| kiwi | 12.5 |" "| x")))
(check "table split" '(("Name" "Qty") :rule ("apple" "3") ("kiwi" "12.5") ("x")) *table*)
(check "table format"
       '("| Name  |  Qty |" "|-------+------|" "| apple |    3 |" "| kiwi  | 12.5 |" "| x     |      |")
       (org-table-format-rows *table*))
(check "insert column" '("| Name  |   |  Qty |" "|-------+---+------|" "| apple |   |    3 |" "| kiwi  |   | 12.5 |" "| x     |   |      |")
       (org-table-format-rows (table-edit-rows *table* 2 1 :insert-column)))
(check-error "move column past the end" (table-edit-rows *table* 2 1 :move-column-right))
(check "next cell at the end adds a row" '(5 0)
       (multiple-value-bind (rows row column) (table-edit-rows *table* 4 1 :next-cell)
         (declare (ignore rows)) (list row column)))
(check "previous cell skips the rule" '(0 1)
       (multiple-value-bind (rows row column) (table-edit-rows *table* 2 0 :previous-cell)
         (declare (ignore rows)) (list row column)))
(check "move row down" 3 (nth-value 1 (table-edit-rows *table* 2 1 :move-row-down)))

;;; lists
(check "item" '(2 "-" #\Space 8) (multiple-value-list (list-item-parse "  - [ ] foo")))
(check "numbered item" '(0 "10." nil 4) (multiple-value-list (list-item-parse "10. x")))
(check "heading is not an item" nil (list-item-parse "* H"))
(check "toggle empty" "- [ ] foo" (list-toggle-checkbox "- foo"))
(check "toggle on" "- [X] foo" (list-toggle-checkbox "- [ ] foo"))
(check "toggle off" "- [ ] foo" (list-toggle-checkbox "- [X] foo"))
(check "toggle partial" "1. [X] a" (list-toggle-checkbox "1. [-] a"))
(check "new item" "  3. [ ] " (list-new-item "  2. [X] a"))
(check "item extent" 3 (list-item-extent '("- a" "  more" "  - sub" "- b")))
(check "renumber" '("1. a" "   1. x" "   2. y" "2. b" "3. c")
       (list-renumber '("1. a" "   5. x" "   9. y" "7. b" "7. c")))

;;; headings, planning, properties
(check "heading split" '(2 "TODO" #\A "write it" ("work" "home"))
       (multiple-value-list (heading-split "** TODO [#A] write it   :work:home:" *kw*)))
(check "colon in title is not a tag" '(1 nil nil "plain: text" nil)
       (multiple-value-list (heading-split "* plain: text" *kw*)))
(check "priority up" "* [#C] x" (heading-cycle-priority "* x" *kw* 1))
(check "priority wraps to none" "* TODO x" (heading-cycle-priority "* TODO [#A] x" *kw* 1))
(check "tags end at column 77" 77 (length (heading-set-tags "* TODO hi" *kw* '("a" "b"))))
(check "tags clear" "* hi" (heading-set-tags "* hi   :a:" *kw* nil))
(check "tag input" '("a" "b" "c") (parse-tag-input ":a:b c:"))
(check "timestamp" "<2026-10-22 Thu>" (format-timestamp 2026 10 22 t))
(check "date +2w" '(2026 11 5) (multiple-value-list (parse-date-input "+2w" 2026 10 22)))
(check "date iso" '(2027 1 3) (multiple-value-list (parse-date-input "2027-01-03" 2026 10 22)))
(check-error "bad date" (parse-date-input "tomorrow" 2026 1 1))
(check "schedule" '("* TODO x" "SCHEDULED: <2026-10-22 Thu>") (head-set-planning '("* TODO x") "SCHEDULED" "<2026-10-22 Thu>"))
(check "closed goes first" '("* DONE x" "CLOSED: [2026-10-22 Thu] SCHEDULED: <2026-10-22 Thu>")
       (head-set-planning '("* DONE x" "SCHEDULED: <2026-10-22 Thu>") "CLOSED" "[2026-10-22 Thu]"))
(check "remove closed" '("* x") (head-set-planning '("* x" "CLOSED: [2026-10-22 Thu]") "CLOSED" nil))
(check "head length" 4 (head-length '("* x" "SCHEDULED: <2026-10-22 Thu>" ":PROPERTIES:" ":END:" "body")))
(check "new property" '("* x" ":PROPERTIES:" ":ID: 1" ":END:") (head-set-property '("* x") "ID" "1"))
(check "replace property" '("* x" "DEADLINE: <2026-10-22 Thu>" ":PROPERTIES:" ":ID: 2" ":B: y" ":END:")
       (head-set-property '("* x" "DEADLINE: <2026-10-22 Thu>" ":PROPERTIES:" ":ID: 1" ":B: y" ":END:") "id" "2"))

;;; sql helpers
(check "csv" '(("a" "b,c" "d\"e") ("1" "" "3")) (csv-parse (format nil "a,\"b,c\",\"d\"\"e\"~%1,,3~%")))
(check "delimited" '(("id" "name") ("1" "x y"))
       (delimited-parse (format nil "id|name~%--|----~%1|x y~%~%(1 rows affected)~%") #\|))
(check "isql statements" (format nil "select 1;~%select 2;~%") (sql-statements-per-line (format nil "select 1;~%select~%2;")))
(check "psql argv" '("psql" "-X" "-q" "--csv" "-d" "m") (sql-command :psql '(("db" . "m"))))

;;; agenda
(defvar *doc*
  (list "* TODO [#A] Write report :work:" "SCHEDULED: <2026-10-22 Thu>" "body"
        "#+begin_src sh" "* not a heading" "#+end_src"
        "** DONE Old" "DEADLINE: <2026-10-20 Tue>"
        "* TODO Pay rent" "DEADLINE: <2026-10-25 Sun>"
        "* TODO [#C] Overdue" "SCHEDULED: <2026-10-20 Tue 09:30>"))
(defvar *entries* (agenda-parse *doc* *kw* "notes.org"))
(check "stamp date" '(2026 10 22) (multiple-value-list (stamp-date "<2026-10-22 Thu>")))
(check "stamp time" '("14:30" nil) (list (stamp-time "<2026-10-22 Thu 14:30>") (stamp-time "<2026-10-22 Thu>")))
(check "day arithmetic" 1 (- (date-day 2026 10 23) (date-day 2026 10 22)))
(check "entries skip blocks" '("Write report" "Old" "Pay rent" "Overdue") (mapcar (lambda (e) (getf e :title)) *entries*))
(check "entry lines" '(1 7 9 11) (mapcar (lambda (e) (getf e :line)) *entries*))
(check "entry done" '(nil t nil nil) (mapcar (lambda (e) (getf e :done)) *entries*))
(defvar *today* (date-day 2026 10 22))
(check "today's items, highest priority first" '("Scheduled:" "In 3 d.:" "Sched.2x:")
       (mapcar #'car (agenda-day-items *entries* *today* *today* 14)))
(check "deadline day" '("Deadline:") (mapcar #'car (agenda-day-items *entries* (+ *today* 3) *today* 14)))
(check "week shape" 11 (length (agenda-week-records *entries* *today* 7 14)))
(check "week header" "Thursday 22 October 2026" (car (first (agenda-week-records *entries* *today* 7 14))))
(check "week item text" t
       (let ((line (car (second (agenda-week-records *entries* *today* 7 14)))))
         (and (contains line "notes:") (contains line "Scheduled:") (contains line "TODO [#A] Write report :work:"))))
(check "todo list order" '("Write report" "Pay rent" "Overdue")
       (mapcar (lambda (r) (getf (cdr r) :title)) (agenda-todo-records *entries*)))
(check "tag search" '("Write report") (mapcar (lambda (r) (getf (cdr r) :title)) (agenda-tag-records *entries* "WORK")))

;;; export
(check "html escape" "a &lt; b &amp; c" (html-escape "a < b & c"))
(check "inline html" "a <b>b</b> <i>c</i> <code>d*e</code> <a href=\"http://x\">y</a> 1*2*3 <del>z</del>"
       (export-inline "a *b* /c/ =d*e= [[http://x][y]] 1*2*3 +z+" :html))
(check "inline markdown" "a **b** *c* `d` [y](http://x) ~~z~~"
       (export-inline "a *b* /c/ ~d~ [[http://x][y]] +z+" :markdown))
(check "bare link" "<a href=\"x.org\">file:x.org</a>" (export-inline "[[file:x.org]]" :html))
(defvar *org-doc*
  (list "#+TITLE: Notes" "#+AUTHOR: Me" "* TODO [#A] First :t:" "SCHEDULED: <2026-10-22 Thu>"
        ":PROPERTIES:" ":ID: 1" ":END:" "Some *bold* text" "continues here."
        "" "- one" "  - nested [[http://a][A]]" "- two" ""
        "1. [X] done" "2. open" ""
        "| h1 | h2 |" "|----+----|" "| a | 1 |" ""
        "#+begin_src sql :exports both" "select 1;" "#+end_src" "#+RESULTS:" "| 1 |" ""
        "#+begin_src sh" "echo 1 < 2" "#+end_src" "#+RESULTS:" ": 1" ""
        "#+begin_quote" "quoted" "#+end_quote" "# a comment" ": example" "-----"))
(check "parse node kinds" '(:heading :planning :paragraph :list :list :table :src :table :src :quote :example :rule)
       (mapcar #'first (export-parse *org-doc* *kw*)))
(check "parse meta" '(("TITLE" . "Notes") ("AUTHOR" . "Me")) (nth-value 1 (export-parse *org-doc* *kw*)))
(check "paragraph joined" "Some *bold* text continues here." (second (third (export-parse *org-doc* *kw*))))
(check "nested list" '(("-" nil "one" (("-" nil "nested [[http://a][A]]" nil))) ("-" nil "two" nil))
       (third (fourth (export-parse *org-doc* *kw*))))
(defvar *html* (export-html *org-doc* *kw*))
(check "html pieces" t
       (and (contains *html* "<title>Notes</title>") (contains *html* "<h1 class=\"title\">Notes</h1>")
            (contains *html* "<h2><span class=\"todo TODO\">TODO</span> <span class=\"priority\">[#A]</span> First")
            (contains *html* "<ul>") (contains *html* (format nil "<li>one~%<ul>")) (contains *html* "<a href=\"http://a\">A</a>")
            (contains *html* "<ol>") (contains *html* "<code>[X]</code> done")
            (contains *html* "<thead>") (contains *html* "<th>h1</th>") (contains *html* "<td>a</td>")
            (contains *html* "<pre class=\"src src-sql\">") (contains *html* "echo 1 &lt; 2")
            (contains *html* "<blockquote>") (contains *html* "<hr>") (contains *html* "<pre class=\"example\">example</pre>")
            (not (contains *html* "ID: 1")) (not (contains *html* "a comment"))
            (not (contains *html* "<pre class=\"example\">1")))) ; the sh block exports code only
(defvar *md* (export-markdown *org-doc* *kw*))
(check "markdown pieces" t
       (and (contains *md* (format nil "# Notes~%~%## **TODO** [#A] First"))
            (contains *md* (format nil "- one~%   - nested [A](http://a)~%- two"))
            (contains *md* (format nil "1. [X] done~%2. open"))
            (contains *md* (format nil "| h1 | h2 |~%| --- | --- |~%| a | 1 |"))
            (contains *md* (format nil "```sql~%select 1;~%```"))
            (contains *md* (format nil "> quoted")) (contains *md* "---")))
(check "results only for exports both" nil
       (contains (export-html (list "#+begin_src sh" "echo hi" "#+end_src" "#+RESULTS:" ": hi") *kw*) "<pre class=\"example\">"))
(check "results with exports results" '(:example :example)
       (let ((nodes (export-parse (list "#+begin_src sh :exports results" "echo hi" "#+end_src" "#+RESULTS:" ": hi"
                                        "" ": more")
                                  *kw*)))
         (mapcar #'first nodes)))

;;; whole-document renderings used by the Lem tests
(check "markdown with title" (format nil "# T~%~%## Head~%~%text **bold**~%")
       (export-markdown (list "#+TITLE: T" "* Head" "text *bold*") *kw*))
(check "markdown without title" (format nil "# Head~%~%text~%") (export-markdown (list "* Head" "text") *kw*))
(check "html heading" t (contains (export-html (list "* Head" "text") *kw*) "<h2>Head</h2>"))


;;; babel: header arguments
(check "src header" '("sql" (("engine" . "postgres") ("db" . "m") ("flag" . "t")) nil)
       (multiple-value-list (src-header "#+begin_src sql :engine postgres :db m :flag")))
(check "multi-word header values and switches"
       '("python" (("var" . "a=1") ("results" . "output raw") ("exports" . "none")) ("-n"))
       (multiple-value-list (src-header "#+begin_src python -n :var a=1 :results output raw :exports none")))
(check "no language" '(nil (("results" . "silent")))
       (subseq (multiple-value-list (src-header "#+begin_src :results silent")) 0 2))
(check "quoted words" '("a" ":var" "x=\"b c\"" "d") (split-words "a :var x=\"b c\" d"))
(check "results merge by category" "replace output" (merge-results "value replace" "output"))
(check "results merge keeps other categories" "value raw silent" (merge-results "value raw" "silent"))
(check "var merge by name" '(("var" . "y=2") ("var" . "x=3") ("tangle" . "yes"))
       (header-args-merge '(("var" . "x=1") ("var" . "y=2")) '(("var" . "x=3") ("tangle" . "yes"))))
(check "last argument wins" '(("exports" . "both")) (header-args-merge '(("exports" . "code")) '(("exports" . "both"))))

;;; babel: document model
(defvar *babel-doc*
  (list "#+PROPERTY: header-args :results output"
        "#+PROPERTY: header-args:python :var base=10"
        "* H1"
        ":PROPERTIES:"
        ":header-args: :tangle yes"
        ":header-args:sh+: :dir /tmp"
        ":END:"
        "#+NAME: first"
        "#+begin_src python :var x=1"
        "  print(x)"
        "  ,* not a heading"
        "#+end_src"
        ""
        "#+RESULTS[abc123]: first"
        ": 1"
        "** Sub"
        "#+NAME: tbl"
        "| a | b |"
        "|---+---|"
        "| 1 | 2 |"
        "#+begin_src sh :results value"
        "echo hi"
        "#+end_src"
        "#+NAME: items"
        "- one"
        "- two"
        "#+CALL: first(x=2)"))
(defvar *records* (babel-scan *babel-doc* *kw*))
(check "element kinds" '(:src :results :table :src :list :call) (mapcar (lambda (r) (getf r :kind)) *records*))
(check "block name and heading" '("first" "H1" 1) (let ((r (first *records*))) (list (getf r :name) (getf r :heading) (getf r :level))))
(check "inherited args"
       '(("exports" . "code") ("session" . "none") ("noweb" . "no") ("cache" . "no") ("eval" . "yes") ("hlines" . "no")
         ("padline" . "yes") ("comments" . "no") ("results" . "replace output") ("var" . "base=10") ("tangle" . "yes")
         ("var" . "x=1"))
       (getf (first *records*) :args))
(check "sub heading block" '("replace value" "yes" "/tmp")
       (let ((args (getf (fourth *records*) :args)))
         (list (header-arg args "results") (header-arg args "tangle") (header-arg args "dir"))))
(check "body is unescaped" '("  print(x)" "  * not a heading") (getf (first *records*) :body))
(check "table record" '("tbl" (("a" "b") :rule ("1" "2"))) (let ((r (third *records*))) (list (getf r :name) (getf r :rows))))
(check "list record" '("items" ("one" "two")) (let ((r (fifth *records*))) (list (getf r :name) (getf r :items))))
(check "find named" '(:src :table :list :results)
       (list (getf (babel-find-named *records* "first") :kind) (getf (babel-find-named *records* "tbl") :kind)
             (getf (babel-find-named *records* "items") :kind)
             (getf (babel-find-named (list '(:kind :results :name "r")) "r") :kind)))
(check "block at line" 8 (getf (babel-block-at *records* 9) :begin))
(check "results element is not scanned" nil (find :example *records* :key (lambda (r) (getf r :kind))))
(check "call text" "first(x=2)" (getf (sixth *records*) :text))
(check "results header" '(t "foo" "abc") (multiple-value-list (results-header "#+RESULTS[abc]: foo")))
(check "plain results header" '(t nil nil) (multiple-value-list (results-header "#+RESULTS:")))
(check "not a results header" nil (results-header "#+RESULTSX:"))
(check "results extent" '(1 3 "abc") (multiple-value-list (results-extent '("" "#+RESULTS[abc]: foo" ": 1" ": 2" "text") "foo")))
(check "results of another name" nil (results-extent '("#+RESULTS: other" ": 1") "foo"))
(check "unnamed results for a named block" '(0 2 nil) (multiple-value-list (results-extent '("#+RESULTS:" "| a |") "foo")))
(check "element lengths" '(3 3 2 1 0)
       (list (results-element-length '("#+begin_src sh" "x" "#+end_src" "after"))
             (results-element-length '(":RESULTS:" "x" ":END:" "after"))
             (results-element-length '("- a" "  more" "after"))
             (results-element-length '("raw text" "" "x"))
             (results-element-length '("" "x"))))
(check "fixed width run" 2 (results-element-length '(": a" ":" "x")))

;;; babel: data
(check "header and data" '((("1" "2") ("3" "4")) ("a" "b"))
       (multiple-value-list (rows-to-value '(("a" "b") :rule ("1" "2") ("3" "4")) nil)))
(check "no header without a rule" '((("a" "b") ("1" "2")) nil)
       (multiple-value-list (rows-to-value '(("a" "b") ("1" "2")) nil)))
(check "colnames yes" '((("1" "2")) ("a" "b"))
       (multiple-value-list (rows-to-value '(("a" "b") ("1" "2")) '(("colnames" . "yes")))))
(check "hlines yes" '(("1" "2") :rule ("3" "4"))
       (rows-to-value '(("1" "2") :rule ("3" "4")) '(("hlines" . "yes") ("colnames" . "no"))))
(check "rownames" '(("2") ("4")) (strip-rows '(("r1" "2") ("r2" "4")) '(("rownames" . "yes") ("colnames" . "no"))))
(check "single column becomes a list" '("x" "y") (rows-to-value '(("x") ("y")) nil))
(check "json" '(1 2.5d0 "a
b" "true" "null" (1) (("k" 2))) (json-parse "[1,2.5,\"a\\nb\",true,null,[1],{\"k\":2}]"))
(check "json python words" '("True" "None") (json-parse "[true,null]" :true "True" :null "None"))
(check "json unicode escape" "A" (json-parse "\"\\u0041\""))
(check-error "bad json" (json-parse "[1,"))
(check "python table literal" "[[\"a\", 1], [\"b\", \"x y\"]]" (babel-encode :python '(("a" "1") ("b" "x y"))))
(check "number text" "2.5" (number-text 2.5d0))
(check "lua table" "{1, 2, \"a\"}" (babel-encode :lua '("1" 2 "a")))
(check "lisp list" "(list 1 (list 2 3))" (babel-encode :lisp '(1 (2 3))))
(check "r vector and matrix" '("c(1, 2)" "rbind(c(1, 2), c(3, 4))")
       (list (babel-encode :r '(1 2)) (babel-encode :r '((1 2) (3 4)))))
(check "shell table" (format nil "'a	1~%b	2'") (babel-encode :shell '(("a" "1") ("b" "2"))))
(check "powershell array" "@(,@(1, 2), ,@(3, 4))" (babel-encode :powershell '((1 2) (3 4))))
(check "assignments" '("n = 3" "const n = 3;" "my $n = 3;" "$n = 3;" "local n = 3" "n=3" "$n = 3" "n <- 3" "(defparameter n 3)")
       (mapcar (lambda (f) (babel-assignment f "n" "3")) '(:python :js :perl :php :lua :shell :powershell :r :lisp)))
(check "string assignment" "name = \"Ada\"" (babel-assignment :python "name" "Ada"))
(check "shell quoting" "'it'\\''s'" (babel-quote :shell "it's"))
(check "python escapes" "\"a\\\"b\\\\c\\nd\"" (babel-quote :python (format nil "a\"b\\c~%d")))
(check "var bindings" '(("a" . "1") ("b" . "x y") ("c" 1 2))
       (babel-var-values '(("var" . "a=1") ("results" . "x") ("var" . "b=\"x y\"") ("var" . "c=tbl"))
                         (lambda (text) (if (string= text "tbl") (values '(1 2) t) (values nil nil)))))
(check-error "bad var" (babel-var-values '(("var" . "oops")) (lambda (text) (declare (ignore text)) nil)))
(check "plain program" (format nil "n = 3~%print(n)~%") (babel-program :python nil "print(n)" :bindings '(("n" . "3"))))
(check "value program" t
       (let ((text (babel-program :python nil "return 1" :bindings '(("n" . "3")) :value t)))
         (and (contains text "n = 3") (contains text "def __org_main__():") (contains text "    return 1")
              (contains text "json.dumps"))))
(check "prologue and epilogue" (format nil "pre~%body~%post~%")
       (babel-program :shell '(("prologue" . "\"pre\"") ("epilogue" . "\"post\"")) "body"))
(check "php program" t
       (let ((text (babel-program :php nil "return $n;" :bindings '(("n" . "1")) :value t)))
         (and (string= "<?php" (subseq text 0 5)) (contains text "global $n;"))))
(check "session program" t
       (let ((text (babel-program :python nil "print(2)" :session-before '("x = 1"))))
         (and (contains text (format nil "x = 1~%print(~S)" *babel-session-marker*)) (contains text "print(2)"))))
(check "split output" '("junk
" "[1]")
       (multiple-value-list (babel-split-output (format nil "junk~%~A[1]~%" *babel-value-marker*) :value t)))
(check "split session output" '("two
" nil)
       (multiple-value-list (babel-split-output (format nil "one~%~A~%two~%" *babel-session-marker*) :session t)))
(check "substitute vars" "select 1, 2, a b" (substitute-vars "select $a, $ab, $s" '(("a" . "1") ("ab" . 2) ("s" . "a b"))))
(check "element values" '((("1" "2")) "a
b" ("x" "y"))
       (list (babel-element-value '("| 1 | 2 |") '(("colnames" . "no")))
             (babel-element-value '(": a" ": b") nil)
             (babel-element-value '("- x" "- y") nil)))
(check "c main wrapper" t (and (contains (babel-wrap-main :c "puts(\"x\");") "int main()")
                               (string= "int main(){}" (babel-wrap-main :c "int main(){}"))))
(check "escape body" '("  ,* x" "  y" "") (babel-escape-body '("* x" "y" "") 2))
(check "dedent body" '("a" "  b") (babel-body-lines '("  a" "    b") 2))
(check "call parse" '("first" ("1" "x=\"a,b\"" "f(2,3)") ":results raw" ":exports none")
       (multiple-value-list (call-parse "first[:results raw](1, x=\"a,b\", f(2,3))[:exports none]")))
(check "call without arguments" '("first" nil nil "") (multiple-value-list (call-parse "first")))
(check "hash" t (let ((h (babel-hash "abc"))) (and (= (length h) 16) (string= h (babel-hash "abc")) (not (string= h (babel-hash "abd"))))))

;;; babel: results
(check "scalar result" '("#+RESULTS:" ": hi") (format-babel-result "hi" '(("results" . "value replace"))))
(check "multiline scalar" '("#+RESULTS:" ": a" ": b") (format-babel-result (format nil "a~%b~%") nil))
(check "number result" '("#+RESULTS:" ": 42") (format-babel-result 42 nil))
(check "list becomes a column" '("#+RESULTS:" "| a |" "| 2 |") (format-babel-result '("a" 2) nil))
(check "table result" '("#+RESULTS:" "| a | 1 |" "| b | 2 |") (format-babel-result '(("a" 1) ("b" 2)) nil))
(check "table with rule" '("#+RESULTS:" "| h |" "|---|" "| 1 |") (format-babel-result '(("h") :rule ("1")) nil))
(check "header put back" '("#+RESULTS:" "| a |" "|---|" "| 1 |") (format-babel-result '(("1")) nil :header '("a")))
(check "list format" '("#+RESULTS:" "- a" "- b") (format-babel-result '("a" "b") '(("results" . "list"))))
(check "table from text" '("#+RESULTS:" "| 1 | 2 |") (format-babel-result "1 2" '(("results" . "table"))))
(check "raw" '("#+RESULTS:" "*bold*") (format-babel-result "*bold*" '(("results" . "raw"))))
(check "code" '("#+RESULTS:" "#+begin_src python" "x" "#+end_src") (format-babel-result "x" '(("results" . "code")) :language "python"))
(check "html" '("#+RESULTS:" "#+begin_export html" "<b>" "#+end_export") (format-babel-result "<b>" '(("results" . "html"))))
(check "drawer" '("#+RESULTS:" ":RESULTS:" "x" ":END:") (format-babel-result "x" '(("results" . "drawer"))))
(check "wrap" '("#+RESULTS:" "#+begin_quote" "x" "#+end_quote") (format-babel-result "x" '(("wrap" . "quote"))))
(check "file" '("#+RESULTS:" "[[file:out.png]]") (format-babel-result "ignored" '(("results" . "file") ("file" . "out.png"))))
(check "file with description" '("#+RESULTS:" "[[file:o.png][Plot]]")
       (format-babel-result "" '(("results" . "file") ("file" . "o.png") ("file-desc" . "\"Plot\""))))
(check "named and cached header" '("#+RESULTS[ab12]: foo" ": 1") (format-babel-result 1 nil :name "foo" :hash "ab12"))
(check "empty result" nil (format-babel-result "" nil))
(check "empty table" nil (format-babel-result nil nil))

;;; babel: where results go
(check "insert a new result" '(:insert 4 ("" "#+RESULTS:" ": 1")) (result-edit '("next") 3 nil '("#+RESULTS:" ": 1") "replace"))
(check "replace an old result" '(:replace 6 3 ("#+RESULTS:" ": 2"))
       (result-edit '("" "#+RESULTS:" ": 1" ": x" "after") 3 nil '("#+RESULTS:" ": 2") "replace"))
(check "append to a result" '(:insert 8 (": 2"))
       (result-edit '("" "#+RESULTS:" ": 1" ": x" "after") 3 nil '("#+RESULTS:" ": 2") "append"))
(check "prepend to a result" '(:insert 6 (": 2"))
       (result-edit '("" "#+RESULTS:" ": 1" ": x" "after") 3 nil '("#+RESULTS:" ": 2") "prepend"))
(check "remove a result and the blank line before it" '(:delete 4 8) (result-edit '("" "#+RESULTS:" ": 1" ": x" "after") 3 nil nil "replace"))
(check "nothing to remove" nil (result-edit '("after") 3 nil nil "replace"))
(check "other block's result is left alone" '(:insert 4 ("" "#+RESULTS: a" ": 1"))
       (result-edit '("#+RESULTS: b" ": 0") 3 "a" '("#+RESULTS: a" ": 1") "replace"))

;;; babel: noweb
(check "noweb expands with indentation" (format nil "a~%  l1~%  l2~%b")
       (noweb-expand (format nil "a~%  <<x>>~%b") (lambda (n) (and (string= n "x") (format nil "l1~%l2")))))
(check "unknown reference stays" "<<nope>>" (noweb-expand "<<nope>>" (lambda (n) (declare (ignore n)) nil)))
(check "inline reference" "f(1, 2)" (noweb-expand "f(<<args>>)" (lambda (n) (and (string= n "args") "1, 2"))))
(check "nested references" "1" (noweb-expand "<<a>>" (lambda (n) (cond ((string= n "a") "<<b>>") ((string= n "b") "1")))))
(check-error "noweb cycle" (noweb-expand "<<a>>" (lambda (n) (declare (ignore n)) "<<a>>")))
(defvar *noweb-records*
  (babel-scan (list "#+NAME: init" "#+begin_src c" "int x = 0;" "#+end_src"
                    "#+begin_src c :noweb-ref body" "x++;" "#+end_src"
                    "#+begin_src c :noweb-ref body" "x--;" "#+end_src")
              *kw*))
(check "resolver by name" "int x = 0;" (funcall (noweb-resolver *noweb-records*) "init"))
(check "resolver by noweb-ref" (format nil "x++;~%x--;") (funcall (noweb-resolver *noweb-records*) "body"))
(check "resolver unknown" nil (funcall (noweb-resolver *noweb-records*) "zzz"))
(check "noweb contexts" '(nil t nil t t nil nil t nil)
       (list (and (noweb-expands-p '(("noweb" . "no")) :eval) t)
             (and (noweb-expands-p '(("noweb" . "yes")) :eval) t)
             (and (noweb-expands-p '(("noweb" . "tangle")) :eval) t)
             (and (noweb-expands-p '(("noweb" . "tangle")) :tangle) t)
             (and (noweb-expands-p '(("noweb" . "no-export")) :eval) t)
             (and (noweb-expands-p '(("noweb" . "no-export")) :export) t)
             (and (noweb-expands-p '(("noweb" . "strip-export")) :export) t)
             (and (noweb-strips-p '(("noweb" . "strip-export"))) t)
             (and (noweb-strips-p '(("noweb" . "yes"))) t)))
(check "no-expand wins" nil (noweb-expands-p '(("noweb" . "yes") ("no-expand" . "t")) :tangle))

;;; export honours inherited header arguments and noweb
(defvar *noweb-export-html*
  (export-html (list "#+PROPERTY: header-args :exports both"
                     "#+NAME: part" "#+begin_src c :exports none" "int x;" "#+end_src"
                     "#+begin_src c :noweb yes" "<<part>>" "int y;" "#+end_src"
                     "#+begin_src c :noweb strip-export" "<<part>>" "int z;" "#+end_src"
                     "#+begin_src c :noweb tangle" "<<part>>" "int w;" "#+end_src")
               *kw*))
(check "export: header-args exports is inherited" nil (contains *noweb-export-html* (format nil "<code>int x;</code>")))
(check "export: noweb yes expands" t (contains *noweb-export-html* (format nil "<code>int x;~%int y;</code>")))
(check "export: strip-export removes references" t (contains *noweb-export-html* (format nil "<code>~%int z;</code>")))
(check "export: noweb tangle keeps references" t (contains *noweb-export-html* (format nil "<code>&lt;&lt;part&gt;&gt;~%int w;</code>")))

;;; babel: tangle
(defvar *tangle-doc*
  (list "* Setup"
        "#+begin_src python :tangle yes :comments link"
        "print(1)"
        "#+end_src"
        "#+begin_src python :tangle yes :noweb tangle"
        "<<body>>"
        "#+end_src"
        "#+begin_src python :noweb-ref body"
        "print(2)"
        "#+end_src"
        "#+begin_src sh :tangle bin/run.sh :shebang \"#!/bin/sh\" :mkdirp yes"
        "echo hi"
        "#+end_src"
        "#+begin_src python :tangle no"
        "skipped"
        "#+end_src"
        "#+begin_src python :tangle yes :padline no"
        "print(3)"
        "#+end_src"))
(defvar *plan* (babel-tangle-plan (babel-scan *tangle-doc* *kw*) "notes" "notes.org"))
(check "tangle files" '("notes.py" "bin/run.sh") (mapcar (lambda (e) (getf e :file)) *plan*))
(check "tangle text" (format nil "# [[file:notes.org::*Setup][Setup:1]]~%print(1)~%~%print(2)~%print(3)~%")
       (getf (first *plan*) :text))
(check "shebang and mode" (list (format nil "#!/bin/sh~%echo hi~%") #o755 t)
       (list (getf (second *plan*) :text) (getf (second *plan*) :mode) (getf (second *plan*) :mkdirp)))
(check "tangle mode parse" '(#o755 nil) (list (parse-file-mode "(identity #o755)") (parse-file-mode "yes")))
(check "tangle file names" '("n.py" "n.sh" "n.js" "n.xyz" "out/f.c")
       (flet ((name (language tangle) (tangle-file-name (list :language language :args (list (cons "tangle" tangle))) "n")))
         (list (name "python" "yes") (name "bash" "yes") (name "js" "yes") (name "xyz" "yes") (name "c" "\"out/f.c\""))))
(check "tangle off" nil (tangle-file-name (list :language "python" :args '(("tangle" . "no"))) "n"))

;;; running real interpreters (each is skipped when the program is not installed)
(defun program-available-p (argv)
  (handler-case (progn (uiop:run-program argv :ignore-error-status t :output nil :error-output nil) t)
    (error () nil)))

(defun run-block (language body &key args bindings (mode :value))
  "Run BODY like org-babel-execute would and return the value (a Lisp object) or the output."
  (let* ((spec (babel-language-spec language *org-babel-languages*))
         (family (getf (cdr spec) :family))
         (args (append args (list (cons "results" (if (eq mode :value) "value" "output")))))
         (program (multiple-value-list
                   (babel-program family args (babel-wrap-main family body) :bindings bindings :value (eq mode :value))))
         (stdout (babel-run spec (first program) args)))
    (multiple-value-bind (output value-text) (babel-split-output stdout :value (second program))
      (if (second program)
          (destructuring-bind (true false null) (json-keywords family)
            (json-parse value-text :true true :false false :null null))
          (string-right-trim '(#\newline) output)))))

(defmacro with-interpreter ((name probe) &body body)
  `(if (program-available-p ',probe) (progn ,@body) (format t "SKIP ~A: not installed~%" ,name)))

(with-interpreter ("python" ("python3" "--version"))
  (check "python value" '(1 (("a" 2) ("b" 3)))
         (run-block "python" "return [t, [[k, v + t] for k, v in d]]" :bindings '(("t" . 1) ("d" ("a" 1) ("b" 2)))))
  (check "python value json types" '(1 2.5d0 "x" "True" "None" (("k" 1)))
         (run-block "python" "return [1, 2.5, 'x', True, None, {'k': 1}]"))
  (check "python output" (format nil "hi 3~%3")
         (run-block "python" (format nil "print('hi', n)~%print(n)") :bindings '(("n" . "3")) :mode :output))
  (check "python list var" "['a', 1]" (run-block "python" "return str(t)" :bindings '(("t" "a" "1")))))
(with-interpreter ("node" ("node" "--version"))
  (check "node value" '(2 ("x" 1)) (run-block "javascript" "return [n + 1, ['x', 1]]" :bindings '(("n" . "1"))))
  (check "node output" "3" (run-block "js" "console.log(n + 2)" :bindings '(("n" . "1")) :mode :output)))
(with-interpreter ("ruby" ("ruby" "--version"))
  (check "ruby value" '(3 "nil") (run-block "ruby" (format nil "x = n + 2~%[x, nil]") :bindings '(("n" . "1"))))
  (check "ruby output" "hi Ada" (run-block "ruby" "puts \"hi #{name}\"" :bindings '(("name" . "Ada")) :mode :output)))
(with-interpreter ("perl" ("perl" "--version"))
  (check "perl value" '(3 "x") (run-block "perl" (format nil "my @r = ($n + 2, 'x');~%\\@r") :bindings '(("n" . "1"))))
  (check "perl output" "7" (run-block "perl" "print $n + 6;" :bindings '(("n" . "1")) :mode :output)))
(with-interpreter ("lua" ("lua" "-v"))
  (check "lua value" '(3 ("x" 1) "a\"b")
         (run-block "lua" (format nil "return {n + 2, {'x', 1}, 'a\"b'}") :bindings '(("n" . "1"))))
  (check "lua output" (format nil "hi~C7" #\tab) (run-block "lua" "print('hi', n)" :bindings '(("n" . "7")) :mode :output)))
(with-interpreter ("R" ("Rscript" "--version"))
  (check "r value vector" '(3 4) (run-block "r" "c(n + 2, 4)" :bindings '(("n" . "1"))))
  (check "r value scalar" "a\"b" (run-block "r" "'a\"b'"))
  (check "r value matrix" '((1 2) (3 4)) (run-block "r" "matrix(c(1, 2, 3, 4), nrow = 2, byrow = TRUE)"))
  (check "r output" "7" (run-block "r" "cat(n + 6)" :bindings '(("n" . "1")) :mode :output))
  (check "r table var" "6" (run-block "r" "cat(sum(t))" :bindings '(("t" (1 2) (3 0))) :mode :output)))
(with-interpreter ("php" ("php" "--version"))
  (check "php value" '(3 "x") (run-block "php" "return [$n + 2, 'x'];" :bindings '(("n" . "1"))))
  (check "php output" "hi" (run-block "php" "echo 'hi';" :mode :output)))
(with-interpreter ("sbcl" ("sbcl" "--version"))
  (check "lisp value" '(3 ("a" 1)) (run-block "lisp" "(list (+ n 2) (list \"a\" 1))" :bindings '(("n" . "1")))))
(with-interpreter ("bash" ("bash" "--version"))
  (check "bash output" "hi 3" (run-block "sh" "echo hi $n" :bindings '(("n" . "3")) :mode :output))
  (check "bash table var" (format nil "a	1") (run-block "sh" "echo \"$t\"" :bindings '(("t" ("a" "1"))) :mode :output))
  (check-error "failing program" (run-block "sh" "echo oops >&2; exit 3" :mode :output))
  (check "working directory" "/tmp" (run-block "sh" "pwd" :args '(("dir" . "/tmp")) :mode :output))
  (check "cmdline" "a b" (run-block "sh" "echo $@" :args '(("cmdline" . "a b")) :mode :output)))
(with-interpreter ("gcc" ("gcc" "--version"))
  (check "c without main" "6" (run-block "c" "printf(\"%d\\n\", 2 * 3);" :mode :output))
  (check "c with main" "ok" (run-block "c" (format nil "#include <stdio.h>~%int main() { puts(\"ok\"); return 0; }") :mode :output))
  (check-error "c compile error" (run-block "c" "this is not C;" :mode :output)))
(with-interpreter ("g++" ("g++" "--version"))
  (check "c++ without main" "5" (run-block "cpp" "cout << 2 + 3 << endl;" :mode :output)))
(with-interpreter ("go" ("go" "version"))
  (check "go" "go!" (run-block "go" (format nil "package main~%import \"fmt\"~%func main() { fmt.Println(\"go!\") }") :mode :output)))
(with-interpreter ("java" ("java" "-version"))
  (check "java" "hi" (run-block "java" "public class Main { public static void main(String[] a) { System.out.println(\"hi\"); } }" :mode :output)))
(check-error "vars unsupported for go" (babel-assignment :go "x" "1"))

;;; further tests, one file per area, in scripts/pure/
(dolist (file (sort (directory (merge-pathnames "scripts/pure/*.lisp" *root*)) #'string< :key #'namestring))
  (let ((*package* (find-package :org-pure-test)))
    (load file)))

(format t "~%~D failure~:P~%" *failures*)
(sb-ext:exit :code (if (zerop *failures*) 0 1))
