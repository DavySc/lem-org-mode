;;; Tests for the pure functions of lem-org-mode. They need only SBCL, not Lem:
;;;
;;;   sbcl --script scripts/pure-test.lisp
;;;
;;; The code between ";;; BEGIN pure ..." and ";;; END pure ..." in the source files is read into a
;;; scratch package (with `editor-error' stubbed) and checked. Exit status 1 when a check fails.

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

(dolist (file '("org-mode.lisp" "agenda.lisp" "export.lisp" "babel.lisp"))
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

;;; sql and babel header
(check "src header" '("sql" (("engine" . "postgres") ("db" . "m") ("flag" . "t")))
       (multiple-value-list (src-header "#+begin_src sql :engine postgres :db m :flag")))
(check "quoted words" '("a" ":var" "x=\"b c\"" "d") (split-words "a :var x=\"b c\" d"))
(check "repeated :var" '(("var" . "a=1") ("var" . "b=\"x y\""))
       (nth-value 1 (src-header "#+begin_src python :var a=1 :var b=\"x y\"")))
(check "csv" '(("a" "b,c" "d\"e") ("1" "" "3")) (csv-parse (format nil "a,\"b,c\",\"d\"\"e\"~%1,,3~%")))
(check "delimited" '(("id" "name") ("1" "x y"))
       (delimited-parse (format nil "id|name~%--|----~%1|x y~%~%(1 rows affected)~%") #\|))
(check "isql statements" (format nil "select 1;~%select 2;~%") (sql-statements-per-line (format nil "select 1;~%select~%2;")))
(check "psql argv" '("psql" "-X" "-q" "--csv" "-d" "m") (sql-command :psql '(("db" . "m"))))
(check "sql results" '("#+RESULTS:" "| id |" "|----|" "|  1 |") (results-lines '(("id") ("1"))))
(check "old results length" 4 (results-region-length '("" "#+RESULTS:" "| a |" ": x" "text")))
(check "no old results" 0 (results-region-length '("" "text")))

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

;;; babel languages
(check "language lookup" "py" (getf (cdr (babel-language-spec "Python" *org-babel-languages*)) :extension))
(check "unknown language" nil (babel-language-spec "cobol" *org-babel-languages*))
(check "python vars" '("n = 3" "name = \"Ada\"") (babel-var-assignments :python '(("var" . "n=3") ("var" . "name=\"Ada\"") ("results" . "table"))))
(check "shell quoting" "'it'\\''s'" (babel-quote :shell "it's"))
(check "python escapes" "\"a\\\"b\\\\c\\nd\"" (babel-quote :python (format nil "a\"b\\c~%d")))
(check "js var" "const x = \"hi\";" (babel-assignment :js "x" "hi"))
(check "powershell var" "$x = 4" (babel-assignment :powershell "x" "4"))
(check "program" (format nil "n = 3~%print(n)~%") (babel-program :python '(("var" . "n=3")) (format nil "print(n)~%")))
(check-error "bad var" (babel-var-assignments :python '(("var" . "oops"))))
(check "output results" '("#+RESULTS:" ": a" ": b") (output-result-lines (format nil "a~%b~%~%")))
(check "empty output" '("#+RESULTS:") (output-result-lines ""))
(check "table results" '("#+RESULTS:" "| a | b |" "| 1 | 2 |") (table-result-lines (format nil "a b~%1 2~%")))
(check "tab table results" '("#+RESULTS:" "| x y | b |") (table-result-lines (format nil "x y~Cb~%" #\tab)))
(check "tangle"
       (list (cons "notes.py" (format nil "print(1)~%~%print(2)~%"))
             (cons "out.sh" (format nil "echo hi~%")))
       (tangle-collect (list "#+begin_src python :tangle yes" "print(1)" "#+end_src" "text"
                             "#+begin_src sh :tangle out.sh" "echo hi" "#+end_src"
                             "#+begin_src python :tangle no" "skipped" "#+end_src"
                             "#+begin_src python :tangle yes" "print(2)" "#+end_src")
                       "notes"))

(format t "~%~D failure~:P~%" *failures*)
(sb-ext:exit :code (if (zerop *failures*) 0 1))
