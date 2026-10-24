(defpackage :lem-org-mode/tests
  (:use :cl :rove :lem :lem-org-mode))
(in-package :lem-org-mode/tests)

;;; Without a window system `message' has nowhere to print; collect the messages instead.
(defvar *messages* '())
(defmethod lem-core::show-message :around (value &key timeout)
  (declare (ignore timeout))
  (push value *messages*))

;;; Without a window `switch-to-buffer' has nothing to switch; make the buffer current instead.
(setf lem-org-mode::*org-switch-function* (lambda (buffer) (setf (lem:current-buffer) buffer)))

(defun make-org-buffer (text)
  (let ((buffer (make-buffer nil :temporary t)))
    (insert-string (buffer-point buffer) text)
    buffer))

(defun text-of (buffer)
  (points-to-string (buffer-start-point buffer) (buffer-end-point buffer)))

(defun line-point (buffer line)
  (move-to-line (copy-point (buffer-start-point buffer) :temporary) line))

(deftest test-mode-registration
  (ok (eq (get-file-mode "notes.org") 'org-mode))
  (ok (subtypep 'org-mode 'lem/language-mode:language-mode)))

(deftest test-heading-level
  (let ((buffer (make-org-buffer (format nil "* One~%** Two~%not a heading~%*bold*~%***~%"))))
    (ok (eql 1 (org-heading-level (line-point buffer 1))))
    (ok (eql 2 (org-heading-level (line-point buffer 2))))
    (ok (null (org-heading-level (line-point buffer 3))))
    (ok (null (org-heading-level (line-point buffer 4))))
    (ok (null (org-heading-level (line-point buffer 5))))))

(deftest test-subtree-bounds
  (let ((buffer (make-org-buffer (format nil "* A~%body~%** A1~%* B~%tail"))))
    (multiple-value-bind (start end) (org-subtree-bounds (line-point buffer 1))
      (ok (= 1 (line-number-at-point start)))
      (ok (= 4 (line-number-at-point end))))
    (multiple-value-bind (start end) (org-subtree-bounds (line-point buffer 4))
      (ok (= 4 (line-number-at-point start)))
      (ok (point= end (buffer-end-point buffer))))
    (ok (null (org-subtree-bounds (line-point buffer 2))))))

(deftest test-cycle-todo
  (let ((buffer (make-org-buffer (format nil "** write tests~%"))))
    (org-cycle-todo (line-point buffer 1))
    (ok (string= (format nil "** TODO write tests~%") (text-of buffer)))
    (org-cycle-todo (line-point buffer 1))
    (ok (string= (format nil "** DONE write tests~%") (text-of buffer)))
    (org-cycle-todo (line-point buffer 1))
    (ok (string= (format nil "** write tests~%") (text-of buffer)))))

(deftest test-shift-subtree
  (let ((buffer (make-org-buffer (format nil "* A~%** A1~%* B~%"))))
    (org-shift-subtree (line-point buffer 1) 1)
    (ok (string= (format nil "** A~%*** A1~%* B~%") (text-of buffer)))
    (org-shift-subtree (line-point buffer 1) -1)
    (ok (string= (format nil "* A~%** A1~%* B~%") (text-of buffer)))
    (ok (signals (org-shift-subtree (line-point buffer 1) -1) 'editor-error))))

(deftest test-move-subtree
  (let ((buffer (make-org-buffer (format nil "* A~%a~%* B~%b"))))
    (let ((point (line-point buffer 1)))
      (org-move-subtree point :down)
      (ok (string= (format nil "* B~%b~%* A~%a") (text-of buffer)))
      (ok (= 3 (line-number-at-point point))))
    (let ((point (line-point buffer 3)))
      (org-move-subtree point :up)
      (ok (string= (format nil "* A~%a~%* B~%b") (text-of buffer)))
      (ok (= 1 (line-number-at-point point))))
    (ok (signals (org-move-subtree (line-point buffer 3) :down) 'editor-error))))

(deftest test-table-format
  (ok (equal (org-table-format-rows (list '("Name" "Qty") :rule '("apple" "3") '("kiwi" "12.5")))
             (list "| Name  |  Qty |"
                   "|-------+------|"
                   "| apple |    3 |"
                   "| kiwi  | 12.5 |"))))

(deftest test-table-edit
  (let ((buffer (make-org-buffer (format nil "|a|bb~%|ccc|d|~%"))))
    (let ((point (line-point buffer 1)))
      (org-table-edit point :align)
      (ok (string= (format nil "| a   | bb |~%| ccc | d  |~%") (text-of buffer)))
      (setf (point-charpos point) 2)
      (org-table-edit point :next-cell)
      (ok (= 1 (line-number-at-point point)))
      (ok (= 8 (point-charpos point)))
      (org-table-edit point :insert-column)
      (ok (string= (format nil "| a   |   | bb |~%| ccc |   | d  |~%") (text-of buffer)))
      (org-table-edit point :move-row-down)
      (ok (string= (format nil "| ccc |   | d  |~%| a   |   | bb |~%") (text-of buffer)))
      (ok (= 2 (line-number-at-point point))))
    (ok (signals (org-table-edit (line-point buffer 1) :previous-cell) 'editor-error))))

(deftest test-list-items
  (let ((buffer (make-org-buffer (format nil "1. a~%   - [ ] sub~%2. b~%"))))
    (let ((point (line-point buffer 1)))
      (org-insert-item point)
      (ok (string= (format nil "1. a~%   - [ ] sub~%2. ~%3. b~%") (text-of buffer)))
      (ok (= 3 (line-number-at-point point))))
    (let ((point (line-point buffer 2)))
      (setf (point-charpos point) 5)
      (org-toggle-checkbox point)
      (ok (string= "   - [X] sub" (line-string point)))
      (ok (= 5 (point-charpos point))))
    (ok (signals (org-toggle-checkbox (line-point (make-org-buffer "plain text") 1)) 'editor-error)
        "toggling needs an item")))

(deftest test-heading-attributes
  (let ((buffer (make-org-buffer (format nil "* TODO write~%body~%* next~%"))))
    (org-cycle-priority (line-point buffer 2) 1)
    (ok (string= "* TODO [#C] write" (line-string (line-point buffer 1))))
    (org-set-tags (line-point buffer 1) '("work"))
    (ok (string= ":work:" (subseq (line-string (line-point buffer 1)) (- 77 6))))
    (org-set-planning (line-point buffer 1) "SCHEDULED" "<2026-10-22 Thu>")
    (ok (string= "SCHEDULED: <2026-10-22 Thu>" (line-string (line-point buffer 2))))
    (org-set-property (line-point buffer 1) "ID" "7")
    (ok (equal (list ":PROPERTIES:" ":ID: 7" ":END:" "body")
               (loop :for n :from 3 :to 6 :collect (line-string (line-point buffer n)))))
    (ok (string= "* next" (line-string (line-point buffer 7))))))

(deftest test-babel-bounds
  (let ((buffer (make-org-buffer (format nil "#+begin_src sql :engine postgres~%select 1;~%#+end_src~%"))))
    (ok (src-block-bounds (line-point buffer 2)))
    (ok (null (src-block-bounds (buffer-end-point buffer))) "no block after the end")))

(deftest test-export
  (let ((html (lem-org-mode::export-html (list "* Head" "text") '("TODO" "DONE")))
        (md (lem-org-mode::export-markdown (list "* Head" "text") '("TODO" "DONE"))))
    (ok (search "Head</h2>" html))
    (ok (string= (format nil "# Head~%~%text~%") md))))

(deftest test-agenda-parse
  (let ((entries (lem-org-mode::agenda-parse (list "* TODO a" "SCHEDULED: <2026-10-22 Thu>" "* DONE b")
                                             '("TODO" "DONE") "x.org")))
    (ok (equal '("a" "b") (mapcar (lambda (e) (getf e :title)) entries)))
    (ok (getf (second entries) :done))))

;;; Babel. These run real interpreters and skip what is not installed.

(defun available-p (&rest argv)
  (handler-case (progn (uiop:run-program argv :ignore-error-status t :output nil :error-output nil) t)
    (error () nil)))

(defun lines (&rest strings)
  (format nil "~{~A~%~}" strings))

(defun run-at (buffer line)
  (org-babel-execute (line-point buffer line)))

(defmacro with-program ((&rest argv) &body body)
  `(if (available-p ,@argv)
       (progn ,@body)
       (skip ,(format nil "~A is not installed" (first argv)))))

(deftest test-babel-shell-result
  (with-program ("bash" "--version")
    (let ((buffer (make-org-buffer (lines "#+begin_src sh" "echo hi" "#+end_src"))))
      (run-at buffer 2)
      (ok (string= (lines "#+begin_src sh" "echo hi" "#+end_src" "" "#+RESULTS:" ": hi") (text-of buffer)))
      (run-at buffer 2)
      (ok (string= (lines "#+begin_src sh" "echo hi" "#+end_src" "" "#+RESULTS:" ": hi") (text-of buffer))
          "a rerun replaces the result"))))

(deftest test-babel-variables-and-references
  (with-program ("python3" "--version")
    (let ((buffer (make-org-buffer
                   (lines "#+NAME: nums" "| 1 |" "| 2 |" "| 3 |"
                          "#+NAME: total" "#+begin_src python :var xs=nums" "return sum(xs)" "#+end_src"
                          "#+begin_src python :var t=total :var k=10" "return t * k" "#+end_src"))))
      (run-at buffer 10)
      (ok (search (lines "#+RESULTS:" ": 60") (text-of buffer)) "a block uses the value of another block")
      (ok (not (search ": 6" (subseq (text-of buffer) 0 (search "t * k" (text-of buffer)))))
          "the referenced block's own result is not written"))))

(deftest test-babel-tables
  (with-program ("python3" "--version")
    (let ((buffer (make-org-buffer
                   (lines "#+NAME: data" "| n | sq |" "|---+----|" "| 1 | 1 |" "| 2 | 4 |"
                          "#+begin_src python :var d=data" "return [[r[0], r[1] + 1] for r in d]" "#+end_src"))))
      (run-at buffer 7)
      (ok (search (lines "#+RESULTS:" "| n | sq |" "|---+----|" "| 1 |  2 |" "| 2 |  5 |") (text-of buffer))
          "the header is taken off the input and put back on the output"))))

(deftest test-babel-call
  (with-program ("python3" "--version")
    (let ((buffer (make-org-buffer
                   (lines "#+NAME: double" "#+begin_src python :var n=1" "return n * 2" "#+end_src"
                          "#+CALL: double(n=21)"))))
      (run-at buffer 5)
      (ok (search (lines "#+CALL: double(n=21)" "" "#+RESULTS: double" ": 42") (text-of buffer))))))

(deftest test-babel-result-handling
  (with-program ("bash" "--version")
    (let ((buffer (make-org-buffer (lines "#+begin_src sh :results output append" "echo a" "#+end_src"))))
      (run-at buffer 2)
      (run-at buffer 2)
      (ok (search (lines "#+RESULTS:" ": a" ": a") (text-of buffer)) "append"))
    (let ((buffer (make-org-buffer (lines "#+begin_src sh :results silent" "echo a" "#+end_src"))))
      (run-at buffer 2)
      (ok (string= (lines "#+begin_src sh :results silent" "echo a" "#+end_src") (text-of buffer)) "silent"))
    (let ((buffer (make-org-buffer (lines "#+begin_src sh :results output raw" "echo '*x*'" "#+end_src"))))
      (run-at buffer 2)
      (ok (search (lines "#+RESULTS:" "*x*") (text-of buffer)) "raw"))
    (let ((buffer (make-org-buffer (lines "#+begin_src sh :results output code" "echo x" "#+end_src"))))
      (run-at buffer 2)
      (ok (search (lines "#+RESULTS:" "#+begin_src sh" "x" "#+end_src") (text-of buffer)) "code"))
    (let ((buffer (make-org-buffer (lines "#+begin_src sh" "echo hi" "#+end_src"))))
      (run-at buffer 2)
      (lem-org-mode::babel-remove-result buffer 2 nil)
      (ok (string= (lines "#+begin_src sh" "echo hi" "#+end_src") (text-of buffer)) "remove"))))

(deftest test-babel-cache
  (with-program ("bash" "--version")
    (let* ((counter (namestring (merge-pathnames (format nil "org-babel-counter-~D" (random 1000000)) (uiop:temporary-directory))))
           (buffer (make-org-buffer (lines "#+begin_src sh :cache yes :results output"
                                           (format nil "echo x >> ~A" counter) (format nil "wc -l < ~A" counter) "#+end_src"))))
      (unwind-protect
           (progn (run-at buffer 2)
                  (run-at buffer 2)
                  (ok (search (lines ": 1") (text-of buffer)) "the second run is a cache hit")
                  (ok (search "#+RESULTS[" (text-of buffer)) "the result carries the hash")
                  (let ((text (text-of buffer)))
                    (setf (lem:buffer-value buffer 'unused) nil)
                    (lem:insert-string (lem:buffer-point buffer) "")
                    (ok (string= text (text-of buffer)))))
        (ignore-errors (delete-file counter))))))

(deftest test-babel-session
  (with-program ("python3" "--version")
    (lem-org-mode::babel-sessions-kill-all)
    (let ((buffer (make-org-buffer
                   (lines "#+begin_src python :session s :results output" "x = 5" "#+end_src"
                          "#+begin_src python :session s :results output" "print(x + 1)" "#+end_src"))))
      (run-at buffer 2)
      (run-at buffer 5)
      (ok (search (lines "#+RESULTS:" ": 6") (text-of buffer)) "the second block sees the first block's variable"))))

(deftest test-babel-noweb
  (with-program ("bash" "--version")
    (let ((buffer (make-org-buffer
                   (lines "#+NAME: greeting" "#+begin_src sh :eval never" "echo hello" "#+end_src"
                          "#+begin_src sh :noweb yes :results output" "  <<greeting>>" "#+end_src"))))
      (run-at buffer 7)
      (ok (search (lines "#+RESULTS:" ": hello") (text-of buffer)) "<<name>> is replaced by the block"))))

(deftest test-babel-eval-never
  (let ((buffer (make-org-buffer (lines "#+begin_src sh :eval never" "echo hi" "#+end_src"))))
    (ok (signals (run-at buffer 2) 'editor-error))))

(deftest test-babel-execute-buffer
  (with-program ("bash" "--version")
    (let ((buffer (make-org-buffer (lines "* A" "#+begin_src sh" "echo one" "#+end_src"
                                          "* B" "#+begin_src sh" "echo two" "#+end_src"))))
      (ok (= 2 (lem-org-mode::execute-range buffer 0 (lambda () (length (lem-org-mode::buffer-lines buffer))))))
      (ok (search ": one" (text-of buffer)))
      (ok (search ": two" (text-of buffer))))))

(deftest test-babel-tangle
  (let* ((directory (uiop:ensure-directory-pathname
                     (merge-pathnames (format nil "org-babel-tangle-~D/" (random 1000000)) (uiop:temporary-directory))))
         (file (merge-pathnames "notes.org" directory)))
    (ensure-directories-exist file)
    (with-open-file (out file :direction :output :if-exists :supersede)
      (write-string (lines "* Code" "#+begin_src python :tangle yes" "print(1)" "#+end_src"
                           "#+begin_src sh :tangle bin/run.sh :shebang \"#!/bin/sh\" :mkdirp yes" "echo hi" "#+end_src")
                    out))
    (unwind-protect
         (let ((buffer (lem:find-file-buffer file)))
           (setf (lem:current-buffer) buffer)
           (lem-org-mode:org-babel-tangle)
           (ok (string= (lines "print(1)") (uiop:read-file-string (merge-pathnames "notes.py" directory))))
           (ok (string= (lines "#!/bin/sh" "echo hi") (uiop:read-file-string (merge-pathnames "bin/run.sh" directory)))))
      (uiop:delete-directory-tree directory :validate t :if-does-not-exist :ignore))))

(deftest test-babel-commands
  (with-program ("bash" "--version")
    (let ((buffer (make-org-buffer (lines "* A" "#+begin_src sh" "echo one" "#+end_src"
                                          "text" "* B" "#+begin_src sh" "echo two" "#+end_src"))))
      (setf (lem:current-buffer) buffer)
      (move-to-line (current-point) 1)
      (lem-org-mode:org-babel-next-src-block)
      (ok (= 2 (line-number-at-point (current-point))) "next block")
      (lem-org-mode:org-babel-next-src-block)
      (ok (= 7 (line-number-at-point (current-point))))
      (lem-org-mode:org-babel-previous-src-block)
      (ok (= 2 (line-number-at-point (current-point))) "previous block")
      (move-to-line (current-point) 7)
      (lem-org-mode:org-babel-execute-subtree)
      (ok (search ": two" (text-of buffer)))
      (ok (not (search ": one" (text-of buffer))) "only the subtree is run")
      (lem-org-mode:org-babel-execute-buffer)
      (ok (search ": one" (text-of buffer)))
      (move-to-line (current-point) (1+ (position "echo two" (lem-org-mode::buffer-lines buffer) :test #'string=)))
      (lem-org-mode:org-babel-remove-result)
      (ok (not (search ": two" (text-of buffer))) "remove-result")
      (move-to-line (current-point) 2)
      (lem-org-mode:org-babel-info)
      (ok (search "sh" (first *messages*)) "info says the language"))))

(deftest test-babel-expand
  (with-program ("python3" "--version")
    (let ((buffer (make-org-buffer (lines "#+NAME: a" "#+begin_src python" "x = 1" "#+end_src"
                                          "#+begin_src python :noweb yes :var n=3 :results output" "<<a>>" "print(x + n)" "#+end_src"))))
      (ok (string= (lines "n = 3" "x = 1" "print(x + n)")
                   (lem-org-mode::babel-expanded-text buffer (line-point buffer 6)))))))

(deftest test-babel-edit-src
  (let* ((buffer (make-org-buffer (lines "* H" "  #+begin_src python" "  a = 1" "  * b" "  #+end_src" "tail")))
         (edit (lem-org-mode::src-edit-buffer buffer (line-point buffer 3))))
    (ok (string= "a = 1
* b" (text-of edit)) "the body is shown dedented and unescaped")
    (lem:insert-string (lem:buffer-end-point edit) (format nil "~%c = 2"))
    (lem-org-mode::src-edit-write-back edit)
    (ok (string= (lines "* H" "  #+begin_src python" "  a = 1" "  ,* b" "  c = 2" "  #+end_src" "tail") (text-of buffer))
        "the text is written back, indented and escaped")))

(deftest test-agenda
  (let ((buffer (make-org-buffer (lines "* TODO [#A] Write :work:" "SCHEDULED: <2026-10-22 Thu>" "* TODO Pay"
                                        "DEADLINE: <2026-10-25 Sun>" "* DONE Old"))))
    (setf (lem:current-buffer) buffer)
    (let* ((entries (lem-org-mode::agenda-entries))
           (agenda (lem-org-mode::agenda-buffer "Todos" (lem-org-mode::agenda-todo-records entries))))
      (ok (= 3 (length entries)))
      (ok (string= "Todos" (line-string (buffer-start-point agenda))))
      (ok (search "Write" (text-of agenda)))
      (ok (not (search "Old" (text-of agenda))) "done entries are not listed")
      (ok (= 3 (length (lem:buffer-value agenda 'lem-org-mode::agenda-targets))) "one target per line, plus the title"))))

(deftest test-export-to-file
  (let* ((directory (uiop:ensure-directory-pathname
                     (merge-pathnames (format nil "org-export-~D/" (random 1000000)) (uiop:temporary-directory))))
         (file (merge-pathnames "doc.org" directory)))
    (ensure-directories-exist file)
    (with-open-file (out file :direction :output :if-exists :supersede)
      (write-string (lines "#+TITLE: T" "* Head" "text *bold*") out))
    (unwind-protect
         (let ((buffer (lem:find-file-buffer file)))
           (setf (lem:current-buffer) buffer)
           (lem-org-mode:org-export-html)
           (lem-org-mode:org-export-markdown)
           (ok (search "<b>bold</b>" (uiop:read-file-string (merge-pathnames "doc.html" directory))))
           (ok (string= (lines "# T" "" "## Head" "" "text **bold**") (uiop:read-file-string (merge-pathnames "doc.md" directory)))))
      (uiop:delete-directory-tree directory :validate t :if-does-not-exist :ignore))))

(deftest test-babel-more-references
  (with-program ("python3" "--version")
    (let ((buffer (make-org-buffer
                   (lines "#+NAME: items" "- 4" "- 5"
                          "#+NAME: words" ": alpha" ": beta"
                          "#+NAME: area" "#+begin_src python :var w=1 :var h=1" "return w * h" "#+end_src"
                          "#+NAME: shown" "#+begin_src python :var i=items :var s=words :var a=area(3, h=4) :results output"
                          "print(i, repr(s), a)" "#+end_src"))))
      (run-at buffer 13)
      (ok (search (lines "#+RESULTS: shown" ": [4, 5] 'alpha\\nbeta' 12") (text-of buffer))
          "lists, example text and calls with positional and keyword arguments"))))

(deftest test-babel-post-and-noweb-call
  (with-program ("python3" "--version")
    (let ((buffer (make-org-buffer
                   (lines "#+NAME: wrap" "#+begin_src python :var data=0" "return data * 10" "#+end_src"
                          "#+begin_src python :post wrap(*this*)" "return 4" "#+end_src"
                          "#+NAME: five" "#+begin_src python" "return 5" "#+end_src"
                          "#+begin_src python :noweb yes :results output" "print(<<five()>> + 1)" "#+end_src"))))
      (run-at buffer 6)
      (ok (search (lines "#+RESULTS:" ": 40") (text-of buffer)) ":post feeds the result to another block")
      (run-at buffer (+ 1 (position "print(<<five()>> + 1)" (lem-org-mode::buffer-lines buffer) :test #'string=)))
      (ok (search (lines "#+RESULTS:" ": 6") (text-of buffer)) "<<name()>> is replaced by the value of the block"))))

(deftest test-babel-header-args-from-properties
  (with-program ("bash" "--version")
    (let ((buffer (make-org-buffer
                   (lines "#+PROPERTY: header-args :results output :var who=\"file\""
                          "* H" ":PROPERTIES:" ":header-args:sh: :var who=\"heading\"" ":END:"
                          "#+begin_src sh" "echo $who" "#+end_src"))))
      (run-at buffer 7)
      (ok (search (lines "#+RESULTS:" ": heading") (text-of buffer)) "heading properties override file-level ones"))))

(deftest test-babel-results-table-and-file
  (with-program ("bash" "--version")
    (let ((buffer (make-org-buffer (lines "#+begin_src sh :results output table" "printf 'a 1\\nb 2\\n'" "#+end_src"))))
      (run-at buffer 2)
      (ok (search (lines "#+RESULTS:" "| a | 1 |" "| b | 2 |") (text-of buffer)) ":results table"))
    (let ((buffer (make-org-buffer (lines "#+begin_src sh :results file :file out.txt" "true" "#+end_src"))))
      (run-at buffer 2)
      (ok (search (lines "#+RESULTS:" "[[file:out.txt]]") (text-of buffer)) ":results file"))))

(deftest test-babel-errors
  (with-program ("bash" "--version")
    (let ((buffer (make-org-buffer (lines "#+begin_src sh" "echo boom >&2; exit 2" "#+end_src"))))
      (ok (signals (run-at buffer 2) 'editor-error))
      (ok (string= (lines "#+begin_src sh" "echo boom >&2; exit 2" "#+end_src") (text-of buffer)) "nothing is written on failure")))
  (let ((buffer (make-org-buffer (lines "#+begin_src cobol" "x" "#+end_src"))))
    (ok (signals (run-at buffer 2) 'editor-error) "no interpreter for the language"))
  (let ((buffer (make-org-buffer (lines "text"))))
    (ok (signals (org-babel-execute (line-point buffer 1)) 'editor-error) "not in a block")))
(deftest test-clocking
  (let ((buffer (make-org-buffer (format nil "* TODO a~%SCHEDULED: <2026-10-22 Thu>~%body~%* b~%"))))
    (org-clock-in (line-point buffer 1) '(2026 10 22 10 0))
    (ok (string= (format nil "* TODO a~%SCHEDULED: <2026-10-22 Thu>~%:LOGBOOK:~%CLOCK: [2026-10-22 Thu 10:00]~%:END:~%body~%* b~%")
                 (buffer-text buffer)))
    (ok (= 90 (org-clock-out (line-point buffer 1) '(2026 10 22 11 30))))
    (ok (string= "CLOCK: [2026-10-22 Thu 10:00]--[2026-10-22 Thu 11:30] =>  1:30" (line-string (line-point buffer 4))))
    (org-clock-in (line-point buffer 1) '(2026 10 22 12 0))
    (org-clock-in (line-point buffer 8) '(2026 10 22 12 30))
    (ok (string= "CLOCK: [2026-10-22 Thu 12:00]--[2026-10-22 Thu 12:30] =>  0:30" (line-string (line-point buffer 4))))
    (ok (string= ":LOGBOOK:" (line-string (line-point buffer 9))))
    (org-clock-cancel (line-point buffer 1))
    (ok (null (org-clock-running (buffer-point buffer))))
    (ok (signals (org-clock-out (line-point buffer 1) '(2026 10 22 13 0)) 'editor-error))))

(deftest test-vi-tab
  (let ((buffer (make-org-buffer (lines "|a|b" "|c|d"))))
    (setf (lem:current-buffer) buffer)
    (lem-org-mode:org-mode)
    (move-to-line (current-point) 1)
    (lem-org-mode::org-vi-tab)
    (ok (string= (lines "| a | b |" "| c | d |") (text-of buffer)) "Tab in an Org buffer is org-cycle (a table cell move)")
    (ok (fboundp 'lem-org-mode::install-vi-bindings))))
