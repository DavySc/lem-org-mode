;;; Smoke test that runs inside a real Lem, without Rove.
;;;
;;;   lem.exe -q --eval "(load \"<repo>/scripts/smoke.lisp\")"
;;;
;;; Results go to scripts/smoke.log (one PASS/FAIL line per check, then DONE),
;;; because the GUI build prints nothing to stdout. Lem is left running; kill it
;;; once the log ends with DONE.

(in-package :cl-user)

(defparameter *smoke-root*
  (merge-pathnames "../" (make-pathname :name nil :type nil :defaults *load-truename*)))

(defparameter *smoke-log* (merge-pathnames "scripts/smoke.log" *smoke-root*))

(defun smoke-say (fmt &rest args)
  (with-open-file (s *smoke-log* :direction :output :if-exists :append :if-does-not-exist :create)
    (apply #'format s fmt args)
    (terpri s)))

(defmacro smoke-check (name expected form)
  `(handler-case
       (let ((actual ,form))
         (if (equal actual ,expected)
             (smoke-say "PASS ~A" ,name)
             (smoke-say "FAIL ~A: expected ~S, got ~S" ,name ,expected actual)))
     (serious-condition (e) (smoke-say "FAIL ~A: ~A" ,name e))))

(with-open-file (s *smoke-log* :direction :output :if-exists :supersede) (declare (ignore s)))

(handler-case
    (progn
      (asdf:load-asd (merge-pathnames "lem-org-mode.asd" *smoke-root*))
      (asdf:load-system "lem-org-mode")
      (smoke-say "PASS load"))
  (serious-condition (e) (smoke-say "FAIL load: ~A" e)))

(in-package :cl-user)

(defun smoke-buffer (text)
  (let ((buffer (lem:make-buffer nil :temporary t)))
    (lem:insert-string (lem:buffer-point buffer) text)
    buffer))

(defun smoke-text (buffer)
  (lem:points-to-string (lem:buffer-start-point buffer) (lem:buffer-end-point buffer)))

(defun smoke-line (buffer n)
  (lem:move-to-line (lem:copy-point (lem:buffer-start-point buffer) :temporary) n))

(smoke-check "file type" 'lem-org-mode:org-mode (lem:get-file-mode "notes.org"))

(smoke-check "heading level"
             '(1 2 nil)
             (let ((b (smoke-buffer (format nil "* A~%** B~%text~%"))))
               (list (lem-org-mode:org-heading-level (smoke-line b 1))
                     (lem-org-mode:org-heading-level (smoke-line b 2))
                     (lem-org-mode:org-heading-level (smoke-line b 3)))))

(smoke-check "todo cycle"
             (list (format nil "** TODO x~%") (format nil "** DONE x~%") (format nil "** x~%"))
             (let ((b (smoke-buffer (format nil "** x~%"))) (out '()))
               (dotimes (i 3)
                 (lem-org-mode:org-cycle-todo (smoke-line b 1))
                 (push (smoke-text b) out))
               (nreverse out)))

(smoke-check "demote/promote"
             (list (format nil "** A~%*** A1~%* B~%") (format nil "* A~%** A1~%* B~%"))
             (let ((b (smoke-buffer (format nil "* A~%** A1~%* B~%"))) (out '()))
               (lem-org-mode:org-shift-subtree (smoke-line b 1) 1)
               (push (smoke-text b) out)
               (lem-org-mode:org-shift-subtree (smoke-line b 1) -1)
               (push (smoke-text b) out)
               (nreverse out)))

(smoke-check "move subtree down and up"
             (list (format nil "* B~%b~%* A~%a") (format nil "* A~%a~%* B~%b"))
             (let ((b (smoke-buffer (format nil "* A~%a~%* B~%b"))) (out '()))
               (lem-org-mode:org-move-subtree (smoke-line b 1) :down)
               (push (smoke-text b) out)
               (lem-org-mode:org-move-subtree (smoke-line b 3) :up)
               (push (smoke-text b) out)
               (nreverse out)))

(smoke-check "table align"
             (format nil "| a   | bb |~%| ccc | d  |~%")
             (let ((b (smoke-buffer (format nil "|a|bb~%|ccc|d|~%"))))
               (lem-org-mode:org-table-edit (smoke-line b 1) :align)
               (smoke-text b)))

(smoke-check "table next cell and move row"
             (list 1 8 (format nil "| ccc |   | d  |~%| a   |   | bb |~%") 2)
             (let* ((b (smoke-buffer (format nil "|a|bb~%|ccc|d|~%")))
                    (p (smoke-line b 1)))
               (lem-org-mode:org-table-edit p :align)
               (setf (lem:point-charpos p) 2)
               (lem-org-mode:org-table-edit p :next-cell)
               (let ((line (lem:line-number-at-point p)) (col (lem:point-charpos p)))
                 (lem-org-mode:org-table-edit p :insert-column)
                 (lem-org-mode:org-table-edit p :move-row-down)
                 (list line col (smoke-text b) (lem:line-number-at-point p)))))

(smoke-check "list item insert and renumber"
             (list (format nil "1. a~%2. ~%3. b~%") 2)
             (let* ((b (smoke-buffer (format nil "1. a~%2. b~%")))
                    (p (smoke-line b 1)))
               (lem-org-mode:org-insert-item p)
               (list (smoke-text b) (lem:line-number-at-point p))))

(smoke-check "item insert with a live point and a sub-item"
             (list (format nil "1. a~%   - [ ] sub~%2. ~%3. b~%") 3)
             (let* ((b (smoke-buffer (format nil "1. a~%   - [ ] sub~%2. b~%")))
                    (p (lem:copy-point (smoke-line b 1) :left-inserting)))
               (lem-org-mode:org-insert-item p)
               (prog1 (list (smoke-text b) (lem:line-number-at-point p))
                 (lem:delete-point p))))

(smoke-check "checkbox toggle keeps point"
             (list "- [X] a" 3)
             (let* ((b (smoke-buffer (format nil "- [ ] a~%")))
                    (p (smoke-line b 1)))
               (setf (lem:point-charpos p) 3)
               (lem-org-mode:org-toggle-checkbox p)
               (list (lem:line-string p) (lem:point-charpos p))))

(smoke-check "priority, planning, property"
             (list "* TODO [#C] x" "SCHEDULED: <2026-10-22 Thu>" ":PROPERTIES:" ":ID: 7")
             (let ((b (smoke-buffer (format nil "* TODO x~%body~%"))))
               (lem-org-mode:org-cycle-priority (smoke-line b 1) 1)
               (lem-org-mode:org-set-planning (smoke-line b 1) "SCHEDULED" "<2026-10-22 Thu>")
               (lem-org-mode:org-set-property (smoke-line b 1) "ID" "7")
               (loop :for n :from 1 :to 4 :collect (lem:line-string (smoke-line b n)))))

(smoke-check "tags"
             ":work:"
             (let ((b (smoke-buffer (format nil "* x~%"))))
               (lem-org-mode:org-set-tags (smoke-line b 1) '("work"))
               (let ((line (lem:line-string (smoke-line b 1))))
                 (subseq line (- (length line) 6)))))

(smoke-check "clock in, out, cancel"
             (list (format nil "* a~%:LOGBOOK:~%CLOCK: [2026-10-22 Thu 10:00]~%:END:~%")
                   90
                   "CLOCK: [2026-10-22 Thu 10:00]--[2026-10-22 Thu 11:30] =>  1:30"
                   (format nil "* a~%body"))
             (let ((b (smoke-buffer (format nil "* a~%"))) (out '()))
               (lem-org-mode:org-clock-in (smoke-line b 1) '(2026 10 22 10 0))
               (push (smoke-text b) out)
               (push (lem-org-mode:org-clock-out (smoke-line b 1) '(2026 10 22 11 30)) out)
               (push (lem:line-string (smoke-line b 3)) out)
               (let ((c (smoke-buffer (format nil "* a~%body"))))
                 (lem-org-mode:org-clock-in (smoke-line c 1) '(2026 10 22 10 0))
                 (lem-org-mode:org-clock-cancel (smoke-line c 1))
                 (push (smoke-text c) out))
               (nreverse out)))

(smoke-check "src block bounds"
             (list 1 3)
             (let ((b (smoke-buffer (format nil "#+begin_src sql~%select 1;~%#+end_src~%"))))
               (multiple-value-bind (begin end) (lem-org-mode:src-block-bounds (smoke-line b 2))
                 (list (lem:line-number-at-point begin) (lem:line-number-at-point end)))))

(smoke-check "agenda entries from buffer text"
             '("Write report" "Pay rent")
             (let ((b (smoke-buffer (format nil "* TODO Write report~%SCHEDULED: <2026-10-22 Thu>~%* TODO Pay rent~%"))))
               (mapcar (lambda (e) (getf e :title))
                       (lem-org-mode::agenda-parse (lem-org-mode::split-text-lines (lem:buffer-text b))
                                                   '("TODO" "DONE") "notes.org"))))

(smoke-check "html export"
             '(t t)
             (let* ((b (smoke-buffer (format nil "#+TITLE: T~%* Head~%text *bold*~%")))
                    (html (lem-org-mode::export-html (lem-org-mode::split-text-lines (lem:buffer-text b))
                                                     '("TODO" "DONE"))))
               (list (and (search "Head</h2>" html) t) (and (search "<b>bold</b>" html) t))))

(smoke-check "markdown export"
             (format nil "# T~%~%## Head~%~%text **bold**~%")
             (let ((b (smoke-buffer (format nil "#+TITLE: T~%* Head~%text *bold*~%"))))
               (lem-org-mode::export-markdown (lem-org-mode::split-text-lines (lem:buffer-text b))
                                              '("TODO" "DONE"))))

(if (ignore-errors (uiop:run-program (list #+win32 "sh" #-win32 "bash" "--version") :output nil :error-output nil) t)
    (smoke-check "sh block runs and a rerun replaces the result"
                 (format nil "#+begin_src sh :var n=3~%echo hi $n~%#+end_src~%~%#+RESULTS:~%: hi 3~%")
                 (let* ((b (smoke-buffer (format nil "#+begin_src sh :var n=3~%echo hi $n~%#+end_src~%")))
                        (p (smoke-line b 2)))
                   (lem-org-mode:org-babel-execute p)
                   (lem-org-mode:org-babel-execute p)
                   (smoke-text b)))
    (smoke-say "SKIP sh block: bash not found"))

(if (ignore-errors (uiop:run-program (list #+win32 "python" #-win32 "python3" "--version") :output nil :error-output nil) t)
    (progn
      (smoke-check "python value, named table, header kept"
                   (format nil "#+NAME: d~%| n | sq |~%|---+----|~%| 1 | 1 |~%| 2 | 4 |~%#+begin_src python :var rows=d~%return [[r[0], r[1] + 1] for r in rows]~%#+end_src~%~%#+RESULTS:~%| n | sq |~%|---+----|~%| 1 |  2 |~%| 2 |  5 |~%")
                   (let ((b (smoke-buffer (format nil "#+NAME: d~%| n | sq |~%|---+----|~%| 1 | 1 |~%| 2 | 4 |~%#+begin_src python :var rows=d~%return [[r[0], r[1] + 1] for r in rows]~%#+end_src~%"))))
                     (lem-org-mode:org-babel-execute (smoke-line b 7))
                     (smoke-text b)))
      (smoke-check "call line"
                   (format nil "#+NAME: f~%#+begin_src python :var n=1~%return n * 2~%#+end_src~%#+CALL: f(n=21)~%~%#+RESULTS: f~%: 42~%")
                   (let ((b (smoke-buffer (format nil "#+NAME: f~%#+begin_src python :var n=1~%return n * 2~%#+end_src~%#+CALL: f(n=21)~%"))))
                     (lem-org-mode:org-babel-execute (smoke-line b 5))
                     (smoke-text b))))
    (smoke-say "SKIP python checks: python3 not found"))

(smoke-check "src edit buffer round trip"
             (format nil "* H~%  #+begin_src python~%  a = 1~%  ,* b~%  c = 2~%  #+end_src~%")
             (let* ((b (smoke-buffer (format nil "* H~%  #+begin_src python~%  a = 1~%  * b~%  #+end_src~%")))
                    (edit (lem-org-mode::src-edit-buffer b (smoke-line b 3))))
               (lem:insert-string (lem:buffer-end-point edit) (format nil "~%c = 2"))
               (lem-org-mode::src-edit-write-back edit)
               (smoke-text b)))

(smoke-check "tangle plan"
             '("notes.py")
             (mapcar (lambda (e) (getf e :file))
                     (lem-org-mode::babel-tangle-plan
                      (lem-org-mode::babel-scan (list "#+begin_src python :tangle yes" "print(1)" "#+end_src") '("TODO" "DONE"))
                      "notes" "notes.org")))
(smoke-check "sqlcmd arguments"
             (list t t t t)
             (let ((argv (lem-org-mode::sql-command :sqlcmd '(("host" . "srv") ("port" . "1433")
                                                               ("trust-cert" . "yes") ("timeout" . "7")))))
               (list (and (member "-b" argv :test #'string=) t)
                     (equal (subseq (member "-l" argv :test #'string=) 0 2) '("-l" "7"))
                     (and (member "-C" argv :test #'string=) t)
                     (and (member "srv,1433" argv :test #'string=) t))))

;; Live SQL Server checks run only when LEM_ORG_SMOKE_MSSQL names a host (Windows authentication).
(let ((host (uiop:getenv "LEM_ORG_SMOKE_MSSQL")))
  (if (and host (plusp (length host)))
      (let ((args (list (cons "host" host) (cons "trust-cert" "yes"))))
        (smoke-check "mssql rows"
                     '(("one" "two") ("1" "x"))
                     (lem-org-mode::run-sql :sqlcmd args "select 1 as one, 'x' as two;"))
        (smoke-check "mssql error is an editor-error"
                     t
                     (handler-case (progn (lem-org-mode::run-sql :sqlcmd args "select * from nope_nope;") nil)
                       (lem:editor-error () t))))
      (smoke-say "SKIP mssql checks (set LEM_ORG_SMOKE_MSSQL to a host)")))

;;; further checks, one file per area, in scripts/smoke.d/
(dolist (file (sort (directory (merge-pathnames "scripts/smoke.d/*.lisp" *smoke-root*)) #'string< :key #'namestring))
  (handler-case (load file)
    (serious-condition (e) (smoke-say "FAIL loading ~A: ~A" (pathname-name file) e))))

(smoke-say "DONE")
