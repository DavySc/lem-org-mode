(in-package :lem-org-mode/tests)

(defun run-at-column (buffer line column)
  (let ((point (line-point buffer line)))
    (character-offset point column)
    (org-babel-execute point)))

(deftest test-babel-inline
  (with-program ("bash" "--version")
    (let ((buffer (make-org-buffer (lines "* H" "Two is src_sh{echo $((1+1))} here."))))
      (run-at-column buffer 2 12)
      (ok (string= (lines "* H" "Two is src_sh{echo $((1+1))} {{{results(=2=)}}} here.") (text-of buffer))
          "an inline block gets a results macro")
      (run-at-column buffer 2 12)
      (ok (string= (lines "* H" "Two is src_sh{echo $((1+1))} {{{results(=2=)}}} here.") (text-of buffer))
          "running it again replaces the macro"))))

(deftest test-babel-inline-call
  (with-program ("python3" "--version")
    (let ((buffer (make-org-buffer (lines "#+NAME: double" "#+begin_src python :var n=1" "return n * 2" "#+end_src"
                                          "Result: call_double(21)"))))
      (run-at-column buffer 5 10)
      (ok (search "call_double(21) {{{results(=42=)}}}" (text-of buffer))))))

(deftest test-babel-header-lines
  (with-program ("bash" "--version")
    (let ((buffer (make-org-buffer (lines "#+header: :results output" "#+begin_src sh :results value" "echo hi" "#+end_src"))))
      (run-at buffer 3)
      (ok (search (lines "#+RESULTS:" ": hi") (text-of buffer))))))

(deftest test-babel-var-index
  (with-program ("python3" "--version")
    (let ((buffer (make-org-buffer (lines "#+NAME: t" "| a | 1 |" "| b | 2 |"
                                          "#+begin_src python :var x=t[1,1] :var col=t[*,0]"
                                          "return [x] + col" "#+end_src"))))
      (run-at buffer 5)
      (ok (search (lines "| 2 |" "| a |" "| b |") (text-of buffer)) "x is the cell, col the column"))))

(deftest test-babel-dir-and-awk
  (with-program ("awk" "--version")
    (let ((buffer (make-org-buffer (lines "#+begin_src awk" "BEGIN { print 6 * 7 }" "#+end_src"))))
      (run-at buffer 2)
      (ok (search ": 42" (text-of buffer)))))
  (with-program ("bash" "--version")
    (let ((buffer (make-org-buffer (lines "#+begin_src sh :dir /tmp :results output" "pwd" "#+end_src"))))
      (run-at buffer 2)
      (ok (search ": /tmp" (text-of buffer))))))

(deftest test-babel-file-ext
  (with-program ("bash" "--version")
    (let* ((dir (uiop:ensure-directory-pathname (format nil "/tmp/org-babel-out-~D" (get-universal-time))))
           (buffer (make-org-buffer (lines "#+NAME: out"
                                           (format nil "#+begin_src sh :dir ~A :file-ext txt :output-dir sub :results file" (namestring dir))
                                           "mkdir -p sub; echo x > sub/out.txt" "#+end_src"))))
      (ensure-directories-exist dir)
      (run-at buffer 3)
      (ok (search "[[file:sub/out.txt]]" (text-of buffer)))
      (ok (probe-file (merge-pathnames "sub/out.txt" dir)))
      (uiop:delete-directory-tree dir :validate t :if-does-not-exist :ignore))))

(deftest test-babel-library
  (with-program ("python3" "--version")
    (let ((lib (make-org-buffer (lines "#+NAME: triple" "#+begin_src python :var n=1" "return 3 * n" "#+end_src")))
          (buffer (make-org-buffer (lines "#+CALL: triple(n=5)"))))
      (setf (lem:current-buffer) lib)
      (lem-org-mode:org-babel-lob-ingest)
      (run-at buffer 1)
      (ok (search ": 15" (text-of buffer)) "a call finds a block ingested from another buffer")
      (lem-org-mode:org-babel-lob-clear))))

(deftest test-babel-demarcate
  (let ((buffer (make-org-buffer (lines "#+begin_src sh" "one" "two" "#+end_src"))))
    (setf (lem:current-buffer) buffer)
    (move-to-line (current-point) 3)
    (lem-org-mode:org-babel-demarcate-block)
    (ok (string= (lines "#+begin_src sh" "one" "#+end_src" "" "#+begin_src sh" "two" "#+end_src") (text-of buffer)))))

;;; Live sessions

(defun session-block (language body &optional (args ":results output"))
  (lines (format nil "#+begin_src ~A :session live ~A" language args) body "#+end_src"))

(defun run-nth-block (buffer n)
  "Run the Nth (from 1) source block of BUFFER, on its first body line."
  (let ((found 0))
    (loop :for line :in (lem-org-mode::buffer-lines buffer)
          :for number :from 1
          :do (when (and (>= (length line) 11) (string-equal "#+begin_src" line :end2 11) (= (incf found) n))
                (return (run-at buffer (1+ number)))))))

(defun session-results (buffer)
  "The text of BUFFER after the last #+RESULTS: line."
  (let ((text (text-of buffer)))
    (string-trim '(#\newline) (subseq text (+ (search "#+RESULTS:" text :from-end t) 11)))))

(deftest test-babel-live-session-no-replay
  (with-program ("python3" "--version")
    (lem-org-mode::babel-sessions-kill-all)
    (let* ((count (format nil "/tmp/org-babel-count-~D" (random 100000)))
           (buffer (make-org-buffer
                    (lines (session-block "python" (format nil "open('~A', 'a').write('x')~%n = 41" count))
                           (session-block "python" "print(n + 1)")))))
      (unwind-protect
           (progn
             (run-nth-block buffer 1)
             (run-nth-block buffer 2)
             (run-nth-block buffer 2)
             (ok (string= (session-results buffer) ": 42") "the second block sees the variable")
             (ok (string= "x" (with-open-file (in count) (read-line in))) "the first block ran exactly once")
             (ok (= 1 (hash-table-count lem-org-mode::*babel-processes*)) "one interpreter is alive")
             (org-babel-reset-sessions)
             (ok (zerop (hash-table-count lem-org-mode::*babel-processes*)) "reset kills the interpreter"))
        (ignore-errors (delete-file count))))))

(deftest test-babel-live-session-value-and-error
  (with-program ("python3" "--version")
    (lem-org-mode::babel-sessions-kill-all)
    (let ((buffer (make-org-buffer
                   (lines (session-block "python" "xs = [1, 2, 3]")
                          (session-block "python" "sum(xs)" ":results value")
                          (session-block "python" "1 / 0")
                          (session-block "python" "print(len(xs))")))))
      (run-nth-block buffer 1)
      (run-nth-block buffer 2)
      (ok (search (lines "#+RESULTS:" ": 6") (text-of buffer)) "value is the last expression")
      (run-nth-block buffer 3)
      (ok (search "ZeroDivisionError" (text-of buffer)) "a traceback is the result")
      (run-nth-block buffer 4)
      (ok (string= (session-results buffer) ": 3") "the session survives an error")
      (lem-org-mode::babel-sessions-kill-all))))

(deftest test-babel-live-session-js-ruby-sh
  (dolist (case '(("node" "js" "var total = 5" "total + 1" "console.log(total * 2)")
                  ("ruby" "ruby" "total = 5" "total + 1" "puts total * 2")))
    (destructuring-bind (program language setup value-code output-code) case
      (with-program (program "--version")
        (lem-org-mode::babel-sessions-kill-all)
        (let ((buffer (make-org-buffer (lines (session-block language setup)
                                              (session-block language value-code ":results value")
                                              (session-block language output-code)))))
          (run-nth-block buffer 1)
          (run-nth-block buffer 2)
          (ok (search (lines "#+RESULTS:" ": 6") (text-of buffer)) (format nil "~A value" language))
          (run-nth-block buffer 3)
          (ok (string= (session-results buffer) ": 10") (format nil "~A output" language))
          (lem-org-mode::babel-sessions-kill-all)))))
  (with-program ("bash" "--version")
    (lem-org-mode::babel-sessions-kill-all)
    (let ((buffer (make-org-buffer (lines (session-block "sh" (format nil "cd /tmp~%x=7"))
                                          (session-block "sh" (format nil "pwd~%echo $((x * 6))"))))))
      (run-nth-block buffer 1)
      (run-nth-block buffer 2)
      (ok (string= (session-results buffer) (format nil ": /tmp~%: 42")) "shell keeps directory and variables")
      (lem-org-mode::babel-sessions-kill-all))))

(deftest test-babel-empty-result-leaves-results-line
  (with-program ("bash" "--version")
    (let ((buffer (make-org-buffer (lines "#+begin_src sh :results output" "true" "#+end_src"))))
      (run-at buffer 2)
      (ok (string= (lines "#+begin_src sh :results output" "true" "#+end_src" "" "#+RESULTS:" "") (text-of buffer))
          "an empty result leaves #+RESULTS: and a blank line, as in Emacs"))))
