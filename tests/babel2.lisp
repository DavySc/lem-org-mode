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
