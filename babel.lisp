;;; Babel: run source blocks in several languages and tangle them to files.
;;;
;;;   #+begin_src python :var n=3 :var name="Ada" :results table :dir ~/work :cmdline --flag
;;;   print(n, name)
;;;   #+end_src
;;;
;;; SQL blocks are run by the engines in org-mode.lisp; every other language is written to a
;;; temporary file and run with the interpreter in `*org-babel-languages*'.

(in-package :lem-org-mode)

;;; BEGIN pure babel-languages code

(defvar *org-babel-languages*
  '((("sh" "shell" "bash") :extension "sh" :family :shell :argv #+win32 ("sh") #-win32 ("bash"))
    (("zsh") :extension "sh" :family :shell :argv ("zsh"))
    (("python" "py" "python3") :extension "py" :family :python :argv #+win32 ("python") #-win32 ("python3"))
    (("javascript" "js" "node") :extension "js" :family :js :argv ("node"))
    (("ruby" "rb") :extension "rb" :family :ruby :argv ("ruby"))
    (("perl") :extension "pl" :family :perl :argv ("perl"))
    (("lua") :extension "lua" :family :lua :argv ("lua"))
    (("r") :extension "R" :family :r :argv ("Rscript"))
    (("powershell" "pwsh" "ps1") :extension "ps1" :family :powershell :argv #+win32 ("powershell" "-NoProfile" "-ExecutionPolicy" "Bypass" "-File") #-win32 ("pwsh" "-NoProfile" "-File"))
    (("lisp" "sbcl" "common-lisp") :extension "lisp" :family :lisp :argv ("sbcl" "--script")))
  "Languages that `org-babel-execute' can run. Each entry is (names :extension EXT :family FAMILY :argv ARGV):
the file with the code is appended to ARGV. FAMILY selects how :var values are written.
On Windows the defaults are `sh', `python' and `powershell', because `bash' and `python3' there are often the WSL launcher and the Microsoft Store stub.
Add or change entries here, for example to run python with another interpreter.")

(defun babel-language-spec (name specs)
  (find-if (lambda (spec) (member name (car spec) :test #'string-equal)) specs))

(defun header-arg (args name)
  (cdr (assoc name args :test #'string=)))

(defun babel-quote (family string)
  "STRING as a string literal of the language FAMILY."
  (flet ((wrap (open close escapes)
           (with-output-to-string (out)
             (write-string open out)
             (loop :for c :across string
                   :do (let ((escape (assoc c escapes)))
                         (if escape (write-string (cdr escape) out) (write-char c out))))
             (write-string close out))))
    (ecase family
      ((:python :js :ruby :lua :lisp :r)
       (wrap "\"" "\"" '((#\\ . "\\\\") (#\" . "\\\"") (#\newline . "\\n"))))
      (:shell (wrap "'" "'" '((#\' . "'\\''"))))
      (:powershell (wrap "'" "'" '((#\' . "''"))))
      (:perl (wrap "'" "'" '((#\' . "\\'") (#\\ . "\\\\")))))))

(defun babel-assignment (family name value)
  "Source text that binds NAME to VALUE, a string that is a number or the contents of a quoted string."
  (let ((literal (if (and (stringp value) (table-number-p value)) value (babel-quote family value))))
    (ecase family
      (:python (format nil "~A = ~A" name literal))
      (:ruby (format nil "~A = ~A" name literal))
      (:js (format nil "const ~A = ~A;" name literal))
      (:perl (format nil "my $~A = ~A;" name literal))
      (:lua (format nil "local ~A = ~A" name literal))
      (:shell (format nil "~A=~A" name literal))
      (:powershell (format nil "$~A = ~A" name literal))
      (:r (format nil "~A <- ~A" name literal))
      (:lisp (format nil "(defparameter ~A ~A)" name literal)))))

(defun babel-var-assignments (family args)
  "Assignments for every :var name=value header argument in ARGS."
  (loop :for (key . spec) :in args
        :when (string= key "var")
          :collect (let ((equals (position #\= spec)))
                     (unless (and equals (plusp equals))
                       (editor-error "Bad :var ~A (use name=value)" spec))
                     (let ((value (subseq spec (1+ equals))))
                       (babel-assignment family (subseq spec 0 equals)
                                         (if (and (>= (length value) 2) (char= (char value 0) #\")
                                                  (char= (char value (1- (length value))) #\"))
                                             (subseq value 1 (1- (length value)))
                                             value))))))

(defun babel-program (family args body)
  "The text to run: the :var assignments followed by BODY."
  (format nil "~{~A~%~}~A" (babel-var-assignments family args) body))

(defun trim-trailing-blank-lines (lines)
  (let ((end (position-if (lambda (line) (string/= (string-trim '(#\space #\tab) line) "")) lines :from-end t)))
    (if end (subseq lines 0 (1+ end)) '())))

(defun output-result-lines (text)
  "The #+RESULTS: block for the program output TEXT: one \": \" line per output line."
  (cons "#+RESULTS:"
        (mapcar (lambda (line) (concatenate 'string ": " line))
                (trim-trailing-blank-lines (split-text-lines text)))))

(defun table-result-lines (text)
  "The #+RESULTS: block for TEXT read as a table: cells are separated by tabs, or by blanks when there are no tabs."
  (let* ((lines (trim-trailing-blank-lines (split-text-lines text)))
         (tabs (find #\tab text))
         (rows (mapcar (lambda (line)
                         (if tabs
                             (loop :with from := 0
                                   :for tab := (position #\tab line :start from)
                                   :collect (string-trim " " (subseq line from tab))
                                   :while tab :do (setf from (1+ tab)))
                             (split-words line)))
                       lines)))
    (cons "#+RESULTS:" (and rows (org-table-format-rows rows)))))

(defun tangle-collect (lines default-base)
  "Collect the code of the source blocks in LINES that have a :tangle argument.
Return a list of (file . text). `:tangle yes' means DEFAULT-BASE with the extension of the language."
  (let ((targets '()) (vector (coerce lines 'vector)))
    (loop :with i := 0
          :while (< i (length vector))
          :do (let ((line (aref vector i)))
                (if (src-begin-line-p line)
                    (let* ((end (or (position-if #'src-end-line-p vector :start i) (length vector)))
                           (body (coerce (subseq vector (1+ i) end) 'list)))
                      (multiple-value-bind (language args) (src-header line)
                        (let* ((tangle (header-arg args "tangle"))
                               (spec (and language (babel-language-spec language *org-babel-languages*)))
                               (extension (or (getf (cdr spec) :extension)
                                              (and language (string-downcase language)) "txt"))
                               (file (cond ((or (null tangle) (string-equal tangle "no")) nil)
                                           ((string-equal tangle "yes") (format nil "~A.~A" default-base extension))
                                           (t (string-trim "\"" tangle)))))
                          (when file
                            (let ((entry (assoc file targets :test #'string=)))
                              (if entry
                                  (setf (cdr entry) (append (cdr entry) (list body)))
                                  (setf targets (append targets (list (list file body)))))))))
                      (setf i (1+ end)))
                    (incf i))))
    (mapcar (lambda (entry)
              (cons (car entry)
                    (format nil "~{~A~^~%~}~%" (loop :for body :in (cdr entry)
                                                    :for first := t :then nil
                                                    :unless first :collect ""
                                                    :append body))))
            targets)))

;;; END pure babel-languages code

(defun babel-run (spec program args)
  "Write PROGRAM to a temporary file, run it with the interpreter of SPEC and return its standard output."
  (let* ((argv (getf (cdr spec) :argv))
         (file (merge-pathnames (format nil "org-babel-~D-~D.~A" (get-universal-time) (random 100000)
                                        (getf (cdr spec) :extension))
                                (uiop:temporary-directory)))
         (directory (header-arg args "dir"))
         (cmdline (split-words (or (header-arg args "cmdline") ""))))
    (unwind-protect
         (progn
           (with-open-file (out file :direction :output :if-exists :supersede :external-format :utf-8)
             (write-string program out))
           (multiple-value-bind (output error-output status)
               (handler-case
                   (uiop:run-program (append argv (list (uiop:native-namestring file)) cmdline)
                                     :output :string :error-output :string :ignore-error-status t
                                     :directory (and directory (uiop:ensure-directory-pathname directory)))
                 (error (e) (editor-error "Cannot run ~A: ~A" (first argv) e)))
             (unless (zerop status)
               (editor-error "~A failed: ~A" (first argv)
                             (let ((message (string-trim '(#\space #\newline #\return)
                                                         (if (plusp (length error-output)) error-output output))))
                               (subseq message 0 (position #\newline message)))))
             output))
      (ignore-errors (delete-file file)))))

(defun babel-result-lines (language args body)
  "Run BODY, a source block in LANGUAGE with header ARGS, and return the #+RESULTS: lines, or NIL for :results silent."
  (let* ((results (or (header-arg args "results") "output"))
         (lines (if (string-equal language "sql")
                    (results-lines (run-sql (sql-engine-kind (or (header-arg args "engine") *org-sql-default-engine*))
                                            args body))
                    (let ((spec (or (babel-language-spec language *org-babel-languages*)
                                    (editor-error "No interpreter for ~A (see *org-babel-languages*)" language))))
                      (let ((text (babel-run spec (babel-program (getf (cdr spec) :family) args body) args)))
                        (if (string-equal results "table")
                            (table-result-lines text)
                            (output-result-lines text)))))))
    (if (string-equal results "silent") nil lines)))

(define-command org-babel-tangle () ()
  "Write the source blocks that have a :tangle argument to their files, next to the buffer."
  (let* ((buffer (current-buffer))
         (file (buffer-filename buffer))
         (files (tangle-collect (split-text-lines (buffer-text buffer))
                                (if file (pathname-name file) (buffer-name buffer)))))
    (unless files
      (editor-error "No source block has a :tangle argument"))
    (dolist (entry files)
      (let ((target (merge-pathnames (car entry) (or (buffer-directory buffer) ""))))
        (ensure-directories-exist target)
        (with-open-file (out target :direction :output :if-exists :supersede :external-format :utf-8)
          (write-string (cdr entry) out))))
    (message "Tangled ~D file~:P" (length files))))
