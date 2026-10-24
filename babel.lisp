;;; Babel: run source blocks, put results in the buffer, tangle, expand and edit blocks.
;;;
;;; The model (header arguments, names, data, result formatting, noweb, tangle plan) is in
;;; babel-model.lisp. Here: the interpreter table, processes, and everything that edits a buffer.
;;;
;;;   #+NAME: squares
;;;   #+begin_src python :var n=4 :results value
;;;   return [[i, i * i] for i in range(n)]
;;;   #+end_src
;;;
;;;   #+begin_src sh :var table=squares :results output
;;;   echo "$table"
;;;   #+end_src
;;;
;;;   #+CALL: squares(n=10)

(in-package :lem-org-mode)

(defvar *org-babel-confirm-evaluate* nil
  "When true, ask before running any block (a block with :eval query always asks).")

(defvar *babel-sessions* (make-hash-table :test 'equal)
  "For :session, the programs already run in each session, replayed before the next block.")

(defvar *babel-header* nil
  "Column names stripped from the first table that a block takes as a variable; put back on a table result.")

(defvar *babel-lines* '()
  "The lines of the buffer being evaluated, for resolving #+RESULTS: references.")

(defvar *babel-depth* 0)

;;; BEGIN pure babel code

(defvar *org-babel-languages*
  '((("sh" "shell" "bash") :extension "sh" :family :shell :argv ("bash"))
    (("zsh") :extension "sh" :family :shell :argv ("zsh"))
    (("python" "py" "python3") :extension "py" :family :python :argv ("python3"))
    (("javascript" "js" "node") :extension "js" :family :js :argv ("node"))
    (("ruby" "rb") :extension "rb" :family :ruby :argv ("ruby"))
    (("perl") :extension "pl" :family :perl :argv ("perl"))
    (("php") :extension "php" :family :php :argv ("php"))
    (("lua") :extension "lua" :family :lua :argv ("lua"))
    (("r") :extension "R" :family :r :argv ("Rscript"))
    (("powershell" "pwsh" "ps1") :extension "ps1" :family :powershell :argv ("pwsh" "-NoProfile" "-File"))
    (("lisp" "sbcl" "common-lisp") :extension "lisp" :family :lisp :argv ("sbcl" "--script"))
    (("c") :extension "c" :family :c :compile ("gcc" "%f" "-o" "%o" "-lm") :argv ("%o"))
    (("cpp" "c++") :extension "cpp" :family :cpp :compile ("g++" "%f" "-o" "%o") :argv ("%o"))
    (("go") :extension "go" :family :go :argv ("go" "run"))
    (("java") :extension "java" :family :java :argv ("java"))
    (("haskell" "hs") :extension "hs" :family :haskell :argv ("runghc"))
    (("rust" "rs") :extension "rs" :family :rust :compile ("rustc" "%f" "-o" "%o") :argv ("%o"))
    (("typescript" "ts") :extension "ts" :family :typescript :argv ("ts-node"))
    (("awk") :extension "awk" :family :awk :argv ("awk" "-f"))
    (("julia") :extension "jl" :family :julia :argv ("julia"))
    (("octave" "matlab") :extension "m" :family :octave :argv ("octave" "--no-gui" "-q"))
    (("scheme" "guile") :extension "scm" :family :scheme :argv ("guile"))
    (("ocaml") :extension "ml" :family :ocaml :argv ("ocaml"))
    (("fortran") :extension "f90" :family :fortran :compile ("gfortran" "%f" "-o" "%o") :argv ("%o"))
    (("tcl") :extension "tcl" :family :tcl :argv ("tclsh"))
    (("dot" "graphviz") :extension "dot" :family :dot :graphics t :argv ("dot" "-T%T" "-o" "%F" "%f"))
    (("gnuplot") :extension "gp" :family :gnuplot :graphics t :argv ("gnuplot")
     :prelude (format nil "set terminal %T~%set output \"%F\"~%"))
    (("mermaid") :extension "mmd" :family :mermaid :graphics t :argv ("mmdc" "-i" "%f" "-o" "%F")))
  "Languages that `org-babel-execute' can run (SQL has its own engines). Each entry is
(names :extension EXT :family FAMILY :argv ARGV [:compile ARGV] [:graphics T] [:prelude TEXT]). The file with the code is
appended to ARGV unless ARGV contains %f; %o stands for the compiled program, %F for the output file named by :file
and %T for its type (the extension). A :graphics language writes the :file named by the block, so its result is a
file link; :prelude is put in front of the code with %F and %T filled in. FAMILY decides how :var values are written and
whether :results value works (python js ruby perl php lua r powershell lisp). Edit this to change an
interpreter, for example python on Windows.")

(defun babel-language-spec (name specs)
  (find-if (lambda (spec) (member name (car spec) :test #'string-equal)) specs))

(defun babel-wrap-main (family body)
  "C, C++ and Rust blocks without a main function get includes and a main around them, as in Org."
  (if (or (not (member family '(:c :cpp :rust))) (search "main" body))
      body
      (if (eq family :rust) (format nil "fn main() {~%~A~%}~%" body)
      (format nil "~A~%int main() {~%~A~%return 0;~%}~%"
              (if (eq family :c)
                  (format nil "#include <stdio.h>~%#include <stdlib.h>~%#include <string.h>~%#include <math.h>")
                  (format nil "#include <iostream>~%#include <string>~%#include <vector>~%#include <cmath>~%using namespace std;"))
              body))))

(defun babel-element-value (lines args)
  "The value of the table, list or fixed-width text in LINES (as found after a #+RESULTS: line)."
  (let ((first (first lines)))
    (cond ((null first) nil)
          ((table-line-p first) (rows-to-value (mapcar #'table-split-line lines) args))
          ((fixed-width-line-p first)
           (format nil "~{~A~^~%~}" (mapcar (lambda (l) (if (> (length l) 2) (subseq l 2) "")) lines)))
          ((list-item-parse first) (mapcar #'fourth (list-flat-items lines)))
          (t (format nil "~{~A~^~%~}" lines)))))

(defun substitute-vars (text bindings)
  "Replace $name in TEXT by the value of each binding (name . value), longest names first."
  (dolist (binding (sort (copy-list bindings) #'> :key (lambda (b) (length (car b)))) text)
    (let ((placeholder (concatenate 'string "$" (car binding)))
          (value (if (consp (cdr binding))
                     (format nil "~{~A~^, ~}" (mapcar #'value-cell-string (cdr binding)))
                     (value-cell-string (cdr binding)))))
      (setf text (with-output-to-string (out)
                   (loop :with start := 0
                         :for at := (search placeholder text :start2 start)
                         :do (write-string text out :start start :end (or at (length text)))
                         :while at
                         :do (write-string value out)
                             (setf start (+ at (length placeholder)))))))))

(defun babel-escape-body (lines indent)
  "Inverse of `babel-body-lines': escape lines starting with * or #+ and restore the block's indentation."
  (mapcar (lambda (line)
            (let ((escaped (if (or (and (plusp (length line)) (char= (char line 0) #\*))
                                   (line-starts-with-ci-p line "#+"))
                               (concatenate 'string "," line)
                               line)))
              (if (blank-string-p escaped) escaped (concatenate 'string (make-string indent :initial-element #\space) escaped))))
          lines))

;;; END pure babel code

;;; Running programs

;;; BEGIN pure process code

(defun substitute-placeholders (argv file exe &optional output type)
  "ARGV with %f replaced by FILE, %o by EXE, %F by OUTPUT and %T by TYPE (also inside a longer word, as in -T%T)."
  (flet ((replace-all (word target value)
           (if (and value (search target word))
               (with-output-to-string (out)
                 (loop :with start := 0
                       :for at := (search target word :start2 start)
                       :do (write-string word out :start start :end (or at (length word)))
                       :while at :do (write-string value out) (setf start (+ at (length target)))))
               word)))
    (mapcar (lambda (word)
              (cond ((string= word "%f") file) ((string= word "%o") exe)
                    (t (replace-all (replace-all word "%F" output) "%T" type))))
            argv)))

(defun last-line-of (text)
  (let ((lines (remove-if #'blank-string-p (split-text-lines text))))
    (if lines (string-trim " " (car (last lines))) "")))

(defvar *babel-directory* nil
  "The directory of the buffer being evaluated: where relative :dir, :file and cross-file references start.")

(defun babel-directory (dir)
  "The directory to run in: DIR (a :dir header argument) resolved against `*babel-directory*', or that directory."
  (let ((dir (and dir (literal-var-value dir))))
    (cond ((and dir *babel-directory*) (merge-pathnames (uiop:ensure-directory-pathname dir) *babel-directory*))
          (dir (uiop:ensure-directory-pathname dir))
          (*babel-directory* (uiop:ensure-directory-pathname *babel-directory*)))))

(defun babel-run (spec program args)
  "Write PROGRAM to a temporary file, compile and run it as SPEC says, and return its standard output."
  (let* ((extension (getf (cdr spec) :extension))
         (base (merge-pathnames (format nil "org-babel-~D-~D" (get-universal-time) (random 100000))
                                (uiop:temporary-directory)))
         (file (uiop:native-namestring (make-pathname :type extension :defaults base)))
         (exe (uiop:native-namestring (if (uiop:os-windows-p) (make-pathname :type "exe" :defaults base) base)))
         (argv (getf (cdr spec) :argv))
         (compile (getf (cdr spec) :compile))
         (directory (babel-directory (header-arg args "dir")))
         (output (let ((f (header-arg args "file")))
                   (and f (uiop:native-namestring (merge-pathnames (literal-var-value f) (or directory ""))))))
         (type (let ((f (header-arg args "file"))) (and f (pathname-type (literal-var-value f)))))
         (cmdline (split-words (or (header-arg args "cmdline") ""))))
    (flet ((run (command &key directory)
             (handler-case
                 (uiop:run-program command :output :string :error-output :string :ignore-error-status t
                                           :directory directory)
               (error (e) (editor-error "Cannot run ~A: ~A" (first command) e)))))
      (unwind-protect
           (progn
             (with-open-file (out file :direction :output :if-exists :supersede :external-format :utf-8)
               (write-string program out))
             (when compile
               (multiple-value-bind (output error-output status) (run (substitute-placeholders compile file exe))
                 (unless (zerop status)
                   (editor-error "~A failed: ~A" (first compile)
                                 (last-line-of (if (plusp (length error-output)) error-output output))))))
             (when (and output (getf (cdr spec) :graphics))
               (ensure-directories-exist output))
             (multiple-value-bind (output error-output status)
                 (run (append (substitute-placeholders argv file exe output type)
                              (unless (or compile (member "%f" argv :test #'string=)) (list file))
                              cmdline)
                      :directory directory)
               (unless (zerop status)
                 (editor-error "~A failed: ~A" (first argv)
                               (last-line-of (if (plusp (length error-output)) error-output output))))
               output))
        (ignore-errors (delete-file file))
        (ignore-errors (when compile (delete-file exe)))))))

;;; END pure process code

;;; Evaluating a block

(defun babel-confirm (args)
  (let ((eval (header-arg args "eval")))
    (cond ((member eval '("never" "no") :test #'string-equal)
           (editor-error "Evaluation of this block is disabled (:eval ~A)" eval))
          ((or *org-babel-confirm-evaluate* (equal eval "query"))
           (unless (prompt-for-y-or-n-p "Run this source block? ")
             (editor-error "Cancelled"))))))

(defun babel-results-tokens (args)
  (split-words (or (header-arg args "results") "")))

(defun babel-positional-overrides (target arguments)
  "ARGUMENTS of a call as :var entries; plain values are bound, in order, to the variables TARGET declares."
  (let ((declared (loop :for (key . spec) :in (getf target :args) :when (string= key "var") :collect (var-name spec)))
        (index 0))
    (loop :for argument :in arguments
          :collect (if (position #\= argument)
                       (cons "var" argument)
                       (prog1 (cons "var" (format nil "~A=~A" (or (nth index declared) (editor-error "Too many arguments")) argument))
                         (incf index))))))

(defun babel-resolve-name (records text args)
  "The value of the :var reference TEXT, as (values value found-p)."
  (let* ((open (position #\( text))
         (name (string-trim " " (subseq text 0 open)))
         (record (and (plusp (length name)) (babel-find-named records name))))
    (when record
      (ecase (getf record :kind)
        (:table (multiple-value-bind (value header) (rows-to-value (getf record :rows) args)
                  (when (and header (null *babel-header*)) (setf *babel-header* header))
                  (values value t)))
        (:list (values (getf record :items) t))
        (:example (values (format nil "~{~A~^~%~}" (getf record :lines)) t))
        (:results
         (let* ((after (nthcdr (1+ (getf record :begin)) *babel-lines*))
                (element (subseq after 0 (results-element-length after))))
           (values (babel-element-value element args) t)))
        (:src
         (values (let ((*babel-header* nil))
                   (babel-evaluate records record
                                   :overrides (and open (babel-positional-overrides record (nth-value 1 (call-parse text))))))
                 t))))))

(defvar *babel-file-cache* (make-hash-table :test 'equal)
  "For each Org file referenced from another, (write-date records lines).")

(defun babel-file-data (file)
  "Return (values records lines) of the Org FILE, read relative to `*babel-directory*' and cached by write date."
  (let ((path (merge-pathnames file (or *babel-directory* ""))))
    (unless (probe-file path) (editor-error "No such file: ~A" file))
    (let ((cached (gethash (namestring path) *babel-file-cache*)))
      (if (and cached (eql (first cached) (file-write-date path)))
          (values (second cached) (third cached))
          (let* ((lines (uiop:read-file-lines path))
                 (records (babel-scan lines *org-todo-keywords*)))
            (setf (gethash (namestring path) *babel-file-cache*) (list (file-write-date path) records lines))
            (values records lines))))))

(defun babel-resolve (records text args)
  "The value of the :var reference TEXT (name, name[index], file.org:name or a call), as (values value found-p)."
  (multiple-value-bind (file reference) (reference-file-split text)
    (if file
        (multiple-value-bind (other-records other-lines) (babel-file-data file)
          (let ((*babel-lines* other-lines))
            (babel-resolve other-records reference args)))
        (multiple-value-bind (name index) (reference-parse text)
          (multiple-value-bind (value found) (babel-resolve-name records name args)
            (values (if (and found index) (value-index value index) value) found))))))

(defun babel-record-args (record overrides)
  "The header arguments with which RECORD runs: its own, OVERRIDES on top, and the output file worked out
from :file, :file-ext and :output-dir. A block that writes a file returns a file link."
  (let* ((args (header-args-merge (getf record :args) overrides))
         (hash (babel-hash (format nil "~{~A~^~%~}~A" (getf record :body) (getf record :language))))
         (file (babel-output-file args (getf record :name) hash)))
    (if file
        (header-args-merge args (list (cons "file" file) (cons "results" "file")))
        args)))

(defun babel-bindings (records args)
  (babel-var-values args (lambda (text) (babel-resolve records text args))))

(defun babel-noweb-text (records args body)
  "BODY with its <<references>> expanded when :noweb asks for it. Calls like <<name(1)>> run the block."
  (if (noweb-expands-p args :eval)
      (let ((plain (noweb-resolver records)))
        (noweb-expand body
                      (lambda (name)
                        (if (position #\( name)
                            (let ((value (babel-resolve records name args)))
                              (and value (format nil "~{~A~^~%~}" (value-text value))))
                            (funcall plain name)))))
      body))

(defun babel-prepare (records record overrides &optional extra-bindings)
  "Work out what running RECORD means. Return a plist: :args :language :body :bindings and, for languages run
through a file, :spec :family :program :value-wrapped, or :sql when it is an SQL block."
  (let* ((args (babel-record-args record overrides))
         (language (or (getf record :language) (editor-error "The source block has no language")))
         (resolvable (remove-if (lambda (entry) (and (string= (car entry) "var")
                                                     (assoc (var-name (cdr entry)) extra-bindings :test #'string=)))
                                args))
         (bindings (append (babel-bindings records resolvable) extra-bindings))
         (body (babel-noweb-text records args (format nil "~{~A~^~%~}" (getf record :body)))))
    (if (member language '("sql" "sqlite") :test #'string-equal)
        (list :args args :language language :body (substitute-vars body bindings) :bindings bindings :sql t)
        (let* ((spec (or (babel-language-spec language *org-babel-languages*)
                         (editor-error "No interpreter for ~A (see *org-babel-languages*)" language)))
               (family (getf (cdr spec) :family))
               (session (let ((s (header-arg args "session"))) (and s (not (string-equal s "none")) s)))
               (tokens (babel-results-tokens args))
               (value-mode (not (member "output" tokens :test #'string-equal))))
          (let ((prelude (getf (cdr spec) :prelude))
                (file (header-arg args "file")))
            (when prelude
              (unless file (editor-error "~A needs a :file header argument" language))
              (setf body (concatenate 'string
                                      (substitute-placeholders (list prelude) "" "" (literal-var-value file)
                                                               (or (pathname-type (literal-var-value file)) "png"))
                                      body))))
          (when (and bindings (not (babel-family-vars-p family)))
            (editor-error "Variables (:var) are not supported for ~A" language))
          (when (and session (not (babel-family-vars-p family)))
            (editor-error ":session is not supported for ~A" language))
          (let ((before (and session (gethash (list (buffer-name (current-buffer)) language session) *babel-sessions*))))
            (multiple-value-bind (program wrapped)
                (babel-program family args (babel-wrap-main family body)
                               :bindings bindings :value value-mode :session-before before)
              (list :args args :language language :body body :bindings bindings :spec spec :family family
                    :program program :value-wrapped wrapped :session session
                    :plain-program (babel-program family args body :bindings bindings))))))))

(defun babel-evaluate (records record &key overrides existing-hash extra-bindings)
  "Run RECORD and return its value (a string, a number, a list or a table). Second value: the program hash.
When :cache yes and the hash equals EXISTING-HASH nothing is run and the value is :cached."
  (when (> *babel-depth* 10)
    (editor-error "Blocks refer to each other too deeply (a cycle?)"))
  (let* ((*babel-depth* (1+ *babel-depth*))
         (*babel-header* nil)
         (plan (babel-prepare records record overrides extra-bindings))
         (args (getf plan :args))
         (hash (babel-hash (format nil "~A~A~S" (or (getf plan :program) (getf plan :body)) (getf plan :language)
                                   (remove "results" args :key #'car :test #'string=)))))
    (babel-confirm args)
    (cond
      ((and (header-flag-p args "cache") existing-hash (string= hash existing-hash))
       (values :cached hash))
      ((getf plan :sql)
       (let* ((rows (run-sql (sql-engine-kind (or (header-arg args "engine")
                                                  (and (string-equal (getf plan :language) "sqlite") "sqlite")
                                                  *org-sql-default-engine*))
                             args (getf plan :body))))
         (values (if (cdr rows) (list* (first rows) :rule (rest rows)) rows) hash *babel-header*)))
      (t
       (let* ((stdout (babel-run (getf plan :spec) (getf plan :program) args))
              (session (getf plan :session)))
         (multiple-value-bind (output value-text)
             (babel-split-output stdout :session (and session t) :value (getf plan :value-wrapped))
           (when session
             (push-session (list (buffer-name (current-buffer)) (getf plan :language) session)
                           (getf plan :plain-program)))
           (values (if (getf plan :value-wrapped)
                       (if value-text
                           (destructuring-bind (true false null) (json-keywords (getf plan :family))
                             (json-parse value-text :true true :false false :null null))
                           (editor-error "The block returned no value (python and js blocks need `return')"))
                       (string-right-trim '(#\newline #\return) output))
                   hash *babel-header*)))))))

(defun push-session (key program)
  (setf (gethash key *babel-sessions*) (append (gethash key *babel-sessions*) (list program))))

(define-command org-babel-reset-sessions () ()
  "Forget the programs that :session blocks replay."
  (clrhash *babel-sessions*)
  (message "Babel sessions reset"))

;;; Writing results

(defun buffer-lines (buffer)
  (split-text-lines (buffer-text buffer)))

(defun line-point (buffer number)
  "A temporary point at the start of line NUMBER (1-based) of BUFFER."
  (move-to-line (copy-point (buffer-start-point buffer) :temporary) number))

(defun babel-apply-edit (buffer edit)
  "Carry out an edit planned by `result-edit'."
  (when edit
    (ecase (first edit)
      (:insert
       (let ((p (line-point buffer (second edit))))
         (line-end p)
         (insert-string p (format nil "~%~{~A~^~%~}" (third edit)))))
      (:replace
       (replace-lines (line-point buffer (second edit)) (third edit) (fourth edit)))
      (:delete
       (let ((a (line-point buffer (second edit)))
             (b (line-point buffer (third edit))))
         (line-end a)
         (line-end b)
         (delete-between-points a b))))))

(defun babel-write-result (buffer end-index name lines handling)
  "Put the result LINES under the block or call that ends at the 0-based END-INDEX; NIL LINES remove the result."
  (babel-apply-edit buffer (result-edit (nthcdr (1+ end-index) (buffer-lines buffer)) end-index name lines handling)))

(defun babel-remove-result (buffer end-index name)
  "Delete the result of the block ending at END-INDEX. True when there was one."
  (let ((edit (result-edit (nthcdr (1+ end-index) (buffer-lines buffer)) end-index name nil "replace")))
    (babel-apply-edit buffer edit)
    (and edit t)))


;;; Library of Babel: named blocks that every file can call or read as :var

(defvar *org-babel-library-files* '()
  "Org files whose named source blocks form the Library of Babel, for #+CALL: and :var in every buffer.")

(defvar *babel-library-ingested* '()
  "Named source blocks added with `org-babel-lob-ingest'.")

(defun named-source-records (records)
  (remove-if-not (lambda (r) (and (eq (getf r :kind) :src) (getf r :name))) records))

(defun babel-library-sync ()
  "Rebuild `*babel-library-records*' from the ingested blocks and `*org-babel-library-files*'."
  (setf *babel-library-records*
        (append *babel-library-ingested*
                (loop :for file :in *org-babel-library-files*
                      :append (handler-case (named-source-records (let ((*babel-directory* nil)) (babel-file-data file)))
                                (editor-error () nil))))))

(define-command org-babel-lob-ingest () ()
  "Add the named source blocks of this buffer to the Library of Babel."
  (let ((records (named-source-records (babel-scan (buffer-lines (current-buffer)) *org-todo-keywords*))))
    (dolist (record records)
      (setf *babel-library-ingested*
            (cons record (remove (getf record :name) *babel-library-ingested*
                                 :key (lambda (r) (getf r :name)) :test #'equal))))
    (message "Ingested ~D block~:P" (length records))))

(define-command org-babel-lob-clear () ()
  "Forget the blocks ingested into the Library of Babel."
  (setf *babel-library-ingested* '())
  (message "Library of Babel cleared"))

;;; Executing blocks and calls

(defun babel-run-record (buffer records record &key overrides (end (getf record :end)) (name (getf record :name)))
  "Run the block RECORD and write its result into BUFFER under the line at the 0-based index END,
titled NAME. A #+CALL: passes its own line and the name of the block it calls. Return the value."
  (let* ((*babel-lines* (buffer-lines buffer))
         (*babel-directory* (buffer-directory buffer))
         (args (babel-record-args record overrides))
         (after (nthcdr (1+ end) *babel-lines*))
         (existing-hash (nth-value 2 (results-extent after name))))
    (multiple-value-bind (value hash header) (babel-evaluate records record :overrides overrides :existing-hash existing-hash)
      (cond
        ((eq value :cached) (message "Cached result is up to date") value)
        (t
         (let* ((post (header-arg args "post"))
                (value (if post (babel-post-process records post value) value))
                (handling (or (find-if (lambda (token) (eq (results-category token) :handling)) (babel-results-tokens args)) "replace"))
                (lines (format-babel-result value args :language (getf record :language) :name name
                                                       :hash (and (header-flag-p args "cache") hash)
                                                       :header header)))
           (cond ((member handling '("silent" "none" "discard") :test #'string-equal)
                  (message "Done: ~A" (or (second lines) "no result")))
                 ((null lines) (babel-remove-result buffer end name) (message "Done (no result)"))
                 (t (babel-write-result buffer end name lines handling)))
           value))))))

(defun babel-post-process (records post value)
  "Run the :post block, e.g. \"format(data=*this*)\", with *this* bound to VALUE."
  (multiple-value-bind (name arguments) (call-parse post)
    (let ((target (or (babel-find-named records name) (editor-error "No block called ~A" name))))
      (unless (eq (getf target :kind) :src) (editor-error ":post needs a source block"))
      (let* ((bound (find-if (lambda (a) (search "*this*" a)) arguments))
             (plain (remove bound arguments))
             (this-name (if (and bound (position #\= bound))
                            (string-trim " " (subseq bound 0 (position #\= bound)))
                            (or (first (loop :for (key . spec) :in (getf target :args)
                                             :when (string= key "var") :collect (var-name spec)))
                                "data"))))
        (values (babel-evaluate records target :overrides (babel-positional-overrides target plain)
                                               :extra-bindings (list (cons this-name value))))))))

(defun babel-record-at-point (buffer point)
  "Return (values records record) for the block, call or inline element on POINT's line."
  (babel-library-sync)
  (let* ((records (babel-scan (buffer-lines buffer) *org-todo-keywords*))
         (line (1- (line-number-at-point point)))
         (column (point-charpos point)))
    (values records (or (babel-block-at records line) (babel-call-at records line)
                        (find-if (lambda (r) (and (member (getf r :kind) '(:inline-src :inline-call))
                                                  (= (getf r :begin) line)
                                                  (<= (getf r :col) column (getf r :col-end))))
                                 records)))))

(defun babel-call-record (records call)
  "Return (values target overrides) for the #+CALL: element CALL."
  (multiple-value-bind (name arguments inside end) (call-parse (getf call :text))
    (let ((target (babel-find-named records name)))
      (unless (and target (eq (getf target :kind) :src))
        (editor-error "No source block called ~A" name))
      (values target
              (append (header-args-parse (or inside "")) (header-args-parse (or end ""))
                      (babel-positional-overrides target arguments))))))

(defun org-call-line-p (point)
  (line-starts-with-ci-p (line-string point) "#+call:"))

(defun babel-run-inline (buffer records element)
  "Run the inline source block or call ELEMENT and put {{{results(...)}}} after it. Return the value."
  (let* ((*babel-lines* (buffer-lines buffer))
         (*babel-directory* (buffer-directory buffer))
         (value (if (eq (getf element :kind) :inline-src)
                    (babel-evaluate records element)
                    (multiple-value-bind (target overrides) (babel-call-record records element)
                      (babel-evaluate records target :overrides overrides))))
         (silent (member "silent" (babel-results-tokens (getf element :args)) :test #'string-equal))
         (number (1+ (getf element :begin))))
    (if silent
        (message "Done: ~A" (inline-result-text value))
        (let ((old (line-string (line-point buffer number))))
          (replace-lines (line-point buffer number) 1
                         (list (inline-replace-result old (getf element :col-end) value)))))
    value))

(defun babel-run-element (buffer records element)
  "Run a source block, inline element or the block that a #+CALL: line names, and write the result."
  (case (getf element :kind)
    (:call
     (multiple-value-bind (target overrides) (babel-call-record records element)
       (babel-run-record buffer records target :overrides overrides
                                               :end (getf element :end) :name (getf target :name))))
    ((:inline-src :inline-call) (babel-run-inline buffer records element))
    (t (babel-run-record buffer records element))))

(defun org-babel-execute (point)
  "Run the source block or #+CALL: line around POINT and write the result under it."
  (let ((buffer (point-buffer point)))
    (multiple-value-bind (records record) (babel-record-at-point buffer point)
      (unless record
        (editor-error "Not in a source block"))
      (babel-run-element buffer records record))))

(define-command org-babel-execute-src-block () ()
  "Run the source block or #+CALL: line at point."
  (org-babel-execute (current-point)))

(defun execute-range (buffer start-line end-line-function)
  "Run every block and call in BUFFER whose begin line lies between START-LINE and (funcall END-LINE-FUNCTION),
top to bottom, rereading the buffer after each since results move later lines. Return the number run."
  (babel-library-sync)
  (let ((count 0))
    (loop :for k :from 0
          :do (let* ((records (babel-scan (buffer-lines buffer) *org-todo-keywords*))
                     (limit (funcall end-line-function))
                     (targets (remove-if-not (lambda (r) (and (member (getf r :kind) '(:src :call))
                                                              (<= start-line (getf r :begin))
                                                              (< (getf r :begin) limit)))
                                             records))
                     (record (nth k targets)))
                (unless record (return))
                (let ((eval (header-arg (getf record :args) "eval")))
                  (unless (member eval '("never" "no" "never-export" "no-export") :test #'string-equal)
                    (handler-case
                        (progn (babel-run-element buffer records record)
                               (incf count))
                      (editor-error (e)
                        (editor-error "Line ~D: ~A" (1+ (getf record :begin)) e)))))))
    count))

(define-command org-babel-execute-buffer () ()
  "Run all source blocks and #+CALL: lines of the buffer, top to bottom."
  (let ((buffer (current-buffer)))
    (message "Ran ~D block~:P"
             (execute-range buffer 0 (lambda () (length (buffer-lines buffer)))))))

(define-command org-babel-execute-subtree () ()
  "Run all source blocks and #+CALL: lines under the current heading."
  (let* ((buffer (current-buffer))
         (heading (heading-point (current-point)))
         (start (1- (line-number-at-point heading))))
    (message "Ran ~D block~:P"
             (execute-range buffer start
                            (lambda ()
                              (let ((p (line-point buffer (1+ start))))
                                (1- (line-number-at-point (nth-value 1 (org-subtree-bounds p))))))))))

;;; Navigation and inspection

(defun babel-blocks (buffer)
  (remove-if-not (lambda (r) (eq (getf r :kind) :src))
                 (babel-scan (buffer-lines buffer) *org-todo-keywords*)))

(define-command org-babel-next-src-block () ()
  "Move to the next source block."
  (let* ((line (1- (line-number-at-point (current-point))))
         (next (find-if (lambda (r) (> (getf r :begin) line)) (babel-blocks (current-buffer)))))
    (unless next (editor-error "No further source block"))
    (move-to-line (current-point) (1+ (getf next :begin)))))

(define-command org-babel-previous-src-block () ()
  "Move to the previous source block."
  (let* ((line (1- (line-number-at-point (current-point))))
         (previous (find-if (lambda (r) (< (getf r :begin) line)) (babel-blocks (current-buffer)) :from-end t)))
    (unless previous (editor-error "No earlier source block"))
    (move-to-line (current-point) (1+ (getf previous :begin)))))

(define-command org-babel-goto-named-src-block () ()
  "Move to the block, table, list or result called NAME."
  (let* ((name (prompt-for-string "Name: "))
         (record (babel-find-named (babel-scan (buffer-lines (current-buffer)) *org-todo-keywords*) name)))
    (unless record (editor-error "Nothing is called ~A" name))
    (move-to-line (current-point) (1+ (getf record :begin)))))

(define-command org-babel-remove-result () ()
  "Delete the result of the source block at point."
  (let ((buffer (current-buffer)))
    (multiple-value-bind (records record) (babel-record-at-point buffer (current-point))
      (declare (ignore records))
      (unless record (editor-error "Not in a source block"))
      (unless (babel-remove-result buffer (getf record :end) (getf record :name))
        (editor-error "This block has no result")))))

(define-command org-babel-demarcate-block () ()
  "Split the source block at point in two at the current line; outside a block insert an empty one."
  (let ((point (current-point)))
    (multiple-value-bind (begin end) (src-block-bounds point)
      (cond
        ((and begin (point< begin (line-start (copy-point point :temporary)))
              (point< (line-start (copy-point point :temporary)) end))
         (let* ((header (line-string begin))
                (indent (make-string (line-indent header) :initial-element #\space)))
           (line-start point)
           (insert-string point (format nil "~A#+end_src~%~%~A~%" indent (string-left-trim " " header)))
           (line-offset point -1)
           (line-start point)))
        (t
         (let ((indent (make-string (line-indent (line-string point)) :initial-element #\space)))
           (line-start point)
           (insert-string point (format nil "~A#+begin_src ~%~A~%~A#+end_src~%" indent indent indent))
           (line-offset point -3)
           (line-end point)))))))

(define-command org-babel-open-src-block-result () ()
  "Move to the #+RESULTS: line of the source block at point."
  (multiple-value-bind (records record) (babel-record-at-point (current-buffer) (current-point))
    (declare (ignore records))
    (unless record (editor-error "Not in a source block"))
    (multiple-value-bind (offset) (results-extent (nthcdr (1+ (getf record :end)) (buffer-lines (current-buffer)))
                                                  (getf record :name))
      (unless offset (editor-error "This block has no result"))
      (move-to-line (current-point) (+ (getf record :end) 2 offset)))))

(define-command org-babel-info () ()
  "Show the language and the header arguments in effect for the source block at point."
  (multiple-value-bind (records record) (babel-record-at-point (current-buffer) (current-point))
    (declare (ignore records))
    (unless record (editor-error "Not in a source block"))
    (message "~A~@[ #+NAME: ~A~]~{ :~A ~A~}" (or (getf record :language) "(no language)") (getf record :name)
             (loop :for (key . value) :in (getf record :args) :append (list key value)))))

(defun babel-expanded-text (buffer point)
  "The program that would run for the source block at POINT: variables, noweb and wrappers expanded."
  (multiple-value-bind (records record) (babel-record-at-point buffer point)
    (unless (and record (eq (getf record :kind) :src)) (editor-error "Not in a source block"))
    (let* ((*babel-lines* (buffer-lines buffer))
           (*babel-header* nil)
           (plan (babel-prepare records record nil)))
      (or (getf plan :program) (getf plan :body)))))

(define-command org-babel-expand-src-block () ()
  "Show the program that would run for the source block at point: variables, noweb and wrappers expanded."
  (let ((text (babel-expanded-text (current-buffer) (current-point)))
        (view (or (get-buffer "*Org Babel Expanded*") (make-buffer "*Org Babel Expanded*"))))
    (erase-buffer view)
    (insert-string (buffer-point view) text)
    (switch-to-buffer view)))

;;; Tangle

(define-command org-babel-tangle () ()
  "Write the source blocks that have a :tangle argument to their files, next to the buffer."
  (let* ((buffer (current-buffer))
         (file (buffer-filename buffer))
         (plan (babel-tangle-plan (babel-scan (buffer-lines buffer) *org-todo-keywords*)
                                  (if file (pathname-name file) (buffer-name buffer))
                                  (if file (file-namestring file) (buffer-name buffer)))))
    (unless plan
      (editor-error "No source block has a :tangle argument"))
    (dolist (entry plan)
      (let ((target (merge-pathnames (getf entry :file) (or (buffer-directory buffer) ""))))
        (when (getf entry :mkdirp)
          (ensure-directories-exist target))
        (with-open-file (out target :direction :output :if-exists :supersede :external-format :utf-8)
          (write-string (getf entry :text) out))
        (when (getf entry :mode)
          (ignore-errors
           (uiop:run-program (list "chmod" (format nil "~O" (getf entry :mode)) (uiop:native-namestring target))
                             :ignore-error-status t)))))
    (message "Tangled ~D file~:P" (length plan))))

;;; Editing a block in its own buffer

(define-minor-mode org-src-edit-mode
    (:name "Org-Src"
     :keymap *org-src-edit-mode-keymap*))

(define-key *org-src-edit-mode-keymap* "C-c '" 'org-edit-src-exit)
(define-key *org-src-edit-mode-keymap* "C-c C-c" 'org-edit-src-exit)
(define-key *org-src-edit-mode-keymap* "C-c C-k" 'org-edit-src-abort)

(defun src-edit-buffer (buffer point)
  "A new buffer holding the body of the source block at POINT, in the mode of its language."
  (multiple-value-bind (records record) (babel-record-at-point buffer point)
    (declare (ignore records))
    (unless (and record (eq (getf record :kind) :src)) (editor-error "Not in a source block"))
    (let* ((language (getf record :language))
           (spec (and language (babel-language-spec language *org-babel-languages*)))
           (extension (or (getf (cdr spec) :extension) (and language (string-downcase language)) "txt"))
           (edit (make-buffer (format nil "*Org Src ~A[~A]*" (or (buffer-name buffer) "") (or language "text")))))
      (erase-buffer edit)
      (insert-string (buffer-point edit) (format nil "~{~A~^~%~}" (getf record :body)))
      (let ((mode (get-file-mode (format nil "edit.~A" extension))))
        (when mode (change-buffer-mode edit mode)))
      (setf (buffer-value edit 'src-origin) (list buffer (getf record :begin) language (getf record :indent)))
      edit)))

(define-command org-edit-src-block () ()
  "Edit the body of the source block at point in a separate buffer in the language's own mode.
Finish with C-c ' (write back) or C-c C-k (discard)."
  (let ((edit (src-edit-buffer (current-buffer) (current-point))))
    (switch-to-buffer edit)
    (org-src-edit-mode t)
    (buffer-start (current-point))))

(defun src-edit-origin (edit)
  (or (buffer-value edit 'src-origin)
      (editor-error "This is not a source edit buffer")))

(defun src-edit-write-back (edit)
  "Copy the text of the edit buffer EDIT into the source block it came from. Return the Org buffer."
  (destructuring-bind (origin begin language indent) (src-edit-origin edit)
    (let* ((body (trim-trailing-blank-lines (split-text-lines (buffer-text edit))))
           (records (babel-scan (buffer-lines origin) *org-todo-keywords*))
           (record (find-if (lambda (r) (and (eq (getf r :kind) :src) (= (getf r :begin) begin))) records)))
      (unless (and record (equal (getf record :language) language))
        (editor-error "The source block moved or changed; copy your text and edit the block again"))
      (let ((old-count (length (getf record :body)))
            (new (babel-escape-body body indent)))
        (if (zerop old-count)
            (let ((p (line-point origin (1+ begin))))
              (line-end p)
              (when new (insert-string p (format nil "~%~{~A~^~%~}" new))))
            (replace-lines (line-point origin (+ begin 2)) old-count (or new (list ""))))
        origin))))

(define-command org-edit-src-exit () ()
  "Write the edited text back into the source block and return to the Org buffer."
  (let* ((edit (current-buffer))
         (origin (src-edit-write-back edit)))
    (switch-to-buffer origin)
    (delete-buffer edit)))

(define-command org-edit-src-abort () ()
  "Discard the edit buffer and return to the Org buffer."
  (let* ((edit (current-buffer))
         (origin (first (src-edit-origin edit))))
    (switch-to-buffer origin)
    (delete-buffer edit)))
