;;; Export: from Org text to HTML and Markdown (ASCII, LaTeX, Org and iCalendar are in export-more.lisp).
;;;
;;; The pipeline is pure so that it can be tested without Lem:
;;;   lines --export-preprocess--> lines   (#+SETUPFILE, #+INCLUDE, {{{macros}}}, COMMENT / :noexport: subtrees)
;;;         --export-parse-------> nodes   (headings, paragraphs, lists, tables, blocks ...) and meta (#+TITLE ...)
;;;         --export-sections----> sections (nested by heading level, with numbers and ids)
;;;         --backends-----------> text
;;; Options come from #+OPTIONS: lines (toc:t num:t H:3 ^:{} tags:t todo:t ...), see `export-options'.

(in-package :lem-org-mode)

;;; BEGIN pure export code

(defvar *org-export-reader* nil
  "A function that returns the lines of a file (for #+INCLUDE: and #+SETUPFILE:) or NIL. Set by the Lem commands.")

(defvar *org-export-html-head* nil
  "Extra text put in the <head> of every exported HTML file.")

(defvar *org-export-with-toc* t)
(defvar *org-export-with-section-numbers* t)
(defvar *org-export-headline-levels* 3)
(defvar *org-export-exclude-tags* '("noexport"))
(defvar *org-export-select-tags* '("export"))
(defvar *org-export-mathjax* t
  "True: exported HTML that contains math loads MathJax from a CDN.")
(defvar *org-export-creator* "Lem Org mode")

(defvar *export-ctx* nil "The `export-ctx' of the export in progress, or NIL.")

(defstruct export-ctx
  (backend :html) (options nil) (meta nil)
  (footnotes (make-array 0 :adjustable t :fill-pointer t))  ; entries: (label number nodes)
  (definitions nil)           ; alist label -> lines
  (ids (make-hash-table :test 'equal))   ; "kind:key" -> html id, for links
  (counters (list :table 0 :figure 0 :listing 0))
  (math nil) (macros nil) (abbreviations nil) (file nil) (mtime nil) (used-ids (make-hash-table :test 'equal)))

;;; Utilities

(defun html-escape (string)
  (with-output-to-string (out)
    (loop :for c :across string
          :do (case c
                (#\& (write-string "&amp;" out))
                (#\< (write-string "&lt;" out))
                (#\> (write-string "&gt;" out))
                (#\" (write-string "&quot;" out))
                (t (write-char c out))))))

(defun word-char-p (c)
  (or (alphanumericp c) (char= c #\_)))

(defun line-indent (line)
  (or (position #\space line :test-not #'char=) (length line)))

(defun blank-string-p (line)
  (string= (string-trim '(#\space #\tab) line) ""))

(defun join-lines (lines)
  (format nil "~{~A~^~%~}" lines))

(defun meta-values (meta key)
  (loop :for (name . value) :in meta :when (string= name key) :collect value))

(defun meta-get (meta key)
  (cdr (assoc key meta :test #'string=)))

(defun keyword-line (line)
  "Return (values name value) for a #+NAME: value line."
  (let* ((s (string-left-trim '(#\space #\tab) line))
         (colon (and (>= (length s) 3) (string= "#+" s :end2 2) (position #\: s))))
    (when (and colon (> colon 2) (not (find #\space s :end colon)))
      (values (string-upcase (subseq s 2 colon)) (string-trim " " (subseq s (1+ colon)))))))

;;; Options

(defun options-parse (string)
  "Read the tokens of an #+OPTIONS: line such as \"toc:2 num:nil ^:{} H:4\" into a plist."
  (let ((options '()))
    (dolist (token (split-words string) (nreverse options))
      (let ((colon (position #\: token :start (min 1 (length token)))))
        (when (and colon (< (1+ colon) (length token)))
          (let* ((key (subseq token 0 colon)) (value (subseq token (1+ colon)))
                 (parsed (cond ((string= value "t") t)
                               ((string= value "nil") nil)
                               ((every #'digit-char-p value) (parse-integer value))
                               ((string= value "{}") :brace)
                               (t value)))
                 (name (cdr (assoc key '(("toc" . :toc) ("num" . :num) ("H" . :levels) ("^" . :sub-super)
                                         ("tags" . :tags) ("todo" . :todo) ("pri" . :priority) ("p" . :planning)
                                         ("\\n" . :breaks) ("e" . :entities) ("f" . :footnotes) ("author" . :author)
                                         ("date" . :date) ("email" . :email) ("title" . :title) ("creator" . :creator)
                                         ("-" . :special-strings) ("*" . :emphasis) ("|" . :tables)
                                         ("timestamp" . :timestamp) ("tasks" . :tasks) ("d" . :drawers)
                                         ("stat" . :stat) ("<" . :active-timestamps) (":" . :fixed-width)
                                         ("skip" . :skip) ("broken-links" . :broken-links) ("arch" . :archived))
                                   :test #'string=))))
            (when name
              (push name options)
              (push parsed options))))))))

(defun export-options (meta)
  "The options in effect: defaults, then the #+OPTIONS: lines in META."
  (let ((options (list :toc *org-export-with-toc* :num *org-export-with-section-numbers*
                       :levels *org-export-headline-levels* :sub-super :brace :tags t :todo t :priority t
                       :planning nil :breaks nil :entities t :footnotes t :author t :date t :email nil :title t
                       :creator t :special-strings t :emphasis t :tables t :timestamp t :tasks t :drawers nil
                       :stat t :active-timestamps t :fixed-width t :skip nil :archived :headline)))
    (dolist (line (meta-values meta "OPTIONS") options)
      (loop :for (key value) :on (options-parse line) :by #'cddr
            :do (setf (getf options key) value)))))

(defvar *default-export-options* nil)

(defun option (name)
  "The value of the export option NAME (:toc :num :tags ...) in the export in progress, or its default."
  (getf (if *export-ctx*
            (export-ctx-options *export-ctx*)
            (or *default-export-options* (setf *default-export-options* (export-options nil))))
        name))

;;; Preprocessing

(defun split-quoted-words (string)
  "Words of STRING; a \"double quoted\" word loses its quotes."
  (mapcar (lambda (w) (if (and (> (length w) 1) (char= (char w 0) #\") (char= (char w (1- (length w))) #\"))
                          (subseq w 1 (1- (length w)))
                          w))
          (split-words string)))

(defun lines-range (lines spec)
  "LINES restricted by SPEC, \"5-10\" (lines 5 to 9), \"5-\" or \"-10\"."
  (let* ((dash (position #\- spec))
         (from (if (and dash (plusp dash)) (parse-integer spec :end dash :junk-allowed t) 1))
         (to (if (and dash (< (1+ dash) (length spec))) (parse-integer spec :start (1+ dash) :junk-allowed t) (1+ (length lines)))))
    (subseq lines (max 0 (1- (or from 1))) (min (length lines) (max 0 (1- (or to (1+ (length lines)))))))))

(defun include-block-lines (lines kind)
  "LINES wrapped as the block KIND of an #+INCLUDE: line (src LANG, example, export BACKEND, quote ...)."
  (let* ((words (split-words kind)) (name (first words)))
    (append (list (format nil "#+begin_~A~{ ~A~}" name (rest words)))
            (if (member name '("src" "example" "export") :test #'string=)
                (mapcar (lambda (l) (if (or (and (plusp (length l)) (char= (char l 0) #\*)) (line-starts-with-ci-p l "#+"))
                                        (concatenate 'string "," l)
                                        l))
                        lines)
                lines)
            (list (format nil "#+end_~A" name)))))

(defun lines-min-level (lines keywords)
  (let ((levels (loop :for l :in lines :for lv := (heading-split l keywords) :when lv :collect lv)))
    (and levels (reduce #'min levels))))

(defun export-expand-includes (lines keywords reader &optional (depth 0))
  "Replace the #+INCLUDE: lines of LINES by the contents of the files READER finds."
  (if (> depth 5)
      lines
      (let ((out '()) (in-block nil))
        (dolist (line lines (nreverse out))
          (cond (in-block (push line out) (when (line-starts-with-ci-p line "#+end_") (setf in-block nil)))
                ((line-starts-with-ci-p line "#+include:")
                 (let* ((words (split-quoted-words (subseq (string-left-trim " " line) 10)))
                        (file (first words))
                        (included (and file reader (funcall reader (subseq file 0 (search "::" file)))))
                        (kind-words (loop :for w :in (rest words) :until (header-key-p w) :collect w))
                        (range (cdr (assoc "lines" (header-args-from-words (member-if #'header-key-p (rest words))) :test #'string=)))
                        (minlevel (let ((m (cdr (assoc "minlevel" (header-args-from-words (member-if #'header-key-p (rest words))) :test #'string=))))
                                    (and m (parse-integer m :junk-allowed t)))))
                   (cond ((null included) (push (format nil "# Cannot include ~A" file) out))
                         (t (let ((body (if range (lines-range included (string-trim "\"" range)) included)))
                              (cond ((and kind-words (not (string= (first kind-words) "org")))
                                     (dolist (l (include-block-lines body (format nil "~{~A~^ ~}" kind-words))) (push l out)))
                                    (t (let* ((body (export-expand-includes body keywords reader (1+ depth)))
                                              (current (lines-min-level body keywords))
                                              (body (if (and minlevel current (/= minlevel current))
                                                        (lines-shift-levels body (- minlevel current) keywords)
                                                        body)))
                                         (dolist (l body) (push l out))))))))))
                (t (push line out)
                   (when (and (line-starts-with-ci-p line "#+begin_") (not (line-starts-with-ci-p line "#+end_")))
                     (setf in-block t))))))))

(defun macro-definitions (lines)
  "The #+MACRO: definitions of LINES as an alist (name . replacement)."
  (loop :for value :in (file-keyword-values lines "MACRO")
        :for space := (position #\space value)
        :when space :collect (cons (subseq value 0 space) (string-trim " " (subseq value space)))))

(defun macro-split-arguments (text)
  "Split the argument text of a macro call at commas that are not escaped with a backslash."
  (let ((args '()) (current (make-string-output-stream)) (i 0))
    (loop :while (< i (length text))
          :do (let ((c (char text i)))
                (cond ((and (char= c #\\) (< (1+ i) (length text)) (char= (char text (1+ i)) #\,))
                       (write-char #\, current) (incf i 2))
                      ((char= c #\,) (push (string-trim " " (get-output-stream-string current)) args) (incf i))
                      (t (write-char c current) (incf i)))))
    (push (string-trim " " (get-output-stream-string current)) args)
    (nreverse args)))

(defun macro-substitute (template arguments)
  "TEMPLATE with $1 .. $9 replaced by ARGUMENTS."
  (with-output-to-string (out)
    (loop :with i := 0
          :while (< i (length template))
          :do (let ((c (char template i)))
                (if (and (char= c #\$) (< (1+ i) (length template)) (digit-char-p (char template (1+ i))))
                    (progn (write-string (or (nth (1- (digit-char-p (char template (1+ i)))) arguments) "") out) (incf i 2))
                    (progn (write-char c out) (incf i)))))))

(defun macro-expand-line (line definitions builtin)
  "LINE with every {{{name(args)}}} replaced. BUILTIN is called with the name and arguments for names that
DEFINITIONS (an alist) lack; it returns a string or NIL (leave the macro alone)."
  (let ((out (make-string-output-stream)) (pos 0))
    (loop
      (let* ((open (search "{{{" line :start2 pos))
             (close (and open (search "}}}" line :start2 (+ open 3)))))
        (cond ((and open close)
               (write-string line out :start pos :end open)
               (let* ((call (subseq line (+ open 3) close))
                      (paren (position #\( call))
                      (name (subseq call 0 paren))
                      (arguments (and paren (char= (char call (1- (length call))) #\))
                                      (macro-split-arguments (subseq call (1+ paren) (1- (length call))))))
                      (definition (assoc name definitions :test #'string=))
                      (replacement (if definition
                                       (macro-substitute (cdr definition) arguments)
                                       (funcall builtin name arguments))))
                 (write-string (or replacement (subseq line open (+ close 3))) out))
               (setf pos (+ close 3)))
              (t (write-string line out :start pos) (return)))))
    (get-output-stream-string out)))

(defun export-expand-macros (lines definitions builtin)
  "LINES with macros expanded outside source, example and export blocks."
  (let ((in-block nil))
    (mapcar (lambda (line)
              (cond (in-block (when (line-starts-with-ci-p line "#+end_") (setf in-block nil)) line)
                    ((and (line-starts-with-ci-p line "#+begin_")
                          (not (line-starts-with-ci-p line "#+begin_quote")) (not (line-starts-with-ci-p line "#+begin_center"))
                          (not (line-starts-with-ci-p line "#+begin_verse")))
                     (setf in-block t) line)
                    ((line-starts-with-ci-p line "#+macro:") line)
                    ((search "{{{" line) (macro-expand-line line definitions builtin))
                    (t line)))
            lines)))

(defun export-filter-subtrees (lines keywords &key (exclude '("noexport")) select (archived :headline))
  "LINES without the subtrees that are commented, carry an EXCLUDE tag, or (when SELECT tags exist in the text) lack a
SELECT tag. An archived subtree keeps only its heading when ARCHIVED is :headline."
  (let* ((v (coerce lines 'vector)) (n (length v)) (heading-indexes (heading-indexes-of v keywords))
         (selecting (and select (some (lambda (i) (intersection select (nth-value 4 (heading-split (aref v i) keywords)) :test #'string-equal))
                                      heading-indexes)))
         (drop (make-array n :initial-element nil))
         (keep-heading-only (make-array n :initial-element nil)))
    (loop :for (index . later) :on heading-indexes
          :do (multiple-value-bind (level todo priority title tags) (heading-split (aref v index) keywords)
                (declare (ignore todo priority))
                (let ((end (or (loop :for j :in later :when (<= (heading-split (aref v j) keywords) level) :return j) n)))
                  (cond ((or (intersection exclude tags :test #'string-equal)
                             (and title (>= (length title) 7) (string= "COMMENT" title :end2 7)
                                  (or (= (length title) 7) (char= (char title 7) #\space))))
                         (loop :for k :from index :below end :do (setf (aref drop k) t)))
                        ((and (member "ARCHIVE" tags :test #'string-equal) archived)
                         (if (eq archived :headline)
                             (loop :for k :from (1+ index) :below end :do (setf (aref drop k) t))
                             (loop :for k :from index :below end :do (setf (aref drop k) t))))))))
    (when selecting
      (let ((selected (make-array n :initial-element nil)))
        (loop :for (index . later) :on heading-indexes
              :do (multiple-value-bind (level todo priority title tags) (heading-split (aref v index) keywords)
                    (declare (ignore todo priority title))
                    (when (intersection select tags :test #'string-equal)
                      (let ((end (or (loop :for j :in later :when (<= (heading-split (aref v j) keywords) level) :return j) n)))
                        (loop :for k :from index :below end :do (setf (aref selected k) t))
                        ;; ancestors keep their heading line
                        (let ((want (1- level)))
                          (loop :for j :in (reverse (subseq heading-indexes 0 (position index heading-indexes)))
                                :while (plusp want)
                                :do (when (= (heading-split (aref v j) keywords) want)
                                      (setf (aref selected j) t (aref keep-heading-only j) t)
                                      (decf want))))))))
        (dotimes (k n)
          (when (and (not (aref selected k)) (>= k (or (first heading-indexes) n)))
            (setf (aref drop k) t)))))
    (loop :for k :from 0 :below n :unless (aref drop k) :collect (aref v k))))

;;; Parsing

(defun block-name (line)
  "For \"#+begin_src sql\" return \"src\" and the rest of the line."
  (let* ((s (string-left-trim '(#\space #\tab) line))
         (words (split-words (subseq s 8))))
    (values (string-downcase (or (first words) "")) (rest words))))

(defun drawer-start-p (line)
  (let ((s (string-trim '(#\space #\tab) line)))
    (and (> (length s) 2) (char= (char s 0) #\:) (char= (char s (1- (length s))) #\:)
         (notany (lambda (c) (member c '(#\space #\tab))) s)
         (not (string-equal s ":END:")))))

(defun ordered-item-p (line)
  (let ((bullet (nth-value 1 (list-item-parse line))))
    (and bullet (digit-char-p (char bullet 0)) t)))

(defun paragraph-break-p (line keywords)
  (or (blank-string-p line)
      (heading-split line keywords)
      (table-line-p line)
      (list-item-parse line)
      (line-starts-with-ci-p line "#+")
      (and (>= (length line) 2) (string= ": " line :end2 2))
      (drawer-start-p line)
      (footnote-definition-label line)))

(defun list-flat-items (lines)
  "Turn the LINES of a list into (indent bullet checkbox text) items; continuation lines join the item text."
  (let ((items '()))
    (dolist (line lines)
      (multiple-value-bind (indent bullet checkbox content) (list-item-parse line)
        (cond (indent
               (push (list indent bullet checkbox (string-trim " " (subseq line content))) items))
              ((and items (string/= (string-trim " " line) ""))
               (setf (fourth (first items))
                     (format nil "~A ~A" (fourth (first items)) (string-trim " " line)))))))
    (nreverse items)))

(defun list-tree (items)
  "Group flat ITEMS (indent bullet checkbox text) into nodes (bullet checkbox text children)."
  (when items
    (let ((indent (first (first items))) (result '()))
      (loop :while items
            :do (let* ((item (pop items))
                       (children (loop :while (and items (> (first (first items)) indent))
                                       :collect (pop items))))
                  (push (list (second item) (third item) (fourth item) (list-tree children)) result)))
      (nreverse result))))

(defun list-region-end (lines start)
  "Index after the last line of the list that begins at START in the vector LINES."
  (let* ((base (line-indent (aref lines start))) (n (length lines)) (j start))
    (loop :while (< j n)
          :do (let ((line (aref lines j)))
                (cond ((blank-string-p line)
                       (let ((next (and (< (1+ j) n) (aref lines (1+ j)))))
                         (if (and next (not (blank-string-p next))
                                  (or (list-item-parse next) (> (line-indent next) base)))
                             (incf j)
                             (return))))
                      ((and (list-item-parse line) (= (line-indent line) base)
                            (not (eq (ordered-item-p line) (ordered-item-p (aref lines start)))))
                       (return))
                      ((or (and (list-item-parse line) (>= (line-indent line) base))
                           (> (line-indent line) base))
                       (incf j))
                      (t (return)))))
    j))

(defun affiliated-wrap (pending node)
  "NODE, wrapped as (:affiliated plist node) when PENDING (a plist of captions, names and attributes) is not empty."
  (if pending (list :affiliated pending node) node))

(defun export-parse (lines keywords)
  "Return (values nodes meta) for the Org text LINES. META is an alist such as ((\"TITLE\" . \"Notes\")) with one entry per
#+KEYWORD: line. Nodes: (:heading level todo priority title tags), (:properties alist), (:planning text), (:paragraph text),
(:list ordered tree), (:table rows), (:src language lines), (:example lines), (:quote nodes), (:center nodes), (:verse lines),
(:raw backend lines), (:block name nodes), (:rule), (:footnote-def label lines), (:toc depth local), and
(:affiliated plist node) for an element with #+CAPTION: #+NAME: or #+ATTR_*: lines."
  (let* ((all-lines lines) (lines (coerce lines 'vector)) (n (length lines)) (i 0)
         (nodes '()) (meta '()) (exports "code") (records nil) (scanned nil) (pending '()))
    (labels ((emit (node)
               (push (affiliated-wrap pending node) nodes)
               (setf pending '()))
             (babel-records ()
               (unless scanned (setf records (babel-scan all-lines keywords) scanned t))
               records)
             (find-end (name from)
               (or (position-if (lambda (line) (and (line-starts-with-ci-p line "#+end_")
                                                    (string-equal name (string-trim " " (subseq (string-left-trim " " line) 6)))))
                                lines :start from)
                   n)))
      (loop :while (< i n)
            :do (let ((line (aref lines i)))
                  (cond
                    ((blank-string-p line) (incf i))
                    ((line-starts-with-ci-p line "#+begin_")
                     (multiple-value-bind (name args) (block-name line)
                       (let* ((end (find-end name (1+ i)))
                              (inner (coerce (subseq lines (1+ i) end) 'list)))
                         (cond ((string= name "src")
                                (multiple-value-bind (language header switches) (src-header line)
                                  (let* ((record (find-if (lambda (r) (and (eq (getf r :kind) :src) (= (getf r :begin) i)))
                                                          (babel-records)))
                                         (args (if record (getf record :args) header))
                                         (body (if record (getf record :body) (babel-body-lines inner (line-indent line)))))
                                    (setf exports (or (header-arg args "exports") "code"))
                                    (when (and record (noweb-expands-p args :export))
                                      (setf body (split-text-lines (noweb-expand (format nil "~{~A~^~%~}" body)
                                                                                 (noweb-resolver (babel-records))))))
                                    (when (and record (noweb-strips-p args))
                                      (setf body (split-text-lines (noweb-expand (format nil "~{~A~^~%~}" body)
                                                                                 (lambda (name) (declare (ignore name)) "")))))
                                    (if (member exports '("code" "both") :test #'string=)
                                        (emit (list :src language body switches))
                                        (setf pending '())))))
                               ((string= name "quote") (emit (list :quote (export-parse inner keywords))))
                               ((string= name "center") (emit (list :center (export-parse inner keywords))))
                               ((string= name "verse") (emit (list :verse inner)))
                               ((string= name "comment") (setf pending '()))
                               ((string= name "export") (emit (list :raw (string-downcase (or (first args) "")) inner)))
                               ((string= name "example") (emit (list :example (babel-body-lines inner (line-indent line)))))
                               (t (emit (list :block name (export-parse inner keywords) args))))
                         (setf i (1+ end)))))
                    ((keyword-line line)
                     (multiple-value-bind (name value) (keyword-line line)
                       (cond ((string= name "RESULTS")
                              (let ((end (or (position-if-not
                                              (lambda (l) (or (table-line-p l)
                                                              (and (>= (length l) 2) (string= ": " l :end2 2))))
                                              lines :start (1+ i))
                                             n)))
                                (when (member exports '("results" "both") :test #'string=)
                                  (let ((results (coerce (subseq lines (1+ i) end) 'list)))
                                    (if (and results (table-line-p (first results)))
                                        (emit (list :table (mapcar #'table-split-line results)))
                                        (emit (list :example (mapcar (lambda (l) (subseq l 2)) results))))))
                                (setf i end)))
                             ((string= name "HTML") (emit (list :raw "html" (list value))) (incf i))
                             ((string= name "LATEX") (emit (list :raw "latex" (list value))) (incf i))
                             ((string= name "ASCII") (emit (list :raw "ascii" (list value))) (incf i))
                             ((string= name "TOC")
                              (let* ((words (split-words value))
                                     (depth (find-if (lambda (w) (every #'digit-char-p w)) words)))
                                (emit (list :toc (and depth (parse-integer depth)) (and (member "local" words :test #'string=) t))))
                              (incf i))
                             ((string= name "CAPTION") (setf pending (append pending (list :caption value))) (incf i))
                             ((string= name "NAME") (setf pending (append pending (list :name value))) (incf i))
                             ((and (> (length name) 5) (string= "ATTR_" name :end2 5))
                              (setf pending (append pending (list (intern name :keyword) (header-args-from-words (split-words value)))))
                              (incf i))
                             (t (push (cons name value) meta) (incf i)))))
                    ((or (string= (string-trim " " line) "#")
                         (line-starts-with-ci-p line "# "))
                     (incf i))
                    ((heading-split line keywords)
                     (multiple-value-bind (level todo priority title tags) (heading-split line keywords)
                       (emit (list :heading level todo priority title tags))
                       (incf i)
                       (when (and (< i n) (planning-line-p (aref lines i)))
                         (emit (list :planning (string-trim " " (aref lines i))))
                         (incf i))
                       (when (and (< i n) (string-equal (string-trim " " (aref lines i)) ":PROPERTIES:"))
                         (let ((end (or (position-if (lambda (l) (string-equal (string-trim " " l) ":END:")) lines :start i) (1- n))))
                           (emit (list :properties (entry-property-alist (coerce (subseq lines i (1+ end)) 'list))))
                           (setf i (1+ end))))))
                    ((drawer-start-p line)
                     (setf i (let ((end (position-if (lambda (l) (string-equal (string-trim " " l) ":END:"))
                                                     lines :start i)))
                               (if end (1+ end) (1+ i)))))
                    ((footnote-definition-label line)
                     (let* ((label (footnote-definition-label line))
                            (end (or (position-if (lambda (l) (or (heading-split l keywords) (footnote-definition-label l)))
                                                  lines :start (1+ i))
                                     n))
                            (body (cons (string-trim " " (subseq line (1+ (position #\] line))))
                                        (coerce (subseq lines (1+ i) end) 'list))))
                       (emit (list :footnote-def label (trim-trailing-blank-lines body)))
                       (setf i end)))
                    ((table-line-p line)
                     (let ((end (or (position-if-not #'table-line-p lines :start i) n)))
                       (emit (list :table (map 'list #'table-split-line (subseq lines i end))))
                       (setf i end)))
                    ((and (>= (length line) 2) (string= ": " line :end2 2))
                     (let ((end (or (position-if-not (lambda (l) (and (>= (length l) 2) (string= ": " l :end2 2)))
                                                     lines :start i)
                                    n)))
                       (emit (list :example (map 'list (lambda (l) (subseq l 2)) (subseq lines i end))))
                       (setf i end)))
                    ((and (>= (length (string-trim " " line)) 5)
                          (every (lambda (c) (char= c #\-)) (string-trim " " line)))
                     (emit (list :rule))
                     (incf i))
                    ((list-item-parse line)
                     (let* ((end (list-region-end lines i))
                            (flat (list-flat-items (coerce (subseq lines i end) 'list))))
                       (emit (list :list (digit-char-p (char (second (first flat)) 0)) (list-tree flat)))
                       (setf i end)))
                    (t
                     (let ((end (or (position-if (lambda (l) (paragraph-break-p l keywords)) lines :start (1+ i)) n)))
                       (emit (list :paragraph
                                   (format nil "~{~A~^~%~}"
                                           (map 'list (lambda (l) (string-trim " " l)) (subseq lines i end)))))
                       (setf i end)))))))
    (values (nreverse nodes) (nreverse meta))))

(defun node-core (node)
  "The node inside an (:affiliated plist node) wrapper, or NODE itself."
  (if (eq (first node) :affiliated) (third node) node))

(defun node-affiliated (node key)
  (and (eq (first node) :affiliated) (getf (second node) key)))

;;; Sections

(defstruct (section (:constructor make-section (heading properties planning body children)))
  heading properties planning body children (number nil) (id nil) (unnumbered nil))

(defun export-sections (nodes)
  "Group NODES into (values preamble-nodes sections): each section holds its heading node, properties, planning
node, body nodes and child sections."
  (let ((preamble '()) (stack '()) (top '()))
    (labels ((core (node) (node-core node))
             (add-body (node)
               (if stack (push node (section-body (first stack))) (push node preamble)))
             (close-to (level)
               (loop :while (and stack (>= (second (section-heading-core (first stack))) level))
                     :do (finish (pop stack))))
             (section-heading-core (s) (node-core (section-heading s)))
             (finish (section)
               (setf (section-body section) (nreverse (section-body section))
                     (section-children section) (nreverse (section-children section)))
               (if stack (push section (section-children (first stack))) (push section top))))
      (dolist (node nodes)
        (case (first (core node))
          (:heading (close-to (second (core node)))
           (push (make-section node nil nil nil nil) stack))
          (:properties (if stack (setf (section-properties (first stack)) (second (core node))) (add-body node)))
          (:planning (if (and stack (null (section-planning (first stack))) (null (section-body (first stack))))
                         (setf (section-planning (first stack)) (second (core node)))
                         (add-body node)))
          (t (add-body node))))
      (close-to 0))
    (values (nreverse preamble) (nreverse top))))

(defun section-title-node (section) (node-core (section-heading section)))
(defun section-level (section) (second (section-title-node section)))
(defun section-title (section) (fifth (section-title-node section)))
(defun section-todo (section) (third (section-title-node section)))
(defun section-tags (section) (sixth (section-title-node section)))
(defun section-property (section name) (cdr (assoc name (section-properties section) :test #'string-equal)))

(defun slug-id (string used)
  "A unique html id made from STRING: org plus hex digits of a hash, with a counter when it is taken."
  (let* ((base (format nil "org~(~7,'0X~)" (logand (parse-integer (babel-hash string) :radix 16) #xfffffff)))
         (id base) (k 1))
    (loop :while (gethash id used) :do (setf id (format nil "~A~D" base (incf k))))
    (setf (gethash id used) t)
    id))

(defun assign-section-numbers (sections ctx &optional (prefix '()) (depth 1))
  "Give every section of SECTIONS (a list, recursively) its number and html id. Return the next free counter."
  (let ((counter 0) (num-limit (let ((num (getf (export-ctx-options ctx) :num))) (cond ((integerp num) num) (num 100) (t 0)))))
    (dolist (section sections)
      (let ((unnumbered (let ((u (section-property section "UNNUMBERED"))) (and u (not (string-equal u "nil"))))))
        (setf (section-unnumbered section) unnumbered)
        (unless unnumbered (incf counter))
        (let ((numbers (append prefix (list counter))))
          (setf (section-number section) (and (not unnumbered) (<= depth num-limit)
                                              (format nil "~{~D~^.~}" numbers)))
          (setf (section-id section)
                (let ((custom (section-property section "CUSTOM_ID")))
                  (or custom (slug-id (format nil "~{~D~^.~} ~A" numbers (section-title section)) (export-ctx-used-ids ctx)))))
          (setf (gethash (concatenate 'string "heading:" (normalize-space (heading-plain-text (section-title section)))) (export-ctx-ids ctx))
                (section-id section))
          (when (section-property section "CUSTOM_ID")
            (setf (gethash (concatenate 'string "custom:" (section-property section "CUSTOM_ID")) (export-ctx-ids ctx)) (section-id section)))
          (when (section-property section "ID")
            (setf (gethash (concatenate 'string "id:" (section-property section "ID")) (export-ctx-ids ctx)) (section-id section)))
          (assign-section-numbers (section-children section) ctx (if unnumbered prefix numbers) (1+ depth)))))))

(defun heading-plain-text (title)
  "TITLE without statistics cookies."
  (multiple-value-bind (start end) (cookie-find title)
    (if start (string-trim " " (concatenate 'string (subseq title 0 start) (subseq title end))) title)))

(defun section-included-p (section)
  "Whether the export shows SECTION, given the tasks: option (nil, :todo, :done or t)."
  (let ((tasks (option :tasks)))
    (cond ((eq tasks t) t)
          ((null tasks) (null (section-todo section)))
          ((eq tasks :todo) (section-todo section))
          (t t))))

;;; Entities

(defparameter *export-entities*
  ;; name  html-entity  latex  utf-8
  '(("alpha" "&alpha;" "\\alpha" "α") ("beta" "&beta;" "\\beta" "β") ("gamma" "&gamma;" "\\gamma" "γ")
    ("delta" "&delta;" "\\delta" "δ") ("epsilon" "&epsilon;" "\\epsilon" "ε") ("zeta" "&zeta;" "\\zeta" "ζ")
    ("eta" "&eta;" "\\eta" "η") ("theta" "&theta;" "\\theta" "θ") ("iota" "&iota;" "\\iota" "ι")
    ("kappa" "&kappa;" "\\kappa" "κ") ("lambda" "&lambda;" "\\lambda" "λ") ("mu" "&mu;" "\\mu" "μ")
    ("nu" "&nu;" "\\nu" "ν") ("xi" "&xi;" "\\xi" "ξ") ("pi" "&pi;" "\\pi" "π") ("rho" "&rho;" "\\rho" "ρ")
    ("sigma" "&sigma;" "\\sigma" "σ") ("tau" "&tau;" "\\tau" "τ") ("upsilon" "&upsilon;" "\\upsilon" "υ")
    ("phi" "&phi;" "\\phi" "φ") ("chi" "&chi;" "\\chi" "χ") ("psi" "&psi;" "\\psi" "ψ") ("omega" "&omega;" "\\omega" "ω")
    ("Gamma" "&Gamma;" "\\Gamma" "Γ") ("Delta" "&Delta;" "\\Delta" "Δ") ("Theta" "&Theta;" "\\Theta" "Θ")
    ("Lambda" "&Lambda;" "\\Lambda" "Λ") ("Xi" "&Xi;" "\\Xi" "Ξ") ("Pi" "&Pi;" "\\Pi" "Π") ("Sigma" "&Sigma;" "\\Sigma" "Σ")
    ("Phi" "&Phi;" "\\Phi" "Φ") ("Psi" "&Psi;" "\\Psi" "Ψ") ("Omega" "&Omega;" "\\Omega" "Ω")
    ("rarr" "&rarr;" "\\rightarrow" "→") ("larr" "&larr;" "\\leftarrow" "←") ("uarr" "&uarr;" "\\uparrow" "↑")
    ("darr" "&darr;" "\\downarrow" "↓") ("harr" "&harr;" "\\leftrightarrow" "↔") ("rArr" "&rArr;" "\\Rightarrow" "⇒")
    ("lArr" "&lArr;" "\\Leftarrow" "⇐") ("hArr" "&hArr;" "\\Leftrightarrow" "⇔") ("to" "&rarr;" "\\to" "→")
    ("times" "&times;" "\\times" "×") ("div" "&divide;" "\\div" "÷") ("pm" "&plusmn;" "\\pm" "±") ("mp" "&#8723;" "\\mp" "∓")
    ("le" "&le;" "\\le" "≤") ("ge" "&ge;" "\\ge" "≥") ("ne" "&ne;" "\\ne" "≠") ("equiv" "&equiv;" "\\equiv" "≡")
    ("approx" "&asymp;" "\\approx" "≈") ("infty" "&infin;" "\\infty" "∞") ("sum" "&sum;" "\\sum" "∑")
    ("prod" "&prod;" "\\prod" "∏") ("int" "&int;" "\\int" "∫") ("partial" "&part;" "\\partial" "∂")
    ("nabla" "&nabla;" "\\nabla" "∇") ("forall" "&forall;" "\\forall" "∀") ("exists" "&exist;" "\\exists" "∃")
    ("in" "&isin;" "\\in" "∈") ("notin" "&notin;" "\\notin" "∉") ("cap" "&cap;" "\\cap" "∩") ("cup" "&cup;" "\\cup" "∪")
    ("subset" "&sub;" "\\subset" "⊂") ("supset" "&sup;" "\\supset" "⊃") ("emptyset" "&empty;" "\\emptyset" "∅")
    ("deg" "&deg;" "^{\\circ}" "°") ("sqrt" "&radic;" "\\sqrt{}" "√") ("and" "&and;" "\\land" "∧") ("or" "&or;" "\\lor" "∨")
    ("nbsp" "&nbsp;" "~" " ") ("ensp" "&ensp;" "\\hspace*{.5em}" " ") ("emsp" "&emsp;" "\\hspace*{1em}" " ")
    ("ndash" "&ndash;" "--" "–") ("mdash" "&mdash;" "---" "—") ("hellip" "&hellip;" "\\ldots{}" "…")
    ("laquo" "&laquo;" "\\guillemotleft{}" "«") ("raquo" "&raquo;" "\\guillemotright{}" "»")
    ("lsquo" "&lsquo;" "`" "‘") ("rsquo" "&rsquo;" "'" "’") ("ldquo" "&ldquo;" "``" "“") ("rdquo" "&rdquo;" "''" "”")
    ("bull" "&bull;" "\\textbullet{}" "•") ("dagger" "&dagger;" "\\dag{}" "†") ("ddagger" "&Dagger;" "\\ddag{}" "‡")
    ("sect" "&sect;" "\\S" "§") ("para" "&para;" "\\P{}" "¶") ("copy" "&copy;" "\\copyright{}" "©")
    ("reg" "&reg;" "\\textregistered{}" "®") ("trade" "&trade;" "\\texttrademark{}" "™") ("euro" "&euro;" "\\euro{}" "€")
    ("pound" "&pound;" "\\pounds{}" "£") ("yen" "&yen;" "\\textyen{}" "¥") ("cent" "&cent;" "\\textcent{}" "¢")
    ("checkmark" "&#10003;" "\\checkmark" "✓") ("star" "&#9734;" "\\star" "☆") ("hearts" "&hearts;" "\\heartsuit" "♥")
    ("micro" "&micro;" "\\mu" "µ") ("frac12" "&frac12;" "\\textonehalf{}" "½") ("frac14" "&frac14;" "\\textonequarter{}" "¼")
    ("shy" "&shy;" "\\-" "")
    ("aacute" "&aacute;" "\\'a" "á") ("eacute" "&eacute;" "\\'e" "é") ("iacute" "&iacute;" "\\'i" "í")
    ("oacute" "&oacute;" "\\'o" "ó") ("uacute" "&uacute;" "\\'u" "ú") ("auml" "&auml;" "\\\"a" "ä")
    ("ouml" "&ouml;" "\\\"o" "ö") ("uuml" "&uuml;" "\\\"u" "ü") ("szlig" "&szlig;" "\\ss{}" "ß")
    ("ccedil" "&ccedil;" "\\c{c}" "ç") ("ntilde" "&ntilde;" "\\~n" "ñ"))
  "Org entities: \\name in the text. Each entry is (name html latex utf-8).")

(defun entity-lookup (name)
  (assoc name *export-entities* :test #'string=))

;;; Inline markup

(defun inline-closing-position (string marker start)
  "Position of the MARKER that closes an emphasis whose text begins at START, or NIL."
  (loop :for j :from start :below (length string)
        :when (and (char= (char string j) marker)
                   (> j start)
                   (not (member (char string (1- j)) '(#\space #\tab #\newline)))
                   (or (= (1+ j) (length string))
                       (not (word-char-p (char string (1+ j))))))
          :return j))

(defun math-fragment-end (string start)
  "If a LaTeX fragment begins at START in STRING return (values end kind text): \\(..\\) \\[..\\] $..$ $$..$$."
  (let ((n (length string)))
    (cond
      ((and (< (1+ start) n) (char= (char string start) #\\) (char= (char string (1+ start)) #\())
       (let ((close (search "\\)" string :start2 (+ start 2))))
         (and close (values (+ close 2) :inline (subseq string (+ start 2) close)))))
      ((and (< (1+ start) n) (char= (char string start) #\\) (char= (char string (1+ start)) #\[))
       (let ((close (search "\\]" string :start2 (+ start 2))))
         (and close (values (+ close 2) :display (subseq string (+ start 2) close)))))
      ((and (< (+ start 2) n) (char= (char string start) #\$) (char= (char string (1+ start)) #\$))
       (let ((close (search "$$" string :start2 (+ start 2))))
         (and close (values (+ close 2) :display (subseq string (+ start 2) close)))))
      ((and (char= (char string start) #\$) (< (1+ start) n)
            (not (member (char string (1+ start)) '(#\space #\tab #\newline #\$ #\,)))
            (or (zerop start) (not (word-char-p (char string (1- start))))))
       (let ((close (position #\$ string :start (1+ start))))
         (and close (> close (1+ start))
              (not (member (char string (1- close)) '(#\space #\tab #\newline)))
              (or (= (1+ close) n) (not (digit-char-p (char string (1+ close)))))
              (not (find #\newline string :start start :end close))
              (not (every (lambda (c) (or (digit-char-p c) (find c ".,"))) (subseq string (1+ start) close)))
              (values (1+ close) :inline (subseq string (1+ start) close))))))))

(defun script-end (string start)
  "For a sub/superscript text beginning at START (after the _ or ^) return (values end text): {braced}, a run of
letters/digits, or *."
  (let ((n (length string)))
    (when (< start n)
      (cond ((char= (char string start) #\{)
             (let ((close (position #\} string :start start)))
               (and close (values (1+ close) (subseq string (1+ start) close)))))
            ((and (eq (option :sub-super) t) (or (alphanumericp (char string start)) (find (char string start) "*+-")))
             (let ((end (or (position-if-not (lambda (c) (or (alphanumericp c) (char= c #\,) (char= c #\.))) string :start start) n)))
               (when (and (> end start) (find (char string (1- end)) ",."))
                 (decf end))
               (and (> end start) (values end (subseq string start end)))))))))

(defun link-target-id (target kind-key)
  "The html id registered for the key KIND-KEY (\"heading:Title\", \"custom:x\", \"id:...\", \"target:name\")."
  (and *export-ctx* (gethash (concatenate 'string kind-key target) (export-ctx-ids *export-ctx*))))

(defun image-file-p (path)
  (let ((dot (position #\. path :from-end t)))
    (and dot (member (string-downcase (subseq path (1+ dot))) '("png" "jpg" "jpeg" "gif" "svg" "webp" "bmp" "tiff" "ico") :test #'string=))))

(defun file-link-url (path search backend)
  "The URL for a file link to PATH: an .org file becomes .html (or .md, .tex ...), SEARCH becomes an anchor."
  (let* ((dot (position #\. path :from-end t))
         (extension (and dot (string-downcase (subseq path (1+ dot)))))
         (path (if (and extension (string= extension "org") (not (eq backend :org)))
                   (concatenate 'string (subseq path 0 dot) (ecase backend (:html ".html") (:markdown ".md") (:ascii ".txt") (:latex ".pdf")))
                   path)))
    (if (and search (eq backend :html) (plusp (length search)) (not (every #'digit-char-p search)))
        (let ((id (link-target-id (string-left-trim "*#" search) (if (char= (char search 0) #\*) "heading:" "custom:"))))
          (if id (format nil "~A#~A" path id) path))
        path)))

(defun resolve-link (target backend)
  "Return (values kind url) for TARGET: kind :external, :internal (url #id), :file or :image; NIL url when unresolvable."
  (let* ((target (link-expand-abbreviation target (if *export-ctx* (export-ctx-abbreviations *export-ctx*) *org-link-abbrev-alist*))))
    (multiple-value-bind (type path search) (link-classify target)
      (ecase type
        (:url (values :external (if (and (> (length path) 4) (string-equal "doi:" path :end2 4))
                                    (concatenate 'string "https://doi.org/" (subseq path 4))
                                    path)))
        (:file (values (if (and (image-file-p path) (null search)) :image :file) (file-link-url path search backend)))
        (:id (let ((id (link-target-id path "id:"))) (values :internal (and id (format nil "#~A" id)))))
        (:heading (let ((id (link-target-id (normalize-space path) "heading:")))
                    (values :internal (and id (format nil "#~A" id)))))
        (:custom-id (let ((id (link-target-id path "custom:"))) (values :internal (and id (format nil "#~A" id)))))
        (:fuzzy (let ((id (or (link-target-id (normalize-space path) "target:") (link-target-id (normalize-space path) "heading:")
                              (link-target-id (normalize-space path) "name:"))))
                  (values :internal (and id (format nil "#~A" id)))))
        (:shell (values :none nil))
        (:coderef (values :none nil))
        (:unsupported (values :none nil))))))

(defun footnote-text (number)
  "The definition of footnote NUMBER rendered for LaTeX, or an empty string."
  (let* ((note (find number (export-ctx-footnotes *export-ctx*) :key #'second))
         (text (and note (third note))))
    (export-inline (if (listp text) (format nil "~{~A~^ ~}" text) (or text "")) :latex)))

(defun footnote-number (label definition)
  "The number of the footnote LABEL in the export in progress, adding it on first use. Anonymous footnotes
(LABEL empty) always get a new one; DEFINITION is the inline text, if any."
  (let* ((ctx *export-ctx*) (notes (export-ctx-footnotes ctx))
         (existing (and (plusp (length label)) (find label notes :key #'first :test #'string=))))
    (if existing
        (second existing)
        (let ((number (1+ (length notes))))
          (vector-push-extend (list label number (or definition (cdr (assoc label (export-ctx-definitions ctx) :test #'string=)))) notes)
          number))))

(defun inline-emit (backend kind &rest arguments)
  "The text of the inline element KIND for BACKEND (:html :markdown :ascii :latex :org)."
  (ecase kind
    (:escape (let ((text (first arguments)))
               (ecase backend
                 (:html (html-escape text))
                 ((:markdown :ascii :org) text)
                 (:latex (latex-escape text)))))
    (:bold (destructuring-bind (body) arguments
             (ecase backend (:html (format nil "<b>~A</b>" body)) (:markdown (format nil "**~A**" body))
               (:ascii (format nil "*~A*" body)) (:latex (format nil "\\textbf{~A}" body)) (:org (format nil "*~A*" body)))))
    (:italic (destructuring-bind (body) arguments
               (ecase backend (:html (format nil "<i>~A</i>" body)) (:markdown (format nil "*~A*" body))
                 (:ascii (format nil "/~A/" body)) (:latex (format nil "\\emph{~A}" body)) (:org (format nil "/~A/" body)))))
    (:underline (destructuring-bind (body) arguments
                  (ecase backend (:html (format nil "<span class=\"underline\">~A</span>" body)) (:markdown body)
                    (:ascii (format nil "_~A_" body)) (:latex (format nil "\\uline{~A}" body)) (:org (format nil "_~A_" body)))))
    (:strike (destructuring-bind (body) arguments
               (ecase backend (:html (format nil "<del>~A</del>" body)) (:markdown (format nil "~~~~~A~~~~" body))
                 (:ascii (format nil "+~A+" body)) (:latex (format nil "\\sout{~A}" body)) (:org (format nil "+~A+" body)))))
    (:code (destructuring-bind (raw escaped) arguments
             (ecase backend (:html (format nil "<code>~A</code>" escaped)) (:markdown (format nil "`~A`" raw))
               (:ascii (format nil "`~A'" raw)) (:latex (format nil "\\texttt{~A}" (latex-escape raw))) (:org (format nil "=~A=" raw)))))
    (:verbatim (destructuring-bind (raw escaped) arguments
                 (ecase backend (:html (format nil "<code>~A</code>" escaped)) (:markdown (format nil "`~A`" raw))
                   (:ascii (format nil "~A" raw)) (:latex (format nil "\\verb|~A|" raw)) (:org (format nil "~~~A~~" raw)))))
    (:link (destructuring-bind (kind url body) arguments
             (declare (ignore kind))
             (ecase backend
               (:html (format nil "<a href=\"~A\">~A</a>" (html-escape url) body))
               (:markdown (format nil "[~A](~A)" body url))
               (:ascii (if (string= body url) url (format nil "~A (~A)" body url)))
               (:latex (format nil "\\href{~A}{~A}" (latex-url url) body))
               (:org (format nil "[[~A][~A]]" url body)))))
    (:image (destructuring-bind (url alt) arguments
              (ecase backend
                (:html (format nil "<img src=\"~A\" alt=\"~A\">" (html-escape url) (html-escape alt)))
                (:markdown (format nil "![~A](~A)" alt url))
                (:ascii (format nil "[~A]" url))
                (:latex (format nil "\\includegraphics[width=.9\\linewidth]{~A}" url))
                (:org (format nil "[[~A]]" url)))))
    (:footnote-ref (destructuring-bind (number &optional text) arguments
                     (ecase backend
                       (:html (format nil "<sup><a id=\"fnr.~D\" class=\"footref\" href=\"#fn.~D\" role=\"doc-backlink\">~D</a></sup>" number number number))
                       (:markdown (format nil "[^~D]" number))
                       (:ascii (format nil "[~D]" number))
                       (:latex (if text (format nil "\\footnote{~A}" text) (format nil "\\footnotemark[~D]" number)))
                       (:org (format nil "[fn:~D]" number)))))
    (:sub (destructuring-bind (body) arguments
            (ecase backend (:html (format nil "<sub>~A</sub>" body)) (:markdown (format nil "<sub>~A</sub>" body))
              (:ascii (format nil "_~A" body)) (:latex (format nil "\\textsubscript{~A}" body)) (:org (format nil "_{~A}" body)))))
    (:sup (destructuring-bind (body) arguments
            (ecase backend (:html (format nil "<sup>~A</sup>" body)) (:markdown (format nil "<sup>~A</sup>" body))
              (:ascii (format nil "^~A" body)) (:latex (format nil "\\textsuperscript{~A}" body)) (:org (format nil "^{~A}" body)))))
    (:timestamp (destructuring-bind (text) arguments
                  (ecase backend
                    (:html (format nil "<span class=\"timestamp-wrapper\"><span class=\"timestamp\">~A</span></span>" (html-escape text)))
                    ((:markdown :ascii :org) text)
                    (:latex (format nil "\\textit{~A}" (latex-escape text))))))
    (:break (ecase backend (:html (format nil "<br>~%")) (:markdown (format nil "  ~%")) (:ascii (format nil "~%")) (:latex (format nil "\\\\~%")) (:org (format nil "\\\\~%"))))
    (:target (destructuring-bind (id) arguments
               (ecase backend (:html (format nil "<a id=\"~A\"></a>" id)) ((:markdown :ascii :latex :org) ""))))
    (:math (destructuring-bind (display text) arguments
             (ecase backend
               (:html (when *export-ctx* (setf (export-ctx-math *export-ctx*) t))
                (if (eq display :display) (format nil "\\[~A\\]" (html-escape text)) (format nil "\\(~A\\)" (html-escape text))))
               ((:markdown :org) (if (eq display :display) (format nil "$$~A$$" text) (format nil "$~A$" text)))
               (:ascii text)
               (:latex (if (eq display :display) (format nil "\\[~A\\]" text) (format nil "\\(~A\\)" text))))))
    (:entity (destructuring-bind (name entry) arguments
               (ecase backend (:html (second entry)) (:latex (format nil "\\ensuremath{~A}" (third entry)))
                 ((:markdown :ascii) (fourth entry)) (:org (format nil "\\~A{}" name)))))
    (:special (destructuring-bind (string) arguments
                (ecase backend
                  (:html (cond ((string= string "---") "&mdash;") ((string= string "--") "&ndash;") (t "&hellip;")))
                  ((:markdown :ascii) (cond ((string= string "---") "—") ((string= string "--") "–") (t "…")))
                  (:latex (cond ((string= string "---") "---") ((string= string "--") "--") (t "\\ldots{}")))
                  (:org string))))
    (:snippet (destructuring-bind (snippet-backend text) arguments
                (if (or (string-equal snippet-backend (string-downcase (symbol-name backend)))
                        (and (eq backend :markdown) (string-equal snippet-backend "md")))
                    text "")))
    (:inline-src (destructuring-bind (language code) arguments
                   (ecase backend
                     (:html (format nil "<code class=\"src src-~A\">~A</code>" (html-escape language) (html-escape code)))
                     (:markdown (format nil "`~A`" code)) (:ascii code) (:latex (format nil "\\texttt{~A}" (latex-escape code)))
                     (:org (format nil "src_~A{~A}" language code)))))))

(defun latex-escape (text)
  (with-output-to-string (out)
    (loop :for c :across text
          :do (case c
                (#\\ (write-string "\\textbackslash{}" out))
                ((#\& #\% #\$ #\# #\_ #\{ #\}) (write-char #\\ out) (write-char c out))
                (#\~ (write-string "\\textasciitilde{}" out))
                (#\^ (write-string "\\textasciicircum{}" out))
                (t (write-char c out))))))

(defun latex-url (url)
  (with-output-to-string (out)
    (loop :for c :across url :do (when (find c "%#&_") (write-char #\\ out)) (write-char c out))))

(defun export-inline (string format)
  "Render the Org markup in STRING for the backend FORMAT: links, *bold*, /italic/, _underline_, =code=, ~code~, +strike+,
footnote references, entities, sub- and superscripts, timestamps, LaTeX fragments, line breaks and special strings."
  (with-output-to-string (out)
    (let ((i 0) (n (length string)) (plain-start nil))
      (labels ((emit (text) (write-string text out))
               (plain (text)
                 ;; text between markup: special strings and escaping
                 (let ((k 0) (m (length text)))
                   (loop :while (< k m)
                         :do (cond ((and (not (eq (option :special-strings) nil)) (<= (+ k 3) m) (string= "---" text :start2 k :end2 (+ k 3)))
                                    (emit (inline-emit format :special "---")) (incf k 3))
                                   ((and (not (eq (option :special-strings) nil)) (<= (+ k 2) m) (string= "--" text :start2 k :end2 (+ k 2)))
                                    (emit (inline-emit format :special "--")) (incf k 2))
                                   ((and (not (eq (option :special-strings) nil)) (<= (+ k 3) m) (string= "..." text :start2 k :end2 (+ k 3)))
                                    (emit (inline-emit format :special "...")) (incf k 3))
                                   ((char= (char text k) #\newline)
                                    (emit (if (option :breaks) (inline-emit format :break) (string #\newline))) (incf k))
                                   (t (emit (inline-emit format :escape (string (char text k)))) (incf k))))))
               (flush () (when plain-start (plain (subseq string plain-start i)) (setf plain-start nil)))
               (start-plain () (unless plain-start (setf plain-start i)))
               (word-before-p () (and (> i 0) (word-char-p (char string (1- i))))))
        (loop :while (< i n)
              :do (let ((c (char string i)))
                    (cond
                      ;; line break: \\ at the end of a line
                      ((and (char= c #\\) (< (1+ i) n) (char= (char string (1+ i)) #\\)
                            (or (= (+ i 2) n) (char= (char string (+ i 2)) #\newline)))
                       (flush) (emit (inline-emit format :break))
                       (setf i (if (and (< (+ i 2) n) (char= (char string (+ i 2)) #\newline)) (+ i 3) (+ i 2))))
                      ;; links [[..][..]]
                      ((and (char= c #\[) (< (1+ i) n) (char= (char string (1+ i)) #\[) (search "]]" string :start2 i))
                       (flush)
                       (let* ((end (search "]]" string :start2 i))
                              (inner (subseq string (+ i 2) end))
                              (split (search "][" inner))
                              (target (if split (subseq inner 0 split) inner))
                              (description (and split (subseq inner (+ split 2)))))
                         (emit (render-link target description format))
                         (setf i (+ end 2))))
                      ;; footnote references
                      ((and (char= c #\[) (<= (+ i 4) n) (string= "[fn:" string :start2 i :end2 (+ i 4)) (option :footnotes) *export-ctx*
                            (footnote-reference-at string i))
                       (flush)
                       (destructuring-bind (start end label definition) (footnote-reference-at string i)
                         (declare (ignore start))
                         (let ((number (footnote-number label definition)))
                           (emit (inline-emit format :footnote-ref number
                                              (and (eq format :latex) (footnote-text number)))))
                         (setf i end)))
                      ;; timestamps
                      ((and (find c "<[") (timestamp-parse string i) (option :timestamp)
                            (or (char= c #\[) (option :active-timestamps)))
                       (flush)
                       (multiple-value-bind (ts end) (timestamp-parse string i)
                         (declare (ignore ts))
                         (emit (inline-emit format :timestamp (subseq string i end)))
                         (setf i end)))
                      ;; angle links and plain URLs
                      ((and (or (char= c #\<) (alpha-char-p c)) (not (word-before-p))
                            (let ((link (find 0 (links-in-line (subseq string i)) :key (lambda (l) (getf l :start)))))
                              (and link (member (getf link :kind) '(:plain :angle)) link)))
                       (flush)
                       (let* ((link (find 0 (links-in-line (subseq string i)) :key (lambda (l) (getf l :start))))
                              (target (getf link :target)))
                         (emit (render-link target nil format))
                         (setf i (+ i (getf link :end)))))
                      ;; targets
                      ((and (char= c #\<) (<= (+ i 2) n) (char= (char string (1+ i)) #\<) (search ">>" string :start2 i))
                       (flush)
                       (let* ((triple (and (< (+ i 2) n) (char= (char string (+ i 2)) #\<)))
                              (close (search (if triple ">>>" ">>") string :start2 i))
                              (name (subseq string (+ i (if triple 3 2)) close))
                              (id (or (link-target-id (normalize-space name) "target:") (format nil "org-target--~A" (substitute #\- #\space (string-downcase name))))))
                         (emit (inline-emit format :target id))
                         (when triple (emit (inline-emit format :escape name)))
                         (setf i (+ close (if triple 3 2)))))
                      ;; export snippets @@html:...@@
                      ((and (char= c #\@) (< (1+ i) n) (char= (char string (1+ i)) #\@) (position #\: string :start i)
                            (search "@@" string :start2 (+ i 2)))
                       (flush)
                       (let* ((colon (position #\: string :start i)) (close (search "@@" string :start2 (+ i 2))))
                         (if (< colon close)
                             (progn (emit (inline-emit format :snippet (subseq string (+ i 2) colon) (subseq string (1+ colon) close)))
                                    (setf i (+ close 2)))
                             (progn (start-plain) (incf i)))))
                      ;; inline source blocks src_lang{code}
                      ((and (char= c #\s) (not (word-before-p)) (inline-src-at string i))
                       (flush)
                       (destructuring-bind (end language code) (inline-src-at string i)
                         (emit (inline-emit format :inline-src language code))
                         (setf i end)))
                      ;; LaTeX fragments
                      ((and (or (char= c #\$) (char= c #\\)) (math-fragment-end string i))
                       (flush)
                       (multiple-value-bind (end kind text) (math-fragment-end string i)
                         (emit (inline-emit format :math kind text))
                         (setf i end)))
                      ;; entities \name or \name{}
                      ((and (char= c #\\) (< (1+ i) n) (alpha-char-p (char string (1+ i))) (option :entities)
                            (let* ((end (or (position-if-not #'alpha-char-p string :start (1+ i)) n))
                                   (entry (entity-lookup (subseq string (1+ i) end))))
                              (and entry (or (>= end n) (not (alpha-char-p (char string end)))) entry)))
                       (flush)
                       (let* ((end (or (position-if-not #'alpha-char-p string :start (1+ i)) n))
                              (name (subseq string (1+ i) end))
                              (end (if (and (< (1+ end) n) (char= (char string end) #\{) (char= (char string (1+ end)) #\})) (+ end 2) end)))
                         (emit (inline-emit format :entity name (entity-lookup name)))
                         (setf i end)))
                      ;; subscripts and superscripts
                      ((and (find c "_^") (> i 0) (not (member (char string (1- i)) '(#\space #\tab #\newline)))
                            (option :sub-super)
                            (script-end string (1+ i)))
                       (flush)
                       (multiple-value-bind (end text) (script-end string (1+ i))
                         (emit (inline-emit format (if (char= c #\_) :sub :sup) (export-inline text format)))
                         (setf i end)))
                      ;; emphasis
                      ((and (find c "*/_=~+") (option :emphasis)
                            (or (= i 0) (not (word-char-p (char string (1- i)))))
                            (< (1+ i) n)
                            (not (member (char string (1+ i)) '(#\space #\tab #\newline)))
                            (inline-closing-position string c (1+ i)))
                       (flush)
                       (let* ((end (inline-closing-position string c (1+ i)))
                              (inner (subseq string (1+ i) end)))
                         (emit (case c
                                 (#\* (inline-emit format :bold (export-inline inner format)))
                                 (#\/ (inline-emit format :italic (export-inline inner format)))
                                 (#\_ (inline-emit format :underline (export-inline inner format)))
                                 (#\+ (inline-emit format :strike (export-inline inner format)))
                                 (#\= (inline-emit format :verbatim inner (html-escape inner)))
                                 (t (inline-emit format :code inner (html-escape inner)))))
                         (setf i (1+ end))))
                      (t (start-plain) (incf i)))))
        (flush)))))

(defun footnote-reference-at (string start)
  "The footnote reference that begins at START in STRING as (start end label definition), or NIL."
  (find start (footnote-references string) :key #'first))

(defun inline-src-at (string start)
  "If src_lang[headers]{code} begins at START return (end language code)."
  (let ((found (find start (inline-babel-find string) :key (lambda (f) (getf f :col)))))
    (and found (eq (getf found :kind) :inline-src)
         (list (getf found :col-end) (getf found :language) (getf found :code)))))

(defun render-link (target description format)
  "A link to TARGET with DESCRIPTION (markup allowed) in FORMAT. An unresolvable internal link keeps just its text."
  (multiple-value-bind (kind url) (resolve-link target format)
    (let ((image-description (and description (multiple-value-bind (k u) (resolve-link description format) (and (eq k :image) u)))))
      (cond
        ((eq kind :none) (if description (export-inline description format) (inline-emit format :escape target)))
        ((null url) (if description (export-inline description format) (inline-emit format :escape (string-left-trim "*#" target))))
        ((and (eq kind :image) (null description)) (inline-emit format :image url (file-namestring url)))
        (image-description (inline-emit format :link kind url (inline-emit format :image image-description (file-namestring image-description))))
        (t (inline-emit format :link kind url (if description (export-inline description format) (inline-emit format :escape (if (eq kind :file) target url)))))))))

;;; Contexts

(defun make-context (backend meta lines &key file mtime)
  "An export context for BACKEND with the options and link tables of the document LINES whose META was read."
  (let ((ctx (make-export-ctx :backend backend :meta meta :file file :mtime mtime)))
    (setf (export-ctx-options ctx) (let ((options (export-options meta)))
                                     options)
          (export-ctx-abbreviations ctx) (append (link-abbreviations-from-lines lines) *org-link-abbrev-alist*)
          (export-ctx-macros ctx) (macro-definitions lines))
    ctx))

(defun register-targets (ctx lines)
  "Record the dedicated targets <<x>> and #+NAME: elements of LINES so that links to them resolve."
  (let ((in-src nil))
    (dolist (line lines)
      (cond (in-src (when (src-end-line-p line) (setf in-src nil)))
            ((src-begin-line-p line) (setf in-src t))
            (t (loop :with from := 0
                     :for open := (search "<<" line :start2 from)
                     :while open
                     :do (let* ((triple (and (< (+ open 2) (length line)) (char= (char line (+ open 2)) #\<)))
                                (start (+ open (if triple 3 2)))
                                (close (search (if triple ">>>" ">>") line :start2 start)))
                           (when close
                             (let ((name (normalize-space (subseq line start close))))
                               (setf (gethash (concatenate 'string "target:" name) (export-ctx-ids ctx))
                                     (format nil "org-target--~A" (substitute #\- #\space (string-downcase name))))))
                           (setf from (+ open 2))))
               (multiple-value-bind (key value) (keyword-line line)
                 (when (and key (string= key "NAME"))
                   (setf (gethash (concatenate 'string "name:" (normalize-space value)) (export-ctx-ids ctx)) value))))))))

;;; Builtin macros

(defun builtin-macro (ctx name arguments)
  (let ((meta (export-ctx-meta ctx)))
    (flet ((formatted (default)
             (format-time-spec (or (first arguments) default) (or (export-ctx-mtime ctx) (current-time-list)))))
      (cond ((string= name "title") (or (meta-get meta "TITLE") ""))
            ((string= name "author") (or (meta-get meta "AUTHOR") ""))
            ((string= name "email") (or (meta-get meta "EMAIL") ""))
            ((string= name "date") (let ((date (meta-get meta "DATE")))
                                     (cond ((null date) "")
                                           ((and arguments (timestamp-parse date))
                                            (let ((ts (timestamp-parse date)))
                                              (format-time-spec (first arguments)
                                                                (list (getf ts :year) (getf ts :month) (getf ts :day)
                                                                      (or (getf ts :hour) 0) (or (getf ts :minute) 0) 0))))
                                           (t date))))
            ((string= name "time") (format-time-spec (or (first arguments) "%Y-%m-%d") (current-time-list)))
            ((string= name "modification-time") (formatted "%Y-%m-%d"))
            ((string= name "input-file") (if (export-ctx-file ctx) (file-namestring (export-ctx-file ctx)) ""))
            ((string= name "keyword") (or (meta-get meta (string-upcase (or (first arguments) ""))) ""))
            ((string= name "results") (or (first arguments) ""))
            (t nil)))))

;;; Preparing a document

(defun prepare-document (lines keywords backend &key file mtime reader (select-tags *org-export-select-tags*)
                                                   (exclude-tags *org-export-exclude-tags*))
  "Run the preprocessing on LINES and return (values lines ctx): setup files and includes are expanded, macros replaced,
excluded subtrees removed, and a context made for BACKEND."
  (let* ((reader (or reader *org-export-reader*))
         (lines (export-expand-includes (export-expand-setupfiles lines reader) keywords reader))
         (early-meta (loop :for line :in lines :for (name value) := (multiple-value-list (keyword-line line))
                           :when name :collect (cons name value)))
         (exclude (append exclude-tags (loop :for v :in (meta-values early-meta "EXCLUDE_TAGS") :append (split-words v))))
         (select (append select-tags (loop :for v :in (meta-values early-meta "SELECT_TAGS") :append (split-words v))))
         (ctx (make-context backend early-meta lines :file file :mtime mtime))
         (lines (export-expand-macros lines (export-ctx-macros ctx)
                                      (lambda (name arguments) (builtin-macro ctx name arguments))))
         (lines (export-filter-subtrees lines keywords :exclude exclude :select select
                                                       :archived (getf (export-ctx-options ctx) :archived))))
    (register-targets ctx lines)
    (values lines ctx)))

(defun export-expand-setupfiles (lines reader)
  "LINES with each #+SETUPFILE: line replaced by the lines of that file."
  (loop :for line :in lines
        :append (if (line-starts-with-ci-p line "#+setupfile:")
                    (let* ((file (first (split-quoted-words (subseq (string-left-trim " " line) 12))))
                           (included (and file reader (funcall reader file))))
                      (or included (list line)))
                    (list line))))

;;; Table of contents

(defun toc-entries (sections depth)
  "A list of (section . children-entries) for the sections up to DEPTH levels."
  (when (and sections (plusp depth))
    (loop :for section :in sections
          :when (section-included-p section)
            :collect (cons section (toc-entries (section-children section) (1- depth))))))

(defun html-toc (entries)
  (when entries
    (format nil "<ul>~%~{~A~%~}</ul>"
            (mapcar (lambda (entry)
                      (let ((section (car entry)))
                        (format nil "<li><a href=\"#~A\">~@[~A. ~]~A</a>~@[~%~A~]</li>"
                                (section-id section) (section-number section)
                                (export-inline (heading-plain-text (section-title section)) :html)
                                (html-toc (cdr entry)))))
                    entries))))

;;; HTML

(defun opt-format (control &rest arguments)
  "The text of (format nil CONTROL ...ARGUMENTS), or \"\" when the first argument is NIL."
  (if (first arguments) (apply #'format nil control arguments) ""))

(defun html-attr (attrs name)
  (cdr (assoc name attrs :test #'string=)))

(defun html-attr-string (attrs allowed)
  (format nil "~{ ~A=\"~A\"~}"
          (loop :for (name . value) :in attrs
                :when (member name allowed :test #'string=)
                  :append (list name (html-escape (string-trim "\"" value))))))

(defun html-list (ordered tree)
  (let ((tag (if ordered "ol" "ul")))
    (if (and tree (every (lambda (item) (search " :: " (third item))) tree))
        (format nil "<dl class=\"org-dl\">~%~{~A~%~}</dl>"
                (mapcar (lambda (item)
                          (destructuring-bind (bullet checkbox text children) item
                            (declare (ignore bullet checkbox))
                            (let ((split (search " :: " text)))
                              (format nil "<dt>~A</dt><dd>~A~@[~%~A~]</dd>"
                                      (export-inline (subseq text 0 split) :html)
                                      (export-inline (subseq text (+ split 4)) :html)
                                      (and children (html-list (digit-char-p (char (first (first children)) 0)) children))))))
                        tree))
        (format nil "<~A class=\"org-~A\">~%~{~A~%~}</~A>" tag tag
                (mapcar (lambda (item)
                          (destructuring-bind (bullet checkbox text children) item
                            (declare (ignore bullet))
                            (format nil "<li>~@[<code>[~A]</code> ~]~A~@[~%~A~]</li>"
                                    (and checkbox (string checkbox))
                                    (export-inline text :html)
                                    (and children (html-list (digit-char-p (char (first (first children)) 0)) children)))))
                        tree)
                tag))))

(defun table-header-count (rows)
  "How many leading rows of ROWS come before the first horizontal rule, or 0."
  (let ((rule (position :rule rows)))
    (if (and rule (plusp rule)) rule 0)))

(defun table-column-alignments (rows)
  "For each column, :right when most of its non-empty cells are numbers, else :left."
  (let ((count (table-column-count rows)) (data (remove :rule rows)))
    (loop :for c :below count
          :collect (let* ((cells (remove "" (mapcar (lambda (r) (or (nth c r) "")) data) :test #'string=))
                          (numbers (count-if #'table-number-p cells)))
                     (if (and (plusp numbers) (>= (* 2 numbers) (length cells))) :right :left)))))

(defun caption-text (affiliated)
  (let ((caption (getf affiliated :caption)))
    (and caption (plusp (length caption)) caption)))

(defun next-counter (kind)
  (incf (getf (export-ctx-counters *export-ctx*) kind)))

(defun html-table (rows affiliated)
  (let* ((head (table-header-count rows)) (alignments (table-column-alignments rows))
         (caption (caption-text affiliated)) (name (getf affiliated :name))
         (attrs (getf affiliated :attr_html)))
    (flet ((row (cells tag)
             (format nil "<tr>~{~A~}</tr>"
                     (loop :for cell :in cells :for alignment :in alignments
                           :collect (format nil "<~A class=\"org-~(~A~)\">~A</~A>" tag alignment (export-inline cell :html) tag)))))
      (concatenate 'string
                   (format nil "<table~A~A>~%" (if name (format nil " id=\"~A\"" (html-escape name)) "")
                           (html-attr-string attrs '("class" "id" "style" "border" "rules" "frame" "cellspacing" "cellpadding")))
                   (if caption
                       (format nil "<caption class=\"t-above\"><span class=\"table-number\">Table ~D:</span> ~A</caption>~%"
                               (next-counter :table) (export-inline caption :html))
                       "")
                   (if (plusp head)
                       (format nil "<thead>~%~{~A~%~}</thead>~%" (mapcar (lambda (r) (row r "th")) (subseq rows 0 head)))
                       "")
                   (format nil "<tbody>~%~{~A~%~}</tbody>~%</table>"
                           (mapcar (lambda (r) (row r "td")) (remove :rule (nthcdr (if (plusp head) head 0) rows))))))))

(defun html-paragraph (text affiliated)
  (let* ((attrs (getf affiliated :attr_html)) (caption (caption-text affiliated))
         (image-only (let ((links (links-in-line text)))
                       (and links (null (cdr links)) (zerop (getf (first links) :start)) (= (getf (first links) :end) (length text))
                            (eq :image (resolve-link (getf (first links) :target) :html)) (null (getf (first links) :description))
                            (getf (first links) :target)))))
    (if image-only
        (let ((url (nth-value 1 (resolve-link image-only :html))))
          (format nil "<div class=\"figure\">~%<p><img src=\"~A\" alt=\"~A\"~A></p>~A~%</div>"
                  (html-escape url) (html-escape (or (html-attr attrs "alt") (file-namestring url)))
                  (html-attr-string attrs '("width" "height" "title" "class" "style"))
                  (if caption
                      (format nil "~%<p><span class=\"figure-number\">Figure ~D: </span>~A</p>" (next-counter :figure) (export-inline caption :html))
                      "")))
        (format nil "<p~A>~A</p>" (html-attr-string attrs '("class" "id" "style")) (export-inline text :html)))))

(defun html-node (node keywords)
  (let* ((affiliated (and (eq (first node) :affiliated) (second node)))
         (node (node-core node)))
    (ecase (first node)
      (:heading "")
      (:properties "")
      (:footnote-def "")
      (:toc "")
      (:planning (if (option :planning)
                     (format nil "<p class=\"planning\"><i>~A</i></p>" (html-escape (second node)))
                     ""))
      (:paragraph (html-paragraph (second node) affiliated))
      (:list (html-list (second node) (third node)))
      (:table (html-table (second node) affiliated))
      (:src (let ((caption (caption-text affiliated)) (name (getf affiliated :name)))
              (format nil "<div class=\"org-src-container\">~%~A<pre class=\"src src-~A\"~A>~A</pre>~%</div>"
                      (if caption
                          (format nil "<label class=\"org-src-name\"><span class=\"listing-number\">Listing ~D: </span>~A</label>~%"
                                  (next-counter :listing) (export-inline caption :html))
                          "")
                      (or (second node) "text")
                      (if name (format nil " id=\"~A\"" (html-escape name)) "")
                      (html-escape (join-lines (third node))))))
      (:example (format nil "<pre class=\"example\">~A</pre>" (html-escape (join-lines (second node)))))
      (:verse (format nil "<p class=\"verse\">~%~{~A~^<br>~%~}~%</p>" (mapcar (lambda (l) (export-inline l :html)) (second node))))
      (:quote (format nil "<blockquote>~%~A~%</blockquote>" (html-nodes (second node) keywords)))
      (:center (format nil "<div class=\"org-center\">~%~A~%</div>" (html-nodes (second node) keywords)))
      (:block (format nil "<div class=\"~A\">~%~A~%</div>" (html-escape (second node)) (html-nodes (third node) keywords)))
      (:rule "<hr>")
      (:raw (if (equal (second node) "html") (join-lines (third node)) "")))))

(defun html-nodes (nodes keywords)
  (join-lines (remove "" (mapcar (lambda (node) (html-node node keywords)) nodes) :test #'string=)))

(defun html-heading-content (section)
  (destructuring-bind (level todo priority title tags) (rest (section-title-node section))
    (declare (ignore level))
    (concatenate 'string
                 (if (section-number section)
                     (format nil "<span class=\"section-number-~D\">~A</span> " (section-html-level section) (section-number section))
                     "")
                 (if (and todo (option :todo)) (format nil "<span class=\"todo ~A\">~A</span> " todo todo) "")
                 (if (and priority (option :priority)) (format nil "<span class=\"priority\">[#~A]</span> " priority) "")
                 (export-inline (if (option :stat) title (heading-plain-text title)) :html)
                 (if (and tags (option :tags))
                     (format nil " <span class=\"tag\">~{<span class=\"~A\">~:*~A</span>~^&#xa0;~}</span>" tags)
                     ""))))

(defun section-html-level (section) (1+ (section-level section)))

(defun html-section (section keywords limit)
  (if (not (section-included-p section))
      ""
      (let* ((level (section-level section)) (h (min 6 (1+ level))) (id (section-id section)))
        (if (> level limit)
            ;; deeper headlines become list items
            (format nil "<ul class=\"org-ul\">~%<li>~A~@[~%~A~]~@[~%~A~]</li>~%</ul>"
                    (html-heading-content section)
                    (let ((body (html-section-body section keywords))) (and (plusp (length body)) body))
                    (let ((children (join-lines (remove "" (mapcar (lambda (c) (html-section c keywords limit)) (section-children section)) :test #'string=))))
                      (and (plusp (length children)) children)))
            (format nil "<div id=\"outline-container-~A\" class=\"outline-~D\">~%<h~D id=\"~A\">~A</h~D>~%~A~@[~%~A~]~%</div>"
                    id h h id (html-heading-content section) h
                    (let ((body (html-section-body section keywords)))
                      (if (plusp (length body))
                          (format nil "<div class=\"outline-text-~D\" id=\"text-~A\">~%~A~%</div>" h id body)
                          ""))
                    (let ((children (join-lines (remove "" (mapcar (lambda (c) (html-section c keywords limit)) (section-children section)) :test #'string=))))
                      (and (plusp (length children)) children)))))))

(defun html-section-body (section keywords)
  (join-lines (remove "" (list (if (and (section-planning section) (option :planning))
                                   (format nil "<p class=\"planning\"><i>~A</i></p>" (html-escape (section-planning section)))
                                   "")
                               (html-nodes (section-body section) keywords))
                      :test #'string=)))

(defun html-footnotes ()
  "The footnote section for the footnotes used so far, or an empty string."
  (let ((notes (export-ctx-footnotes *export-ctx*)))
    (if (zerop (length notes))
        ""
        (format nil "<div id=\"footnotes\">~%<h2 class=\"footnotes\">Footnotes: </h2>~%<div id=\"text-footnotes\">~%~{~A~%~}</div>~%</div>"
                (map 'list (lambda (note)
                             (destructuring-bind (label number lines) note
                               (declare (ignore label))
                               (format nil "<div class=\"footdef\"><sup><a id=\"fn.~D\" class=\"footnum\" href=\"#fnr.~D\" role=\"doc-backlink\">~D</a></sup> <div class=\"footpara\" role=\"doc-footnote\"><p class=\"footpara\">~A</p></div></div>"
                                       number number number
                                       (export-inline (format nil "~{~A~^ ~}" (if (stringp lines) (list lines) lines)) :html))))
                     notes)))))

(defparameter *html-style*
  "body{max-width:50em;margin:2em auto;padding:0 1em;font-family:sans-serif;line-height:1.5}
pre{background:#f4f4f4;padding:.6em;overflow:auto}table{border-collapse:collapse}
td,th{border:1px solid #ccc;padding:.2em .6em}.org-right{text-align:right}.todo{color:#c00;font-weight:bold}
.TODO{color:#c00}.DONE{color:#080}.tag{font-size:smaller;color:#666}.planning{color:#666}
.timestamp{color:#666}.underline{text-decoration:underline}.footpara{display:inline}
#table-of-contents ul{list-style:none;padding-left:1.2em}.figure p{text-align:center}")

(defun export-html (lines keywords &key body-only file mtime reader)
  "Render the Org text LINES as an HTML document (or, with BODY-ONLY, just the content)."
  (multiple-value-bind (lines ctx) (prepare-document lines keywords :html :file file :mtime mtime :reader reader)
    (let ((*export-ctx* ctx))
      (multiple-value-bind (nodes meta) (export-parse lines keywords)
        (setf (export-ctx-meta ctx) meta
              (export-ctx-options ctx) (export-options meta)
              (export-ctx-definitions ctx)
              (loop :for node :in nodes :for core := (node-core node)
                    :when (eq (first core) :footnote-def) :collect (cons (second core) (format nil "~{~A~^ ~}" (third core)))))
        (multiple-value-bind (preamble sections) (export-sections nodes)
          (assign-section-numbers sections ctx)
          (let* ((title (meta-get meta "TITLE")) (author (meta-get meta "AUTHOR")) (date (meta-get meta "DATE"))
                 (email (meta-get meta "EMAIL"))
                 (limit (let ((h (option :levels))) (if (integerp h) h 3)))
                 (toc-option (option :toc))
                 (toc-depth (cond ((integerp toc-option) toc-option) (toc-option limit) (t 0)))
                 (toc-node-p (some (lambda (n) (eq (first (node-core n)) :toc)) nodes))
                 (make-toc (lambda (depth)
                             (let ((entries (toc-entries sections (or depth (if (plusp toc-depth) toc-depth limit)))))
                               (and entries
                                    (format nil "<div id=\"table-of-contents\" role=\"doc-toc\">~%<h2>Table of Contents</h2>~%<div id=\"text-table-of-contents\" role=\"doc-toc\">~%~A~%</div>~%</div>"
                                            (html-toc entries))))))
                 (toc-html (and (plusp toc-depth) (funcall make-toc nil)))
                 (body (join-lines (remove "" (list (html-nodes (substitute-toc preamble make-toc) keywords)
                                                    (join-lines (remove "" (mapcar (lambda (s) (html-section s keywords limit)) sections) :test #'string=)))
                                           :test #'string=)))
                 (footnotes (html-footnotes)))
            (if body-only
                (format nil "~A~@[~%~A~]~%" body (and (plusp (length footnotes)) footnotes))
                (format nil "<!DOCTYPE html>~%<html lang=\"~A\">~%<head>~%<meta charset=\"utf-8\">~%<title>~A</title>~%<meta name=\"generator\" content=\"~A\">~%~@[<meta name=\"author\" content=\"~A\">~%~]~@[<meta name=\"description\" content=\"~A\">~%~]~@[<meta name=\"keywords\" content=\"~A\">~%~]<style>~A</style>~%~{~A~%~}~@[~A~%~]~A</head>~%<body>~%<div id=\"content\" class=\"content\">~%~@[<h1 class=\"title\">~A</h1>~%~]~@[~A~%~]~A~%~@[~A~%~]</div>~%<div id=\"postamble\" class=\"status\">~%~@[<p class=\"author\">Author: ~A</p>~%~]~@[<p class=\"date\">Date: ~A</p>~%~]~@[<p class=\"email\">Email: ~A</p>~%~]~:[~;<p class=\"creator\">Generated by ~A</p>~%~]</div>~%</body>~%</html>~%"
                        (or (meta-get meta "LANGUAGE") "en")
                        (html-escape (or title ""))
                        (html-escape *org-export-creator*)
                        (and author (option :author) (html-escape author))
                        (let ((d (meta-get meta "DESCRIPTION"))) (and d (html-escape d)))
                        (let ((k (meta-get meta "KEYWORDS"))) (and k (html-escape k)))
                        *html-style*
                        (append (meta-values meta "HTML_HEAD") (meta-values meta "HTML_HEAD_EXTRA"))
                        *org-export-html-head*
                        (if (and (export-ctx-math ctx) *org-export-mathjax*)
                            (format nil "<script async src=\"https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js\"></script>~%")
                            "")
                        (and title (option :title) (export-inline title :html))
                        (and (not toc-node-p) toc-html)
                        body
                        (and (plusp (length footnotes)) footnotes)
                        (and author (option :author) (html-escape author))
                        (and date (option :date) (html-escape (if (timestamp-parse date) (subseq date 1 (1- (length date))) date)))
                        (and email (option :email) (html-escape email))
                        (option :creator) (html-escape *org-export-creator*)))))))))

(defun substitute-toc (nodes make-toc)
  "NODES with each (:toc depth local) replaced by a raw html node holding (funcall MAKE-TOC depth)."
  (mapcar (lambda (node)
            (if (eq (first (node-core node)) :toc)
                (list :raw "html" (list (or (funcall make-toc (second (node-core node))) "")))
                node))
          nodes))

;;; Markdown

(defun markdown-list (ordered tree depth)
  (join-lines
   (loop :for item :in tree
         :for number :from 1
         :collect (destructuring-bind (bullet checkbox text children) item
                    (declare (ignore bullet))
                    (format nil "~A~A ~@[[~A] ~]~A~@[~%~A~]"
                            (make-string (* 3 depth) :initial-element #\space)
                            (if ordered (format nil "~D." number) "-")
                            (and checkbox (string checkbox))
                            (export-inline (let ((split (search " :: " text)))
                                             (if split (format nil "*~A* ~A" (subseq text 0 split) (subseq text (+ split 4))) text))
                                           :markdown)
                            (and children
                                 (markdown-list (digit-char-p (char (first (first children)) 0)) children (1+ depth))))))))

(defun markdown-node (node offset keywords)
  (let ((affiliated (and (eq (first node) :affiliated) (second node)))
        (node (node-core node)))
    (ecase (first node)
      (:heading
       (destructuring-bind (level todo priority title tags) (rest node)
         (declare (ignore tags))
         (format nil "~A ~@[**~A** ~]~@[[#~A] ~]~A" (make-string (min 6 (+ level offset)) :initial-element #\#)
                 todo priority (export-inline title :markdown))))
      ((:properties :footnote-def :toc) "")
      (:planning (if (option :planning) (format nil "*~A*" (second node)) ""))
      (:paragraph (export-inline (second node) :markdown))
      (:list (markdown-list (second node) (third node) 0))
      (:table
       (let* ((rows (remove :rule (second node))) (head (max 1 (table-header-count (second node))))
              (alignments (table-column-alignments (second node))))
         (flet ((row (cells) (format nil "| ~{~A~^ | ~} |" (mapcar (lambda (c) (export-inline c :markdown)) cells))))
           (join-lines (append (and (caption-text affiliated) (list (format nil "*~A*" (caption-text affiliated)) ""))
                               (mapcar #'row (subseq rows 0 (min head (length rows))))
                               (list (format nil "|~{~A~}" (mapcar (lambda (a) (if (eq a :right) " ---: |" " --- |")) alignments)))
                               (mapcar #'row (nthcdr (min head (length rows)) rows)))))))
      (:src (format nil "```~A~%~A~%```" (or (second node) "") (join-lines (third node))))
      ((:example :verse) (format nil "```~%~A~%```" (join-lines (second node))))
      (:quote (join-lines (mapcar (lambda (line) (if (string= line "") ">" (format nil "> ~A" line)))
                                  (split-text-lines (markdown-nodes (second node) offset keywords)))))
      ((:center) (markdown-nodes (second node) offset keywords))
      (:block (markdown-nodes (third node) offset keywords))
      (:rule "---")
      (:raw (if (member (second node) '("md" "markdown") :test #'equal) (join-lines (third node)) "")))))

(defun markdown-nodes (nodes offset keywords)
  (format nil "~{~A~^~%~%~}"
          (remove "" (mapcar (lambda (node) (markdown-node node offset keywords)) nodes) :test #'string=)))

(defun markdown-footnotes ()
  (let ((notes (export-ctx-footnotes *export-ctx*)))
    (format nil "~{~A~^~%~}"
            (map 'list (lambda (note)
                         (destructuring-bind (label number lines) note
                           (declare (ignore label))
                           (format nil "[^~D]: ~A" number (export-inline (format nil "~{~A~^ ~}" (if (stringp lines) (list lines) lines)) :markdown))))
                 notes))))

(defun export-markdown (lines keywords &key file mtime reader)
  "Render the Org text LINES as Markdown."
  (multiple-value-bind (lines ctx) (prepare-document lines keywords :markdown :file file :mtime mtime :reader reader)
    (let ((*export-ctx* ctx))
      (multiple-value-bind (nodes meta) (export-parse lines keywords)
        (setf (export-ctx-meta ctx) meta
              (export-ctx-options ctx) (export-options meta)
              (export-ctx-definitions ctx)
              (loop :for node :in nodes :for core := (node-core node)
                    :when (eq (first core) :footnote-def) :collect (cons (second core) (format nil "~{~A~^ ~}" (third core)))))
        (multiple-value-bind (preamble sections) (export-sections nodes)
          (declare (ignore preamble))
          (assign-section-numbers sections ctx)
          (let* ((title (meta-get meta "TITLE"))
                 (body (markdown-nodes nodes (if title 1 0) keywords))
                 (footnotes (markdown-footnotes)))
            (format nil "~@[# ~A~%~%~]~A~%~@[~%~A~%~]" (and title (option :title) (export-inline title :markdown))
                    body (and (plusp (length footnotes)) footnotes))))))))

;;; END pure export code

(defun export-to-file (buffer type function &rest keys)
  "Write FUNCTION's rendering of BUFFER to a file with extension TYPE next to it, or to a new buffer when it has no file."
  (let* ((file (buffer-filename buffer))
         (*org-export-reader* (export-file-reader buffer))
         (text (apply function (split-text-lines (buffer-text buffer)) (org-keywords)
                      :file file :mtime (and file (probe-file file) (file-time-list file)) keys)))
    (cond (file
           (let ((target (make-pathname :type type :defaults file)))
             (with-open-file (out target :direction :output :if-exists :supersede :external-format :utf-8)
               (write-string text out))
             (message "Exported to ~A" target)
             target))
          (t
           (let* ((name (format nil "*Org ~A Export*" (string-upcase type)))
                  (export (or (get-buffer name) (make-buffer name))))
             (erase-buffer export)
             (insert-string (buffer-point export) text)
             (org-switch-to-buffer export)
             export)))))

(defun file-time-list (file)
  (multiple-value-bind (s mi h d mo y) (decode-universal-time (file-write-date file))
    (list y mo d h mi s)))

(defun export-file-reader (buffer)
  "A function that reads the lines of a file relative to the directory of BUFFER."
  (lambda (name)
    (let ((path (merge-pathnames (expand-user-path name) (or (buffer-directory buffer) ""))))
      (let ((open (buffer-for-file (uiop:native-namestring path))))
        (cond (open (split-text-lines (buffer-text open)))
              ((probe-file path) (uiop:read-file-lines path)))))))

(define-command org-export-html () ()
  "Export the buffer to an HTML file next to it."
  (export-to-file (current-buffer) "html" #'export-html))

(define-command org-export-markdown () ()
  "Export the buffer to a Markdown file next to it."
  (export-to-file (current-buffer) "md" #'export-markdown))
