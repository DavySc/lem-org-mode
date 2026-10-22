;;; Babel model: everything about source blocks that does not touch a buffer or a process.
;;;
;;; Header arguments and their inheritance, the document model (named blocks, tables, results),
;;; data passed to and from blocks, result formatting, noweb expansion and the tangle plan.
;;; `babel.lisp' runs blocks and edits buffers on top of this.

(in-package :lem-org-mode)

;;; BEGIN pure babel-model code

(defvar *org-babel-default-header-args*
  '(("results" . "value replace") ("exports" . "code") ("session" . "none") ("noweb" . "no")
    ("tangle" . "no") ("cache" . "no") ("eval" . "yes") ("hlines" . "no") ("padline" . "yes")
    ("comments" . "no"))
  "Header arguments every block starts from. Add to this to change a default for the whole mode,
for example (push '(\"results\" . \"output\") *org-babel-default-header-args*).")

;;; Header arguments

(defun header-key-p (word)
  (and (> (length word) 1) (char= (char word 0) #\:)))

(defun header-args-from-words (words)
  "Turn WORDS such as (\":var\" \"x=1\" \":results\" \"output\" \"raw\") into an alist of (name . value)."
  (let ((args '()))
    (loop :while words
          :do (let ((word (pop words)))
                (when (header-key-p word)
                  (let ((values (loop :while (and words (not (header-key-p (first words))))
                                      :collect (pop words))))
                    (push (cons (string-downcase (subseq word 1))
                                (if values (format nil "~{~A~^ ~}" values) "t"))
                          args)))))
    (nreverse args)))

(defun header-args-parse (string)
  (header-args-from-words (split-words string)))

(defun src-header (line)
  "Return (values language args switches) for a #+begin_src LINE. ARGS is an alist of (name . value),
names without the colon, a repeated name (:var) kept as often as it occurs."
  (let* ((words (cdr (split-words line)))
         (language (and words (not (header-key-p (first words))) (pop words)))
         (switches (loop :while (and words (not (header-key-p (first words)))) :collect (pop words))))
    (values language (header-args-from-words words) switches)))

(defun header-arg (args name)
  (cdr (assoc name args :test #'string=)))

(defun header-flag-p (args name)
  "True when the header argument NAME is set to something other than no / nil / none."
  (let ((value (header-arg args name)))
    (and value (not (member value '("no" "nil" "none" "never") :test #'string-equal)))))

(defvar *results-categories*
  '((:collection "value" "output")
    (:type "table" "vector" "list" "scalar" "verbatim" "file")
    (:format "raw" "code" "drawer" "html" "latex" "org" "pp" "link" "graphics")
    (:handling "replace" "silent" "none" "discard" "append" "prepend")))

(defun results-category (token)
  (car (find-if (lambda (category) (member token (cdr category) :test #'string-equal))
                *results-categories*)))

(defun merge-results (old new)
  "Combine the :results tokens OLD and NEW: a token replaces the old one of its category."
  (let ((tokens (split-words old)))
    (dolist (token (split-words new))
      (let ((category (results-category token)))
        (setf tokens (append (if category
                                 (remove-if (lambda (existing) (eq (results-category existing) category)) tokens)
                                 tokens)
                             (list token)))))
    (format nil "~{~A~^ ~}" tokens)))

(defun var-name (spec)
  (string-trim " " (subseq spec 0 (position #\= spec))))

(defun header-args-merge (&rest layers)
  "Merge the alists LAYERS, later ones winning. :var accumulates by variable name, :results by token category."
  (let ((result '()))
    (dolist (layer layers)
      (dolist (entry layer)
        (let ((key (car entry)))
          (cond ((string= key "var")
                 (setf result (append (remove-if (lambda (old) (and (string= (car old) "var")
                                                                    (string= (var-name (cdr old)) (var-name (cdr entry)))))
                                                 result)
                                      (list entry))))
                ((string= key "results")
                 (let ((old (assoc "results" result :test #'string=)))
                   (setf result (append (remove "results" result :key #'car :test #'string=)
                                        (list (cons "results" (if old (merge-results (cdr old) (cdr entry)) (cdr entry))))))))
                (t (setf result (append (remove key result :key #'car :test #'string=) (list entry))))))))
    result))

;;; Document model

(defun results-header (line)
  "For a #+RESULTS[hash]: name line return (values t name hash)."
  (let ((s (string-left-trim '(#\space #\tab) line)))
    (when (and (>= (length s) 10) (string-equal "#+RESULTS" s :end2 9))
      (let ((rest (subseq s 9)) (hash nil))
        (when (char= (char rest 0) #\[)
          (let ((close (position #\] rest)))
            (when close
              (setf hash (subseq rest 1 close)
                    rest (subseq rest (1+ close))))))
        (when (and (plusp (length rest)) (char= (char rest 0) #\:))
          (let ((name (string-trim " " (subseq rest 1))))
            (values t (and (plusp (length name)) name) hash)))))))

(defun property-header-args (drawer-lines)
  "Read :header-args:, :header-args+: and :header-args:LANG: from the lines of a :PROPERTIES: drawer.
Return ((lang . alist) ...), LANG being NIL for the generic ones."
  (let ((entries '()))
    (dolist (line drawer-lines)
      (let ((s (string-trim '(#\space #\tab) line)))
        (when (and (> (length s) 13) (string-equal ":header-args" s :end2 12))
          (let ((i 12))
            (when (char= (char s i) #\+) (incf i))
            (when (and (< i (length s)) (char= (char s i) #\:))
              (incf i)
              (let* ((stop (or (position #\space s :start i) (length s)))
                     (segment (subseq s i stop))
                     (language (and (> (length segment) 1) (char= (char segment (1- (length segment))) #\:)
                                    (string-right-trim "+" (subseq segment 0 (1- (length segment))))))
                     (args (if language (subseq s stop) (subseq s i))))
                (push (cons language (header-args-parse args)) entries)))))))
    (nreverse entries)))

(defun file-header-args (lines)
  "The ((lang . alist) ...) set by #+PROPERTY: header-args lines anywhere in LINES."
  (let ((entries '()))
    (dolist (line lines)
      (when (line-starts-with-ci-p line "#+property:")
        (let* ((words (split-words (subseq (string-left-trim " " line) 11)))
               (key (first words)))
          (when (and key (>= (length key) 11) (string-equal "header-args" key :end2 11))
            (let ((language (and (> (length key) 12) (char= (char key 11) #\:) (subseq key 12))))
              (push (cons language (header-args-from-words (rest words))) entries))))))
    (nreverse entries)))

(defun args-for-language (entries language)
  "Concatenate the alists of ENTRIES, ((lang . alist) ...), that apply to LANGUAGE: generic ones first."
  (append (loop :for entry :in entries :when (null (car entry)) :append (cdr entry))
          (loop :for entry :in entries
                :when (and (car entry) language (string-equal (car entry) language)) :append (cdr entry))))

(defun babel-body-lines (raw indent)
  "Block body lines: the indentation of the #+begin_src line is removed and ,* and ,#+ unescaped."
  (mapcar (lambda (line)
            (let* ((strip (min indent (or (position #\space line :test-not #'char=) 0)))
                   (line (subseq line strip))
                   (comma (position #\, line)))
              (if (and comma (every (lambda (c) (char= c #\space)) (subseq line 0 comma))
                       (< (1+ comma) (length line))
                       (or (char= (char line (1+ comma)) #\*)
                           (and (< (+ comma 2) (length line)) (string= "#+" line :start2 (1+ comma) :end2 (+ comma 3)))))
                  (concatenate 'string (subseq line 0 comma) (subseq line (1+ comma)))
                  line)))
          raw))

(defun babel-scan (lines keywords)
  "Return the elements of the Org text LINES in order, as property lists.
:kind is :src, :table, :list, :example, :results or :call. Every element has :begin and :end, 0-based line
indexes, and :name from a preceding #+NAME:. A :src also has :language :switches :own (its header arguments)
:args (the inherited ones merged) :body :indent :heading and :level."
  (let* ((v (coerce lines 'vector)) (n (length v)) (i 0)
         (file-args (file-header-args lines))
         (stack '()) (pending nil) (records '()))
    (flet ((emit (record) (push record records)))
      (loop :while (< i n)
            :do (let ((line (aref v i)))
                  (cond
                    ((src-begin-line-p line)
                     (let* ((end (position-if #'src-end-line-p v :start (1+ i)))
                            (last (or end (1- n)))
                            (indent (line-indent line)))
                       (multiple-value-bind (language own switches) (src-header line)
                         (emit (list :kind :src :name pending :begin i :end last
                                     :language language :switches switches :own own
                                     :args (apply #'header-args-merge
                                                  *org-babel-default-header-args*
                                                  (args-for-language file-args language)
                                                  (append (mapcar (lambda (entry)
                                                                    (args-for-language (getf entry :args) language))
                                                                  (reverse stack))
                                                          (list own)))
                                     :body (babel-body-lines (coerce (subseq v (1+ i) (or end n)) 'list) indent)
                                     :indent indent
                                     :heading (getf (first stack) :title) :level (getf (first stack) :level))))
                       (setf pending nil i (1+ last))))
                    ((line-starts-with-ci-p line "#+name:")
                     (setf pending (string-trim " " (subseq (string-left-trim " " line) 7)))
                     (incf i))
                    ((line-starts-with-ci-p line "#+call:")
                     (emit (list :kind :call :name pending :begin i :end i
                                 :text (string-trim " " (subseq (string-left-trim " " line) 7))))
                     (setf pending nil)
                     (incf i))
                    ((results-header line)
                     (multiple-value-bind (ok name hash) (results-header line)
                       (declare (ignore ok))
                       (let ((length (results-element-length (coerce (subseq v (min n (1+ i))) 'list))))
                         (emit (list :kind :results :name name :hash hash :begin i :end (+ i length)))
                         (setf pending nil)
                         (setf i (+ i 1 length)))))
                    ((heading-split line keywords)
                     (let* ((level (heading-split line keywords))
                            (title (nth-value 3 (heading-split line keywords)))
                            (j (1+ i)))
                       (when (and (< j n) (planning-line-p (aref v j))) (incf j))
                       (let ((args '()))
                         (when (and (< j n) (string-equal (string-trim " " (aref v j)) ":PROPERTIES:"))
                           (let ((end (or (position-if (lambda (l) (string-equal (string-trim " " l) ":END:")) v :start j) n)))
                             (setf args (property-header-args (coerce (subseq v (1+ j) end) 'list)))))
                         (setf stack (remove-if (lambda (entry) (>= (getf entry :level) level)) stack))
                         (push (list :level level :title title :args args) stack)))
                     (setf pending nil)
                     (incf i))
                    ((table-line-p line)
                     (let ((end (or (position-if-not #'table-line-p v :start i) n)))
                       (emit (list :kind :table :name pending :begin i :end (1- end)
                                   :rows (map 'list #'table-split-line (subseq v i end))))
                       (setf pending nil i end)))
                    ((and (list-item-parse line) (not (heading-split line keywords)))
                     (let ((end (or (position-if-not (lambda (l) (or (list-item-parse l)
                                                                     (and (not (blank-string-p l)) (> (line-indent l) 0))))
                                                     v :start i)
                                    n)))
                       (emit (list :kind :list :name pending :begin i :end (1- end)
                                   :items (mapcar #'fourth (list-flat-items (coerce (subseq v i end) 'list)))))
                       (setf pending nil i end)))
                    ((and (>= (length line) 1) (char= (char line 0) #\:) (or (= (length line) 1) (char= (char line 1) #\space)))
                     (let ((end (or (position-if-not (lambda (l) (and (>= (length l) 1) (char= (char l 0) #\:)
                                                                      (or (= (length l) 1) (char= (char l 1) #\space))))
                                                     v :start i)
                                    n)))
                       (emit (list :kind :example :name pending :begin i :end (1- end)
                                   :lines (map 'list (lambda (l) (if (> (length l) 2) (subseq l 2) "")) (subseq v i end))))
                       (setf pending nil i end)))
                    ((line-starts-with-ci-p line "#+begin_")
                     (let* ((name (string-downcase (or (first (split-words (subseq (string-left-trim " " line) 8))) "")))
                            (end (or (position-if (lambda (l) (and (line-starts-with-ci-p l "#+end_")
                                                                   (string-equal name (string-trim " " (subseq (string-left-trim " " l) 6)))))
                                                  v :start (1+ i))
                                     (1- n))))
                       (when (string= name "example")
                         (emit (list :kind :example :name pending :begin i :end end
                                     :lines (coerce (subseq v (1+ i) end) 'list))))
                       (setf pending nil i (1+ end))))
                    ((or (blank-string-p line) (line-starts-with-ci-p line "#+")) (incf i))
                    (t (setf pending nil) (incf i))))))
    (nreverse records)))

(defun babel-find-named (records name)
  "The element called NAME: a source block, else a table / list / example, else a #+RESULTS: with that name."
  (flet ((named (kinds)
           (find-if (lambda (r) (and (member (getf r :kind) kinds) (equal (getf r :name) name))) records)))
    (or (named '(:src)) (named '(:table :list :example)) (named '(:results)))))

(defun babel-block-at (records line)
  "The source block whose lines include the 0-based LINE."
  (find-if (lambda (r) (and (eq (getf r :kind) :src) (<= (getf r :begin) line (getf r :end)))) records))

(defun babel-call-at (records line)
  (find-if (lambda (r) (and (eq (getf r :kind) :call) (= (getf r :begin) line))) records))

;;; Existing results

(defun fixed-width-line-p (line)
  (and (>= (length line) 1) (char= (char line 0) #\:) (or (= (length line) 1) (char= (char line 1) #\space))))

(defun results-element-length (lines)
  "How many of LINES, the lines after a #+RESULTS: line, belong to the result."
  (let ((first (first lines)))
    (cond ((or (null first) (blank-string-p first)) 0)
          ((line-starts-with-ci-p first "#+begin_")
           (let* ((name (string-downcase (or (first (split-words (subseq (string-left-trim " " first) 8))) "")))
                  (end (position-if (lambda (l) (and (line-starts-with-ci-p l "#+end_")
                                                     (string-equal name (string-trim " " (subseq (string-left-trim " " l) 6)))))
                                    lines)))
             (if end (1+ end) (length lines))))
          ((string-equal (string-trim " " first) ":RESULTS:")
           (let ((end (position-if (lambda (l) (string-equal (string-trim " " l) ":END:")) lines)))
             (if end (1+ end) (length lines))))
          ((table-line-p first) (or (position-if-not #'table-line-p lines) (length lines)))
          ((fixed-width-line-p first) (or (position-if-not #'fixed-width-line-p lines) (length lines)))
          ((list-item-parse first)
           (or (position-if-not (lambda (l) (or (list-item-parse l) (and (not (blank-string-p l)) (> (line-indent l) 0)))) lines)
               (length lines)))
          (t (or (position-if #'blank-string-p lines) (length lines))))))

(defun results-extent (lines name)
  "Find the result of a block called NAME (or NIL) in LINES, the lines right after its #+end_src.
Return (values offset count hash): the #+RESULTS: line starts OFFSET lines in and, with its content, spans COUNT lines."
  (let ((blank (or (position-if-not #'blank-string-p lines) (length lines))))
    (when (< blank (length lines))
      (multiple-value-bind (ok header-name hash) (results-header (nth blank lines))
        (when (and ok (or (null header-name) (equal header-name name)))
          (values blank (+ 1 (results-element-length (nthcdr (1+ blank) lines))) hash))))))

;;; Planning edits to the buffer

(defun result-edit (after end-index name lines handling)
  "Plan how to put the result LINES (the #+RESULTS: line first, or NIL to remove the result) under a block or call.
AFTER are the buffer lines following the block's last line, at the 0-based END-INDEX. HANDLING is replace, append or
prepend. Line numbers in the answer are 1-based. One of:
  (:insert N lines)    add LINES as new lines after line N
  (:replace S COUNT lines)   replace COUNT lines starting at line S
  (:delete A B)        delete lines A+1 to B
or NIL when there is nothing to do."
  (multiple-value-bind (offset count) (results-extent after name)
    (cond
      ((null lines)
       (and offset (list :delete (1+ end-index) (+ end-index 1 offset count))))
      ((and offset (string-equal handling "append"))
       (list :insert (+ end-index 1 offset count) (rest lines)))
      ((and offset (string-equal handling "prepend"))
       (list :insert (+ end-index 2 offset) (rest lines)))
      (offset
       (list :replace (+ end-index 2 offset) count lines))
      (t
       (list :insert (1+ end-index) (cons "" lines))))))

;;; Values: scalars (string or number), lists, tables (lists of rows, :rule for a horizontal line)

(defun number-text (number)
  (let ((*read-default-float-format* 'double-float))
    (princ-to-string number)))

(defun value-cell-string (x)
  (cond ((null x) "") ((stringp x) x) ((numberp x) (number-text x)) ((eq x :rule) "") (t (princ-to-string x))))

(defun value-table-p (value)
  (and (consp value) (every (lambda (x) (or (consp x) (eq x :rule))) value)))

(defun json-parse (string &key (true "true") (false "false") (null "null"))
  "Parse the JSON in STRING. An array becomes a list, an object a list of (key value) rows."
  (let ((i 0) (n (length string)))
    (labels ((fail () (editor-error "Cannot read the result: ~A" (subseq string 0 (min 60 n))))
             (ws () (loop :while (and (< i n) (member (char string i) '(#\space #\tab #\newline #\return))) :do (incf i)))
             (word (text) (when (and (<= (+ i (length text)) n) (string= text string :start2 i :end2 (+ i (length text))))
                            (incf i (length text)) t))
             (value ()
               (ws)
               (when (>= i n) (fail))
               (let ((c (char string i)))
                 (cond ((char= c #\[) (incf i) (items #\] #'value))
                       ((char= c #\{) (incf i) (items #\} #'pair))
                       ((char= c #\") (incf i) (str))
                       ((word "true") true)
                       ((word "false") false)
                       ((word "null") null)
                       (t (num)))))
             (items (close reader)
               (let ((result '()))
                 (ws)
                 (if (and (< i n) (char= (char string i) close))
                     (incf i)
                     (loop (push (funcall reader) result)
                           (ws)
                           (when (>= i n) (fail))
                           (cond ((char= (char string i) #\,) (incf i))
                                 ((char= (char string i) close) (incf i) (return))
                                 (t (fail)))))
                 (nreverse result)))
             (pair ()
               (ws)
               (unless (and (< i n) (char= (char string i) #\")) (fail))
               (incf i)
               (let ((key (str)))
                 (ws)
                 (unless (and (< i n) (char= (char string i) #\:)) (fail))
                 (incf i)
                 (list key (value))))
             (str ()
               (with-output-to-string (out)
                 (loop (when (>= i n) (fail))
                       (let ((c (char string i)))
                         (incf i)
                         (cond ((char= c #\") (return))
                               ((char= c #\\)
                                (when (>= i n) (fail))
                                (let ((e (char string i)))
                                  (incf i)
                                  (case e
                                    (#\n (write-char #\newline out)) (#\t (write-char #\tab out))
                                    (#\r (write-char #\return out)) (#\b (write-char #\backspace out))
                                    (#\f (write-char #\page out))
                                    (#\u (when (> (+ i 4) n) (fail))
                                     (write-char (code-char (parse-integer string :start i :end (+ i 4) :radix 16)) out)
                                     (incf i 4))
                                    (t (write-char e out)))))
                               (t (write-char c out)))))))
             (num ()
               (let ((start i))
                 (loop :while (and (< i n) (find (char string i) "+-0123456789.eE")) :do (incf i))
                 (when (= start i) (fail))
                 (let ((text (subseq string start i)))
                   (if (find-if (lambda (c) (find c ".eE")) text)
                       (let ((*read-default-float-format* 'double-float) (*read-eval* nil))
                         (let ((x (read-from-string text))) (if (numberp x) x (fail))))
                       (or (parse-integer text :junk-allowed t) (fail)))))))
      (value))))

(defun json-keywords (family)
  "The words for true, false and null that a language FAMILY would print."
  (case family
    (:python (list "True" "False" "None"))
    (:ruby (list "true" "false" "nil"))
    (:lisp (list "t" "nil" "nil"))
    (t (list "true" "false" "null"))))

;;; Data going into a block

(defun strip-rows (rows args)
  "Apply :hlines, :colnames and :rownames to the table ROWS. Return (values data header)."
  (let* ((colnames (header-arg args "colnames"))
         (header nil)
         (rows rows))
    (when (and rows (not (eq (first rows) :rule))
               (cond ((and colnames (string-equal colnames "yes")) t)
                     ((and colnames (member colnames '("no" "nil") :test #'string-equal)) nil)
                     (t (eq (second rows) :rule))))
      (setf header (first rows)
            rows (rest rows))
      (when (eq (first rows) :rule) (setf rows (rest rows))))
    (when (not (header-flag-p args "hlines"))
      (setf rows (remove :rule rows)))
    (when (and (header-arg args "rownames") (string-equal (header-arg args "rownames") "yes"))
      (setf rows (mapcar (lambda (row) (if (eq row :rule) row (rest row))) rows)
            header (rest header)))
    (values rows header)))

(defun rows-to-value (rows args)
  "The value a block sees for a table: a single-column table becomes a flat list."
  (multiple-value-bind (data header) (strip-rows rows args)
    (values (if (and data (every (lambda (row) (and (consp row) (null (cdr row)))) data))
                (mapcar #'first data)
                data)
            header)))

(defun babel-encode-scalar (family x)
  (cond ((null x) (babel-quote family ""))
        ((numberp x) (number-text x))
        ((eq x :rule) (babel-quote family "hline"))
        ((table-number-p x) x)
        (t (babel-quote family x))))

(defun babel-encode (family value)
  "Source text of VALUE (a scalar, list or table) in the language FAMILY."
  (flet ((join (items) (format nil "~{~A~^, ~}" items)))
    (cond ((not (consp value)) (babel-encode-scalar family value))
          (t
           (let ((items (mapcar (lambda (x) (if (consp x) (babel-encode family x) (babel-encode-scalar family x))) value)))
             (ecase family
               ((:python :js :ruby :perl :php) (format nil "[~A]" (join items)))
               (:lua (format nil "{~A}" (join items)))
               (:lisp (format nil "(list ~{~A~^ ~})" items))
               (:r (if (every (lambda (x) (not (consp x))) value)
                       (format nil "c(~A)" (join items))
                       (format nil "rbind(~A)" (join items))))
               (:powershell (format nil "@(~{~A~^, ~})" (mapcar (lambda (item) (if (char= (char item 0) #\@) (format nil ",~A" item) item)) items)))
               (:shell (babel-quote :shell
                                    (format nil "~{~A~^~%~}"
                                            (mapcar (lambda (x) (if (consp x)
                                                                    (format nil "~{~A~^	~}" (mapcar #'value-cell-string x))
                                                                    (value-cell-string x)))
                                                    value))))))))))

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
       (wrap "\"" "\"" '((#\\ . "\\\\") (#\" . "\\\"") (#\newline . "\\n") (#\return . "\\r") (#\tab . "\\t"))))
      ((:perl :php) (wrap "'" "'" '((#\' . "\\'") (#\\ . "\\\\"))))
      (:shell (wrap "'" "'" '((#\' . "'\\''"))))
      (:powershell (wrap "'" "'" '((#\' . "''")))))))

(defun babel-family-vars-p (family)
  (member family '(:python :js :ruby :perl :php :lua :r :powershell :lisp :shell)))

(defun babel-assignment (family name value)
  "Source text that binds NAME to VALUE."
  (unless (babel-family-vars-p family)
    (editor-error "Variables (:var) are not supported for ~(~A~)" family))
  (let ((literal (babel-encode family value)))
    (ecase family
      (:python (format nil "~A = ~A" name literal))
      (:ruby (format nil "~A = ~A" name literal))
      (:js (format nil "const ~A = ~A;" name literal))
      (:perl (format nil "my $~A = ~A;" name literal))
      (:php (format nil "$~A = ~A;" name literal))
      (:lua (format nil "local ~A = ~A" name literal))
      (:shell (format nil "~A=~A" name literal))
      (:powershell (format nil "$~A = ~A" name literal))
      (:r (format nil "~A <- ~A" name literal))
      (:lisp (format nil "(defparameter ~A ~A)" name literal)))))

(defun literal-var-value (text)
  "The value of a :var that is not a reference: a number, a \"quoted string\" or a bare string."
  (let ((text (string-trim " " text)))
    (cond ((and (>= (length text) 2) (char= (char text 0) #\") (char= (char text (1- (length text))) #\"))
           (subseq text 1 (1- (length text))))
          (t text))))

(defun matching-close (text open-position)
  "Position of the bracket that closes the one at OPEN-POSITION in TEXT, skipping nested brackets and strings."
  (let ((depth 0) (quoted nil))
    (loop :for i :from open-position :below (length text)
          :for c := (char text i)
          :do (cond ((char= c #\") (setf quoted (not quoted)))
                    (quoted)
                    ((find c "([") (incf depth))
                    ((find c ")]") (decf depth) (when (zerop depth) (return i)))))))

(defun split-top-level (text)
  "Split TEXT at commas outside brackets and strings."
  (let ((parts '()) (start 0) (quoted nil) (depth 0))
    (loop :for i :from 0 :to (length text)
          :for c := (and (< i (length text)) (char text i))
          :do (cond ((eql c #\") (setf quoted (not quoted)))
                    ((and (not quoted) (member c '(#\( #\[))) (incf depth))
                    ((and (not quoted) (member c '(#\) #\]))) (decf depth))
                    ((or (null c) (and (eql c #\,) (not quoted) (zerop depth)))
                     (let ((part (string-trim " " (subseq text start i))))
                       (when (plusp (length part)) (push part parts)))
                     (setf start (1+ i)))))
    (nreverse parts)))

(defun call-parse (text)
  "Parse name[inside](arguments)[end] of a #+CALL: line. Return (values name arguments inside-args end-args),
ARGUMENTS being a list of strings such as \"x=1\" split at top-level commas."
  (let* ((text (string-trim " " text))
         (stop (or (position-if (lambda (c) (find c "([ ")) text) (length text)))
         (name (subseq text 0 stop))
         (pos stop) inside end arguments)
    (flet ((group (open)
             (when (and (< pos (length text)) (char= (char text pos) open))
               (let ((closing (matching-close text pos)))
                 (when closing
                   (prog1 (subseq text (1+ pos) closing) (setf pos (1+ closing))))))))
      (setf inside (group #\[))
      (let ((arguments-text (group #\()))
        (when arguments-text (setf arguments (split-top-level arguments-text))))
      (setf end (group #\[))
      (values name arguments inside (or end (string-trim " " (subseq text (min pos (length text)))))))))

;;; Value mode: wrap the body so that its result is printed as JSON after a marker

(defparameter *babel-value-marker* "ORG-BABEL-VALUE-4c1e:")
(defparameter *babel-session-marker* "ORG-BABEL-SESSION-4c1e:")

(defun indent-lines (text prefix)
  (format nil "~{~A~^~%~}" (mapcar (lambda (line) (if (blank-string-p line) line (concatenate 'string prefix line)))
                                   (split-text-lines text))))

(defun babel-value-program (family body &optional names)
  "BODY wrapped so that it prints its value as JSON after the value marker, or NIL when FAMILY cannot do that.
NAMES are the variables the body uses (php functions only see globals that they declare)."
  (let ((m *babel-value-marker*))
    (case family
      (:python (format nil "def __org_main__():~%~A~%import json as __org_json~%print(~S + __org_json.dumps(__org_main__(), default=str))~%"
                       (if (blank-string-p body) "    pass" (indent-lines body "    ")) m))
      (:js (format nil "const __org_main__ = () => {~%~A~%};~%console.log(~S + JSON.stringify(__org_main__()));~%" body m))
      (:ruby (format nil "require 'json'~%__org_r = begin~%~A~%end~%puts ~S + __org_r.to_json~%" body m))
      (:perl (format nil "use JSON::PP;~%my $__org_r = do {~%~A~%};~%print ~S . JSON::PP->new->allow_nonref->encode($__org_r) . \"\\n\";~%" body m))
      (:php (format nil "$__org_main__ = function () {~%~@[global ~{$~A~^, ~};~%~]~A~%};~%echo ~S . json_encode($__org_main__()) . \"\\n\";~%" names body m))
      (:lua (format nil "local function __org_enc(v)~%  local t = type(v)~%  if t == 'table' then~%    local parts = {}~%    for _, x in ipairs(v) do parts[#parts + 1] = __org_enc(x) end~%    return '[' .. table.concat(parts, ',') .. ']'~%  elseif t == 'number' or t == 'boolean' then return tostring(v)~%  elseif v == nil then return 'null'~%  else return '\"' .. tostring(v):gsub('[\"\\\\]', '\\\\%0'):gsub('\\n', '\\\\n') .. '\"' end~%end~%local function __org_main__()~%~A~%end~%print(~S .. __org_enc(__org_main__()))~%" body m))
      (:powershell (format nil "$__org_r = & {~%~A~%}~%Write-Output (~S + (ConvertTo-Json -InputObject $__org_r -Compress -Depth 8))~%" body m))
      (:lisp (format nil "(defun __org-enc (v)~%  (cond ((null v) \"[]\") ((stringp v) (prin1-to-string v)) ((numberp v) (format nil \"~~A\" v))~%        ((consp v) (format nil \"[~~{~~A~~^,~~}]\" (mapcar (function __org-enc) v)))~%        (t (prin1-to-string (princ-to-string v)))))~%(let ((__org-r (progn~%~A~%)))~%  (format t \"~A~~A~~%\" (__org-enc __org-r)))~%" body m))
      (:r (format nil ".org_q <- function(x) if (is.numeric(x)) as.character(x) else paste0('\"', gsub('\"', '\\\\\\\\\"', as.character(x)), '\"')~%.org_enc <- function(v) {~%  if (is.data.frame(v)) v <- as.matrix(v)~%  if (is.matrix(v)) return(paste0('[', paste(apply(v, 1, function(r) paste0('[', paste(sapply(r, .org_q), collapse=','), ']')), collapse=','), ']'))~%  if (length(v) == 1) return(.org_q(v))~%  paste0('[', paste(sapply(v, .org_q), collapse=','), ']')~%}~%.org_r <- local({~%~A~%})~%cat(~S, .org_enc(.org_r), '\\n', sep='')~%" body m))
      (t nil))))

(defun babel-print-statement (family text)
  "A statement in FAMILY that prints the line TEXT."
  (ecase family
    ((:python :lua) (format nil "print(~S)" text))
    (:js (format nil "console.log(~S);" text))
    (:ruby (format nil "puts ~S" text))
    (:perl (format nil "print ~S . \"\\n\";" text))
    (:php (format nil "echo ~S . \"\\n\";" text))
    (:shell (format nil "echo ~S" text))
    (:powershell (format nil "Write-Output ~S" text))
    (:lisp (format nil "(format t \"~A~~%\")" text))
    (:r (format nil "cat(~S, '\\n', sep='')" text))))

(defun babel-var-values (args resolve)
  "The :var bindings of ARGS as (name . value). RESOLVE is called with the text after the = sign and
returns (values value found); if not found the text is a literal."
  (loop :for (key . spec) :in args
        :when (string= key "var")
          :collect (let ((equals (position #\= spec)))
                     (unless (and equals (plusp equals))
                       (editor-error "Bad :var ~A (use name=value)" spec))
                     (let ((text (string-trim " " (subseq spec (1+ equals)))))
                       (multiple-value-bind (value found) (funcall resolve text)
                         (cons (string-trim " " (subseq spec 0 equals))
                               (if found value (literal-var-value text))))))))

(defun babel-program (family args body &key bindings value session-before)
  "The program text to run: variable BINDINGS, the SESSION-BEFORE bodies, then BODY wrapped in :prologue / :epilogue
and, when VALUE is true, in the value wrapper. Return (values text value-wrapped-p)."
  (let* ((prologue (header-arg args "prologue"))
         (epilogue (header-arg args "epilogue"))
         (full (let ((text (format nil "~@[~A~%~]~A~@[~%~A~]" (and prologue (literal-var-value prologue)) body
                                   (and epilogue (literal-var-value epilogue)))))
                 (if (and (plusp (length text)) (char/= (char text (1- (length text))) #\newline))
                     (concatenate 'string text (string #\newline))
                     text)))
         (wrapped (and value (babel-value-program family full (mapcar #'car bindings)))))
    (values (format nil "~@[~A~%~]~{~A~%~}~{~A~%~}~@[~A~%~]~A"
                    (and (eq family :php) "<?php")
                    (loop :for (name . v) :in bindings :collect (babel-assignment family name v))
                    session-before
                    (and session-before (babel-print-statement family *babel-session-marker*))
                    (or wrapped full))
            (and wrapped t))))

(defun babel-split-output (stdout &key session value)
  "Split what a program printed. Return (values output value-text): OUTPUT is the text after the session
marker, VALUE-TEXT the text after the value marker, if any."
  (let ((text stdout))
    (when session
      (let ((at (search *babel-session-marker* text :from-end t)))
        (when at
          (setf text (let ((newline (position #\newline text :start at)))
                       (if newline (subseq text (1+ newline)) ""))))))
    (if value
        (let ((at (search *babel-value-marker* text :from-end t)))
          (if at
              (values (subseq text 0 at)
                      (let* ((start (+ at (length *babel-value-marker*))) (end (position #\newline text :start start)))
                        (subseq text start end)))
              (values text nil)))
        (values text nil))))

;;; Results

(defun text-to-rows (text)
  "Read TEXT as a table: cells are separated by tabs, or by blanks when there are no tabs."
  (let ((tabs (find #\tab text)))
    (mapcar (lambda (line)
              (if tabs
                  (loop :with from := 0
                        :for tab := (position #\tab line :start from)
                        :collect (string-trim " " (subseq line from tab))
                        :while tab :do (setf from (1+ tab)))
                  (split-words line)))
            (trim-trailing-blank-lines (split-text-lines text)))))

(defun trim-trailing-blank-lines (lines)
  (let ((end (position-if (lambda (line) (not (blank-string-p line))) lines :from-end t)))
    (if end (subseq lines 0 (1+ end)) '())))

(defun value-text (value)
  "VALUE as plain text lines (for raw and wrapped results)."
  (cond ((null value) '())
        ((stringp value) (trim-trailing-blank-lines (split-text-lines value)))
        ((numberp value) (list (number-text value)))
        ((value-table-p value)
         (org-table-format-rows (mapcar (lambda (row) (if (eq row :rule) row (mapcar #'value-cell-string row))) value)))
        (t (mapcar (lambda (x) (format nil "- ~A" (value-cell-string x))) value))))

(defun format-babel-result (value args &key language name hash header)
  "The lines to put in the buffer for a block's VALUE: the #+RESULTS: line and its content, or NIL when empty.
HEADER, the column names stripped from the input, is put back on a table."
  (let* ((tokens (split-words (or (header-arg args "results") "")))
         (has (lambda (token) (member token tokens :test #'string-equal)))
         (wrap (header-arg args "wrap"))
         (value (cond ((and (stringp value) (funcall has "table")) (text-to-rows value))
                      ((and (consp value) (not (value-table-p value)) (not (funcall has "list")))
                       (mapcar #'list value))
                      (t value)))
         (value (if (and header (value-table-p value)) (list* header :rule value) value))
         (file (and (funcall has "file") (header-arg args "file")))
         (raw (cond (file (list (let ((description (header-arg args "file-desc")))
                                  (if description
                                      (format nil "[[file:~A][~A]]" file (literal-var-value description))
                                      (format nil "[[file:~A]]" file)))))
                    (t (value-text value))))
         (fixed (lambda () (mapcar (lambda (line) (concatenate 'string ": " line)) raw)))
         (body (cond ((null raw) '())
                     (wrap (append (list (format nil "#+begin_~A" (if (string-equal wrap "t") "results" wrap)))
                                   raw (list (format nil "#+end_~A" (if (string-equal wrap "t") "results" (first (split-words wrap)))))))
                     ((funcall has "drawer") (append (list ":RESULTS:") raw (list ":END:")))
                     ((funcall has "code") (append (list (format nil "#+begin_src ~A" (or language ""))) raw (list "#+end_src")))
                     ((funcall has "html") (append (list "#+begin_export html") raw (list "#+end_export")))
                     ((funcall has "latex") (append (list "#+begin_export latex") raw (list "#+end_export")))
                     ((funcall has "org") (append (list "#+begin_src org") raw (list "#+end_src")))
                     (file raw)
                     ((funcall has "raw") raw)
                     ((value-table-p value) raw)
                     ((and (consp value) (funcall has "list")) raw)
                     ((consp value) raw)
                     (t (funcall fixed)))))
    (when body
      (cons (format nil "#+RESULTS~@[[~A]~]:~@[ ~A~]" hash name) body))))

(defun babel-hash (string)
  "A 64-bit FNV-1a hash of STRING as 16 hex digits, for :cache."
  (let ((hash #xcbf29ce484222325))
    (loop :for c :across string
          :do (setf hash (logand #xffffffffffffffff (* (logxor hash (char-code c)) #x100000001b3))))
    (format nil "~(~16,'0X~)" hash)))

;;; Noweb

(defun noweb-expand-line (line resolver depth)
  (when (> depth 20) (editor-error "Noweb references nest too deeply (a cycle?)"))
  (let ((out (make-string-output-stream)) (pos 0)
        (indent (subseq line 0 (or (position-if-not (lambda (c) (member c '(#\space #\tab))) line) (length line)))))
    (loop
      (let* ((open (search "<<" line :start2 pos))
             (close (and open (search ">>" line :start2 (+ open 2)))))
        (cond ((and open close (> close (+ open 2)))
               (let* ((name (subseq line (+ open 2) close))
                      (replacement (and (not (find #\< name)) (funcall resolver (string-trim " " name)))))
                 (write-string (subseq line pos open) out)
                 (if replacement
                     (write-string (indent-continuation (noweb-expand replacement resolver (1+ depth)) indent) out)
                     (write-string (subseq line open (+ close 2)) out))
                 (setf pos (+ close 2))))
              (t (write-string (subseq line pos) out) (return)))))
    (get-output-stream-string out)))

(defun indent-continuation (text indent)
  (with-output-to-string (out)
    (loop :for c :across text
          :do (write-char c out)
              (when (char= c #\newline) (write-string indent out)))))

(defun noweb-expand (text resolver &optional (depth 0))
  "Replace every <<name>> in TEXT by (funcall RESOLVER name); a NIL answer leaves the reference alone.
Continuation lines of a replacement get the indentation of the line that held the reference."
  (format nil "~{~A~^~%~}" (mapcar (lambda (line) (noweb-expand-line line resolver depth)) (split-text-lines text))))

(defun noweb-resolver (records)
  "A resolver for `noweb-expand': the body of the block called NAME, or of all blocks with :noweb-ref NAME."
  (lambda (name)
    (let ((refs (remove-if-not (lambda (r) (and (eq (getf r :kind) :src)
                                                (equal (header-arg (getf r :args) "noweb-ref") name)))
                               records)))
      (if refs
          (let ((separator (let ((s (header-arg (getf (first refs) :args) "noweb-sep")))
                             (if s (literal-var-value s) (string #\newline)))))
            (with-output-to-string (out)
              (loop :for r :in refs :for first := t :then nil
                    :do (unless first (write-string separator out))
                        (format out "~{~A~^~%~}" (getf r :body)))))
          (let ((named (find-if (lambda (r) (and (eq (getf r :kind) :src) (equal (getf r :name) name))) records)))
            (and named (format nil "~{~A~^~%~}" (getf named :body))))))))

(defun noweb-expands-p (args context)
  "Whether :noweb in ARGS means expansion in CONTEXT, one of :eval :tangle :export.
yes expands everywhere; tangle only when tangling; no-export and strip-export everywhere but export
(strip-export removes the references there, see `noweb-strips-p'); eval only when running."
  (let ((mode (or (header-arg args "noweb") "no")))
    (and (not (header-arg args "no-expand"))
         (ecase context
           (:eval (member mode '("yes" "eval" "query" "no-export" "strip-export") :test #'string-equal))
           (:tangle (member mode '("yes" "tangle" "no-export" "strip-export") :test #'string-equal))
           (:export (member mode '("yes") :test #'string-equal))))))

(defun noweb-strips-p (args)
  "Whether :noweb strip-export asks for the references to be removed from an export."
  (equal (and (header-arg args "noweb") (string-downcase (header-arg args "noweb"))) "strip-export"))

;;; Tangle

(defvar *org-babel-comment-prefixes*
  '(("python" . "#") ("sh" . "#") ("shell" . "#") ("bash" . "#") ("zsh" . "#") ("ruby" . "#") ("perl" . "#")
    ("r" . "#") ("powershell" . "#") ("yaml" . "#") ("makefile" . "#") ("toml" . "#") ("conf" . "#")
    ("js" . "//") ("javascript" . "//") ("c" . "//") ("cpp" . "//") ("java" . "//") ("go" . "//") ("rust" . "//")
    ("php" . "//") ("typescript" . "//") ("lisp" . ";;") ("emacs-lisp" . ";;") ("elisp" . ";;") ("scheme" . ";;")
    ("clojure" . ";;") ("lua" . "--") ("sql" . "--") ("haskell" . "--"))
  "Line comment prefix of a language, for :comments when tangling.")

(defun tangle-file-name (record default-base)
  "The file a block is tangled to, or NIL."
  (let ((tangle (header-arg (getf record :args) "tangle"))
        (language (getf record :language)))
    (cond ((or (null tangle) (string-equal tangle "no")) nil)
          ((string-equal tangle "yes")
           (format nil "~A.~A" default-base
                   (or (cdr (assoc language '(("python" . "py") ("sh" . "sh") ("shell" . "sh") ("bash" . "sh") ("javascript" . "js")
                                              ("js" . "js") ("ruby" . "rb") ("perl" . "pl") ("lua" . "lua") ("lisp" . "lisp")
                                              ("emacs-lisp" . "el") ("elisp" . "el") ("r" . "R") ("powershell" . "ps1")
                                              ("c" . "c") ("cpp" . "cpp") ("java" . "java") ("go" . "go") ("rust" . "rs")
                                              ("php" . "php") ("sql" . "sql") ("haskell" . "hs") ("yaml" . "yaml") ("html" . "html")
                                              ("css" . "css") ("typescript" . "ts"))
                                   :test #'equalp))
                       (and language (string-downcase language)) "txt")))
          (t (string-trim "\"" tangle)))))

(defun parse-file-mode (string)
  "The octal mode in a :tangle-mode such as (identity #o755) or #o755, or NIL."
  (let ((at (search "#o" string)))
    (and at (parse-integer string :start (+ at 2) :junk-allowed t :radix 8))))

(defun babel-tangle-plan (records default-base source-name)
  "Plan the files written by tangling. Return a list of plists (:file :text :mode :mkdirp), in order of first use.
RECORDS come from `babel-scan'; DEFAULT-BASE names the file for :tangle yes; SOURCE-NAME is the Org file for link comments."
  (let ((files '()) (resolver (noweb-resolver records)) (counts (make-hash-table :test 'equal)))
    (dolist (record records)
      (when (eq (getf record :kind) :src)
        (let* ((args (getf record :args))
               (file (tangle-file-name record default-base)))
          (when file
            (let* ((body (format nil "~{~A~^~%~}" (getf record :body)))
                   (body (if (noweb-expands-p args :tangle) (noweb-expand body resolver) body))
                   (language (getf record :language))
                   (prefix (or (cdr (assoc language *org-babel-comment-prefixes* :test #'equalp)) "#"))
                   (comments (header-arg args "comments"))
                   (heading (or (getf record :heading) "(no heading)"))
                   (count (incf (gethash heading counts 0)))
                   (comment-lines
                     (when (and comments (member comments '("link" "org" "both") :test #'string-equal))
                       (append (when (member comments '("link" "both") :test #'string-equal)
                                 (list (format nil "~A [[file:~A::*~A][~A:~D]]" prefix source-name heading heading count)))
                               (when (member comments '("org" "both") :test #'string-equal)
                                 (list (format nil "~A ~A" prefix heading))))))
                   (entry (or (find file files :key (lambda (f) (getf f :file)) :test #'string=)
                              (let ((new (list :file file :chunks '() :mode nil :mkdirp nil :shebang nil)))
                                (setf files (append files (list new)))
                                new))))
              (setf (getf entry :chunks)
                    (append (getf entry :chunks)
                            (list (list :text (format nil "~{~A~%~}~A" comment-lines body)
                                        :pad (not (and (header-arg args "padline") (string-equal (header-arg args "padline") "no")))))))
              (when (header-flag-p args "mkdirp") (setf (getf entry :mkdirp) t))
              (let ((mode (header-arg args "tangle-mode")))
                (when mode (setf (getf entry :mode) (parse-file-mode mode))))
              (let ((shebang (header-arg args "shebang")))
                (when (and shebang (null (getf entry :shebang)))
                  (setf (getf entry :shebang) (literal-var-value shebang))))
              nil)))))
    (mapcar (lambda (entry)
              (list :file (getf entry :file)
                    :text (with-output-to-string (out)
                            (when (getf entry :shebang) (format out "~A~%" (getf entry :shebang)))
                            (loop :for chunk :in (getf entry :chunks)
                                  :for first := t :then nil
                                  :do (when (and (not first) (getf chunk :pad)) (terpri out))
                                      (format out "~A~%" (getf chunk :text))))
                    :mode (or (getf entry :mode) (and (getf entry :shebang) #o755))
                    :mkdirp (getf entry :mkdirp)))
            files)))

;;; END pure babel-model code
