;;; Tables beyond editing: spreadsheet formulas (#+TBLFM:), sums, conversion from and to text, CSV.
;;;
;;;   | 3 | 4 |    |
;;;   #+TBLFM: $3=$1*$2;%.1f::@>$1=vsum(@I..@>>)
;;;
;;; Fields are $COLUMN, @ROW and @ROW$COLUMN (rows count data lines, not horizontal rules); @I..@II is the part
;;; between two rules; @> and $> are the last row and column, @< and $< the first; @-1 and $-1 are relative.
;;; Functions: vsum vmean vmax vmin vcount vprod vmedian (of a range) and abs sqrt round floor ceiling exp ln log10
;;; sin cos tan min max if.

(in-package :lem-org-mode)

;;; BEGIN pure tables code

;;; Reading a cell

(defun cell-number (string)
  "The number written in the cell STRING, or NIL. Empty cells are not numbers."
  (let ((s (string-trim " " string)))
    (when (and (plusp (length s)) (some #'digit-char-p s)
               (every (lambda (c) (or (digit-char-p c) (find c "+-.eE"))) s)
               (digit-char-p (char s (if (find (char s 0) "+-") (min 1 (1- (length s))) 0))))
      (let ((*read-default-float-format* 'double-float) (*read-eval* nil))
        (let ((value (ignore-errors (read-from-string s))))
          (and (realp value) value))))))

(defun format-calc-number (x &optional spec)
  "X as the text of a table field. SPEC is a format such as %.2f; without it integers print as integers and other
numbers with at most 12 significant digits."
  (let ((x (if (and (rationalp x) (not (integerp x))) (coerce x 'double-float) x)))
    (cond
      ((and spec (plusp (length spec)) (char= (char spec 0) #\%))
       (let* ((dot (position #\. spec))
              (kind (char spec (1- (length spec))))
              (digits (and dot (parse-integer spec :start (1+ dot) :junk-allowed t))))
         (case kind
           (#\f (let ((*read-default-float-format* 'double-float))
                  (format nil "~,vF" (or digits 6) x)))
           (#\d (format nil "~D" (round x)))
           (#\e (format nil "~,v,,,,,'eE" (or digits 6) x))
           (t (format-calc-number x nil)))))
      ((integerp x) (format nil "~D" x))
      ((and (floatp x) (= x (ffloor x)) (< (abs x) 1d15)) (format nil "~D" (round x)))
      (t (let ((*read-default-float-format* 'double-float))
           (let* ((magnitude (if (zerop x) 0 (floor (log (abs x) 10))))
                  (decimals (max 0 (- 11 magnitude)))
                  (fixed (if (< -5 magnitude 15)
                             (format nil "~,vF" (min decimals 20) x)
                             (format nil "~,11,,,,,'eE" x))))
             (if (find #\. fixed)
                 (let ((trimmed (string-right-trim "0" fixed)))
                   (if (char= (char trimmed (1- (length trimmed))) #\.) (subseq trimmed 0 (1- (length trimmed))) trimmed))
                 fixed)))))))

;;; Tokens and parser

(defun formula-field-token-end (text start)
  "If a field reference begins at START in TEXT return the index after it."
  (let ((i start) (n (length text)) (any nil))
    (flet ((signed-number ()
             (when (and (< i n) (char= (char text i) #\-)) (incf i))
             (loop :while (and (< i n) (digit-char-p (char text i))) :do (incf i))))
      (when (and (< i n) (char= (char text i) #\@))
        (setf any t)
        (incf i)
        (when (>= i n) (return-from formula-field-token-end nil))
        (cond ((and (< i n) (find (char text i) "<>"))
               (let ((c (char text i))) (loop :while (and (< i n) (char= (char text i) c)) :do (incf i))))
              ((and (< i n) (char= (char text i) #\I))
               (loop :while (and (< i n) (char= (char text i) #\I)) :do (incf i)))
              (t (signed-number))))
      (when (and (< i n) (char= (char text i) #\$))
        (setf any t)
        (incf i)
        (when (>= i n) (return-from formula-field-token-end nil))
        (if (and (< i n) (find (char text i) "<>")) (incf i) (signed-number)))
      (and any (> i start) i))))

(defun formula-tokenize (text)
  "Tokens of a formula expression: numbers, (:field text), (:name string), operators as strings."
  (let ((tokens '()) (i 0) (n (length text)))
    (loop :while (< i n)
          :do (let ((c (char text i)))
                (cond
                  ((find c " 	") (incf i))
                  ((or (char= c #\@) (char= c #\$))
                   (let ((end (formula-field-token-end text i)))
                     (unless end (editor-error "Bad field reference in the formula near ~A" (subseq text i)))
                     (push (list :field (subseq text i end)) tokens)
                     (setf i end)))
                  ((or (digit-char-p c) (and (char= c #\.) (< (1+ i) n) (digit-char-p (char text (1+ i)))))
                   (let ((end (or (position-if-not (lambda (ch) (or (digit-char-p ch) (char= ch #\.))) text :start i) n)))
                     ;; exponent
                     (when (and (< end n) (find (char text end) "eE")
                                (< (1+ end) n) (or (digit-char-p (char text (1+ end)))
                                                   (and (find (char text (1+ end)) "+-") (< (+ end 2) n) (digit-char-p (char text (+ end 2))))))
                       (setf end (or (position-if-not #'digit-char-p text :start (+ end 2)) n)))
                     (push (or (cell-number (subseq text i end)) (editor-error "Bad number ~A" (subseq text i end))) tokens)
                     (setf i end)))
                  ((alpha-char-p c)
                   (let ((end (or (position-if-not (lambda (ch) (or (alphanumericp ch) (char= ch #\_))) text :start i) n)))
                     (push (list :name (string-downcase (subseq text i end))) tokens)
                     (setf i end)))
                  (t (let ((two (and (< (1+ i) n) (subseq text i (+ i 2)))))
                       (cond ((member two '("<=" ">=" "==" "!=" "&&" "||" "..") :test #'equal)
                              (push two tokens) (incf i 2))
                             ((find c "+-*/^%()<>,=")
                              (push (string c) tokens) (incf i))
                             (t (editor-error "Unexpected ~A in the formula" c))))))))
    (nreverse tokens)))

(defun formula-parse (text)
  "Parse the expression TEXT into a tree: numbers, (:field ref), (:range ref ref), (:call name args...),
(:binary op a b), (:unary op a)."
  (let ((tokens (formula-tokenize text)))
    (labels ((peek () (first tokens))
             (next () (pop tokens))
             (op-p (x) (and (stringp (peek)) (member (peek) x :test #'string=)))
             (expect (x) (unless (equal (peek) x) (editor-error "Expected ~A in the formula" x)) (next))
             (binary-level (operators sub)
               (let ((left (funcall sub)))
                 (loop :while (op-p operators)
                       :do (let ((op (next))) (setf left (list :binary op left (funcall sub)))))
                 left))
             (expression () (binary-level '("||") #'conjunction))
             (conjunction () (binary-level '("&&") #'comparison))
             (comparison () (binary-level '("==" "=" "!=" "<" ">" "<=" ">=") #'additive))
             (additive () (binary-level '("+" "-") #'multiplicative))
             (multiplicative () (binary-level '("*" "/" "%") #'power))
             (power () (let ((base (unary)))
                         (if (op-p '("^")) (progn (next) (list :binary "^" base (power))) base)))
             (unary () (cond ((op-p '("-")) (next) (list :unary "-" (unary)))
                             ((op-p '("+")) (next) (unary))
                             (t (primary))))
             (primary ()
               (let ((token (next)))
                 (cond ((numberp token) token)
                       ((equal token "(") (prog1 (expression) (expect ")")))
                       ((and (consp token) (eq (first token) :field))
                        (if (op-p '("..")) (progn (next) (let ((end (next)))
                                                          (unless (and (consp end) (eq (first end) :field)) (editor-error "Bad range"))
                                                          (list :range (second token) (second end))))
                            (list :field (second token))))
                       ((and (consp token) (eq (first token) :name))
                        (if (op-p '("("))
                            (progn (next)
                                   (let ((args '()))
                                     (unless (op-p '(")"))
                                       (loop (push (expression) args)
                                             (if (op-p '(",")) (next) (return))))
                                     (expect ")")
                                     (list* :call (second token) (nreverse args))))
                            (editor-error "Unknown name ~A in the formula" (second token))))
                       (t (editor-error "Cannot read the formula near ~A" (or token "its end")))))))
      (prog1 (expression)
        (when tokens (editor-error "Unexpected ~A at the end of the formula" (peek)))))))

;;; Evaluation

(defstruct (sheet (:constructor make-sheet (rows)))
  "The data of a table while formulas run: ROWS are lists of cell strings or :rule; the data rows are indexed from 1."
  rows)

(defun sheet-data-rows (sheet) (remove :rule (sheet-rows sheet)))

(defun sheet-rule-positions (sheet)
  "For each horizontal rule, how many data rows lie above it."
  (let ((count 0) (positions '()))
    (dolist (row (sheet-rows sheet) (nreverse positions))
      (if (eq row :rule) (push count positions) (incf count)))))

(defun field-parse (text)
  "Split a field reference such as @2$3, @-1, $>, @I into (row column), each (:abs n) (:rel n) (:last k) (:first) (:rule n)
or NIL."
  (let ((row nil) (column nil) (i 0) (n (length text)))
    (flet ((number-at ()
             (let ((end (or (position-if-not (lambda (c) (or (digit-char-p c) (char= c #\-))) text :start i) n)))
               (prog1 (parse-integer text :start i :end end) (setf i end)))))
      (when (and (< i n) (char= (char text i) #\@))
        (incf i)
        (setf row
              (cond ((char= (char text i) #\>) (let ((k 0)) (loop :while (and (< i n) (char= (char text i) #\>)) :do (incf k) (incf i)) (list :last (1- k))))
                    ((char= (char text i) #\<) (let ((k 0)) (loop :while (and (< i n) (char= (char text i) #\<)) :do (incf k) (incf i)) (list :first (1- k))))
                    ((char= (char text i) #\I) (let ((k 0)) (loop :while (and (< i n) (char= (char text i) #\I)) :do (incf k) (incf i)) (list :rule k)))
                    ((char= (char text i) #\-) (list :rel (number-at)))
                    (t (list :abs (number-at))))))
      (when (and (< i n) (char= (char text i) #\$))
        (incf i)
        (setf column
              (cond ((char= (char text i) #\>) (incf i) (list :last 0))
                    ((char= (char text i) #\<) (incf i) (list :first 0))
                    ((char= (char text i) #\-) (list :rel (number-at)))
                    (t (list :abs (number-at))))))
      (list row column))))

(defun resolve-index (spec current total)
  "The 1-based index for SPEC, with CURRENT the index of the current row or column and TOTAL how many there are."
  (if (null spec)
      current
      (ecase (first spec)
        (:abs (second spec))
        (:rel (+ current (second spec)))
        (:last (- total (second spec)))
        (:first (+ 1 (second spec))))))

(defun sheet-cell (sheet row column)
  "The text of the data cell at ROW and COLUMN (1-based), or NIL when it does not exist."
  (let ((data (sheet-data-rows sheet)))
    (and (<= 1 row (length data)) (<= 1 column (length (nth (1- row) data)))
         (nth (1- column) (nth (1- row) data)))))

(defun sheet-set-cell (sheet row column value)
  "Store VALUE (a string) in the data cell at ROW and COLUMN when there is such a cell."
  (let ((data (sheet-data-rows sheet)))
    (when (and (<= 1 row (length data)) (<= 1 column (length (nth (1- row) data))))
      (setf (nth (1- column) (nth (1- row) data)) value))))

(defun sheet-column-count (sheet)
  (reduce #'max (sheet-data-rows sheet) :key #'length :initial-value 0))

(defun cell-value (text)
  "The value of a cell for arithmetic: a number, or the string; an empty cell counts as 0."
  (cond ((null text) 0)
        ((string= (string-trim " " text) "") 0)
        (t (or (cell-number text) (string-trim " " text)))))

(defun field-range-values (sheet start end current-row current-column)
  "The values of the fields between the references START and END (strings, see `field-parse')."
  (destructuring-bind (srow scol) (field-parse start)
    (destructuring-bind (erow ecol) (field-parse end)
      (let* ((data (sheet-data-rows sheet)) (rows (length data)) (columns (sheet-column-count sheet))
             (rules (sheet-rule-positions sheet))
             (first-row (if (and srow (eq (first srow) :rule))
                            (1+ (nth (1- (second srow)) rules))
                            (resolve-index srow current-row rows)))
             (last-row (if (and erow (eq (first erow) :rule))
                           (nth (1- (second erow)) rules)
                           (resolve-index erow current-row rows)))
             (first-column (resolve-index scol current-column columns))
             (last-column (resolve-index ecol current-column columns)))
        (when (and (null srow) (null erow)) (setf first-row current-row last-row current-row))
        (when (and (null scol) (null ecol)) (setf first-column 1 last-column columns))
        (loop :for r :from (min first-row last-row) :to (max first-row last-row)
              :append (loop :for c :from (min first-column last-column) :to (max first-column last-column)
                            :collect (cell-value (sheet-cell sheet r c))))))))

(defun numbers-only (values)
  (remove-if-not #'realp values))

(defun formula-truth (x) (not (or (null x) (eql x 0) (eql x 0d0))))

(defun formula-eval (tree sheet row column)
  "Evaluate the formula TREE for the field at ROW and COLUMN of SHEET."
  (labels ((ev (node) (formula-eval node sheet row column))
           (num (x) (if (realp x) x (editor-error "Not a number: ~A" x)))
           (vector-args (args) (loop :for a :in args
                                     :append (let ((v (ev a))) (if (listp v) v (list v))))))
    (cond
      ((numberp tree) tree)
      ((eq (first tree) :field)
       (destructuring-bind (r c) (field-parse (second tree))
         (let* ((data (sheet-data-rows sheet))
                (rr (resolve-index r row (length data)))
                (cc (resolve-index c column (sheet-column-count sheet))))
           (cell-value (sheet-cell sheet rr cc)))))
      ((eq (first tree) :range)
       (field-range-values sheet (second tree) (third tree) row column))
      ((eq (first tree) :unary) (- (num (ev (third tree)))))
      ((eq (first tree) :binary)
       (let ((op (second tree)) (a (ev (third tree))) (b (ev (fourth tree))))
         (cond ((string= op "+") (+ (num a) (num b)))
               ((string= op "-") (- (num a) (num b)))
               ((string= op "*") (* (num a) (num b)))
               ((string= op "/") (if (zerop (num b)) (editor-error "Division by zero") (/ (num a) (num b))))
               ((string= op "%") (mod (num a) (num b)))
               ((string= op "^") (expt (num a) (num b)))
               ((member op '("==" "=") :test #'string=) (if (equal a b) 1 (if (and (realp a) (realp b) (= a b)) 1 0)))
               ((string= op "!=") (if (equal a b) 0 1))
               ((string= op "<") (if (< (num a) (num b)) 1 0))
               ((string= op ">") (if (> (num a) (num b)) 1 0))
               ((string= op "<=") (if (<= (num a) (num b)) 1 0))
               ((string= op ">=") (if (>= (num a) (num b)) 1 0))
               ((string= op "&&") (if (and (formula-truth a) (formula-truth b)) 1 0))
               ((string= op "||") (if (or (formula-truth a) (formula-truth b)) 1 0)))))
      ((eq (first tree) :call)
       (let ((name (second tree)) (args (cddr tree)))
         (flet ((scalar (i) (num (ev (nth i args)))))
           (cond
             ((string= name "if") (if (formula-truth (ev (first args))) (ev (second args)) (ev (third args))))
             ((string= name "vsum") (reduce #'+ (numbers-only (vector-args args))))
             ((string= name "vprod") (reduce #'* (numbers-only (vector-args args)) :initial-value 1))
             ((string= name "vcount") (length (remove-if (lambda (v) (and (numberp v) (zerop v) nil)) (vector-args args))))
             ((string= name "vmean") (let ((v (numbers-only (vector-args args))))
                                       (if v (/ (reduce #'+ v) (length v)) (editor-error "No numbers to average"))))
             ((string= name "vmax") (let ((v (numbers-only (vector-args args)))) (if v (reduce #'max v) 0)))
             ((string= name "vmin") (let ((v (numbers-only (vector-args args)))) (if v (reduce #'min v) 0)))
             ((string= name "vmedian") (let* ((v (sort (copy-list (numbers-only (vector-args args))) #'<)) (n (length v)))
                                         (cond ((zerop n) 0) ((oddp n) (nth (floor n 2) v))
                                               (t (/ (+ (nth (1- (floor n 2)) v) (nth (floor n 2) v)) 2)))))
             ((string= name "min") (reduce #'min (mapcar #'num (vector-args args))))
             ((string= name "max") (reduce #'max (mapcar #'num (vector-args args))))
             ((string= name "abs") (abs (scalar 0)))
             ((string= name "sqrt") (sqrt (coerce (scalar 0) 'double-float)))
             ((string= name "round") (if (second args) (let ((m (expt 10 (round (scalar 1))))) (/ (round (* (scalar 0) m)) m)) (round (scalar 0))))
             ((string= name "floor") (floor (scalar 0)))
             ((string= name "ceiling") (ceiling (scalar 0)))
             ((string= name "exp") (exp (coerce (scalar 0) 'double-float)))
             ((string= name "ln") (log (coerce (scalar 0) 'double-float)))
             ((string= name "log10") (log (coerce (scalar 0) 'double-float) 10))
             ((string= name "sin") (sin (coerce (scalar 0) 'double-float)))
             ((string= name "cos") (cos (coerce (scalar 0) 'double-float)))
             ((string= name "tan") (tan (coerce (scalar 0) 'double-float)))
             (t (editor-error "Unknown function ~A" name))))))
      (t (editor-error "Cannot evaluate the formula")))))

;;; Formulas of a #+TBLFM line

(defun tblfm-split (line)
  "The formulas of a #+TBLFM: LINE as a list of strings."
  (let* ((s (string-left-trim " 	" line))
         (colon (position #\: s))
         (text (subseq s (1+ colon))))
    (remove "" (loop :with start := 0
                     :for at := (search "::" text :start2 start)
                     :collect (string-trim " 	" (subseq text start at))
                     :while at :do (setf start (+ at 2)))
            :test #'string=)))

(defun tblfm-line-p (line)
  (line-starts-with-ci-p line "#+tblfm:"))

(defun formula-split-format (rhs)
  "Split the right side RHS of a formula at the last ; into (values expression format-flags)."
  (let ((semi (position #\; rhs :from-end t)))
    (if semi
        (values (subseq rhs 0 semi) (string-trim " " (subseq rhs (1+ semi))))
        (values rhs nil))))

(defun formula-targets (lhs sheet)
  "The (row . column) cells that the left side LHS of a formula assigns: a column, a row, a field or a range."
  (let* ((data (sheet-data-rows sheet)) (rows (length data)) (columns (sheet-column-count sheet))
         (rules (sheet-rule-positions sheet))
         (dots (search ".." lhs)))
    (flet ((computed-rows ()
             ;; rows below the first rule and above the last when there are several rules
             (cond ((null rules) (loop :for r :from 1 :to rows :collect r))
                   ((null (rest rules)) (loop :for r :from (1+ (first rules)) :to rows :collect r))
                   (t (loop :for r :from (1+ (first rules)) :to (car (last rules)) :collect r)))))
      (cond
        (dots
         (let ((range (field-range-cells sheet (subseq lhs 0 dots) (subseq lhs (+ dots 2)))))
           range))
        (t (destructuring-bind (r c) (field-parse lhs)
             (cond ((and (null r) c) (let ((column (resolve-index c 1 columns)))
                                       (mapcar (lambda (row) (cons row column)) (computed-rows))))
                   ((and r (null c)) (let ((row (resolve-index r 1 rows)))
                                       (loop :for column :from 1 :to columns :collect (cons row column))))
                   (t (list (cons (resolve-index r 1 rows) (resolve-index c 1 columns)))))))))))

(defun field-range-cells (sheet start end)
  (destructuring-bind (srow scol) (field-parse start)
    (destructuring-bind (erow ecol) (field-parse end)
      (let* ((rows (length (sheet-data-rows sheet))) (columns (sheet-column-count sheet))
             (r1 (resolve-index srow 1 rows)) (r2 (resolve-index erow 1 rows))
             (c1 (resolve-index scol 1 columns)) (c2 (resolve-index ecol 1 columns)))
        (loop :for r :from (min r1 r2) :to (max r1 r2)
              :append (loop :for c :from (min c1 c2) :to (max c1 c2) :collect (cons r c)))))))

(defun apply-formula (sheet formula)
  "Evaluate FORMULA, a string LHS=RHS[;format], and store the results in SHEET."
  (let ((equals (position #\= formula)))
    (unless equals (editor-error "A formula needs an = sign: ~A" formula))
    (let ((lhs (string-trim " " (subseq formula 0 equals)))
          (rhs (string-trim " " (subseq formula (1+ equals)))))
      (when (and (plusp (length rhs)) (char= (char rhs 0) #\=)) (setf rhs (subseq rhs 1)))
      (multiple-value-bind (expression flags) (formula-split-format rhs)
        (let ((tree (formula-parse (string-trim " " expression)))
              (spec (and flags (find #\% flags) (subseq flags (position #\% flags)))))
          (dolist (target (formula-targets lhs sheet))
            (let ((value (handler-case (formula-eval tree sheet (car target) (cdr target))
                           (error () nil))))
              (sheet-set-cell sheet (car target) (cdr target)
                              (cond ((null value) "#ERROR")
                                    ((realp value) (format-calc-number value spec))
                                    (t (princ-to-string value)))))))))))

(defun table-recalculate-rows (rows formulas)
  "ROWS (see `table-split-line') after applying FORMULAS in order. Rows are padded to the width of the table."
  (let* ((width (table-column-count rows))
         (sheet (make-sheet (mapcar (lambda (row) (if (eq row :rule) row (copy-list (table-pad-row row width)))) rows))))
    (dolist (formula formulas)
      (apply-formula sheet formula))
    (sheet-rows sheet)))

;;; Sums

(defun column-sum-text (cells)
  "The sum of the numbers among CELLS (strings), formatted."
  (format-calc-number (reduce #'+ (remove nil (mapcar #'cell-number cells)))))

;;; Text <-> table

(defun split-separated-line (line separator)
  (loop :with start := 0
        :for at := (position separator line :start start)
        :collect (string-trim " " (subseq line start at))
        :while at :do (setf start (1+ at))))

(defun guess-separator (lines)
  "The separator that LINES use: a tab, else a comma when most lines have one, else blanks (NIL)."
  (cond ((some (lambda (l) (find #\tab l)) lines) #\tab)
        ((and lines (every (lambda (l) (find #\, l)) lines)) #\,)
        ((and lines (every (lambda (l) (find #\; l)) lines)) #\;)
        (t nil)))

(defun lines-to-rows (lines &optional (separator (guess-separator lines)))
  "Text LINES as table rows, split at SEPARATOR (a character) or at blanks when it is NIL. Blank lines are dropped."
  (loop :for line :in lines
        :unless (blank-string-p line)
          :collect (if separator (split-separated-line line separator) (split-words line))))

(defun csv-quote (cell separator)
  (if (or (find separator cell) (find #\" cell) (find #\newline cell))
      (format nil "\"~A\"" (with-output-to-string (out)
                             (loop :for c :across cell :do (when (char= c #\") (write-char #\" out)) (write-char c out))))
      cell))

(defun rows-to-delimited (rows separator)
  "The lines of the table ROWS written with SEPARATOR between the cells; rules are left out."
  (loop :for row :in rows
        :unless (eq row :rule)
          :collect (format nil (concatenate 'string "~{~A~^" (string separator) "~}")
                           (mapcar (lambda (cell) (csv-quote cell separator)) row))))

;;; END pure tables code

(defun table-end-line (start)
  "A point on the last line of the table that begins at START."
  (with-point ((p start))
    (loop :while (and (line-offset p 1) (org-table-at-point-p p)))
    (unless (org-table-at-point-p p) (line-offset p -1))
    (copy-point p :temporary)))

(defun table-formula-lines (end)
  "The #+TBLFM: lines that follow the table ending at END, as a list of strings."
  (with-point ((p end))
    (loop :while (and (line-offset p 1) (tblfm-line-p (line-string p)))
          :collect (line-string p))))

(defun org-table-recalculate (point)
  "Recompute the table at POINT with its #+TBLFM: lines. Return the number of formulas applied."
  (unless (org-table-at-point-p point) (editor-error "Not in a table"))
  (let* ((start (table-first-line point))
         (strings (table-line-strings start))
         (end (table-end-line start))
         (formula-lines (table-formula-lines end))
         (formulas (loop :for line :in formula-lines :append (tblfm-split line))))
    (unless formulas (editor-error "No #+TBLFM: line for this table"))
    (let* ((indent (subseq (first strings) 0 (position #\| (first strings))))
           (rows (table-recalculate-rows (mapcar #'table-split-line strings) formulas))
           (lines (mapcar (lambda (line) (concatenate 'string indent line)) (org-table-format-rows rows))))
      (replace-lines start (length strings) lines)
      (length formulas))))

(defun table-point-above-tblfm (point)
  "When POINT is on a #+TBLFM: line, a point on the last line of the table above it."
  (when (tblfm-line-p (line-string point))
    (let ((p (copy-point point :temporary)))
      (loop :while (and (line-offset p -1) (tblfm-line-p (line-string p))))
      (and (org-table-at-point-p p) p))))

(define-command org-table-recalculate-command () ()
  "Recompute the table at point with its #+TBLFM: line."
  (let ((point (or (table-point-above-tblfm (current-point)) (current-point))))
    (message "~D formula~:P applied" (org-table-recalculate point))))

(defun org-table-recalculate-buffer (buffer)
  "Recompute every table of BUFFER that has a #+TBLFM: line. Return how many."
  (let ((count 0))
    (loop :for i :from 1
          :for lines := (split-text-lines (buffer-text buffer))
          :while (<= i (length lines))
          :do (when (and (tblfm-line-p (nth (1- i) lines)) (> i 1) (org-table-line-string-p (nth (- i 2) lines)))
                (org-table-recalculate (move-to-line (copy-point (buffer-start-point buffer) :temporary) (1- i)))
                (incf count)
                ;; the table may have changed size
                ))
    count))

(defun org-table-line-string-p (string) (table-line-p string))

(define-command org-table-recalculate-buffer-command () ()
  "Recompute all tables with a #+TBLFM: line in the buffer."
  (message "~D table~:P recomputed" (org-table-recalculate-buffer (current-buffer))))

(defun org-table-column-formula-set (point formula)
  "Add or replace the column formula FORMULA (such as $3=$1*$2) in the #+TBLFM: line of the table at POINT."
  (let* ((start (table-first-line point)) (end (table-end-line start))
         (target (subseq formula 0 (position #\= formula)))
         (existing (table-formula-lines end))
         (formulas (loop :for line :in existing :append (tblfm-split line)))
         (formulas (append (remove-if (lambda (f) (string= (string-trim " " (subseq f 0 (position #\= f))) target)) formulas)
                           (list formula)))
         (new (format nil "#+TBLFM: ~{~A~^::~}" formulas)))
    (if existing
        (replace-lines (move-to-line (copy-point end :temporary) (1+ (line-number-at-point end))) (length existing) (list new))
        (progn (line-end end) (insert-string end (format nil "~%~A" new))))))

(define-command org-table-eval-formula () ()
  "Set the formula of the current column (for example $3=$1*$2) and recompute the table."
  (let* ((point (current-point))
         (column (1+ (table-cell-index (line-string point) (point-charpos point))))
         (formula (prompt-for-string (format nil "Formula for column $~D: " column) :initial-value (format nil "$~D=" column))))
    (org-table-column-formula-set point formula)
    (org-table-recalculate point)))

(define-command org-table-sum () ()
  "Show the sum of the numbers in the current column above the cursor (or in the region)."
  (let* ((point (current-point)) (start (table-first-line point))
         (column (table-cell-index (line-string point) (point-charpos point)))
         (rows (mapcar #'table-split-line (table-line-strings start)))
         (row (- (line-number-at-point point) (line-number-at-point start)))
         (cells (loop :for i :from 0 :below row :for r := (nth i rows)
                      :unless (eq r :rule) :collect (or (nth column r) ""))))
    (message "Sum: ~A" (column-sum-text cells))))

(defun org-table-convert-lines (start-line end-line)
  "Turn the lines START-LINE to END-LINE (1-based) of the current buffer into a table."
  (let* ((buffer (current-buffer))
         (lines (loop :for i :from start-line :to end-line :collect (line-string (line-point-in buffer i))))
         (rows (lines-to-rows lines)))
    (replace-lines (line-point-in buffer start-line) (1+ (- end-line start-line)) (org-table-format-rows rows))))

(define-command org-table-convert-region () ()
  "Make a table of the region, or of the current paragraph: cells are split at tabs, commas or blanks."
  (let* ((buffer (current-buffer)) (point (current-point)))
    (if (buffer-mark-p buffer)
        (let ((a (line-number-at-point (region-beginning buffer))) (b (line-number-at-point (region-end buffer))))
          (org-table-convert-lines (min a b) (max a b)))
        (let ((from (line-number-at-point point)) (to (line-number-at-point point)))
          (loop :while (and (> from 1) (not (blank-string-p (line-string (line-point-in buffer (1- from)))))) :do (decf from))
          (loop :while (and (< to (buffer-nlines buffer)) (not (blank-string-p (line-string (line-point-in buffer (1+ to)))))) :do (incf to))
          (org-table-convert-lines from to)))))

(defun org-table-export (point file separator)
  "Write the table at POINT to FILE as text with SEPARATOR between cells. Return the number of rows."
  (let* ((rows (mapcar #'table-split-line (table-line-strings (table-first-line point))))
         (lines (rows-to-delimited rows separator)))
    (with-open-file (out file :direction :output :if-exists :supersede :external-format :utf-8)
      (format out "~{~A~%~}" lines))
    (length lines)))

(define-command org-table-export-command () ()
  "Write the table at point to a CSV file (or .tsv for tabs)."
  (let* ((file (prompt-for-string "Export to file: "))
         (separator (if (uiop:string-suffix-p file ".tsv") #\tab #\,)))
    (message "~D row~:P written" (org-table-export (current-point) file separator))))

(define-command org-table-import () ()
  "Insert the CSV or tab separated file as a table at point."
  (let* ((file (prompt-for-string "Import file: "))
         (lines (uiop:read-file-lines file))
         (separator (if (uiop:string-suffix-p file ".csv") #\, (guess-separator lines)))
         (rows (if (eql separator #\,) (csv-parse (format nil "~{~A~%~}" lines)) (lines-to-rows lines separator))))
    (insert-string (current-point) (format nil "~{~A~%~}" (org-table-format-rows rows)))))

(define-key *org-mode-keymap* "C-c +" 'org-table-sum)
(define-key *org-mode-keymap* "C-c =" 'org-table-eval-formula)
(define-key *org-mode-keymap* "C-c |" 'org-table-convert-region)
(define-command org-ctrl-c-star () ()
  "In a table recompute it from its #+TBLFM: line, elsewhere turn the line into a heading or back."
  (if (org-table-at-point-p (current-point))
      (org-table-recalculate-command)
      (org-toggle-heading)))

(define-key *org-mode-keymap* "C-c *" 'org-ctrl-c-star)
