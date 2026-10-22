#| link : https://orgmode.org/org.html |#

(defpackage :lem-org-mode
  (:use :cl :lem :lem/language-mode)
  (:export :*org-mode-hook*
           :*org-todo-keywords*
           :org-mode
           :org-heading-level
           :org-subtree-bounds
           :org-cycle-todo
           :org-shift-subtree
           :org-move-subtree
           :org-table-format-rows
           :org-table-edit))
(in-package :lem-org-mode)

(defvar *org-todo-keywords* '("TODO" "DONE")
  "Keywords that `org-todo' cycles through. The first one starts a task and the last one finishes it.")

(defun heading-attribute (level)
  (case level
    (1 'document-header1-attribute)
    (2 'document-header2-attribute)
    (3 'document-header3-attribute)
    (4 'document-header4-attribute)
    (5 'document-header5-attribute)
    (t 'document-header6-attribute)))

(defun make-heading-pattern (level)
  "Match a heading with exactly LEVEL stars, colouring a leading todo keyword separately."
  (make-tm-match (format nil "^\\*{~D} (?:(~{~A~^|~})\\b)?.*$" level *org-todo-keywords*)
                 :name (heading-attribute level)
                 :captures (vector nil (make-tm-name 'syntax-keyword-attribute))))

(defun make-tmlanguage-org ()
  (make-tmlanguage
   :patterns (make-tm-patterns
              (make-tm-region "^[ \\t]*#\\+(?:begin|BEGIN)_\\w+"
                              "^[ \\t]*#\\+(?:end|END)_\\w+"
                              :name 'document-code-block-attribute)
              (make-tm-match "^[ \\t]*#\\+\\w+:.*$" :name 'document-metadata-attribute)
              (make-tm-match "^[ \\t]*# .*$" :name 'syntax-comment-attribute)
              (make-tm-match "^[ \\t]*\\|.*$" :name 'document-table-attribute)
              (make-tm-match "^[ \\t]*(?:[-+]|\\d+[.)])[ \\t]+(?:\\[[ X-]\\][ \\t]+)?"
                             :name 'document-list-attribute)
              (make-tm-match "\\[\\[[^]]+\\](?:\\[[^]]+\\])?\\]" :name 'document-link-attribute)
              (make-tm-match "(?<![\\w*])\\*[^*\\s](?:[^*]*[^*\\s])?\\*(?![\\w*])"
                             :name 'document-bold-attribute)
              (make-tm-match "(?<![\\w/])/[^/\\s](?:[^/]*[^/\\s])?/(?![\\w/])"
                             :name 'document-italic-attribute)
              (make-tm-match "(?<![\\w=])=[^=\\s](?:[^=]*[^=\\s])?=(?![\\w=])"
                             :name 'document-inline-code-attribute)
              (make-tm-match "(?<![\\w~])~[^~\\s](?:[^~]*[^~\\s])?~(?![\\w~])"
                             :name 'document-inline-code-attribute)
              (make-tm-match ":(?:[\\w@#%]+:)+[ \\t]*$" :name 'syntax-constant-attribute)
              (make-heading-pattern 1)
              (make-heading-pattern 2)
              (make-heading-pattern 3)
              (make-heading-pattern 4)
              (make-heading-pattern 5)
              (make-heading-pattern 6))))

(defvar *org-syntax-table*
  (let ((table (make-syntax-table :space-chars '(#\space #\tab #\newline)
                                  :paren-pairs '((#\( . #\))
                                                 (#\[ . #\])
                                                 (#\{ . #\}))
                                  :line-comment-string "#")))
    (set-syntax-parser table (make-tmlanguage-org))
    table))

(define-major-mode org-mode language-mode
    (:name "Org"
     :keymap *org-mode-keymap*
     :syntax-table *org-syntax-table*
     :mode-hook *org-mode-hook*)
  (setf (variable-value 'enable-syntax-highlight) t
        (variable-value 'indent-tabs-mode) nil
        (variable-value 'line-comment) "#"
        (variable-value 'insertion-line-comment) "# "
        (variable-value 'fold-region-function) 'fold-region-at-heading))

(define-key *org-mode-keymap* "Tab" 'org-cycle)
(define-key *org-mode-keymap* "C-c C-t" 'org-todo)
(define-key *org-mode-keymap* "Shift-Tab" 'org-table-previous-cell)
(define-key *org-mode-keymap* "M-Left" 'org-meta-left)
(define-key *org-mode-keymap* "M-Right" 'org-meta-right)
(define-key *org-mode-keymap* "M-Up" 'org-meta-up)
(define-key *org-mode-keymap* "M-Down" 'org-meta-down)
(define-key *org-mode-keymap* "M-Shift-Left" 'org-table-delete-column)
(define-key *org-mode-keymap* "M-Shift-Right" 'org-table-insert-column)
(define-key *org-mode-keymap* "M-Shift-Up" 'org-table-delete-row)
(define-key *org-mode-keymap* "M-Shift-Down" 'org-table-insert-row)
(define-key *org-mode-keymap* "C-c C-c" 'org-ctrl-c-ctrl-c)
(define-key *org-mode-keymap* "C-c -" 'org-table-insert-hline)
(define-key *org-mode-keymap* "M-Return" 'org-insert-heading)

(defun org-heading-level (point)
  "Return the number of stars if POINT's line is an Org heading, otherwise NIL."
  (let* ((string (line-string point))
         (stars (position-if-not (lambda (c) (char= c #\*)) string)))
    (when (and stars (plusp stars) (char= (char string stars) #\space))
      stars)))

(defun org-subtree-bounds (point)
  "Return (values start end) of the subtree headed by POINT's line, or NIL when it is not a heading.
END is the start of the next heading of the same or a higher level, or the end of the buffer."
  (let ((level (org-heading-level point))
        (start (line-start (copy-point point :temporary)))
        (end (copy-point point :temporary)))
    (when level
      (loop :while (line-offset end 1)
            :do (let ((next-level (org-heading-level end)))
                  (when (and next-level (<= next-level level))
                    (return-from org-subtree-bounds (values start (line-start end))))))
      (values start (buffer-end end)))))

(defun fold-region-at-heading (point)
  "Return (values start end) for the body of the heading at POINT, or NIL when it has none."
  (multiple-value-bind (start end) (org-subtree-bounds point)
    (when start
      (with-point ((next start))
        (when (and (line-offset next 1) (point< next end))
          (values start end))))))

(define-command org-cycle () ()
  "In a table move to the next cell; on a heading fold or unfold the subtree; otherwise indent the line."
  (cond ((org-table-at-point-p (current-point))
         (org-table-next-cell (current-point)))
        ((org-heading-level (current-point))
         (fold-toggle-at-point (current-point)))
        (t (indent))))

(defun next-todo-keyword (current)
  (if current
      (second (member current *org-todo-keywords* :test #'string=))
      (first *org-todo-keywords*)))

(defun heading-todo-keyword (text)
  "Return the todo keyword that TEXT, the heading line after its stars, starts with."
  (find-if (lambda (keyword)
             (and (uiop:string-prefix-p keyword text)
                  (or (= (length text) (length keyword))
                      (char= (char text (length keyword)) #\space))))
           *org-todo-keywords*))

(defun org-cycle-todo (point)
  "Advance the todo keyword of the heading at POINT: none, TODO, DONE, none."
  (let ((level (org-heading-level point)))
    (unless level
      (editor-error "Not on a heading"))
    (let* ((text (subseq (line-string point) (1+ level)))
           (current (heading-todo-keyword text))
           (next (next-todo-keyword current)))
      (with-point ((start point))
        (character-offset (line-start start) (1+ level))
        (when current
          (with-point ((end start))
            (character-offset end (length current))
            (when (and (null next) (eql (character-at end) #\space))
              (character-offset end 1))
            (delete-between-points start end)))
        (when next
          (insert-string start (if (or current (zerop (length text)))
                                   next
                                   (format nil "~A " next))))))))

(define-command org-todo () ()
  "Cycle the todo keyword of the current heading."
  (org-cycle-todo (current-point)))

(defun subtree-headings (start end)
  "Return (line-offset . level) for every heading line in [START, END), offsets counted from START."
  (with-point ((p start))
    (loop :for offset :from 0
          :for level := (org-heading-level p)
          :when level :collect (cons offset level)
          :while (and (line-offset p 1) (point< p end)))))

(defun org-shift-subtree (point delta)
  "Add DELTA stars (+1 demotes, -1 promotes) to every heading in the subtree at POINT."
  (multiple-value-bind (start end) (org-subtree-bounds point)
    (unless start
      (editor-error "Not on a heading"))
    (let ((headings (subtree-headings start end)))
      (when (and (minusp delta)
                 (some (lambda (heading) (= (cdr heading) 1)) headings))
        (editor-error "Cannot promote: a heading is already at the top level"))
      (dolist (heading headings)
        (with-point ((p start))
          (line-offset p (car heading))
          (if (plusp delta)
              (insert-character p #\*)
              (delete-character p 1)))))))

;;; Tables
;;;
;;; The editing rules are pure functions on lists of rows (a row is a list of cell strings, or
;;; :rule for a horizontal line) so they can be tested without a buffer. `org-table-edit' glues
;;; them to a buffer.

;;; BEGIN pure table code

(defun table-line-p (string)
  "True when STRING, ignoring leading blanks, starts with a vertical bar."
  (let ((i (position-if-not (lambda (c) (member c '(#\space #\tab))) string)))
    (and i (char= (char string i) #\|))))

(defun table-rule-line-p (string)
  (and (table-line-p string)
       (let ((i (position #\| string)))
         (and (< (1+ i) (length string))
              (char= (char string (1+ i)) #\-)))))

(defun table-split-line (string)
  "Return the trimmed cells of the table line STRING, or :rule for a horizontal line."
  (if (table-rule-line-p string)
      :rule
      (let* ((string (string-right-trim '(#\space #\tab) string))
             (cells (loop :with from := (1+ (position #\| string))
                          :for bar := (position #\| string :start from)
                          :collect (string-trim '(#\space #\tab) (subseq string from bar))
                          :while bar
                          :do (setf from (1+ bar)))))
        (if (char= (char string (1- (length string))) #\|)
            (or (butlast cells) (list ""))
            cells))))

(defun table-number-p (string)
  (let ((start (if (and (plusp (length string)) (find (char string 0) "+-")) 1 0)))
    (and (< start (length string))
         (<= (count #\. string) 1)
         (some #'digit-char-p string)
         (every (lambda (c) (or (digit-char-p c) (char= c #\.))) (subseq string start)))))

(defun table-column-count (rows)
  (reduce #'max (remove :rule rows) :key #'length :initial-value 1))

(defun table-pad-row (row count)
  (if (eq row :rule)
      row
      (append row (make-list (max 0 (- count (length row))) :initial-element ""))))

(defun org-table-format-rows (rows)
  "Return the lines of the aligned table ROWS. Columns that are mostly numbers are right-aligned."
  (let* ((count (table-column-count rows))
         (rows (mapcar (lambda (row) (table-pad-row row count)) rows))
         (data (remove :rule rows))
         (widths (loop :for c :below count
                       :collect (reduce #'max data :key (lambda (row) (length (nth c row))) :initial-value 1)))
         (right (loop :for c :below count
                      :collect (let* ((cells (remove "" (mapcar (lambda (row) (nth c row)) data)
                                                     :test #'string=))
                                      (numbers (count-if #'table-number-p cells)))
                                 (and (plusp numbers) (>= (* 2 numbers) (length cells)))))))
    (mapcar (lambda (row)
              (if (eq row :rule)
                  (format nil "|~{~A~^+~}|"
                          (mapcar (lambda (w) (make-string (+ w 2) :initial-element #\-)) widths))
                  (format nil "|~{ ~A |~}"
                          (loop :for cell :in row
                                :for w :in widths
                                :for r :in right
                                :collect (format nil (if r "~V@A" "~VA") w cell)))))
            rows)))

(defun table-new-row (count)
  (make-list count :initial-element ""))

(defun table-insert-at (list index item)
  (append (subseq list 0 index) (list item) (nthcdr index list)))

(defun table-remove-at (list index)
  (append (subseq list 0 index) (nthcdr (1+ index) list)))

(defun table-swap (list i j)
  (let ((copy (copy-list list)))
    (rotatef (nth i copy) (nth j copy))
    copy))

(defun table-map-columns (rows function)
  "Pad every data row to the same width and replace it with (funcall FUNCTION row)."
  (let ((count (table-column-count rows)))
    (mapcar (lambda (row)
              (if (eq row :rule) row (funcall function (table-pad-row row count))))
            rows)))

(defun table-insert-column (rows column)
  "Insert an empty column before COLUMN. Return (values rows row column)."
  (table-map-columns rows (lambda (row) (table-insert-at row column ""))))

(defun table-delete-column (rows column)
  (if (<= (table-column-count rows) 1)
      (editor-error "Cannot delete the only column")
      (table-map-columns rows (lambda (row) (table-remove-at row column)))))

(defun table-move-column (rows column delta)
  (let ((target (+ column delta)))
    (unless (< -1 target (table-column-count rows))
      (editor-error "Cannot move the column further"))
    (table-map-columns rows (lambda (row) (table-swap row column target)))))

(defun table-rule-index (rows row delta)
  "Index of the nearest data row from ROW in direction DELTA, skipping rules, or NIL."
  (loop :for i := (+ row delta) :then (+ i delta)
        :while (< -1 i (length rows))
        :unless (eq (nth i rows) :rule) :return i))

(defun table-edit-rows (rows row column operation)
  "Apply OPERATION (a keyword) to ROWS with the cursor in cell (ROW, COLUMN).
Return (values new-rows new-row new-column)."
  (let ((count (table-column-count rows)))
    (ecase operation
      (:insert-column (values (table-insert-column rows column) row column))
      (:delete-column (values (table-delete-column rows column) row (min column (- count 2))))
      (:move-column-left (values (table-move-column rows column -1) row (1- column)))
      (:move-column-right (values (table-move-column rows column 1) row (1+ column)))
      (:insert-row (values (table-insert-at rows row (table-new-row count)) (1+ row) column))
      (:delete-row (let ((new (table-remove-at rows row)))
                     (when (null new) (editor-error "Cannot delete the only row"))
                     (values new (min row (1- (length new))) column)))
      (:move-row-up (progn (unless (plusp row) (editor-error "Cannot move the row further"))
                           (values (table-swap rows row (1- row)) (1- row) column)))
      (:move-row-down (progn (unless (< (1+ row) (length rows)) (editor-error "Cannot move the row further"))
                             (values (table-swap rows row (1+ row)) (1+ row) column)))
      (:insert-hline (values (table-insert-at rows (1+ row) :rule) row column))
      (:align (values rows row column))
      (:next-cell
       (cond ((< (1+ column) count) (values rows row (1+ column)))
             (t (let ((next (table-rule-index rows row 1)))
                  (if next
                      (values rows next 0)
                      (values (append rows (list (table-new-row count))) (length rows) 0))))))
      (:previous-cell
       (cond ((plusp column) (values rows row (1- column)))
             (t (let ((previous (table-rule-index rows row -1)))
                  (unless previous (editor-error "Already in the first cell"))
                  (values rows previous (1- count)))))))))

;;; END pure table code

(defun org-table-at-point-p (point)
  (table-line-p (line-string point)))

(defun table-first-line (point)
  (let ((p (copy-point point :temporary)))
    (line-start p)
    (loop :while (and (line-offset p -1) (org-table-at-point-p p))
          :finally (unless (org-table-at-point-p p) (line-offset p 1)))
    p))

(defun table-line-strings (start)
  (with-point ((p start))
    (loop :collect (line-string p)
          :while (and (line-offset p 1) (org-table-at-point-p p)))))

(defun table-cell-index (string charpos)
  "Index of the cell of table line STRING that contains CHARPOS."
  (max 0 (1- (count #\| string :end (min charpos (length string))))))

(defun table-cell-charpos (string column)
  "Character offset of the content of cell COLUMN in the aligned line STRING."
  (let ((bar (position #\| string)))
    (dotimes (i column) (setf bar (position #\| string :start (1+ bar))))
    (min (length string) (+ bar 2))))

(defun org-table-edit (point operation)
  "Apply OPERATION (see `table-edit-rows') to the table at POINT, realign it and put point in the resulting cell."
  (unless (org-table-at-point-p point)
    (editor-error "Not in a table"))
  (let* ((start (table-first-line point))
         (strings (table-line-strings start))
         (row (- (line-number-at-point point) (line-number-at-point start)))
         (column (table-cell-index (line-string point) (point-charpos point)))
         (indent (subseq (first strings) 0 (position #\| (first strings)))))
    (multiple-value-bind (rows new-row new-column)
        (table-edit-rows (mapcar #'table-split-line strings) row column operation)
      (let ((lines (mapcar (lambda (line) (concatenate 'string indent line))
                           (org-table-format-rows rows))))
        (with-point ((end start))
          (dotimes (i (1- (length strings))) (line-offset end 1))
          (line-end end)
          (delete-between-points start end)
          (insert-string start (format nil "~{~A~^~%~}" lines)))
        (move-point point start)
        (line-offset point new-row)
        (setf (point-charpos point)
              (table-cell-charpos (line-string point) (min new-column (1- (table-column-count rows)))))
        point))))

(defun org-table-next-cell (point)
  (org-table-edit point :next-cell))

(define-command org-table-previous-cell () ()
  "Move to the previous table cell, realigning the table."
  (org-table-edit (current-point) :previous-cell))

(define-command org-table-align () ()
  "Realign the table at point."
  (org-table-edit (current-point) :align))

(define-command org-table-insert-column () ()
  "Insert an empty column before the current one."
  (org-table-edit (current-point) :insert-column))

(define-command org-table-delete-column () ()
  "Delete the current column."
  (org-table-edit (current-point) :delete-column))

(define-command org-table-insert-row () ()
  "Insert an empty row above the current one."
  (org-table-edit (current-point) :insert-row))

(define-command org-table-delete-row () ()
  "Delete the current row."
  (org-table-edit (current-point) :delete-row))

(define-command org-table-insert-hline () ()
  "Insert a horizontal line below the current row."
  (org-table-edit (current-point) :insert-hline))

(define-command org-ctrl-c-ctrl-c () ()
  "Do the context-appropriate action: realign the table at point."
  (if (org-table-at-point-p (current-point))
      (org-table-edit (current-point) :align)
      (editor-error "Nothing to do here")))

(defmacro define-meta-command (name table-operation heading-form doc)
  `(define-command ,name () ()
     ,doc
     (if (org-table-at-point-p (current-point))
         (org-table-edit (current-point) ,table-operation)
         ,heading-form)))

(define-meta-command org-meta-left :move-column-left (org-shift-subtree (current-point) -1)
  "In a table move the column left, otherwise promote the subtree.")
(define-meta-command org-meta-right :move-column-right (org-shift-subtree (current-point) 1)
  "In a table move the column right, otherwise demote the subtree.")
(define-meta-command org-meta-up :move-row-up (org-move-subtree (current-point) :up)
  "In a table move the row up, otherwise move the subtree up.")
(define-meta-command org-meta-down :move-row-down (org-move-subtree (current-point) :down)
  "In a table move the row down, otherwise move the subtree down.")

(define-command org-promote-subtree () ()
  "Make the current heading and its subtree one level higher."
  (org-shift-subtree (current-point) -1))

(define-command org-demote-subtree () ()
  "Make the current heading and its subtree one level lower."
  (org-shift-subtree (current-point) 1))

(defun previous-sibling-start (point level)
  (with-point ((p point))
    (line-start p)
    (loop :while (line-offset p -1)
          :do (let ((previous-level (org-heading-level p)))
                (when (and previous-level (<= previous-level level))
                  (return (and (= previous-level level)
                               (copy-point p :temporary))))))))

(defun with-trailing-newline (string)
  (if (and (plusp (length string))
           (char= (char string (1- (length string))) #\newline))
      string
      (concatenate 'string string (string #\newline))))

(defun swap-adjacent-regions (start middle end)
  "Swap the text of [START, MIDDLE) and [MIDDLE, END) and return the line count of the former second part."
  (let* ((head (points-to-string start middle))
         (tail (points-to-string middle end))
         (tail-line (with-trailing-newline tail)))
    (delete-between-points start end)
    (insert-string start (concatenate 'string
                                      tail-line
                                      (if (string= tail tail-line)
                                          head
                                          (string-right-trim '(#\newline) head))))
    (count #\newline tail-line)))

(defun org-move-subtree (point direction)
  "Swap the subtree at POINT with its previous (:up) or next (:down) sibling, keeping point on the heading."
  (multiple-value-bind (start end) (org-subtree-bounds point)
    (unless start
      (editor-error "Not on a heading"))
    (let ((level (org-heading-level start)))
      (ecase direction
        (:up
         (let ((previous (previous-sibling-start start level)))
           (unless previous
             (editor-error "Cannot move the subtree up"))
           (swap-adjacent-regions previous start end)
           (move-point point previous)))
        (:down
         (unless (eql (org-heading-level end) level)
           (editor-error "Cannot move the subtree down"))
         (let ((next-end (nth-value 1 (org-subtree-bounds end))))
           (let ((lines (swap-adjacent-regions start end next-end)))
             (move-point point start)
             (line-offset point lines))))))))

(define-command org-move-subtree-up () ()
  "Move the current subtree above the previous sibling."
  (org-move-subtree (current-point) :up))

(define-command org-move-subtree-down () ()
  "Move the current subtree below the next sibling."
  (org-move-subtree (current-point) :down))

(defun enclosing-heading (point)
  (with-point ((p point))
    (line-start p)
    (loop
      (when (org-heading-level p)
        (return (copy-point p :temporary)))
      (unless (line-offset p -1)
        (return nil)))))

(define-command org-insert-heading () ()
  "Insert a heading after the current subtree, at the same level."
  (let ((heading (enclosing-heading (current-point)))
        (point (current-point)))
    (if (null heading)
        (insert-string point "* ")
        (let ((stars (make-string (org-heading-level heading) :initial-element #\*)))
          (move-point point (nth-value 1 (org-subtree-bounds heading)))
          (unless (start-line-p point)
            (insert-character point #\newline))
          (insert-string point (format nil "~A ~%" stars))
          (character-offset point -1)))))

(define-file-type ("org") org-mode)
