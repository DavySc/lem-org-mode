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
           :org-table-edit
           :org-insert-item
           :org-toggle-checkbox
           :org-cycle-priority
           :org-set-tags
           :org-set-planning
           :org-set-property
           :org-babel-execute
           :src-block-bounds
           :*org-agenda-files*
           :*org-agenda-span*
           :*org-deadline-warning-days*
           :*org-babel-languages*
           :*org-sql-default-engine*
           :*org-tags-column*
           :org-agenda-week
           :org-agenda-todo-list
           :org-agenda-tags
           :org-export-html
           :org-export-markdown
           :org-babel-tangle))
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
(define-key *org-mode-keymap* "M-Return" 'org-meta-return)
(define-key *org-mode-keymap* "Shift-Up" 'org-priority-up)
(define-key *org-mode-keymap* "Shift-Down" 'org-priority-down)
(define-key *org-mode-keymap* "C-c C-s" 'org-schedule)
(define-key *org-mode-keymap* "C-c C-d" 'org-deadline)
(define-key *org-mode-keymap* "C-c C-q" 'org-set-tags-command)
(define-key *org-mode-keymap* "C-c C-x p" 'org-set-property-command)
(define-key *org-mode-keymap* "C-c a a" 'org-agenda-week)
(define-key *org-mode-keymap* "C-c a t" 'org-agenda-todo-list)
(define-key *org-mode-keymap* "C-c a m" 'org-agenda-tags)
(define-key *org-mode-keymap* "C-c C-e h" 'org-export-html)
(define-key *org-mode-keymap* "C-c C-e m" 'org-export-markdown)
(define-key *org-mode-keymap* "C-c C-v t" 'org-babel-tangle)

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
  (org-cycle-todo (current-point))
  (org-todo-update-closed (current-point)))

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

(defun org-insert-heading-command ()
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

(define-command org-insert-heading () ()
  "Insert a heading after the current subtree, at the same level."
  (org-insert-heading-command))

(define-command org-meta-return () ()
  "On a list item insert an item after it, otherwise insert a heading after the current subtree."
  (if (org-item-at-point-p (current-point))
      (org-insert-item (current-point))
      (org-insert-heading-command)))

;;; Plain lists and checkboxes
;;;
;;; An item is a line that starts with optional blanks and a bullet (-, +, a * that is indented,
;;; or a number followed by . or )), then a space and an optional [ ], [X] or [-] checkbox.

;;; BEGIN pure list code

(defun list-item-parse (line)
  "Return (values indent bullet checkbox content-start) when LINE is a list item, otherwise NIL.
CHECKBOX is the character inside the brackets, or NIL."
  (let* ((indent (or (position #\space line :test-not #'char=) (length line)))
         (end (length line))
         (bullet-end
           (cond ((>= indent end) nil)
                 ((or (find (char line indent) "-+")
                      (and (char= (char line indent) #\*) (plusp indent)))
                  (1+ indent))
                 ((digit-char-p (char line indent))
                  (let ((digits (position-if-not #'digit-char-p line :start indent)))
                    (and digits (find (char line digits) ".)") (1+ digits)))))))
    (when (and bullet-end (< bullet-end end) (char= (char line bullet-end) #\space))
      (let* ((text (position #\space line :start bullet-end :test-not #'char=))
             (checkbox (and text
                            (<= (+ text 3) end)
                            (char= (char line text) #\[)
                            (find (char line (+ text 1)) " X-")
                            (char= (char line (+ text 2)) #\])
                            (or (= (+ text 3) end) (char= (char line (+ text 3)) #\space))
                            (char line (+ text 1)))))
        (values indent
                (subseq line indent bullet-end)
                checkbox
                (cond (checkbox (min end (+ text 4)))
                      (text text)
                      (t end)))))))

(defun list-toggle-checkbox (line)
  "Toggle the checkbox of the item LINE: add an empty one, check [ ] and [-], clear [X]. NIL when not an item."
  (multiple-value-bind (indent bullet checkbox content) (list-item-parse line)
    (when indent
      (let ((head (+ indent (length bullet))))
        (cond ((null checkbox)
               (concatenate 'string (subseq line 0 head) " [ ] " (subseq line content)))
              (t
               (concatenate 'string (subseq line 0 (position #\[ line :start head))
                            (if (char= checkbox #\X) "[ ]" "[X]")
                            (subseq line (+ 3 (position #\[ line :start head))))))))))

(defun list-next-bullet (bullet)
  "The bullet that follows BULLET: a number is incremented, anything else is kept."
  (if (digit-char-p (char bullet 0))
      (format nil "~D~A" (1+ (parse-integer bullet :junk-allowed t)) (char bullet (1- (length bullet))))
      bullet))

(defun list-new-item (line)
  "Return the text of an empty item that follows the item LINE, with the same indentation."
  (multiple-value-bind (indent bullet checkbox) (list-item-parse line)
    (format nil "~A~A ~A" (make-string indent :initial-element #\space) (list-next-bullet bullet)
            (if checkbox "[ ] " ""))))

(defun list-item-extent (lines)
  "LINES starts with an item. Return how many lines belong to it: the item, its continuations and sub-items."
  (let ((indent (list-item-parse (first lines))))
    (1+ (or (position-if (lambda (line)
                           (or (string= (string-trim " " line) "")
                               (<= (or (position #\space line :test-not #'char=) 0) indent)))
                         (rest lines))
            (length (rest lines))))))

(defun list-renumber (lines)
  "Renumber the ordered items in LINES, a contiguous block of list lines, level by level."
  (let ((counters '()))
    (mapcar (lambda (line)
              (multiple-value-bind (indent bullet checkbox content) (list-item-parse line)
                (declare (ignore checkbox content))
                (cond ((null indent) line)
                      (t
                       (setf counters (remove-if (lambda (entry) (> (car entry) indent)) counters))
                       (if (digit-char-p (char bullet 0))
                           (let ((entry (or (assoc indent counters)
                                            (car (push (cons indent 0) counters)))))
                             (incf (cdr entry))
                             (format nil "~A~D~A~A" (subseq line 0 indent) (cdr entry)
                                     (char bullet (1- (length bullet)))
                                     (subseq line (+ indent (length bullet)))))
                           (progn (setf counters (remove indent counters :key #'car))
                                  line))))))
            lines)))

;;; END pure list code

(defun item-block-start (point)
  "Return a point at the first line of the run of list lines that contains POINT."
  (let ((p (copy-point point :temporary)))
    (line-start p)
    (loop :for q := (copy-point p :temporary)
          :while (and (line-offset q -1) (list-line-p (line-string q)))
          :do (move-point p q))
    p))

(defun list-line-p (string)
  (and (plusp (length (string-trim " " string)))
       (not (org-heading-string-p string))
       (or (list-item-parse string)
           (char= (char string 0) #\space))))

(defun org-heading-string-p (string)
  (let ((stars (position-if-not (lambda (c) (char= c #\*)) string)))
    (and stars (plusp stars) (char= (char string stars) #\space))))

(defun following-lines (point)
  "The strings of POINT's line and every line after it."
  (with-point ((p point))
    (line-start p)
    (loop :collect (line-string p) :while (line-offset p 1))))

(defun replace-lines (start count new-lines)
  "Replace COUNT lines from START by NEW-LINES. Only the span that differs is edited, so points outside it keep their place."
  (with-point ((end start))
    (dotimes (i (1- count)) (line-offset end 1))
    (line-end end)
    (let* ((old (points-to-string start end))
           (new (format nil "~{~A~^~%~}" new-lines))
           (prefix (or (mismatch old new) (length old)))
           (room (- (min (length old) (length new)) prefix))
           (suffix (loop :for i :below room
                         :while (char= (char old (- (length old) i 1)) (char new (- (length new) i 1)))
                         :count t)))
      (unless (string= old new)
        (with-point ((a start) (b start))
          (character-offset a prefix)
          (character-offset b (- (length old) suffix))
          (delete-between-points a b)
          (insert-string a (subseq new prefix (- (length new) suffix))))))))

(defun org-renumber-list (point)
  "Renumber the ordered list around POINT."
  (let* ((start (item-block-start point))
         (lines (following-lines start))
         (count (or (position-if-not #'list-line-p lines) (length lines))))
    (replace-lines start count (list-renumber (subseq lines 0 count)))))

(defun org-item-at-point-p (point)
  (list-item-parse (line-string point)))

(defun org-insert-item (point)
  "Insert an item after the item at POINT, with its sub-items, and renumber. Point moves to the new item."
  (unless (org-item-at-point-p point)
    (editor-error "Not on a list item"))
  (let* ((lines (following-lines point))
         (new (list-new-item (first lines)))
         (extent (list-item-extent lines)))
    (line-start point)
    (dotimes (i (1- extent)) (line-offset point 1))
    (line-end point)
    ;; Compute the target before inserting: whether POINT advances over inserted text depends on
    ;; its kind (a temporary point stays behind, the current point moves on).
    (let ((new-line (1+ (line-number-at-point point))))
      (insert-string point (format nil "~%~A" new))
      (org-renumber-list point)
      (move-to-line point new-line)
      (line-end point))
    point))

(defun org-toggle-checkbox (point)
  (let ((new (list-toggle-checkbox (line-string point))))
    (unless new
      (editor-error "Not on a list item"))
    (replace-lines (line-start (copy-point point :temporary)) 1 (list new))))

(define-command org-toggle-checkbox-command () ()
  "Toggle the checkbox of the current list item."
  (org-toggle-checkbox (current-point)))

;;; Priorities, tags, planning and properties of a heading
;;;
;;; The pure functions work on the heading line or on the "head" of an entry: the heading, an
;;; optional planning line (SCHEDULED, DEADLINE, CLOSED) and an optional :PROPERTIES: drawer.

;;; BEGIN pure heading code

(defun heading-split (line keywords)
  "Return (values level todo priority title tags) of the heading LINE, or NIL when it is not a heading."
  (let ((stars (position-if-not (lambda (c) (char= c #\*)) line)))
    (when (and stars (plusp stars) (char= (char line stars) #\space))
      (let ((rest (string-left-trim " " (subseq line stars)))
            (todo nil) (priority nil) (tags nil))
        (flet ((word-at-start-p (word)
                 (and (>= (length rest) (length word))
                      (string= word rest :end2 (length word))
                      (or (= (length rest) (length word))
                          (char= (char rest (length word)) #\space)))))
          (let ((keyword (find-if #'word-at-start-p keywords)))
            (when keyword
              (setf todo keyword
                    rest (string-left-trim " " (subseq rest (length keyword))))))
          (when (and (>= (length rest) 4)
                     (char= (char rest 0) #\[) (char= (char rest 1) #\#)
                     (upper-case-p (char rest 2)) (char= (char rest 3) #\])
                     (or (= (length rest) 4) (char= (char rest 4) #\space)))
            (setf priority (char rest 2)
                  rest (string-left-trim " " (subseq rest 4)))))
        (let* ((trimmed (string-right-trim " 	" rest))
               (cut (position-if (lambda (c) (member c '(#\space #\tab))) trimmed :from-end t))
               (token (and cut (subseq trimmed (1+ cut)))))
          (when (and token (> (length token) 2)
                     (char= (char token 0) #\:) (char= (char token (1- (length token))) #\:)
                     (notany (lambda (c) (member c '(#\space #\tab))) token)
                     (not (search "::" token)))
            (setf tags (loop :with from := 1
                             :for colon := (position #\: token :start from)
                             :while colon
                             :collect (subseq token from colon)
                             :do (setf from (1+ colon)))
                  rest (string-right-trim " 	" (subseq trimmed 0 cut))))
          (values stars todo priority (if tags rest trimmed) tags))))))

(defvar *org-tags-column* 77
  "Column at which the last character of a heading's tags is placed, when the title leaves room.")

(defun heading-join (level todo priority title tags)
  "The inverse of `heading-split'."
  (let* ((body (format nil "~{~A~^ ~}"
                       (remove nil (list todo
                                         (and priority (format nil "[#~A]" priority))
                                         (and (plusp (length title)) title)))))
         (line (format nil "~A ~A" (make-string level :initial-element #\*) body)))
    (if tags
        (let* ((tag-string (format nil ":~{~A~^:~}:" tags))
               (line (string-right-trim " " line))
               (pad (max 1 (- *org-tags-column* (length line) (length tag-string)))))
          (concatenate 'string line (make-string pad :initial-element #\space) tag-string))
        (if (zerop (length body)) (format nil "~A " (make-string level :initial-element #\*)) line))))

(defun heading-set-tags (line keywords tags)
  (multiple-value-bind (level todo priority title) (heading-split line keywords)
    (unless level (editor-error "Not on a heading"))
    (heading-join level todo priority title tags)))

(defun heading-cycle-priority (line keywords delta)
  "Move the priority of the heading LINE through none, C, B, A (DELTA +1, up) or the reverse (DELTA -1)."
  (multiple-value-bind (level todo priority title tags) (heading-split line keywords)
    (unless level (editor-error "Not on a heading"))
    (let* ((ring '(nil #\C #\B #\A))
           (next (nth (mod (+ (or (position priority ring) 0) delta) (length ring)) ring)))
      (heading-join level todo next title tags))))

(defun parse-tag-input (string)
  "Split STRING such as \"work:home\" or \":work:home:\" or \"work home\" into tags."
  (let ((tags '()) (start 0))
    (loop :for i :from 0 :to (length string)
          :do (when (or (= i (length string)) (find (char string i) ": 	"))
                (when (> i start) (push (subseq string start i) tags))
                (setf start (1+ i))))
    (nreverse tags)))

(defun planning-line-p (line)
  (let ((s (string-left-trim " " line)))
    (some (lambda (keyword)
            (and (>= (length s) (1+ (length keyword)))
                 (string= keyword s :end2 (length keyword))
                 (char= (char s (length keyword)) #\:)))
          '("SCHEDULED" "DEADLINE" "CLOSED"))))

(defun planning-parse (line)
  "Return an alist of (keyword . timestamp) found in the planning LINE."
  (loop :for keyword :in '("CLOSED" "SCHEDULED" "DEADLINE")
        :for at := (search (format nil "~A: " keyword) line)
        :for open := (and at (+ at (length keyword) 2))
        :for close := (and open (< open (length line))
                           (position (if (char= (char line open) #\<) #\> #\]) line :start open))
        :when close :collect (cons keyword (subseq line open (1+ close)))))

(defun planning-format (alist)
  (format nil "~{~A~^ ~}"
          (loop :for keyword :in '("CLOSED" "SCHEDULED" "DEADLINE")
                :for entry := (assoc keyword alist :test #'string=)
                :when entry :collect (format nil "~A: ~A" (car entry) (cdr entry)))))

(defun timestamp-day-name (year month day)
  (nth (nth-value 6 (decode-universal-time (encode-universal-time 0 0 12 day month year 0) 0))
       '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun")))

(defun format-timestamp (year month day &optional active)
  (format nil (if active "<~4,'0D-~2,'0D-~2,'0D ~A>" "[~4,'0D-~2,'0D-~2,'0D ~A]")
          year month day (timestamp-day-name year month day)))

(defun parse-date-input (string year month day)
  "Return (values year month day) for STRING: empty (today, given as YEAR MONTH DAY), YYYY-MM-DD, +Nd or +Nw."
  (let ((string (string-trim " " string)))
    (cond ((string= string "") (values year month day))
          ((and (char= (char string 0) #\+) (> (length string) 2)
                (find (char string (1- (length string))) "dw")
                (parse-integer string :start 1 :end (1- (length string)) :junk-allowed t))
           (let* ((n (parse-integer string :start 1 :end (1- (length string))))
                  (n (if (char= (char string (1- (length string))) #\w) (* 7 n) n)))
             (multiple-value-bind (s mi h d m y)
                 (decode-universal-time (+ (encode-universal-time 0 0 12 day month year 0) (* n 86400)) 0)
               (declare (ignore s mi h))
               (values y m d))))
          ((and (= (length string) 10) (char= (char string 4) #\-) (char= (char string 7) #\-))
           (let ((y (parse-integer string :end 4 :junk-allowed t))
                 (m (parse-integer string :start 5 :end 7 :junk-allowed t))
                 (d (parse-integer string :start 8 :junk-allowed t)))
             (unless (and y m d (<= 1 m 12) (<= 1 d 31))
               (editor-error "Invalid date: ~A" string))
             (values y m d)))
          (t (editor-error "Invalid date: ~A (use YYYY-MM-DD, +3d or +1w)" string)))))

(defun head-length (lines)
  "How many of LINES, which start with a heading, form its head: heading, planning line, properties drawer."
  (let ((count 1))
    (when (and (nth count lines) (planning-line-p (nth count lines)))
      (incf count))
    (when (and (nth count lines) (string-equal (string-trim " " (nth count lines)) ":PROPERTIES:"))
      (let ((end (position-if (lambda (line) (string-equal (string-trim " " line) ":END:"))
                              lines :start count)))
        (when end (setf count (1+ end)))))
    count))

(defun head-split (head)
  "Return (values heading planning drawer) for the HEAD lines."
  (let* ((planning (and (second head) (planning-line-p (second head)) (second head)))
         (drawer (nthcdr (if planning 2 1) head)))
    (values (first head) planning drawer)))

(defun head-set-planning (head keyword stamp)
  "Set (or, when STAMP is NIL, remove) the planning KEYWORD in the HEAD lines."
  (multiple-value-bind (heading planning drawer) (head-split head)
    (let* ((alist (remove keyword (and planning (planning-parse planning)) :key #'car :test #'string=))
           (alist (if stamp (acons keyword stamp alist) alist)))
      (append (list heading)
              (and alist (list (planning-format alist)))
              drawer))))

(defun head-set-property (head name value)
  "Set the property NAME to VALUE in the :PROPERTIES: drawer of the HEAD lines, creating the drawer if needed."
  (multiple-value-bind (heading planning drawer) (head-split head)
    (let* ((prefix (format nil ":~A:" name))
           (new (if (zerop (length value)) prefix (format nil "~A ~A" prefix value)))
           (body (if drawer (butlast (rest drawer)) '()))
           (index (position-if (lambda (line)
                                 (let ((s (string-left-trim " " line)))
                                   (and (>= (length s) (length prefix))
                                        (string-equal prefix s :end2 (length prefix)))))
                               body)))
      (append (list heading)
              (and planning (list planning))
              (list ":PROPERTIES:")
              (if index
                  (append (subseq body 0 index)
                          (list (let ((existing (string-left-trim " " (nth index body))))
                                  (concatenate 'string (subseq existing 0 (length prefix))
                                               (subseq new (length prefix)))))
                          (nthcdr (1+ index) body))
                  (append body (list new)))
              (list ":END:")))))

;;; END pure heading code

(defun today ()
  (multiple-value-bind (s mi h day month year) (decode-universal-time (get-universal-time))
    (declare (ignore s mi h))
    (values year month day)))

(defun heading-point (point)
  "A point on the line of the heading that owns POINT, or an editor error."
  (or (enclosing-heading point)
      (editor-error "Not under a heading")))

(defun org-edit-head (point function)
  "Replace the head (heading, planning line, properties drawer) of the entry at POINT by (FUNCTION head-lines)."
  (let* ((heading (heading-point point))
         (lines (let ((all (following-lines heading)))
                  (subseq all 0 (or (position-if #'org-heading-string-p all :start 1) (length all)))))
         (count (head-length lines)))
    (replace-lines heading count (funcall function (subseq lines 0 count)))))

(defun org-edit-heading-line (point function)
  "Replace the text of the heading line that owns POINT by (FUNCTION line)."
  (let ((heading (heading-point point)))
    (replace-lines heading 1 (list (funcall function (line-string heading))))))

(defun org-set-tags (point tags)
  (org-edit-heading-line point (lambda (line) (heading-set-tags line *org-todo-keywords* tags))))

(defun org-cycle-priority (point delta)
  (org-edit-heading-line point (lambda (line) (heading-cycle-priority line *org-todo-keywords* delta))))

(defun org-set-planning (point keyword stamp)
  (org-edit-head point (lambda (head) (head-set-planning head keyword stamp))))

(defun org-set-property (point name value)
  (org-edit-head point (lambda (head) (head-set-property head name value))))

(defun org-todo-update-closed (point)
  "Add a CLOSED timestamp when the entry at POINT is in its last todo state, and remove it otherwise."
  (let* ((heading (heading-point point))
         (done (equal (nth-value 1 (heading-split (line-string heading) *org-todo-keywords*))
                      (car (last *org-todo-keywords*)))))
    (org-set-planning point "CLOSED" (and done (multiple-value-call #'format-timestamp (today))))))

(define-command org-set-tags-command () ()
  "Set the tags of the current heading. Enter them as work:home or :work:home:."
  (let ((heading (heading-point (current-point))))
    (multiple-value-bind (level todo priority title tags)
        (heading-split (line-string heading) *org-todo-keywords*)
      (declare (ignore todo priority title))
      (unless level (editor-error "Not on a heading"))
      (org-set-tags (current-point)
                    (parse-tag-input (prompt-for-string "Tags: "
                                                        :initial-value (format nil "~{~A~^:~}" tags)))))))

(define-command org-priority-up () ()
  "Raise the priority of the current heading: none, C, B, A, none."
  (org-cycle-priority (current-point) 1))

(define-command org-priority-down () ()
  "Lower the priority of the current heading: none, A, B, C, none."
  (org-cycle-priority (current-point) -1))

(defun prompt-for-planning (keyword)
  (multiple-value-bind (year month day) (today)
    (multiple-value-bind (y m d)
        (parse-date-input (prompt-for-string (format nil "~A date (YYYY-MM-DD, +3d, +1w; empty for today): "
                                                    (string-capitalize keyword)))
                          year month day)
      (org-set-planning (current-point) keyword (format-timestamp y m d (string/= keyword "CLOSED"))))))

(define-command org-schedule () ()
  "Set the SCHEDULED date of the current entry."
  (prompt-for-planning "SCHEDULED"))

(define-command org-deadline () ()
  "Set the DEADLINE date of the current entry."
  (prompt-for-planning "DEADLINE"))

(define-command org-set-property-command () ()
  "Set a property in the :PROPERTIES: drawer of the current entry."
  (let ((name (prompt-for-string "Property: ")))
    (org-set-property (current-point) name (prompt-for-string (format nil "~A value: " name)))))

;;; Babel: run SQL source blocks
;;;
;;;   #+begin_src sql :engine postgres :db mydb :host localhost :user me
;;;   select 1;
;;;   #+end_src
;;;
;;; C-c C-c inside the block runs it and puts the result table under a #+RESULTS: line.

;;; BEGIN pure babel code

(defun src-begin-line-p (line)
  (let ((s (string-left-trim " 	" line)))
    (and (>= (length s) 11) (string-equal "#+begin_src" s :end2 11))))

(defun src-end-line-p (line)
  (let ((s (string-left-trim " 	" line)))
    (and (>= (length s) 9) (string-equal "#+end_src" s :end2 9))))

(defun split-words (string)
  "Split STRING at blanks. A double-quoted part stays in one word, quotes included."
  (let ((words '()) (start nil) (quoted nil))
    (loop :for i :from 0 :to (length string)
          :for c := (and (< i (length string)) (char string i))
          :for blank := (or (null c) (and (not quoted) (member c '(#\space #\tab))))
          :do (when (eql c #\") (setf quoted (not quoted)))
              (cond ((and blank start) (push (subseq string start i) words) (setf start nil))
                    ((and (not blank) (not start)) (setf start i))))
    (nreverse words)))

(defun src-header (line)
  "Return (values language args) for a #+begin_src LINE. ARGS is an alist of (name . value), names without the colon."
  (let* ((words (split-words line))
         (rest (cddr words))
         (args '()))
    (loop :while rest
          :do (let ((word (first rest)))
                (if (and (> (length word) 1) (char= (char word 0) #\:))
                    (let ((value (and (second rest) (not (char= (char (second rest) 0) #\:))
                                      (second rest))))
                      (push (cons (string-downcase (subseq word 1)) (or value "t")) args)
                      (setf rest (nthcdr (if value 2 1) rest)))
                    (setf rest (rest rest)))))
    (values (second words) (nreverse args))))

(defun csv-parse (text)
  "Parse CSV TEXT, with quoted fields, into a list of rows of strings."
  (let ((rows '()) (row '()) (field (make-string-output-stream)) (quoted nil) (started nil) (i 0)
        (n (length text)))
    (flet ((end-field () (push (get-output-stream-string field) row) (setf started nil))
           (end-row () (push (nreverse row) rows) (setf row '())))
      (loop :while (< i n)
            :do (let ((c (char text i)))
                  (cond (quoted
                         (cond ((and (char= c #\") (< (1+ i) n) (char= (char text (1+ i)) #\"))
                                (write-char #\" field) (incf i))
                               ((char= c #\") (setf quoted nil))
                               (t (write-char c field))))
                        ((char= c #\") (setf quoted t started t))
                        ((char= c #\,) (end-field) (setf started t))
                        ((char= c #\return))
                        ((char= c #\newline) (when (or started row) (end-field) (end-row)))
                        (t (write-char c field) (setf started t))))
               (incf i))
      (when (or started row) (end-field) (end-row)))
    (nreverse rows)))

(defun delimited-parse (text delimiter)
  "Split the lines of TEXT at DELIMITER, dropping blank lines, rulers and \"(n rows affected)\" footers."
  (let ((rows '()))
    (loop :with start := 0
          :for end := (or (position #\newline text :start start) (length text))
          :do (let ((line (string-trim '(#\space #\tab #\return) (subseq text start end))))
                (unless (or (string= line "")
                            (every (lambda (c) (or (char= c #\-) (char= c delimiter) (char= c #\space))) line)
                            (and (char= (char line 0) #\() (search "affected)" line)))
                  (push (loop :with from := 0
                              :for bar := (position delimiter line :start from)
                              :collect (string-trim " " (subseq line from bar))
                              :while bar :do (setf from (1+ bar)))
                        rows)))
          :while (< end (length text))
          :do (setf start (1+ end)))
    (nreverse rows)))

(defun sql-statements-per-line (sql)
  "Put every ;-terminated statement of SQL on one line, as isql expects."
  (format nil "~{~A~%~}"
          (loop :with flat := (substitute #\space #\newline sql)
                :with start := 0
                :for semi := (position #\; flat :start start)
                :for piece := (string-trim " " (subseq flat start semi))
                :when (plusp (length piece)) :collect (format nil "~A;" piece)
                :while semi :do (setf start (1+ semi)))))

(defvar *org-sql-default-engine* "postgres")

(defun sql-engine-kind (name)
  (cond ((member name '("postgres" "postgresql" "psql") :test #'string-equal) :psql)
        ((member name '("mssql" "sqlserver" "sqlcmd") :test #'string-equal) :sqlcmd)
        ((member name '("db2i" "ibmi" "odbc" "isql") :test #'string-equal) :isql)
        (t (editor-error "Unknown SQL engine ~A (use postgres, mssql or db2i)" name))))

(defun sql-command (kind args)
  "Return (values argv parser) to run SQL of engine KIND with the header ARGS (an alist)."
  (flet ((arg (name) (cdr (assoc name args :test #'string=))))
    (ecase kind
      (:psql
       (values (append (list "psql" "-X" "-q" "--csv")
                       (and (arg "host") (list "-h" (arg "host")))
                       (and (arg "port") (list "-p" (arg "port")))
                       (and (arg "user") (list "-U" (arg "user")))
                       (and (arg "db") (list "-d" (arg "db"))))
               (lambda (output) (csv-parse output))))
      (:sqlcmd
       (values (append (list "sqlcmd" "-W" "-s" "|")
                       (and (arg "host") (list "-S" (if (arg "port")
                                                         (format nil "~A,~A" (arg "host") (arg "port"))
                                                         (arg "host"))))
                       (and (arg "db") (list "-d" (arg "db")))
                       (if (arg "user")
                           (append (list "-U" (arg "user"))
                                   (and (arg "password") (list "-P" (arg "password"))))
                           (list "-E")))
               (lambda (output) (delimited-parse output #\|))))
      (:isql
       (values (append (list "isql" (or (arg "db") (editor-error "The db2i engine needs :db <ODBC data source>")))
                       (and (arg "user") (list (arg "user")))
                       (and (arg "user") (arg "password") (list (arg "password")))
                       (list "-b" "-c" "-d|"))
               (lambda (output) (delimited-parse output #\|)))))))

(defun results-lines (rows)
  "The #+RESULTS: block for ROWS, with a rule under the header row, as lines of text."
  (cons "#+RESULTS:"
        (if (null rows)
            (list ": (no rows)")
            (org-table-format-rows (if (cdr rows)
                                       (list* (first rows) :rule (rest rows))
                                       rows)))))

(defun results-region-length (lines)
  "LINES follow a #+end_src line. Return how many of them (blanks, #+RESULTS: and its output) form an old result."
  (let* ((blanks (or (position-if (lambda (line) (string/= (string-trim " " line) "")) lines)
                     (length lines)))
         (header (nth blanks lines)))
    (if (and header (>= (length header) 10) (string-equal "#+RESULTS:" header :end2 10))
        (+ blanks 1
           (or (position-if-not (lambda (line)
                                  (or (table-line-p line)
                                      (and (>= (length line) 2) (string= ": " line :end2 2))))
                                (nthcdr (1+ blanks) lines))
               (length (nthcdr (1+ blanks) lines))))
        0)))

;;; END pure babel code

(defun src-block-bounds (point)
  "Return (values begin end) points on the #+begin_src and #+end_src lines around POINT, or NIL."
  (let ((begin (copy-point point :temporary))
        (end (copy-point point :temporary)))
    (line-start begin)
    (line-start end)
    (when (src-end-line-p (line-string begin))
      (line-offset begin -1))
    (loop :until (src-begin-line-p (line-string begin))
          :do (when (or (src-end-line-p (line-string begin)) (not (line-offset begin -1)))
                (return-from src-block-bounds nil)))
    (loop :until (src-end-line-p (line-string end))
          :do (unless (line-offset end 1)
                (return-from src-block-bounds nil)))
    (values begin end)))

(defun run-sql (kind args sql)
  "Run SQL with the command for engine KIND and return the output rows."
  (multiple-value-bind (argv parser) (sql-command kind args)
    (multiple-value-bind (output error-output status)
        (handler-case
            (uiop:run-program argv
                              :input (make-string-input-stream (if (eq kind :isql) (sql-statements-per-line sql) sql))
                              :output :string :error-output :string :ignore-error-status t)
          (error (e) (editor-error "Cannot run ~A: ~A" (first argv) e)))
      (unless (zerop status)
        (editor-error "~A failed: ~A" (first argv)
                      (let ((message (string-trim '(#\space #\newline #\return) (if (plusp (length error-output)) error-output output))))
                        (subseq message 0 (position #\newline message)))))
      (funcall parser output))))

(defun insert-results (end lines)
  "Write the result LINES (starting with #+RESULTS:) under the #+end_src line at END, replacing an older result."
  (let ((after (let ((p (copy-point end :temporary))) (and (line-offset p 1) p)))
        (new (cons "" lines)))
    (let ((old (if after (results-region-length (following-lines after)) 0)))
      (if (plusp old)
          (replace-lines after old new)
          (progn (line-end end)
                 (insert-string end (format nil "~%~{~A~^~%~}" new)))))))

(defun org-babel-execute (point)
  "Run the source block around POINT and write its result under it."
  (multiple-value-bind (begin end) (src-block-bounds point)
    (unless begin
      (editor-error "Not in a source block"))
    (multiple-value-bind (language args) (src-header (line-string begin))
      (unless language
        (editor-error "The source block has no language"))
      (let* ((body (with-point ((body-start begin))
                     (line-offset body-start 1)
                     (if (point< body-start end)
                         (points-to-string body-start (line-start (copy-point end :temporary)))
                         "")))
             (lines (babel-result-lines language args body)))
        (if lines
            (insert-results end lines)
            (message "Done (results silenced)"))))))

(define-command org-ctrl-c-ctrl-c () ()
  "Do the context-appropriate action: realign a table, toggle a checkbox, or run a source block."
  (let ((point (current-point)))
    (cond ((org-table-at-point-p point) (org-table-edit point :align))
          ((org-item-at-point-p point) (org-toggle-checkbox point))
          ((src-block-bounds point) (org-babel-execute point))
          (t (editor-error "Nothing to do here")))))

(define-file-type ("org") org-mode)
