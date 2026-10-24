;;; Dynamic blocks:  #+BEGIN: clocktable :scope file :maxlevel 2 :block thisweek  ...  #+END:
;;;
;;; C-c C-c on the #+BEGIN: line (or C-c C-x C-u) rewrites the block. The clock table summarizes the CLOCK lines
;;; of the entries; the column view shows chosen properties of every entry as a table. Add your own with
;;; `*org-dblock-writers*'.

(in-package :lem-org-mode)

(defvar *org-dblock-writers* '()
  "Alist of (NAME . FUNCTION) for dynamic blocks other than clocktable and columnview. FUNCTION is called with the
parameters of the block (an alist of (name . value-string)) and the entries of the buffer, and returns the lines
to put inside the block.")

;;; BEGIN pure dblocks code

(defun dblock-begin-line-p (line)
  (let ((s (string-left-trim " 	" line)))
    (and (>= (length s) 8) (string-equal "#+BEGIN:" s :end2 8))))

(defun dblock-end-line-p (line)
  (let ((s (string-left-trim " 	" line)))
    (and (>= (length s) 6) (string-equal "#+END:" s :end2 6))))

(defun dblock-parse-begin (line)
  "Return (values name params) for the #+BEGIN: LINE."
  (let* ((words (split-words (subseq (string-left-trim " 	" line) 8)))
         (name (first words)))
    (values (and name (string-downcase name)) (header-args-from-words (rest words)))))

(defun dblock-param (params name &optional default)
  (let ((value (cdr (assoc name params :test #'string=))))
    (if (and value (not (string= value "t")) (plusp (length value)))
        (string-trim "\"" value)
        (or (and value (string= value "t") "t") default))))

;;; Clock table

(defun absolute-minutes (ts)
  (+ (* 1440 (timestamp-day ts)) (or (timestamp-minutes ts) 0)))

(defun clock-minutes-clipped (clock from to)
  "The minutes of CLOCK = (start end minutes) that lie between the absolute minutes FROM and TO (either may be NIL)."
  (let ((start (absolute-minutes (first clock))) (end (absolute-minutes (second clock))))
    (max 0 (- (if to (min end to) end) (if from (max start from) start)))))

(defun week-start-day (day)
  (- day (day-weekday day)))

(defun clocktable-range (block today)
  "The absolute-minute range (values from to) for the :block value BLOCK, relative to the day number TODAY."
  (flet ((days (first count) (values (* 1440 first) (* 1440 (+ first count)))))
    (multiple-value-bind (year month) (day-date today)
      (cond
        ((null block) (values nil nil))
        ((string-equal block "today") (days today 1))
        ((string-equal block "yesterday") (days (1- today) 1))
        ((string-equal block "thisweek") (days (week-start-day today) 7))
        ((string-equal block "lastweek") (days (- (week-start-day today) 7) 7))
        ((string-equal block "thismonth") (days (date-day year month 1) (days-in-month year month)))
        ((string-equal block "lastmonth")
         (multiple-value-bind (y m) (date-shift year month 1 -1 :m) (days (date-day y m 1) (days-in-month y m))))
        ((string-equal block "thisyear") (days (date-day year 1 1) (if (leap-year-p year) 366 365)))
        ((string-equal block "lastyear") (days (date-day (1- year) 1 1) (if (leap-year-p (1- year)) 366 365)))
        ((and (= (length block) 4) (every #'digit-char-p block))
         (let ((y (parse-integer block))) (days (date-day y 1 1) (if (leap-year-p y) 366 365))))
        ((and (= (length block) 7) (char= (char block 4) #\-) (not (char= (char block 5) #\W)))
         (let ((y (parse-integer block :end 4)) (m (parse-integer block :start 5)))
           (days (date-day y m 1) (days-in-month y m))))
        ((and (= (length block) 8) (char= (char block 5) #\W))
         (let* ((y (parse-integer block :end 4)) (w (parse-integer block :start 6))
                (jan4 (date-day y 1 4)) (first (+ (week-start-day jan4) (* 7 (1- w)))))
           (days first 7)))
        ((= (length block) 10) (let ((d (timestamp-parse (format nil "<~A>" block)))) (if d (days (timestamp-day d) 1) (values nil nil))))
        (t (values nil nil))))))

(defun clocktable-bounds (params today)
  "Return (values from to) absolute minutes from :block, :tstart and :tend."
  (let ((block (dblock-param params "block")))
    (if block
        (clocktable-range block today)
        (let* ((start (dblock-param params "tstart")) (end (dblock-param params "tend"))
               (start-ts (and start (timestamp-parse (if (find (char start 0) "<[") start (format nil "<~A>" start)))))
               (end-ts (and end (timestamp-parse (if (find (char end 0) "<[") end (format nil "<~A>" end))))))
          (values (and start-ts (absolute-minutes start-ts))
                  (and end-ts (absolute-minutes end-ts)))))))

(defun entry-total-clock (entries entry from to)
  "Minutes clocked on ENTRY and its descendants within [FROM, TO)."
  (flet ((own (e) (reduce #'+ (getf e :clocks) :key (lambda (c) (clock-minutes-clipped c from to)))))
    (+ (own entry) (reduce #'+ (entry-descendants entries entry) :key #'own))))

(defun clocktable-rows (entries &key (maxlevel 100) from to scope-entry tags)
  "Rows (level title minutes entry) for the clock table: the entries (up to MAXLEVEL, under SCOPE-ENTRY when given)
with time in the range. TAGS restricts to entries with one of those tags."
  (let ((pool (if scope-entry (entry-descendants entries scope-entry) entries)))
    (loop :for entry :in pool
          :for minutes := (entry-total-clock entries entry from to)
          :when (and (plusp minutes) (<= (getf entry :level) maxlevel)
                     (or (null tags) (some (lambda (tag) (member tag (getf entry :all-tags) :test #'string-equal)) tags)))
            :collect (list (getf entry :level) (getf entry :title) minutes entry))))

(defun clocktable-total (entries rows from to)
  "The total clocked time of the top-most entries among ROWS."
  (declare (ignore entries from to))
  (let ((total 0) (covered-until nil))
    (dolist (row rows total)
      (let ((entry (fourth row)))
        (unless (and covered-until (equal (getf entry :source) (car covered-until))
                     (< (getf entry :line) (cdr covered-until)))
          (incf total (third row))
          (setf covered-until (cons (getf entry :source) (getf entry :subtree-end))))))))

(defun clocktable-lines (rows total &key (link nil) (minlevel 1))
  "The text lines of a clock table for ROWS and the TOTAL minutes."
  (let* ((cells (mapcar (lambda (row)
                          (destructuring-bind (level title minutes entry) row
                            (list (concatenate 'string
                                               (if (> level minlevel)
                                                   (format nil "~{~A~}  " (loop :repeat (- level minlevel) :collect "\\_"))
                                                   "")
                                               (if link
                                                   (format nil "[[file:~A::*~A][~A]]" (getf entry :source) title title)
                                                   title))
                                  (string-left-trim " " (format-clock-duration minutes)))))
                        rows)))
    (org-table-format-rows
     (append (list (list "Headline" "Time") :rule
                   (list "*Total time*" (format nil "*~A*" (string-left-trim " " (format-clock-duration total)))))
             (and cells (cons :rule cells))))))

(defun clocktable-caption (time)
  (format nil "#+CAPTION: Clock summary at ~A" (format-clock-stamp time)))

;;; Column view

(defun columns-parse (spec)
  "Parse a COLUMNS format such as \"%25ITEM %TODO %Effort(Est){:}\" into ((property title summary width) ...)."
  (let ((columns '()))
    (dolist (word (split-words spec) (nreverse columns))
      (when (and (> (length word) 1) (char= (char word 0) #\%))
        (let* ((i 1) (n (length word)))
          (let ((width-end (or (position-if-not #'digit-char-p word :start i) n)))
            (let* ((width (and (> width-end i) (parse-integer word :start i :end width-end)))
                   (name-end (or (position-if (lambda (c) (find c "({")) word :start width-end) n))
                   (name (subseq word width-end name-end))
                   (title name) (summary nil) (pos name-end))
              (when (and (< pos n) (char= (char word pos) #\())
                (let ((close (position #\) word :start pos)))
                  (setf title (subseq word (1+ pos) close) pos (1+ close))))
              (when (and (< pos n) (char= (char word pos) #\{))
                (let ((close (position #\} word :start pos)))
                  (setf summary (subseq word (1+ pos) close))))
              (push (list name title summary width) columns))))))))

(defun entry-column-value (entry property)
  (cond ((string-equal property "ITEM") (getf entry :title))
        ((string-equal property "TODO") (or (getf entry :todo) ""))
        ((string-equal property "PRIORITY") (if (getf entry :priority) (string (getf entry :priority)) ""))
        ((string-equal property "TAGS") (if (getf entry :tags) (format nil ":~{~A~^:~}:" (getf entry :tags)) ""))
        ((string-equal property "CATEGORY") (getf entry :category))
        (t (or (cdr (assoc (string-upcase property) (getf entry :props) :test #'string=)) ""))))

(defun column-summary (kind values)
  "VALUES (strings) summarized by KIND: + sum, : time sum (H:MM), max, min, mean."
  (let ((numbers (remove nil (mapcar (lambda (v) (if (string= kind ":") (effort-minutes v) (cell-number v))) values))))
    (cond ((null numbers) "")
          ((string= kind "+") (format-calc-number (reduce #'+ numbers)))
          ((string= kind ":") (string-left-trim " " (format-clock-duration (reduce #'+ numbers))))
          ((string= kind "max") (format-calc-number (reduce #'max numbers)))
          ((string= kind "min") (format-calc-number (reduce #'min numbers)))
          ((string= kind "mean") (format-calc-number (/ (reduce #'+ numbers) (length numbers))))
          (t ""))))

(defun columnview-rows (entries columns)
  "Table rows for ENTRIES and COLUMNS; columns with a summary type show the summary over the subtree when the entry
has children with values."
  (mapcar (lambda (entry)
            (mapcar (lambda (column)
                      (destructuring-bind (property title summary width) column
                        (declare (ignore title width))
                        (let ((own (entry-column-value entry property)))
                          (if summary
                              (let ((children (remove-if-not (lambda (e) (= (getf e :level) (1+ (getf entry :level))))
                                                             (entry-descendants entries entry))))
                                (if children
                                    (column-summary summary (mapcar (lambda (c) (let ((v (entry-column-value c property)))
                                                                                  (if (string= v "") (first (columnview-summary-of entries c property summary)) v)))
                                                                    children))
                                    own))
                              own))))
                    columns))
          entries))

(defun columnview-summary-of (entries entry property summary)
  "A one element list: the summary of the children of ENTRY (for nested summaries) or its own value."
  (let ((children (remove-if-not (lambda (e) (= (getf e :level) (1+ (getf entry :level)))) (entry-descendants entries entry))))
    (list (if children
              (column-summary summary (mapcar (lambda (c) (first (columnview-summary-of entries c property summary))) children))
              (entry-column-value entry property)))))

(defun columnview-lines (entries columns)
  (org-table-format-rows
   (append (list (mapcar #'second columns) :rule)
           (columnview-rows entries columns))))

;;; CLOCK line durations

(defun clock-line-recompute (line)
  "LINE, a finished CLOCK line, with its => duration made right. Other lines are returned unchanged."
  (let ((clock (parse-clock-line line)))
    (if clock
        (let* ((stamps (timestamps-in-text line))
               (end (third (second stamps))))
          (concatenate 'string (subseq line 0 end) " => " (format-clock-duration (third clock))))
        line)))

;;; END pure dblocks code

(defun org-dblock-line-p (point)
  (dblock-begin-line-p (line-string point)))

(defun dblock-extent (point)
  "Return (values begin-line end-line) numbers (1-based) of the dynamic block whose #+BEGIN: is on POINT's line."
  (let* ((buffer (point-buffer point)) (lines (split-text-lines (buffer-text buffer)))
         (begin (1- (line-number-at-point point)))
         (end (position-if #'dblock-end-line-p lines :start (1+ begin))))
    (unless end (editor-error "This dynamic block has no #+END: line"))
    (values (1+ begin) (1+ end))))

(defun dblock-lines-for (name params buffer begin-line)
  "The lines a dynamic block NAME produces now."
  (let* ((entries (agenda-entries nil buffer))
         (now (now)))
    (cond
      ((string= name "clocktable")
       (let* ((scope (dblock-param params "scope" "file"))
              (all (if (string= scope "agenda") (agenda-entries) entries))
              (scope-entry (when (string= scope "subtree")
                             (let ((heading (loop :for e :in (reverse entries) :when (< (getf e :line) begin-line) :return e)))
                               (unless heading (editor-error "The clocktable is not under a heading"))
                               heading)))
              (maxlevel (let ((m (dblock-param params "maxlevel"))) (if m (parse-integer m :junk-allowed t) 100)))
              (tags (let ((tags (dblock-param params "tags"))) (and tags (not (string= tags "t")) (parse-tag-input tags)))))
         (multiple-value-bind (from to) (clocktable-bounds params (date-day (first now) (second now) (third now)))
           (let* ((rows (clocktable-rows all :maxlevel (or maxlevel 100) :from from :to to :scope-entry scope-entry :tags tags))
                  (total (clocktable-total all rows from to)))
             (cons (clocktable-caption now)
                   (clocktable-lines rows total :link (dblock-param params "link")
                                                :minlevel (if scope-entry (1+ (getf scope-entry :level)) 1)))))))
      ((string= name "columnview")
       (let* ((format-string (or (dblock-param params "format")
                                 (first (file-keyword-values (split-text-lines (buffer-text buffer)) "COLUMNS"))
                                 "%ITEM %TODO %PRIORITY %TAGS"))
              (columns (columns-parse format-string))
              (maxlevel (let ((m (dblock-param params "maxlevel"))) (and m (parse-integer m :junk-allowed t))))
              (shown (if maxlevel (remove-if (lambda (e) (> (getf e :level) maxlevel)) entries) entries)))
         (unless columns (editor-error "No column format (#+COLUMNS: or :format)"))
         (let ((rows (columnview-rows entries columns)))
           (org-table-format-rows
            (append (list (mapcar #'second columns) :rule)
                    (loop :for entry :in entries :for row :in rows :when (member entry shown) :collect row))))))
      (t (let ((writer (cdr (assoc name *org-dblock-writers* :test #'string=))))
           (unless writer (editor-error "No dynamic block writer for ~A" name))
           (funcall writer params entries))))))

(defun org-dblock-update (point)
  "Rewrite the dynamic block whose #+BEGIN: line is POINT's line."
  (multiple-value-bind (name params) (dblock-parse-begin (line-string point))
    (multiple-value-bind (begin-line end-line) (dblock-extent point)
      (let* ((buffer (point-buffer point))
             (new (dblock-lines-for name params buffer begin-line))
             (inside (- end-line begin-line 1)))
        (replace-line-span (line-point-in buffer begin-line) 1 inside new)
        (length new)))))

(define-command org-dblock-update-command () ()
  "Update the dynamic block at point."
  (let ((point (current-point)))
    (unless (org-dblock-line-p point)
      (let ((p (copy-point point :temporary)))
        (loop :until (or (org-dblock-line-p p) (not (line-offset p -1))))
        (unless (org-dblock-line-p p) (editor-error "Not in a dynamic block"))
        (setf point p)))
    (org-dblock-update point)))

(defun org-update-all-dblocks (buffer)
  "Update every dynamic block of BUFFER from the bottom up. Return how many."
  (let ((count 0))
    (loop :for i :from (buffer-nlines buffer) :downto 1
          :do (let ((p (line-point-in buffer i)))
                (when (org-dblock-line-p p)
                  (org-dblock-update p)
                  (incf count))))
    count))

(define-command org-update-all-dblocks-command () ()
  "Update all dynamic blocks and recompute all tables of the buffer."
  (message "~D dynamic block~:P updated" (org-update-all-dblocks (current-buffer))))

(define-command org-clock-report () ()
  "Insert a clock table for this file and fill it in."
  (let ((point (current-point)))
    (line-start point)
    (insert-string point (format nil "#+BEGIN: clocktable :scope file :maxlevel 3~%#+END:~%"))
    (org-dblock-update (line-point-in (current-buffer) (line-number-at-point point)))))

(define-command org-clock-goto () ()
  "Jump to the entry with the running clock."
  (let ((running (org-clock-running (current-point))))
    (unless running (editor-error "No running clock"))
    (move-point (current-point) running)))

(defun org-clock-update-line (point)
  "Make the => duration of the CLOCK line at POINT right."
  (let ((line (line-string point)))
    (replace-lines (line-start (copy-point point :temporary)) 1 (list (clock-line-recompute line)))))

(define-key *org-mode-keymap* "C-c C-x C-u" 'org-update-all-dblocks-command)
(define-key *org-mode-keymap* "C-c C-x C-r" 'org-clock-report)
(define-key *org-mode-keymap* "C-c C-x C-j" 'org-clock-goto)
