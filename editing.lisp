;;; Editing helpers: structure templates (<s TAB, C-c C-,), plain list editing (indent, move, bullets), drawers,
;;; COMMENT headings, relative timers, and RET in tables.

(in-package :lem-org-mode)

(defvar *org-timer-start* nil "The universal time at which the relative timer started, or NIL.")
(defvar *org-timer-paused* nil "While the timer is paused: the seconds that had passed.")

;;; BEGIN pure editing code

(defvar *org-structure-template-alist*
  '(("a" . "export ascii") ("c" . "center") ("C" . "comment") ("e" . "example") ("E" . "export")
    ("h" . "export html") ("l" . "export latex") ("q" . "quote") ("s" . "src") ("v" . "verse"))
  "Keys for `org-insert-structure-template' and the <s TAB shortcuts, with the block each one makes.")

(defun structure-template-lines (kind body indent)
  "The lines of a #+begin_KIND ... #+end_ block around BODY (a list of lines), indented by INDENT blanks."
  (let* ((pad (make-string indent :initial-element #\space))
         (word (subseq kind 0 (position #\space kind)))
         (begin (format nil "~A#+begin_~A" pad kind))
         (begin (if (and (string= word "src") (not (find #\space kind))) (concatenate 'string begin " ") begin)))
    (append (list begin)
            (mapcar (lambda (line) (if (blank-string-p line) line (concatenate 'string pad line))) body)
            (list (format nil "~A#+end_~A" pad word)))))

(defun tempo-expand (line alist)
  "If LINE is only <KEY (optionally indented, KEY one or more letters of ALIST) return the block lines that replace it
and the number of the line (0-based) the cursor goes to, else NIL. <s gives an empty src block."
  (let* ((trimmed (string-left-trim " " line))
         (indent (- (length line) (length trimmed))))
    (when (and (> (length trimmed) 1) (char= (char trimmed 0) #\<)
               (every #'alpha-char-p (subseq trimmed 1)))
      (let ((entry (assoc (subseq trimmed 1) alist :test #'string=)))
        (when entry
          (values (structure-template-lines (cdr entry) (list "") indent) 1))))))

;;; Plain lists

(defun list-item-end (lines index)
  "The index after the item at INDEX of LINES and its continuation lines and sub-items."
  (+ index (list-item-extent (nthcdr index lines))))

(defun list-shift-lines (lines delta)
  "LINES moved DELTA columns to the right (left when negative); blank lines are left alone."
  (mapcar (lambda (line)
            (cond ((blank-string-p line) line)
                  ((plusp delta) (concatenate 'string (make-string delta :initial-element #\space) line))
                  (t (subseq line (min (- delta) (line-indent line))))))
          lines))

(defun list-indent-item (lines index delta)
  "LINES with the item at INDEX (and what belongs to it) moved DELTA columns; ordered lists are renumbered."
  (let* ((end (list-item-end lines index))
         (indent (list-item-parse (nth index lines))))
    (when (and (minusp delta) (< indent (- delta)) (zerop indent))
      (editor-error "Cannot outdent the item further"))
    (append (subseq lines 0 index)
            (list-shift-lines (subseq lines index end) delta)
            (nthcdr end lines))))

(defun list-block-bounds (lines index)
  "The start and end indexes of the run of list lines that contains INDEX."
  (let ((start index) (end (1+ index)))
    (loop :while (and (> start 0) (list-line-p (nth (1- start) lines))) :do (decf start))
    (loop :while (and (< end (length lines)) (list-line-p (nth end lines))) :do (incf end))
    (values start end)))

(defun list-move-item (lines index direction)
  "LINES with the item at INDEX swapped with the previous (-1) or next (1) item of the same indentation.
The second value is the new index of the moved item."
  (let* ((indent (list-item-parse (nth index lines)))
         (end (list-item-end lines index)))
    (multiple-value-bind (block-start block-end) (list-block-bounds lines index)
      (if (minusp direction)
          (let ((previous (loop :for i :from (1- index) :downto block-start
                                :for ind := (list-item-parse (nth i lines))
                                :when (and ind (< ind indent)) :return nil
                                :when (and ind (= ind indent)) :return i)))
            (unless previous (editor-error "Cannot move the item up"))
            (values (append (subseq lines 0 previous) (subseq lines index end) (subseq lines previous index) (nthcdr end lines))
                    previous))
          (let ((next (and (< end block-end) (eql (list-item-parse (nth end lines)) indent) end)))
            (unless next (editor-error "Cannot move the item down"))
            (let ((next-end (list-item-end lines next)))
              (values (append (subseq lines 0 index) (subseq lines next next-end) (subseq lines index end) (nthcdr next-end lines))
                      (+ index (- next-end next)))))))))

(defparameter *bullet-ring* '("-" "+" "1." "1)"))

(defun list-cycle-bullet-line (line)
  "LINE, a list item, with the next bullet of - + 1. 1)."
  (multiple-value-bind (indent bullet) (list-item-parse line)
    (let* ((kind (if (digit-char-p (char bullet 0)) (concatenate 'string "1" (string (char bullet (1- (length bullet))))) bullet))
           (next (or (second (member kind *bullet-ring* :test #'string=)) (first *bullet-ring*))))
      (concatenate 'string (subseq line 0 indent) next (subseq line (+ indent (length bullet)))))))

(defun line-make-item (line)
  "LINE as a list item: `- ' is put in front of the text."
  (let ((indent (line-indent line)))
    (concatenate 'string (subseq line 0 indent) "- " (subseq line indent))))

;;; COMMENT headings

(defun heading-toggle-comment (line keywords)
  "LINE, a heading, with the COMMENT keyword added after its todo keyword, or removed."
  (multiple-value-bind (level todo priority title tags) (heading-split line keywords)
    (unless level (editor-error "Not on a heading"))
    (let* ((commented (and (>= (length title) 7) (string= "COMMENT" title :end2 7)
                           (or (= (length title) 7) (char= (char title 7) #\space))))
           (new-title (if commented (string-left-trim " " (subseq title 7)) (format nil "COMMENT~@[ ~A~]" (and (plusp (length title)) title)))))
      (heading-join level todo priority new-title tags))))

;;; Timers

(defun format-timer-seconds (seconds)
  (format nil "~D:~2,'0D:~2,'0D" (floor seconds 3600) (mod (floor seconds 60) 60) (mod seconds 60)))

(defun timer-elapsed (start paused now)
  "Seconds on the relative timer: PAUSED when it is paused, else NOW minus START; NIL without a timer."
  (cond (paused paused) (start (- now start))))

;;; END pure editing code

;;; Structure templates

(defun org-insert-structure-template (point key)
  "Wrap the lines of the region (or insert an empty block) at POINT for the template KEY of the alist."
  (let ((entry (assoc key *org-structure-template-alist* :test #'string=)))
    (unless entry (editor-error "No structure template ~A" key))
    (let* ((buffer (point-buffer point))
           (indent (line-indent (line-string point))))
      (if (buffer-mark-p buffer)
          (let* ((a (line-number-at-point (region-beginning buffer))) (b (line-number-at-point (region-end buffer)))
                 (from (min a b)) (to (max a b))
                 (body (loop :for i :from from :to to :collect (line-string (line-point-in buffer i)))))
            (replace-lines (line-point-in buffer from) (1+ (- to from))
                           (structure-template-lines (cdr entry) body (line-indent (first body))))
            (buffer-mark-cancel buffer))
          (let ((lines (structure-template-lines (cdr entry) (list "") indent)))
            (line-start point)
            (insert-string point (format nil "~{~A~%~}" lines))
            (line-offset point -2)
            (line-end point))))))

(define-command org-insert-structure-template-command () ()
  "Wrap the region in, or insert, a block: a)scii c)enter C)omment e)xample E)xport h)tml l)atex q)uote s)rc v)erse."
  (org-insert-structure-template
   (current-point)
   (prompt-for-string (format nil "Block type: ~{~A~^ ~}: " (mapcar (lambda (e) (format nil "~A)~A" (car e) (cdr e))) *org-structure-template-alist*)))))

(defun org-tempo-expand (point)
  "Expand a <s style shortcut on POINT's line. True when something was expanded."
  (multiple-value-bind (lines cursor) (tempo-expand (line-string point) *org-structure-template-alist*)
    (when lines
      (let ((line (line-number-at-point point)))
        (replace-lines (line-start (copy-point point :temporary)) 1 lines)
        (move-to-line point (+ line cursor))
        (line-end point)
        t))))

;;; Lists

(defun item-index-at (point)
  (1- (line-number-at-point point)))

(defun org-edit-list-block (point function)
  "Replace the list block around POINT by (FUNCTION lines index) where LINES are the buffer's lines from the block
start and INDEX is POINT's line within them. Return nothing useful."
  (let* ((buffer (point-buffer point))
         (all (split-text-lines (buffer-text buffer)))
         (index (item-index-at point)))
    (multiple-value-bind (start end) (list-block-bounds all index)
      (let* ((region (subseq all start end))
             (new (funcall function region (- index start))))
        (replace-lines (line-point-in buffer (1+ start)) (length region) (list-renumber new))))))

(defun org-indent-item (point delta)
  (unless (org-item-at-point-p point) (editor-error "Not on a list item"))
  (let ((indent (list-item-parse (line-string point))))
    (when (and (minusp delta) (zerop indent)) (editor-error "Cannot outdent the item further"))
    (org-edit-list-block point (lambda (lines index) (list-indent-item lines index delta)))
    (setf (point-charpos point) (max 0 (+ (point-charpos point) delta)))))

(defun org-move-item (point direction)
  "Swap the list item at POINT with its neighbour; the cursor stays on the item."
  (unless (org-item-at-point-p point) (editor-error "Not on a list item"))
  (let* ((buffer (point-buffer point))
         (all (split-text-lines (buffer-text buffer)))
         (index (item-index-at point))
         (column (point-charpos point)))
    (multiple-value-bind (start end) (list-block-bounds all index)
      (multiple-value-bind (new new-index) (list-move-item (subseq all start end) (- index start) direction)
        (replace-lines (line-point-in buffer (1+ start)) (- end start) (list-renumber new))
        (move-to-line point (+ start new-index 1))
        (setf (point-charpos point) column)))))

(define-command org-cycle-list-bullet () ()
  "Change the bullet of the list item at point: - + 1. 1)."
  (let ((point (current-point)))
    (unless (org-item-at-point-p point) (editor-error "Not on a list item"))
    (replace-lines (line-start (copy-point point :temporary)) 1 (list (list-cycle-bullet-line (line-string point))))
    (org-renumber-list point)))

(define-command org-ctrl-c-dash () ()
  "In a table insert a horizontal line, on a list item change its bullet, elsewhere turn the line into an item."
  (let ((point (current-point)))
    (cond ((org-table-at-point-p point) (org-table-edit point :insert-hline))
          ((org-item-at-point-p point) (org-cycle-list-bullet))
          (t (replace-lines (line-start (copy-point point :temporary)) 1 (list (line-make-item (line-string point))))))))

;;; Drawers, COMMENT

(define-command org-insert-drawer () ()
  "Insert a drawer with a given name, around the region if there is one."
  (let* ((name (string-upcase (prompt-for-string "Drawer name: ")))
         (point (current-point)) (buffer (current-buffer)))
    (if (buffer-mark-p buffer)
        (let* ((a (line-number-at-point (region-beginning buffer))) (b (line-number-at-point (region-end buffer)))
               (from (min a b)) (to (max a b))
               (body (loop :for i :from from :to to :collect (line-string (line-point-in buffer i)))))
          (replace-lines (line-point-in buffer from) (1+ (- to from)) (append (list (format nil ":~A:" name)) body (list ":END:")))
          (buffer-mark-cancel buffer))
        (progn (line-start point)
               (insert-string point (format nil ":~A:~%~%:END:~%" name))
               (line-offset point -2)))))

(define-command org-toggle-comment () ()
  "Toggle the COMMENT keyword of the current heading: its subtree is not exported."
  (org-edit-heading-line (current-point) (lambda (line) (heading-toggle-comment line (org-keywords)))))

;;; Timers

(defun org-timer-seconds ()
  (timer-elapsed *org-timer-start* *org-timer-paused* (get-universal-time)))

(define-command org-timer-start () ()
  "Start the relative timer."
  (setf *org-timer-start* (get-universal-time) *org-timer-paused* nil)
  (message "Timer started"))

(define-command org-timer-pause-or-continue () ()
  "Pause the relative timer, or continue it."
  (cond ((null *org-timer-start*) (editor-error "No timer"))
        (*org-timer-paused* (setf *org-timer-start* (- (get-universal-time) *org-timer-paused*) *org-timer-paused* nil)
                            (message "Timer continues"))
        (t (setf *org-timer-paused* (org-timer-seconds)) (message "Timer paused at ~A" (format-timer-seconds *org-timer-paused*)))))

(define-command org-timer-stop () ()
  "Stop the relative timer."
  (setf *org-timer-start* nil *org-timer-paused* nil)
  (message "Timer stopped"))

(define-command org-timer () ()
  "Insert the time on the relative timer."
  (let ((seconds (org-timer-seconds)))
    (unless seconds (editor-error "No timer"))
    (insert-string (current-point) (format-timer-seconds seconds))))

(define-command org-timer-item () ()
  "Insert a list item that starts with the time on the relative timer."
  (let ((seconds (org-timer-seconds)))
    (unless seconds (editor-error "No timer"))
    (let ((point (current-point)))
      (if (org-item-at-point-p point)
          (progn (org-insert-item point) (insert-string point (format nil "~A :: " (format-timer-seconds seconds))))
          (insert-string point (format nil "- ~A :: " (format-timer-seconds seconds)))))))

;;; RET

(define-command org-return () ()
  "In a table go to the next row (making a new one at the end), elsewhere insert a newline."
  (if (org-table-at-point-p (current-point))
      (org-table-edit (current-point) :next-row)
      (insert-character (current-point) #\newline)))

(define-key *org-mode-keymap* "C-c C-," 'org-insert-structure-template-command)
(define-key *org-mode-keymap* "C-c -" 'org-ctrl-c-dash)
(define-key *org-mode-keymap* "C-c C-x d" 'org-insert-drawer)
(define-key *org-mode-keymap* "C-c ;" 'org-toggle-comment)
(define-key *org-mode-keymap* "C-c C-x 0" 'org-timer-start)
(define-key *org-mode-keymap* "C-c C-x ," 'org-timer-pause-or-continue)
(define-key *org-mode-keymap* "C-c C-x _" 'org-timer-stop)
(define-key *org-mode-keymap* "C-c C-x ." 'org-timer)
(define-key *org-mode-keymap* "C-c C-x -" 'org-timer-item)
(define-key *org-mode-keymap* "Return" 'org-return)
