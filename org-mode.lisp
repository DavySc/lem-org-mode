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
           :org-move-subtree))
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
(define-key *org-mode-keymap* "M-Left" 'org-promote-subtree)
(define-key *org-mode-keymap* "M-Right" 'org-demote-subtree)
(define-key *org-mode-keymap* "M-Up" 'org-move-subtree-up)
(define-key *org-mode-keymap* "M-Down" 'org-move-subtree-down)
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
  "Fold or unfold the subtree when point is on a heading, otherwise indent the line."
  (if (org-heading-level (current-point))
      (fold-toggle-at-point (current-point))
      (indent)))

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
