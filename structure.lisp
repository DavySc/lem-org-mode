;;; Structure: moving around and folding the outline, copying / cutting / pasting subtrees, archiving, sorting,
;;; sparse trees, properties with inheritance, and todo dependencies.

(in-package :lem-org-mode)

(defvar *org-archive-location* "%s_archive::"
  "Where `org-archive-subtree' puts a subtree: FILE::HEADING, with %s standing for the name of the current file.
An empty FILE means the current file; an empty HEADING puts the subtree at the end of the file. The #+ARCHIVE:
keyword of a file and the ARCHIVE property of an entry override it.")

(defvar *org-enforce-todo-dependencies* nil
  "True: an entry cannot be finished while it has open child tasks or, under a parent with the ORDERED property,
while an earlier sibling is open.")

(defvar *org-subtree-clipboard* nil
  "The text of the subtree last cut or copied, a string.")

;;; BEGIN pure structure code

(defun lines-shift-levels (lines delta keywords)
  "LINES with every heading (outside #+begin_ blocks) moved DELTA levels. A heading at level 1 stays there."
  (let ((in-block nil))
    (mapcar (lambda (line)
              (cond (in-block (when (line-starts-with-ci-p line "#+end_") (setf in-block nil)) line)
                    ((line-starts-with-ci-p line "#+begin_") (setf in-block t) line)
                    (t (let ((level (heading-split line keywords)))
                         (if level
                             (let ((new (max 1 (+ level delta))))
                               (concatenate 'string (make-string new :initial-element #\*) (subseq line level)))
                             line)))))
            lines)))

(defun archive-location-parse (location source)
  "Split LOCATION, such as \"%s_archive::* Archive\", into (values file heading). %s becomes SOURCE (a file name
or NIL); FILE is NIL for the current file, HEADING NIL for the end of the file."
  (let* ((at (search "::" location))
         (file-part (subseq location 0 at))
         (heading (and at (string-trim " " (subseq location (+ at 2)))))
         (file (let ((text (with-output-to-string (out)
                             (loop :with start := 0
                                   :for p := (search "%s" file-part :start2 start)
                                   :do (write-string file-part out :start start :end (or p (length file-part)))
                                   :while p
                                   :do (write-string (or source "") out) (setf start (+ p 2))))))
                 (and (plusp (length text)) text))))
    (values file (and heading (plusp (length heading)) heading))))

(defun heading-title-text (line)
  "The text of a heading line without its stars."
  (string-left-trim "*" (string-left-trim " " (string-left-trim "*" line))))

(defun subtree-for-archive (lines delta properties keywords)
  "The subtree LINES moved DELTA levels, with the PROPERTIES alist set in the drawer of its first heading."
  (let* ((shifted (lines-shift-levels lines delta keywords))
         (head-count (head-length shifted))
         (head (subseq shifted 0 head-count)))
    (dolist (property properties)
      (setf head (head-set-property head (car property) (cdr property))))
    (append head (nthcdr head-count shifted))))

;;; Sorting

(defun sort-key-value (kind block keywords)
  "The key of the entry BLOCK (its lines, heading first) for the sort KIND: :alpha :numeric :time :priority :todo."
  (let* ((heading (first block))
         (title (multiple-value-bind (level todo priority text) (heading-split heading keywords)
                  (declare (ignore level todo priority))
                  (or text heading))))
    (ecase kind
      (:alpha (string-downcase title))
      (:alpha-case title)
      (:numeric (let ((digits (position-if #'digit-char-p title)))
                  (if digits
                      (let ((end (position-if-not (lambda (c) (or (digit-char-p c) (char= c #\.))) title :start digits)))
                        (or (ignore-errors (let ((*read-eval* nil)) (read-from-string (subseq title digits end)))) 0))
                      0)))
      (:time (let ((found (some (lambda (line) (first (timestamps-in-text line))) block)))
               (if found (timestamp-day (first found)) most-positive-fixnum)))
      (:priority (priority-rank (nth-value 2 (heading-split heading keywords))))
      (:todo (let ((todo (nth-value 1 (heading-split heading keywords))))
               (or (and todo (position todo keywords :test #'string=)) (length keywords)))))))

(defun sort-blocks (blocks kind reverse keywords &key (key #'sort-key-value))
  "BLOCKS (lists of lines) sorted by KIND; REVERSE sorts the other way. The sort is stable. KEY computes the sort
key of a block from KIND, the block and KEYWORDS."
  (let* ((keyed (mapcar (lambda (block) (cons (funcall key kind block keywords) block)) blocks))
         (less (if (member kind '(:alpha :alpha-case))
                   (if reverse #'string> #'string<)
                   (if reverse #'> #'<))))
    (mapcar #'cdr (stable-sort keyed less :key #'car))))

(defun split-into-blocks (lines level keywords)
  "Split LINES, a run of sibling subtrees, into one block per heading of LEVEL. Nothing before the first heading."
  (let ((blocks '()) (current nil) (in-block nil))
    (dolist (line lines)
      (let ((heading (and (not in-block) (eql (heading-split line keywords) level))))
        (cond (in-block (when (line-starts-with-ci-p line "#+end_") (setf in-block nil)))
              ((line-starts-with-ci-p line "#+begin_") (setf in-block t)))
        (when heading
          (when current (push (nreverse current) blocks))
          (setf current nil))
        (push line current)))
    (when current (push (nreverse current) blocks))
    (nreverse blocks)))

(defun sort-subtree-lines (lines level kind reverse keywords)
  "The sibling subtrees LINES (all of LEVEL) sorted."
  (apply #'append (sort-blocks (split-into-blocks lines level keywords) kind reverse keywords)))

(defun list-block-key (kind block keywords)
  "The sort key of a list item BLOCK: its text after the bullet and checkbox."
  (declare (ignore keywords))
  (multiple-value-bind (indent bullet box content) (list-item-parse (first block))
    (declare (ignore indent bullet box))
    (let ((text (subseq (first block) content)))
      (ecase kind
        (:alpha (string-downcase text))
        (:alpha-case text)
        (:numeric (let ((digits (position-if #'digit-char-p text)))
                    (if digits
                        (or (parse-integer text :start digits :junk-allowed t) 0)
                        0)))))))

(defun sort-list-lines (lines kind reverse)
  "The list item LINES (items with their continuation lines) sorted; KIND :alpha :alpha-case or :numeric."
  (let ((blocks '()) (current nil))
    (dolist (line lines)
      (if (list-item-parse line)
          (progn (when current (push (nreverse current) blocks)) (setf current (list line)))
          (push line current)))
    (when current (push (nreverse current) blocks))
    (list-renumber (apply #'append (sort-blocks (nreverse blocks) kind reverse '() :key #'list-block-key)))))

(defun sort-table-rows (rows column kind reverse)
  "The data ROWS (lists of cells, :rule rows stay where they are) sorted by COLUMN."
  (let* ((data (remove :rule rows))
         (sorted (stable-sort (copy-list data)
                              (cond ((member kind '(:numeric))
                                     (if reverse #'> #'<))
                                    (t (if reverse #'string> #'string<)))
                              :key (lambda (row)
                                     (let ((cell (or (nth column row) "")))
                                       (case kind
                                         (:numeric (or (ignore-errors (let ((*read-eval* nil)) (let ((x (read-from-string cell))) (and (numberp x) x)))) 0))
                                         (:alpha-case cell)
                                         (t (string-downcase cell))))))))
    (let ((remaining sorted))
      (mapcar (lambda (row) (if (eq row :rule) row (pop remaining))) rows))))

(defun parse-sort-kind (char)
  "The sort kind and direction for the key character CHAR: a n t p o, upper case sorts the other way."
  (let ((kind (ecase (char-downcase char) (#\a :alpha) (#\n :numeric) (#\t :time) (#\p :priority) (#\o :todo) (#\c :alpha-case)))
        (reverse (upper-case-p char)))
    (values kind reverse)))

;;; Todo dependencies

(defun entry-blocked-reason (lines index keywords done-keywords)
  "Why the entry at INDEX of LINES cannot be finished, or NIL: an open child task, or (under a parent with the
ORDERED property) an open earlier sibling."
  (let* ((level (heading-split (nth index lines) keywords))
         (end (or (position-if (lambda (l) (let ((lv (heading-split l keywords))) (and lv (<= lv level))))
                               lines :start (1+ index))
                  (length lines))))
    (or (loop :for i :from (1+ index) :below end
              :do (multiple-value-bind (lv todo) (heading-split (nth i lines) keywords)
                    (when (and lv todo (not (member todo done-keywords :test #'string=)))
                      (return (format nil "it has an open child task: ~A" (heading-title-text (nth i lines)))))))
        (let ((parent (loop :for i :from (1- index) :downto 0
                            :when (let ((lv (heading-split (nth i lines) keywords))) (and lv (< lv level)))
                              :return i)))
          (when (and parent
                     (let ((head (subseq (nthcdr parent lines) 0 (head-length (nthcdr parent lines)))))
                       (assoc "ORDERED" (entry-property-alist head) :test #'string=)))
            (loop :for i :from (1+ parent) :below index
                  :do (multiple-value-bind (lv todo) (heading-split (nth i lines) keywords)
                        (when (and lv (= lv level) todo (not (member todo done-keywords :test #'string=)))
                          (return (format nil "an earlier sibling is still open: ~A" (heading-title-text (nth i lines))))))))))))

;;; Sparse tree lines

(defun entry-outline-path-text (entry)
  (format nil "~{~A > ~}~A" (getf entry :path) (agenda-entry-text entry)))

(defun sparse-record (entry)
  (cons (format nil "L~D ~A ~A" (getf entry :line) (make-string (getf entry :level) :initial-element #\*)
                (entry-outline-path-text entry))
        entry))

;;; END pure structure code

;;; Outline navigation

(defun next-heading-point (point &optional (direction 1))
  "A point on the next (1) or previous (-1) heading line from POINT, or NIL."
  (let ((p (copy-point point :temporary)))
    (loop :while (line-offset p direction)
          :do (when (org-heading-level p) (return (line-start p))))))

(defun same-level-heading-point (point direction)
  (let ((level (org-heading-level (heading-point point)))
        (p (copy-point (heading-point point) :temporary)))
    (loop :while (line-offset p direction)
          :do (let ((l (org-heading-level p)))
                (cond ((null l))
                      ((= l level) (return (line-start p)))
                      ((< l level) (return nil)))))))

(defun parent-heading-point (point)
  (let* ((heading (heading-point point))
         (level (org-heading-level heading))
         (p (copy-point heading :temporary)))
    (loop :while (line-offset p -1)
          :do (let ((l (org-heading-level p)))
                (when (and l (< l level)) (return (line-start p)))))))

(defmacro define-heading-motion (name doc (point) &body body)
  `(define-command ,name () ()
     ,doc
     (let ((target (let ((,point (current-point))) ,@body)))
       (unless target (editor-error "No such heading"))
       (move-point (current-point) target))))

(define-heading-motion org-next-visible-heading "Move to the next heading." (point)
  (next-heading-point point 1))
(define-heading-motion org-previous-visible-heading "Move to the previous heading." (point)
  (next-heading-point point -1))
(define-heading-motion org-forward-heading-same-level "Move to the next heading of the same level." (point)
  (same-level-heading-point point 1))
(define-heading-motion org-backward-heading-same-level "Move to the previous heading of the same level." (point)
  (same-level-heading-point point -1))
(define-heading-motion outline-up-heading "Move to the parent heading." (point)
  (parent-heading-point point))

;;; Visibility

(defun heading-fold-overlay (heading)
  "The fold overlay that starts on the line of HEADING, or NIL."
  (find-if (lambda (overlay) (and (overlay-get overlay :fold) (same-line-p (overlay-start overlay) heading)))
           (buffer-overlays (point-buffer heading))))

(defun fold-range (start end)
  "Fold from the line of START to END (the region after the heading line), as a single placeholder."
  (when (and (point< start end) (not (same-line-p start end)))
    (let ((overlay (place-region-placeholder-overlay start end :is-line-fold t)))
      (overlay-put overlay :fold t)
      overlay)))

(defun org-fold-subtree (heading)
  "Fold the body and sub-headings of the entry at HEADING."
  (unless (heading-fold-overlay heading)
    (multiple-value-bind (start end) (org-subtree-bounds heading)
      (when (and start (line-offset (copy-point start :temporary) 1))
        (let ((from (copy-point start :temporary)) (to (copy-point end :temporary)))
          (when (and (start-line-p to) (line-offset to -1)) (line-end to))
          (fold-range from to))))))

(defun org-unfold-heading (heading)
  (let ((overlay (heading-fold-overlay heading)))
    (when overlay (delete-overlay overlay) t)))

(defun org-unfold-all (buffer)
  (dolist (overlay (copy-list (buffer-overlays buffer)))
    (when (overlay-get overlay :fold) (delete-overlay overlay))))

(defun heading-points (buffer &optional (max-level 100))
  (let ((points '()) (p (copy-point (buffer-start-point buffer) :temporary)))
    (loop
      (let ((level (org-heading-level p)))
        (when (and level (<= level max-level)) (push (copy-point (line-start p) :temporary) points)))
      (unless (line-offset p 1) (return)))
    (nreverse points)))

(defun org-hide-body-of (heading)
  "Fold the text between HEADING and its first sub-heading (or the end of the entry)."
  (let* ((from (copy-point heading :temporary))
         (to (copy-point heading :temporary)))
    (when (line-offset from 1)
      (multiple-value-bind (start end) (org-subtree-bounds heading)
        (declare (ignore start))
        (move-point to from)
        (loop :while (and (point< to end) (not (org-heading-level to)) (line-offset to 1)))
        (when (and (org-heading-level to) (line-offset to -1)) (line-end to))
        (when (and (point= to end) (start-line-p to) (line-offset to -1)) (line-end to))
        (fold-range (line-start (copy-point heading :temporary)) to)))))

(defun org-set-visibility (buffer mode)
  "MODE :overview (top-level headings only), :contents (all headings, no text), :all (everything)."
  (org-unfold-all buffer)
  (ecase mode
    (:all nil)
    (:overview (dolist (heading (heading-points buffer))
                 (when (eql 1 (org-heading-level heading)) (org-fold-subtree heading))))
    (:contents (dolist (heading (reverse (heading-points buffer)))
                 (org-hide-body-of heading)))))

(define-command org-overview () () "Show only the top-level headings." (org-set-visibility (current-buffer) :overview))
(define-command org-content () () "Show all headings but no text." (org-set-visibility (current-buffer) :contents))
(define-command org-show-all () () "Show everything." (org-set-visibility (current-buffer) :all))

(define-command org-global-cycle () ()
  "Cycle the whole buffer: overview, contents, everything."
  (let* ((buffer (current-buffer))
         (state (or (buffer-value buffer 'org-visibility) :all))
         (next (ecase state (:all :overview) (:overview :contents) (:contents :all))))
    (org-set-visibility buffer next)
    (setf (buffer-value buffer 'org-visibility) next)
    (message "~A" (ecase next (:overview "OVERVIEW") (:contents "CONTENTS") (:all "SHOW ALL")))))

(defun child-headings (heading)
  "Points on the direct sub-headings of the entry at HEADING."
  (let ((level (org-heading-level heading)) (children '()))
    (multiple-value-bind (start end) (org-subtree-bounds heading)
      (declare (ignore start))
      (let ((p (copy-point heading :temporary)))
        (loop :while (and (line-offset p 1) (point< p end))
              :do (let ((l (org-heading-level p)))
                    (when (and l (= l (1+ level))) (push (copy-point (line-start p) :temporary) children))))))
    (nreverse children)))

(defun org-cycle-heading (point)
  "Cycle the visibility of the entry at POINT: folded, children visible, whole subtree visible."
  (let ((heading (line-start (copy-point point :temporary))))
    (cond ((heading-fold-overlay heading)
           ;; folded -> show children (each child itself folded)
           (org-unfold-heading heading)
           (dolist (child (child-headings heading)) (org-fold-subtree child))
           (setf (buffer-value (point-buffer point) (list 'org-cycle-state (line-number-at-point heading))) :children))
          ((eq (buffer-value (point-buffer point) (list 'org-cycle-state (line-number-at-point heading))) :children)
           ;; children shown -> show the entire subtree
           (dolist (child (child-headings heading)) (org-unfold-heading child))
           (multiple-value-bind (start end) (org-subtree-bounds heading)
             (dolist (overlay (copy-list (buffer-overlays (point-buffer point))))
               (when (and (overlay-get overlay :fold) (point<= start (overlay-start overlay)) (point< (overlay-start overlay) end))
                 (delete-overlay overlay))))
           (setf (buffer-value (point-buffer point) (list 'org-cycle-state (line-number-at-point heading))) :subtree))
          (t
           (org-fold-subtree heading)
           (setf (buffer-value (point-buffer point) (list 'org-cycle-state (line-number-at-point heading))) :folded)))))

(define-command org-show-subtree () () "Show the whole subtree at point."
  (let ((heading (heading-point (current-point))))
    (multiple-value-bind (start end) (org-subtree-bounds heading)
      (dolist (overlay (copy-list (buffer-overlays (current-buffer))))
        (when (and (overlay-get overlay :fold) (point<= start (overlay-start overlay)) (point< (overlay-start overlay) end))
          (delete-overlay overlay))))))

(define-command org-hide-subtree () () "Fold the subtree at point."
  (org-fold-subtree (heading-point (current-point))))

;;; Subtree text

(defun subtree-line-strings (heading)
  "The lines of the whole subtree at HEADING."
  (multiple-value-bind (start end) (org-subtree-bounds heading)
    (unless start (editor-error "Not on a heading"))
    (split-text-lines (string-right-trim '(#\newline) (points-to-string start end)))))

(defun delete-subtree (heading)
  (multiple-value-bind (start end) (org-subtree-bounds heading)
    (unless start (editor-error "Not on a heading"))
    (let ((to (copy-point end :temporary)))
      (delete-between-points (copy-point start :temporary) to))))

(defun org-copy-subtree (point)
  "Remember the subtree at POINT in `*org-subtree-clipboard*'. Return its text."
  (setf *org-subtree-clipboard* (format nil "~{~A~^~%~}" (subtree-line-strings (heading-point point)))))

(defun org-cut-subtree (point)
  "Remember the subtree at POINT and remove it from the buffer."
  (let ((heading (heading-point point)))
    (prog1 (org-copy-subtree point)
      (delete-subtree heading))))

(defun insert-subtree-after (heading lines)
  "Insert the subtree LINES after the subtree at HEADING (as its next sibling, at its level). Return a point on the new heading."
  (let* ((level (org-heading-level heading))
         (shifted (lines-shift-levels lines (- level (heading-split (first lines) (org-keywords))) (org-keywords)))
         (end (nth-value 1 (org-subtree-bounds heading)))
         (point (copy-point end :temporary)))
    (unless (start-line-p point) (insert-character point #\newline))
    (let ((line (line-number-at-point point)))
      (insert-string point (format nil "~{~A~%~}" shifted))
      (move-to-line (copy-point (buffer-start-point (point-buffer heading)) :temporary) line))))

(defun org-paste-subtree (point &optional (text *org-subtree-clipboard*))
  "Paste TEXT, a subtree, after the entry at POINT as its next sibling, or at the end of the buffer without one."
  (unless text (editor-error "Nothing to paste"))
  (let ((lines (trim-trailing-blank-lines (split-text-lines text))))
    (if (enclosing-heading point)
        (insert-subtree-after (enclosing-heading point) lines)
        (let ((p (copy-point (buffer-end-point (point-buffer point)) :temporary)))
          (unless (start-line-p p) (insert-character p #\newline))
          (insert-string p (format nil "~{~A~%~}" lines))
          p))))

(define-command org-copy-subtree-command () ()
  "Copy the subtree at point."
  (org-copy-subtree (current-point))
  (message "Subtree copied"))

(define-command org-cut-subtree-command () ()
  "Cut the subtree at point."
  (org-cut-subtree (current-point))
  (message "Subtree cut"))

(define-command org-paste-subtree-command () ()
  "Paste the last cut or copied subtree after the current one."
  (move-point (current-point) (org-paste-subtree (current-point))))

;;; Archiving

(defun archive-time-text ()
  (destructuring-bind (year month day hour minute) (now)
    (format nil "~4,'0D-~2,'0D-~2,'0D ~A ~2,'0D:~2,'0D" year month day (timestamp-day-name year month day) hour minute)))

(defun buffer-keyword (buffer name)
  "The value of the first #+NAME: line of BUFFER."
  (first (file-keyword-values (split-text-lines (buffer-text buffer)) name)))

(defun entry-at-line (buffer line)
  (find line (agenda-entries nil buffer) :key (lambda (e) (getf e :line))))

(defun archive-target-buffer (file source-buffer)
  (cond ((null file) source-buffer)
        (t (let ((path (merge-pathnames file (or (buffer-directory source-buffer) ""))))
             (or (buffer-for-file (uiop:native-namestring path))
                 (find-file-buffer (uiop:native-namestring path)))))))

(defun find-heading-line (buffer title)
  "A point on the first heading of BUFFER whose text is TITLE (given with or without stars), or NIL."
  (let ((wanted (string-trim " " (string-left-trim "*" title))))
    (find-if (lambda (point) (string= (string-trim " " (heading-title-text (line-string point))) wanted))
             (heading-points buffer))))

(defun org-archive-subtree (point &key (location nil))
  "Move the subtree at POINT to its archive location, with ARCHIVE_ properties that say where it came from."
  (let* ((heading (heading-point point))
         (buffer (point-buffer heading))
         (source (buffer-filename buffer))
         (location (or location
                       (property-value-in-entry heading "ARCHIVE")
                       (buffer-keyword buffer "ARCHIVE")
                       *org-archive-location*)))
    (multiple-value-bind (file target-heading) (archive-location-parse location source)
      (let* ((target (archive-target-buffer file buffer))
             (entry (entry-at-line buffer (line-number-at-point heading)))
             (lines (subtree-line-strings heading))
             (level (org-heading-level heading))
             (anchor (and target-heading (find-heading-line target target-heading))))
        (when (and (eq target buffer) anchor
                   (multiple-value-bind (s e) (org-subtree-bounds heading) (and (point<= s anchor) (point< anchor e))))
          (editor-error "Cannot archive a subtree into itself"))
        (let* ((new-level (if anchor (1+ (org-heading-level anchor)) 1))
               (properties (append (list (cons "ARCHIVE_TIME" (archive-time-text))
                                         (cons "ARCHIVE_FILE" (or source (buffer-name buffer))))
                                   (and entry (list (cons "ARCHIVE_CATEGORY" (getf entry :category))))
                                   (and entry (getf entry :todo) (list (cons "ARCHIVE_TODO" (getf entry :todo))))
                                   (and entry (getf entry :path)
                                        (list (cons "ARCHIVE_OLPATH" (format nil "~{~A~^/~}" (getf entry :path)))))
                                   (and entry (set-difference (getf entry :all-tags) (getf entry :tags) :test #'string=)
                                        (list (cons "ARCHIVE_ITAGS"
                                                    (format nil "~{~A~^ ~}" (set-difference (getf entry :all-tags) (getf entry :tags) :test #'string=)))))))
               (new-lines (with-todo-buffer (heading)
                            (subtree-for-archive lines (- new-level level) properties (org-keywords)))))
          (when (and (null anchor) target-heading)
            ;; create the archive heading at the end of the target
            (let ((end (copy-point (buffer-end-point target) :temporary)))
              (unless (start-line-p end) (insert-character end #\newline))
              (insert-string end (format nil "* ~A~%" (string-left-trim "* " target-heading))))
            (setf anchor (find-heading-line target target-heading)
                  new-level 2
                  new-lines (with-todo-buffer (heading) (subtree-for-archive lines (- 2 level) properties (org-keywords)))))
          (delete-subtree heading)
          (when anchor (setf anchor (find-heading-line target target-heading)))
          (let ((end (if anchor
                         (copy-point (nth-value 1 (org-subtree-bounds anchor)) :temporary)
                         (copy-point (buffer-end-point target) :temporary))))
            (unless (start-line-p end) (insert-character end #\newline))
            (insert-string end (format nil "~{~A~%~}" new-lines)))
          (when (and (not (eq target buffer)) (buffer-filename target))
            (save-buffer target))
          target)))))

(define-command org-archive-subtree-command () ()
  "Archive the subtree at point: move it to the archive location."
  (org-archive-subtree (current-point))
  (message "Subtree archived"))

(defun org-toggle-archive-tag (point)
  "Add the ARCHIVE tag to the heading at POINT, or remove it."
  (let* ((heading (heading-point point)))
    (multiple-value-bind (level todo priority title tags) (heading-split (line-string heading) (org-keywords))
      (declare (ignore level todo priority title))
      (org-set-tags point (if (member "ARCHIVE" tags :test #'string=)
                              (remove "ARCHIVE" tags :test #'string=)
                              (append tags (list "ARCHIVE")))))))

(define-command org-toggle-archive-tag-command () ()
  "Toggle the ARCHIVE tag of the current heading."
  (org-toggle-archive-tag (current-point)))

(defun org-archive-to-sibling (point)
  "Move the subtree at POINT under an `Archive' sibling heading with the ARCHIVE tag, creating it when needed."
  (let* ((heading (heading-point point))
         (level (org-heading-level heading))
         (parent (parent-heading-point heading))
         (lines (subtree-line-strings heading)))
    (delete-subtree heading)
    (let ((archive (let ((candidates (if parent
                                         (child-headings (heading-point parent))
                                         (remove-if-not (lambda (p) (eql 1 (org-heading-level p))) (heading-points (point-buffer point))))))
                     (find-if (lambda (p) (let ((tags (nth-value 4 (heading-split (line-string p) (org-keywords)))))
                                            (member "ARCHIVE" tags :test #'string=)))
                              candidates))))
      (unless archive
        (let ((end (copy-point (if parent (nth-value 1 (org-subtree-bounds (heading-point parent))) (buffer-end-point (point-buffer point))) :temporary)))
          (unless (start-line-p end) (insert-character end #\newline))
          (insert-string end (format nil "~A Archive~%" (make-string level :initial-element #\*)))
          (let ((created (copy-point end :temporary)))
            (line-offset created -1)
            (setf archive (line-start created))
            (org-set-tags archive (list "ARCHIVE")))))
      (let ((end (copy-point (nth-value 1 (org-subtree-bounds archive)) :temporary)))
        (unless (start-line-p end) (insert-character end #\newline))
        (insert-string end (format nil "~{~A~%~}" (lines-shift-levels lines (- (1+ (org-heading-level archive)) level) (org-keywords))))))))

(define-command org-archive-to-archive-sibling () ()
  "Move the subtree at point under an Archive sibling heading tagged ARCHIVE."
  (org-archive-to-sibling (current-point)))

;;; Sorting

(defun region-lines-after (heading)
  "The lines of the entries below HEADING (its children), without the heading's own text."
  (let ((all (subtree-line-strings heading)))
    (nthcdr (or (position-if #'org-heading-string-p all :start 1) (length all)) all)))

(defun org-sort-entries (point char)
  "Sort the children of the entry at POINT (or the top-level entries when POINT is before the first heading) by the key CHAR."
  (multiple-value-bind (kind reverse) (parse-sort-kind char)
    (with-todo-buffer (point)
      (let* ((heading (enclosing-heading point)) (keywords (org-keywords)))
        (if heading
            (let* ((level (1+ (org-heading-level heading)))
                   (children (region-lines-after heading)))
              (unless children (editor-error "No sub-entries to sort"))
              (multiple-value-bind (start end) (org-subtree-bounds heading)
                (let* ((own (subseq (subtree-line-strings heading) 0 (- (length (subtree-line-strings heading)) (length children))))
                       (sorted (sort-subtree-lines children level kind reverse keywords)))
                  (delete-between-points (copy-point start :temporary) (copy-point end :temporary))
                  (insert-string (copy-point start :temporary) (format nil "~{~A~%~}" (append own sorted))))))
            (let* ((buffer (point-buffer point))
                   (lines (split-text-lines (buffer-text buffer)))
                   (first-heading (position-if #'org-heading-string-p lines)))
              (unless first-heading (editor-error "No entries to sort"))
              (let ((sorted (sort-subtree-lines (nthcdr first-heading lines) 1 kind reverse keywords)))
                (replace-lines (move-to-line (copy-point (buffer-start-point buffer) :temporary) (1+ first-heading))
                               (- (length lines) first-heading) sorted))))))))

(defun org-sort-list (point char)
  "Sort the plain list around POINT by the key CHAR (a alphabetic, n numeric)."
  (multiple-value-bind (kind reverse) (parse-sort-kind char)
    (let* ((start (item-block-start point))
           (lines (following-lines start))
           (count (or (position-if-not #'list-line-p lines) (length lines))))
      (replace-lines start count (sort-list-lines (subseq lines 0 count) kind reverse)))))

(defun org-sort-table (point char)
  "Sort the data rows of the table at POINT by the column of the cursor."
  (multiple-value-bind (kind reverse) (parse-sort-kind char)
    (let* ((start (table-first-line point))
           (strings (table-line-strings start))
           (column (table-cell-index (line-string point) (point-charpos point)))
           (rows (mapcar #'table-split-line strings))
           (indent (subseq (first strings) 0 (position #\| (first strings)))))
      (replace-lines start (length strings)
                     (mapcar (lambda (line) (concatenate 'string indent line))
                             (org-table-format-rows (sort-table-rows rows column kind reverse)))))))

(define-command org-sort () ()
  "Sort the table, list or sub-entries at point. Keys: a alphabetic, n numeric, t time, p priority, o todo; upper case reverses."
  (let* ((point (current-point))
         (answer (prompt-for-string "Sort by [a n t p o c] (capital reverses): "))
         (char (if (plusp (length answer)) (char answer 0) (editor-error "No sort key"))))
    (cond ((org-table-at-point-p point) (org-sort-table point char))
          ((org-item-at-point-p point) (org-sort-list point char))
          (t (org-sort-entries point char)))))

;;; Properties with inheritance

(defun org-entry-get (point name &optional inherit)
  "The value of the property NAME of the entry at POINT. With INHERIT, look in the ancestors when the entry has none."
  (let ((heading (enclosing-heading point)))
    (loop :while heading
          :do (let ((value (property-value-in-entry heading name)))
                (when value (return-from org-entry-get value)))
              (setf heading (and inherit (parent-heading-point heading))))
    nil))

(defun org-entry-delete (point name)
  "Remove the property NAME from the entry at POINT."
  (org-edit-head point
                 (lambda (head)
                   (multiple-value-bind (heading planning drawer) (head-split head)
                     (let* ((prefix (format nil ":~A:" name))
                            (body (remove-if (lambda (line)
                                               (let ((s (string-left-trim " 	" line)))
                                                 (and (>= (length s) (length prefix)) (string-equal prefix s :end2 (length prefix)))))
                                             (if drawer (butlast (rest drawer)) '()))))
                       (append (list heading) (and planning (list planning))
                               (and body (append (list ":PROPERTIES:") body (list ":END:")))))))))

(define-command org-delete-property-command () ()
  "Remove a property from the current entry."
  (org-entry-delete (current-point) (prompt-for-string "Property: ")))

(define-command org-toggle-ordered-property () ()
  "Toggle the ORDERED property of the current entry: its children must be finished in order."
  (let ((point (current-point)))
    (if (property-value-in-entry (heading-point point) "ORDERED")
        (org-entry-delete point "ORDERED")
        (org-set-property point "ORDERED" "t"))))

;;; Dependencies

(defun check-todo-dependencies (point new)
  "Signal an editor error when finishing the entry at POINT is not allowed."
  (when (and *org-enforce-todo-dependencies* new (org-done-keyword-p new))
    (let* ((buffer (point-buffer point))
           (lines (split-text-lines (buffer-text buffer)))
           (reason (with-todo-buffer (point)
                     (entry-blocked-reason lines (1- (line-number-at-point (heading-point point)))
                                           (org-keywords) (sequences-done-keywords (org-todo-sequences))))))
      (when reason (editor-error "Cannot finish this entry: ~A" reason)))))

;;; Structure commands

(define-command org-toggle-heading () ()
  "Turn the current line into a heading, or a heading into a plain line."
  (let* ((point (current-point)) (line (line-string point)))
    (replace-lines (line-start (copy-point point :temporary)) 1
                   (list (cond ((org-heading-level point) (string-left-trim " " (subseq line (org-heading-level point))))
                               ((list-item-parse line)
                                (multiple-value-bind (indent bullet box content) (list-item-parse line)
                                  (declare (ignore indent bullet box))
                                  (concatenate 'string "* " (subseq line content))))
                               (t (concatenate 'string "* " line)))))))

(define-command org-insert-subheading () ()
  "Insert a heading one level below the current one, after its body."
  (org-insert-heading-command)
  (org-shift-subtree-line (current-point) 1))

(defun org-shift-subtree-line (point delta)
  "Add DELTA stars to the heading line at POINT only."
  (let ((line (line-string point)) (level (org-heading-level point)))
    (when level
      (replace-lines (line-start (copy-point point :temporary)) 1
                     (list (concatenate 'string (make-string (max 1 (+ level delta)) :initial-element #\*) (subseq line level)))))))

(define-command org-reset-checkbox-state-subtree () ()
  "Uncheck every checkbox in the subtree at point."
  (let* ((heading (heading-point (current-point))))
    (multiple-value-bind (start end) (org-subtree-bounds heading)
      (let* ((count (- (line-number-at-point end) (line-number-at-point start)))
             (lines (subtree-line-strings heading)))
        (declare (ignorable count))
        (replace-lines start (length lines)
                       (mapcar (lambda (line)
                                 (multiple-value-bind (indent bullet box) (list-item-parse line)
                                   (declare (ignore bullet))
                                   (if (and indent box (char/= box #\Space)) (item-set-checkbox line #\Space) line)))
                               lines))))
    (org-update-cookies (current-point))))

;;; Sparse trees: the matching entries of the buffer in a list you can jump from

(defun show-sparse-entries (title entries)
  (show-agenda title (mapcar #'sparse-record entries) nil))

(define-command org-occur () ()
  "List the entries of this buffer whose heading or text matches a regexp."
  (let* ((regexp (prompt-for-string "Regexp: "))
         (entries (remove-if-not (lambda (e) (regex-search-p regexp (entry-search-text e))) (agenda-entries nil (current-buffer)))))
    (show-sparse-entries (format nil "~D match~:*~[es~;~:;es~] for ~A" (length entries) regexp) entries)))

(define-command org-show-todo-tree () ()
  "List the open todo entries of this buffer."
  (show-sparse-entries "TODO tree"
                       (remove-if-not (lambda (e) (and (getf e :todo) (not (getf e :done)))) (agenda-entries nil (current-buffer)))))

(define-command org-tags-sparse-tree () ()
  "List the entries of this buffer that match a tags / property expression."
  (let ((predicate (match-compile (prompt-for-string "Match: ") (today-day))))
    (show-sparse-entries "Tags tree" (remove-if-not predicate (agenda-entries nil (current-buffer))))))

(define-command org-check-deadlines () ()
  "List the entries of this buffer whose deadline is near or past."
  (let ((today (today-day)))
    (show-sparse-entries "Deadlines"
                         (remove-if-not (lambda (e)
                                          (let ((d (entry-day e :deadline)))
                                            (and d (not (getf e :done)) (<= d (+ today *org-deadline-warning-days*)))))
                                        (agenda-entries nil (current-buffer))))))

(define-command org-sparse-tree () ()
  "Ask for a kind of sparse tree: r)egexp t)odo m)atch d)eadlines."
  (let ((answer (prompt-for-string "Sparse tree: r)egexp t)odo m)atch d)eadlines: ")))
    (cond ((string= answer "r") (org-occur))
          ((string= answer "t") (org-show-todo-tree))
          ((string= answer "m") (org-tags-sparse-tree))
          ((string= answer "d") (org-check-deadlines))
          (t (editor-error "No sparse tree ~A" answer)))))

;;; org-goto

(define-command org-goto () ()
  "Jump to a heading of this buffer chosen by its outline path."
  (let* ((entries (agenda-entries nil (current-buffer)))
         (names (mapcar (lambda (e) (cons (format nil "~{~A/~}~A" (getf e :path) (getf e :title)) e)) entries))
         (answer (prompt-for-string "Go to: " :completion-function (lambda (s) (remove-if-not (lambda (n) (search s n :test #'char-equal)) (mapcar #'car names)))
                                              :test-function (lambda (s) (assoc s names :test #'string=))))
         (entry (cdr (assoc answer names :test #'string=))))
    (unless entry (editor-error "No such heading"))
    (move-to-line (current-point) (getf entry :line))))

(define-key *org-mode-keymap* "C-c C-n" 'org-next-visible-heading)
(define-key *org-mode-keymap* "C-c C-p" 'org-previous-visible-heading)
(define-key *org-mode-keymap* "C-c C-f" 'org-forward-heading-same-level)
(define-key *org-mode-keymap* "C-c C-b" 'org-backward-heading-same-level)
(define-key *org-mode-keymap* "C-c C-u" 'outline-up-heading)
(define-key *org-mode-keymap* "C-c C-j" 'org-goto)
(define-key *org-mode-keymap* "C-c C-x M-w" 'org-copy-subtree-command)
(define-key *org-mode-keymap* "C-c C-x C-w" 'org-cut-subtree-command)
(define-key *org-mode-keymap* "C-c C-x C-y" 'org-paste-subtree-command)
(define-key *org-mode-keymap* "C-c C-x C-s" 'org-archive-subtree-command)
(define-key *org-mode-keymap* "C-c C-x a" 'org-toggle-archive-tag-command)
(define-key *org-mode-keymap* "C-c C-x A" 'org-archive-to-archive-sibling)
(define-key *org-mode-keymap* "C-c ^" 'org-sort)
(define-key *org-mode-keymap* "C-c /" 'org-sparse-tree)
(define-key *org-mode-keymap* "C-c *" 'org-toggle-heading)
(define-key *org-mode-keymap* "C-c C-x o" 'org-toggle-ordered-property)
(define-key *org-mode-keymap* "C-c C-x M-m" 'org-insert-subheading)
(define-key *org-mode-keymap* "C-c C-x d" 'org-delete-property-command)
