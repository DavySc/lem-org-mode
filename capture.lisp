;;; Capture and refile: quickly put a note, task or journal entry where it belongs, and move entries between places.
;;;
;;;   (setf lem-org-mode:*org-capture-templates*
;;;         '(("t" "Task" entry (file+headline "~/org/inbox.org" "Tasks") "* TODO %?\n  %U\n  %a")
;;;           ("j" "Journal" entry (file+datetree "~/org/journal.org") "* %<%H:%M> %?" :empty-lines 1)))
;;;
;;; The pure part expands templates and works out where the new text goes in the lines of the target file;
;;; the Lem part prompts, shows the capture buffer and writes to the file's buffer.

(in-package :lem-org-mode)

(defvar *org-capture-templates* '()
  "Capture templates: (KEY DESCRIPTION TYPE TARGET TEMPLATE . OPTIONS). TYPE is entry, item, checkitem, table-line or
plain. TARGET is (file FILE), (file+headline FILE HEADLINE), (file+olp FILE HEADLINE...), (file+datetree FILE),
(file+olp+datetree FILE HEADLINE...), (file+regexp FILE REGEXP) or (function FUNCTION). TEMPLATE is a string, or
(file FILE). OPTIONS: :prepend :empty-lines N :immediate-finish :jump-to-captured :kill-buffer :tree-type (week).")

(defvar *org-default-notes-file* nil
  "The file that a (file \"\") target, or a template without a file, uses.")

(defvar *org-refile-targets* '((:current . (:maxlevel 1)))
  "Where `org-refile' can put an entry: a list of (FILES . SETTINGS). FILES is :current, a file name or a list of
them; SETTINGS a plist with :maxlevel (deepest heading level offered), :tag and :regexp.")

(defvar *org-reverse-note-order* nil
  "True: captured and refiled entries go at the beginning of their target instead of the end.")

(defvar *org-last-stored-link* nil
  "The link made by `org-store-link', for %a in capture templates.")

(defvar *org-user-name* "user"
  "What %n stands for in capture templates.")

;;; BEGIN pure capture code

;;; Time formats

(defparameter *full-day-names* #("Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "Sunday"))
(defparameter *full-month-names* #("January" "February" "March" "April" "May" "June" "July" "August" "September"
                                   "October" "November" "December"))

(defun format-time-spec (spec time)
  "SPEC, a strftime-like format (%Y %y %m %d %e %H %M %S %a %A %b %B %j %F %T %R %%), for TIME, a list (year month day
hour minute second)."
  (destructuring-bind (year month day hour minute &optional (second 0)) time
    (let ((weekday (day-weekday (date-day year month day))))
      (with-output-to-string (out)
        (loop :with i := 0
              :while (< i (length spec))
              :do (let ((c (char spec i)))
                    (if (and (char= c #\%) (< (1+ i) (length spec)))
                        (let ((d (char spec (1+ i))))
                          (incf i 2)
                          (case d
                            (#\Y (format out "~4,'0D" year))
                            (#\y (format out "~2,'0D" (mod year 100)))
                            (#\m (format out "~2,'0D" month))
                            (#\d (format out "~2,'0D" day))
                            (#\e (format out "~2D" day))
                            (#\H (format out "~2,'0D" hour))
                            (#\M (format out "~2,'0D" minute))
                            (#\S (format out "~2,'0D" second))
                            (#\a (write-string (aref *day-names* weekday) out))
                            (#\A (write-string (aref *full-day-names* weekday) out))
                            (#\b (write-string (aref *month-names* (1- month)) out))
                            (#\B (write-string (aref *full-month-names* (1- month)) out))
                            (#\j (format out "~3,'0D" (1+ (- (date-day year month day) (date-day year 1 1)))))
                            (#\F (format out "~4,'0D-~2,'0D-~2,'0D" year month day))
                            (#\T (format out "~2,'0D:~2,'0D:~2,'0D" hour minute second))
                            (#\R (format out "~2,'0D:~2,'0D" hour minute))
                            (#\% (write-char #\% out))
                            (t (write-char #\% out) (write-char d out))))
                        (progn (write-char c out) (incf i)))))))))

(defun time-stamp-text (time active with-time)
  (destructuring-bind (year month day hour minute &optional second) time
    (declare (ignore second))
    (timestamp-format (list :active active :year year :month month :day day
                            :hour (and with-time hour) :minute (and with-time minute)))))

;;; Template expansion

(defun capture-expand (template context)
  "Expand the %-escapes of TEMPLATE. CONTEXT is a plist: :time (year month day hour minute second), :initial,
:annotation, :link, :kill, :clipboard, :clock-title, :clock-link, :user, :file, :path, and :prompt, a function
(kind prompt default options) used for %^t, %^g and %^{...} that returns a string (a timestamp for kind :date).
Return (values text cursor tags): CURSOR is the character offset of %? or NIL, TAGS the tags asked with %^g."
  (let ((out (make-string-output-stream)) (n (length template)) (i 0) (cursor nil) (tags nil)
        (prompts '()) (written 0) (time (getf context :time)) (ask (getf context :prompt)))
    (labels ((emit (text) (write-string text out) (incf written (length text)))
             (column-indent (text)
               ;; continuation lines of a multi-line value start under the %
               (let ((column (let* ((so-far (get-output-stream-string out))
                                    (line-start (let ((p (position #\newline so-far :from-end t))) (if p (1+ p) 0))))
                               (write-string so-far out)
                               (- (length so-far) line-start))))
                 (indent-continuation text (make-string column :initial-element #\space))))
             (asking (kind prompt default options)
               (if ask (funcall ask kind prompt default options) (or default "")))
             (date-escape (char prompted)
               (let* ((active (member char '(#\t #\T)))
                      (with-time (member char '(#\T #\U))))
                 (if prompted
                     (emit (asking :date (if active "Date" "Date (inactive)") nil (list active with-time)))
                     (emit (time-stamp-text time active with-time))))))
      (loop :while (< i n)
            :do (let ((c (char template i)))
                  (cond
                    ((and (char= c #\%) (< (1+ i) n))
                     (let ((d (char template (1+ i))))
                       (case d
                         (#\? (setf cursor written) (incf i 2))
                         ((#\t #\T #\u #\U) (date-escape d nil) (incf i 2))
                         (#\^ (let ((e (and (< (+ i 2) n) (char template (+ i 2)))))
                                (cond ((member e '(#\t #\T #\u #\U)) (date-escape e t) (incf i 3))
                                      ((member e '(#\g #\G))
                                       (let ((answer (asking :tags "Tags" nil nil)))
                                         (setf tags (append tags (parse-tag-input answer))))
                                       (incf i 3))
                                      ((eql e #\{)
                                       (let* ((close (position #\} template :start (+ i 3)))
                                              (spec (subseq template (+ i 3) (or close n)))
                                              (parts (loop :with start := 0
                                                           :for bar := (position #\| spec :start start)
                                                           :collect (subseq spec start bar)
                                                           :while bar :do (setf start (1+ bar))))
                                              (answer (asking :string (first parts) (second parts) (cddr parts))))
                                         (setf prompts (append prompts (list answer)))
                                         (emit answer)
                                         (setf i (if close (1+ close) n))))
                                      (t (emit "%") (incf i)))))
                         (#\< (let ((close (position #\> template :start (+ i 2))))
                                (if close
                                    (progn (emit (format-time-spec (subseq template (+ i 2) close) time))
                                           (setf i (1+ close)))
                                    (progn (emit "%") (incf i)))))
                         (#\\ (let ((digit (and (< (+ i 2) n) (digit-char-p (char template (+ i 2))))))
                                (if (and digit (<= 1 digit (length prompts)))
                                    (progn (emit (nth (1- digit) prompts)) (incf i 3))
                                    (progn (emit "%") (incf i)))))
                         (#\% (emit "%") (incf i 2))
                         (t (let ((value (case d
                                           (#\i (getf context :initial))
                                           (#\a (getf context :annotation))
                                           (#\A (getf context :annotation))
                                           (#\l (getf context :link))
                                           (#\c (getf context :kill))
                                           (#\x (getf context :clipboard))
                                           (#\k (getf context :clock-title))
                                           (#\K (getf context :clock-link))
                                           (#\n (or (getf context :user) *org-user-name*))
                                           (#\f (getf context :file))
                                           (#\F (getf context :path))
                                           (t :none))))
                              (if (eq value :none)
                                  (progn (emit (string c)) (incf i))
                                  (progn (emit (if (find #\newline (or value "")) (column-indent value) (or value "")))
                                         (incf i 2))))))))
                    (t (write-char c out) (incf written) (incf i))))))
    (values (get-output-stream-string out) cursor tags)))

;;; Date tree

(defun datetree-titles (year month day)
  "The headings of the date tree for a date: (\"2026\" \"2026-10 October\" \"2026-10-22 Thursday\")."
  (list (format nil "~D" year)
        (format nil "~4,'0D-~2,'0D ~A" year month (aref *full-month-names* (1- month)))
        (format nil "~4,'0D-~2,'0D-~2,'0D ~A" year month day
                (aref *full-day-names* (day-weekday (date-day year month day))))))

;;; Finding places in the lines of a file

(defun lines-heading-end (lines index keywords)
  "The index of the line after the subtree whose heading is at INDEX."
  (let ((level (heading-split (nth index lines) keywords)))
    (or (position-if (lambda (l) (let ((lv (heading-split l keywords))) (and lv (<= lv level))))
                     lines :start (1+ index))
        (length lines))))

(defun lines-find-child (lines start end level title keywords)
  "The index of the heading of LEVEL with the text TITLE between START and END, or NIL."
  (loop :for i :from start :below end
        :for line := (nth i lines)
        :when (multiple-value-bind (lv todo priority text) (heading-split line keywords)
                (declare (ignore todo priority))
                (and lv (= lv level) (string= (string-trim " " (or text "")) title)))
          :return i))

(defun lines-insert-at (lines index new)
  (append (subseq lines 0 index) new (nthcdr index lines)))

(defun lines-ensure-heading (lines parent-index level title keywords &key sorted)
  "Make sure the heading TITLE of LEVEL exists as a child in the scope of PARENT-INDEX (NIL: the whole file).
Return (values new-lines index). With SORTED a new heading is put before the first greater sibling."
  (let* ((start (if parent-index (1+ parent-index) 0))
         (end (if parent-index (lines-heading-end lines parent-index keywords) (length lines)))
         (found (lines-find-child lines start end level title keywords)))
    (if found
        (values lines found)
        (let* ((position (or (and sorted
                                  (loop :for i :from start :below end
                                        :for line := (nth i lines)
                                        :when (multiple-value-bind (lv todo priority text) (heading-split line keywords)
                                                (declare (ignore todo priority))
                                                (and lv (= lv level) (string> (string-trim " " (or text "")) title)))
                                          :return i))
                             ;; end of the scope, before trailing blank lines
                             (let ((last (position-if-not #'blank-string-p lines :end end :from-end t)))
                               (if (and last (>= last start)) (1+ last) (if parent-index (1+ parent-index) end)))))
               (heading (format nil "~A ~A" (make-string level :initial-element #\*) title)))
          (values (lines-insert-at lines position (list heading)) position)))))

(defun lines-find-olp (lines olp keywords &key create)
  "Find the heading path OLP (a list of titles) in LINES. Return (values lines index level); with CREATE missing
headings are added at the end. INDEX is NIL when the path is empty or not found."
  (let ((parent nil) (level 0))
    (dolist (title olp)
      (incf level)
      (if create
          (multiple-value-setq (lines parent) (lines-ensure-heading lines parent level title keywords))
          (let ((start (if parent (1+ parent) 0))
                (end (if parent (lines-heading-end lines parent keywords) (length lines))))
            (setf parent (lines-find-child lines start end level title keywords))
            (unless parent (return-from lines-find-olp (values lines nil level))))))
    (values lines parent (if olp level 0))))

(defun lines-datetree (lines olp year month day keywords)
  "Ensure the date tree for the date below the path OLP. Return (values new-lines day-heading-index day-level)."
  (multiple-value-bind (lines parent level) (lines-find-olp lines olp keywords :create t)
    (let ((titles (datetree-titles year month day)))
      (dolist (title titles)
        (incf level)
        (multiple-value-setq (lines parent) (lines-ensure-heading lines parent level title keywords :sorted t)))
      (values lines parent level))))

(defun scope-body-end (lines start end)
  "The index just after the last non-blank line in [START, END), at least START."
  (let ((last (position-if-not #'blank-string-p lines :start start :end end :from-end t)))
    (if last (1+ last) start)))

(defun capture-insert (lines target type text keywords &key prepend (empty-lines 0) date)
  "Put the captured TEXT (a string) into LINES according to TARGET, a plist: :olp (heading path), :datetree,
:regexp. TYPE is entry, item, checkitem, table-line or plain. DATE is (year month day) for a date tree. Return
(values new-lines first-index): the index of the first inserted line."
  (let* ((text-lines (trim-trailing-blank-lines (split-text-lines text)))
         (parent nil) (level 0))
    (cond ((getf target :datetree)
           (destructuring-bind (year month day) date
             (multiple-value-setq (lines parent level) (lines-datetree lines (getf target :olp) year month day keywords))))
          ((getf target :olp)
           (multiple-value-setq (lines parent level) (lines-find-olp lines (getf target :olp) keywords :create t))))
    (let* ((start (if parent (1+ parent) 0))
           (end (if parent (lines-heading-end lines parent keywords) (length lines)))
           (regexp-line (and (getf target :regexp)
                             (position-if (lambda (l) (regex-search-p (getf target :regexp) l)) lines :start start :end end))))
      (when (and (getf target :regexp) (null regexp-line))
        (editor-error "No match for ~A in the target file" (getf target :regexp)))
      (let* ((head-end (if parent
                           (+ parent (head-length (nthcdr parent lines)))
                           (or (position-if (lambda (l) (heading-split l keywords)) lines) (length lines))))
             (body-end (scope-body-end lines start (if parent (lines-heading-end lines parent keywords) end)))
             (new-lines
               (ecase (intern (string-upcase (string type)) :keyword)
                 (:entry (lines-shift-levels text-lines
                                             (- (1+ (if regexp-line
                                                        (or (heading-split (nth regexp-line lines) keywords) 0)
                                                        level))
                                                (or (heading-split (first text-lines) keywords) 1))
                                             keywords))
                 ((:item :checkitem :plain) text-lines)
                 (:table-line text-lines)))
             (kind (intern (string-upcase (string type)) :keyword))
             (position
               (cond
                 (regexp-line (if prepend regexp-line (1+ regexp-line)))
                 ((member kind '(:item :checkitem))
                  ;; into the first list of the scope: after its last item, or before the first with prepend
                  (let* ((first-item (position-if #'list-item-parse lines :start head-end :end end)))
                    (cond ((null first-item) (if prepend head-end body-end))
                          (prepend first-item)
                          (t (let ((run-end (or (position-if-not (lambda (l) (or (list-item-parse l) (and (not (blank-string-p l)) (> (line-indent l) 0))))
                                                                 lines :start first-item :end end)
                                                end)))
                               (scope-body-end lines first-item run-end))))))
                 ((eq kind :table-line)
                  (let* ((first-row (position-if #'table-line-p lines :start head-end :end end)))
                    (cond ((null first-row) (if prepend head-end body-end))
                          (prepend first-row)
                          (t (or (position-if-not #'table-line-p lines :start first-row :end end) end)))))
                 (prepend head-end)
                 (t body-end)))
             (blank-before (if (and (plusp empty-lines) (not prepend) (> position 0)
                                    (not (blank-string-p (nth (1- position) lines))))
                               (make-list empty-lines :initial-element "")
                               '()))
             (blank-after (if (plusp empty-lines) (make-list empty-lines :initial-element "") '())))
        (values (lines-insert-at lines position (append blank-before new-lines blank-after))
                (+ position (length blank-before)))))))

;;; Refile

(defun refile-targets (entries settings)
  "The (name . entry) pairs that the SETTINGS (plist :maxlevel :tag :regexp) allow among ENTRIES."
  (let ((maxlevel (or (getf settings :maxlevel) 1)))
    (loop :for entry :in entries
          :when (and (<= (getf entry :level) maxlevel)
                     (or (null (getf settings :tag)) (member (getf settings :tag) (getf entry :all-tags) :test #'string-equal))
                     (or (null (getf settings :regexp)) (regex-search-p (getf settings :regexp) (getf entry :title))))
            :collect (cons (format nil "~{~A/~}~A" (getf entry :path) (getf entry :title)) entry))))

(defun lines-remove-subtree (lines index keywords)
  "Return (values lines-without subtree-lines) for the subtree whose heading is at INDEX."
  (let ((end (lines-heading-end lines index keywords)))
    (values (append (subseq lines 0 index) (nthcdr end lines))
            (subseq lines index end))))

(defun lines-put-subtree (lines parent-index subtree keywords &key prepend)
  "LINES with the SUBTREE lines added as a child of the heading at PARENT-INDEX (NIL: top level), last unless
PREPEND. Return (values new-lines index)."
  (let* ((level (if parent-index (heading-split (nth parent-index lines) keywords) 0))
         (shifted (lines-shift-levels subtree (- (1+ level) (heading-split (first subtree) keywords)) keywords))
         (start (if parent-index (1+ parent-index) 0))
         (end (if parent-index (lines-heading-end lines parent-index keywords) (length lines)))
         (position (if prepend
                       (if parent-index (+ parent-index (head-length (nthcdr parent-index lines))) (or (position-if (lambda (l) (heading-split l keywords)) lines) end))
                       (scope-body-end lines start end))))
    (values (lines-insert-at lines position shifted) position)))

;;; END pure capture code

;;; Targets

(defun target-kind (name)
  (cdr (assoc (string-downcase (symbol-name name))
              '(("file" . :file) ("file+headline" . :headline) ("file+olp" . :olp) ("file+datetree" . :datetree)
                ("file+olp+datetree" . :olp-datetree) ("file+regexp" . :regexp) ("function" . :function)
                ("id" . :id) ("clock" . :clock))
              :test #'string=)))

(defun capture-target-parse (target)
  "Return (values file plist) for a template TARGET: the file and a plist with :olp :datetree :regexp."
  (let ((kind (target-kind (first target))))
    (ecase kind
      (:file (values (second target) nil))
      (:headline (values (second target) (list :olp (list (third target)))))
      (:olp (values (second target) (list :olp (cddr target))))
      (:datetree (values (second target) (list :datetree t)))
      (:olp-datetree (values (second target) (list :datetree t :olp (cddr target))))
      (:regexp (values (second target) (list :regexp (third target))))
      (:function (let ((result (funcall (second target)))) (values (first result) (rest result)))))))

(defun resolve-capture-file (file)
  (let ((name (if (or (null file) (string= file "")) *org-default-notes-file* file)))
    (unless name (editor-error "No file for this capture target (set *org-default-notes-file*)"))
    (uiop:native-namestring (uiop:parse-native-namestring (if (and (plusp (length name)) (char= (char name 0) #\~))
                                                               (namestring (merge-pathnames (subseq name 2) (user-homedir-pathname)))
                                                               name)))))

(defun file-buffer-or-load (file)
  (or (buffer-for-file file)
      (if (probe-file file)
          (find-file-buffer file)
          (let ((buffer (find-file-buffer file)))
            buffer))))

(defun apply-new-lines (buffer lines)
  "Make the text of BUFFER equal LINES, editing only the span that differs."
  (let ((old (split-text-lines (buffer-text buffer))))
    (replace-lines (buffer-start-point buffer) (length old) lines)))

(defun buffer-lines-list (buffer)
  (split-text-lines (buffer-text buffer)))

;;; Capturing

(defun current-time-list ()
  (multiple-value-bind (s mi h d mo y) (decode-universal-time (get-universal-time))
    (list y mo d h mi s)))

(defun capture-context (&key initial annotation prompt)
  (let ((buffer (current-buffer)))
    (list :time (current-time-list) :initial initial :annotation (or annotation *org-last-stored-link*)
          :link *org-last-stored-link*
          :file (and (buffer-filename buffer) (file-namestring (buffer-filename buffer)))
          :path (buffer-filename buffer)
          :clock-title nil :prompt prompt
          :kill (ignore-errors (funcall (find-symbol "PEEK-KILLRING-ITEM" "LEM-CORE")
                                        (funcall (find-symbol "CURRENT-KILLRING" "LEM-CORE")) 0)))))

(defun interactive-capture-prompt (kind prompt default options)
  (ecase kind
    (:date (multiple-value-bind (day hour minute) (date-input-parse (prompt-for-string (format nil "~A: " prompt)) (today-day))
             (timestamp-format (timestamp-with-day (list :active (first options) :hour (and (second options) hour)
                                                         :minute (and (second options) minute))
                                                   day))))
    (:tags (prompt-for-string "Tags: "))
    (:string (if options
                 (prompt-for-string (format nil "~A (~{~A~^, ~}): " prompt options) :initial-value default)
                 (prompt-for-string (format nil "~A: " prompt) :initial-value default)))))

(defun capture-template-text (template)
  (if (stringp template)
      template
      (let ((file (resolve-capture-file (second template))))
        (if (probe-file file) (uiop:read-file-string file) (editor-error "No template file ~A" file)))))

(defun capture-option (template key &optional default)
  (let ((value (getf (nthcdr 5 template) key :missing)))
    (if (eq value :missing) default value)))

(defun org-capture-finish-text (template text)
  "Write the captured TEXT into the target of the capture TEMPLATE. Return (values buffer line) of the new text."
  (destructuring-bind (key description type target body &rest options) template
    (declare (ignore key description body options))
    (multiple-value-bind (file plist) (capture-target-parse target)
      (let* ((buffer (file-buffer-or-load (resolve-capture-file file)))
             (lines (buffer-lines-list buffer))
             (date (let ((time (current-time-list))) (list (first time) (second time) (third time)))))
        (multiple-value-bind (new-lines index)
            (with-todo-buffer ((buffer-start-point buffer))
              (capture-insert lines plist type text (org-keywords)
                              :prepend (or (capture-option template :prepend) *org-reverse-note-order*)
                              :empty-lines (or (capture-option template :empty-lines) 0)
                              :date date))
          (apply-new-lines buffer new-lines)
          (values buffer (1+ index)))))))

(defun find-capture-template (key)
  (or (find key *org-capture-templates* :key #'first :test #'string=)
      (editor-error "No capture template ~A" key)))

(defun org-capture-string (key text)
  "Capture TEXT with the template KEY without asking anything: %? is dropped and TEXT is used where %i stands."
  (let ((template (find-capture-template key)))
    (let ((expanded (capture-expand (capture-template-text (fifth template))
                                    (capture-context :initial text))))
      (org-capture-finish-text template expanded))))

(define-minor-mode org-capture-mode
    (:name "Capture"
     :keymap *org-capture-mode-keymap*))

(define-key *org-capture-mode-keymap* "C-c C-c" 'org-capture-finalize)
(define-key *org-capture-mode-keymap* "C-c C-k" 'org-capture-kill)
(define-key *org-capture-mode-keymap* "C-c C-w" 'org-capture-refile)

(defun capture-menu-text ()
  (format nil "Capture template:~{ ~A) ~A~}: " (loop :for template :in *org-capture-templates*
                                                      :append (list (first template) (second template)))))

(define-command org-capture () ()
  "Capture a note or task with one of `*org-capture-templates*'. Finish with C-c C-c, abort with C-c C-k."
  (unless *org-capture-templates* (editor-error "No capture templates (set *org-capture-templates*)"))
  (let* ((key (prompt-for-string (capture-menu-text)))
         (template (find-capture-template key))
         (origin (current-buffer))
         (initial (and (buffer-mark-p origin)
                       (points-to-string (region-beginning origin) (region-end origin))))
         (context (capture-context :initial initial :prompt #'interactive-capture-prompt)))
    (multiple-value-bind (text cursor tags) (capture-expand (capture-template-text (fifth template)) context)
      (when tags
        (let ((first-line (first (split-text-lines text))))
          (when (heading-split first-line (org-keywords))
            (setf text (format nil "~A~@[~%~A~]" (heading-set-tags first-line (org-keywords) tags)
                               (let ((rest (cdr (split-text-lines text)))) (and rest (format nil "~{~A~^~%~}" rest)))) ))))
      (if (capture-option template :immediate-finish)
          (progn (org-capture-finish-text template text)
                 (message "Captured"))
          (let ((buffer (make-buffer (format nil "CAPTURE-~A" (second template)))))
            (erase-buffer buffer)
            (insert-string (buffer-point buffer) text)
            (change-buffer-mode buffer 'org-mode)
            (setf (buffer-value buffer 'capture-template) template)
            (switch-to-buffer buffer)
            (org-capture-mode t)
            (buffer-start (current-point))
            (when cursor (character-offset (current-point) cursor)))))))

(defun capture-buffer-template ()
  (or (buffer-value (current-buffer) 'capture-template)
      (editor-error "This is not a capture buffer")))

(define-command org-capture-finalize () ()
  "Put the captured text where its template says and close the capture buffer."
  (let* ((capture (current-buffer))
         (template (capture-buffer-template))
         (text (buffer-text capture)))
    (multiple-value-bind (buffer line) (org-capture-finish-text template text)
      (delete-buffer capture)
      (when (capture-option template :jump-to-captured)
        (switch-to-buffer buffer)
        (move-to-line (current-point) line))
      (message "Captured"))))

(define-command org-capture-kill () ()
  "Discard the capture."
  (let ((capture (current-buffer)))
    (capture-buffer-template)
    (delete-buffer capture)
    (message "Capture aborted")))

(define-command org-capture-refile () ()
  "Refile the captured entry to a place of your choice instead of its template's target."
  (let* ((capture (current-buffer)))
    (capture-buffer-template)
    (let ((text (buffer-text capture)))
      (refile-text-to-target (prompt-for-refile-target) text)
      (delete-buffer capture))))

;;; Refile

(defun refile-target-entries ()
  "All (name . entry) refile targets. A name is the outline path, prefixed with the file when several files are used."
  (let ((result '()) (multiple (> (length *org-refile-targets*) 1)))
    (dolist (setting *org-refile-targets*)
      (let ((files (let ((f (car setting))) (cond ((eq f :current) (list :current)) ((listp f) f) (t (list f))))))
        (dolist (file files)
          (let* ((entries (if (eq file :current)
                              (agenda-entries nil (current-buffer))
                              (agenda-entries (list (resolve-capture-file file)))))
                 (prefix (and (or multiple (not (eq file :current)))
                              (if (eq file :current) (buffer-name (current-buffer)) (file-namestring file)))))
            (dolist (target (refile-targets entries (cdr setting)))
              (push (cons (if prefix (format nil "~A:~A" prefix (car target)) (car target)) (cdr target)) result))))))
    (nreverse result)))

(defun prompt-for-refile-target ()
  (let* ((targets (refile-target-entries))
         (answer (prompt-for-string "Refile to: "
                                    :completion-function (lambda (s) (remove-if-not (lambda (n) (search s n :test #'char-equal)) (mapcar #'car targets)))
                                    :test-function (lambda (s) (assoc s targets :test #'string=)))))
    (or (cdr (assoc answer targets :test #'string=)) (editor-error "No such target: ~A" answer))))

(defun refile-text-to-target (entry text &key prepend)
  "Put the subtree TEXT under the target ENTRY (a plist from `agenda-parse'). Return the buffer written."
  (let* ((buffer (agenda-entry-buffer entry))
         (lines (buffer-lines-list buffer))
         (subtree (trim-trailing-blank-lines (split-text-lines text))))
    (multiple-value-bind (new-lines index)
        (with-todo-buffer ((buffer-start-point buffer))
          (lines-put-subtree lines (1- (getf entry :line)) subtree (org-keywords) :prepend (or prepend *org-reverse-note-order*)))
      (declare (ignore index))
      (apply-new-lines buffer new-lines)
      buffer)))

(defun org-refile (point target &key copy)
  "Move (or with COPY, copy) the subtree at POINT under the heading described by TARGET, an agenda entry. When
source and target are the same buffer the target line is adjusted for the removal."
  (let* ((heading (heading-point point))
         (buffer (point-buffer heading))
         (text (format nil "~{~A~^~%~}" (subtree-line-strings heading)))
         (target-buffer (agenda-entry-buffer target)))
    (when (and (eq buffer target-buffer))
      (multiple-value-bind (start end) (org-subtree-bounds heading)
        (let ((line (getf target :line)))
          (when (and (>= line (line-number-at-point start)) (< line (line-number-at-point end)))
            (editor-error "Cannot refile a subtree into itself"))
          (unless copy
            ;; remove first, then the target line moves up if it was below
            (let ((count (- (line-number-at-point end) (line-number-at-point start))))
              (when (> line (line-number-at-point start))
                (setf target (let ((copy (copy-list target))) (setf (getf copy :line) (- line count)) copy)))
              (delete-subtree heading)
              (return-from org-refile (refile-text-to-target target text)))))))
    (refile-text-to-target target text)
    (unless copy (delete-subtree heading))
    target-buffer))

(define-command org-refile-command () ()
  "Move the subtree at point under another heading."
  (let ((target (prompt-for-refile-target)))
    (org-refile (current-point) target)
    (message "Refiled")))

(define-command org-refile-copy () ()
  "Copy the subtree at point under another heading."
  (let ((target (prompt-for-refile-target)))
    (org-refile (current-point) target :copy t)
    (message "Copied")))

(define-command org-refile-goto () ()
  "Jump to a refile target."
  (let ((target (prompt-for-refile-target)))
    (switch-to-buffer (agenda-entry-buffer target))
    (move-to-line (current-point) (getf target :line))))

(define-key *org-mode-keymap* "C-c c" 'org-capture)
(define-key *org-mode-keymap* "C-c C-w" 'org-refile-command)
(define-key *org-mode-keymap* "C-c M-w" 'org-refile-copy)
(define-key *org-mode-keymap* "C-c C-x j" 'org-refile-goto)
