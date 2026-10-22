;;; Agenda: week view, global todo list and tag search over the buffer or a set of Org files.

(in-package :lem-org-mode)

(defvar *org-agenda-files* '()
  "Files (or directories, whose .org files are used) that the agenda reads. When empty the current buffer is used.")

(defvar *org-agenda-span* 7
  "Number of days in the agenda week view.")

(defvar *org-deadline-warning-days* 14
  "A deadline is shown on today's agenda when it is at most this many days away.")

;;; BEGIN pure agenda code

(defun stamp-date (stamp)
  "Return (values year month day) of an Org timestamp such as <2026-10-22 Thu>, or NIL."
  (when (and (>= (length stamp) 11) (char= (char stamp 5) #\-) (char= (char stamp 8) #\-))
    (let ((year (parse-integer stamp :start 1 :end 5 :junk-allowed t))
          (month (parse-integer stamp :start 6 :end 8 :junk-allowed t))
          (day (parse-integer stamp :start 9 :end 11 :junk-allowed t)))
      (and year month day (values year month day)))))

(defun stamp-time (stamp)
  "The HH:MM of the timestamp STAMP, or NIL."
  (let ((colon (position #\: stamp :start (min 11 (length stamp)))))
    (when (and colon (>= colon 2) (<= (+ colon 3) (length stamp))
               (digit-char-p (char stamp (- colon 1))) (digit-char-p (char stamp (+ colon 1))))
      (subseq stamp (- colon 2) (+ colon 3)))))

(defun date-day (year month day)
  "Days since 1900-01-01, so that dates can be compared and subtracted."
  (floor (encode-universal-time 0 0 12 day month year 0) 86400))

(defun day-date (days)
  (multiple-value-bind (s mi h day month year) (decode-universal-time (+ (* days 86400) 43200) 0)
    (declare (ignore s mi h))
    (values year month day)))

(defun line-starts-with-ci-p (line prefix)
  (let ((s (string-left-trim '(#\space #\tab) line)))
    (and (>= (length s) (length prefix)) (string-equal prefix s :end2 (length prefix)))))

(defun agenda-parse (lines keywords source)
  "Return the entries of the Org text LINES as property lists. Heading-like lines inside #+begin_ blocks are skipped."
  (let ((done (car (last keywords))) (in-block nil) (entries '()))
    (loop :for rest :on lines
          :for line := (first rest)
          :for number :from 1
          :do (cond (in-block
                     (when (line-starts-with-ci-p line "#+end_") (setf in-block nil)))
                    ((line-starts-with-ci-p line "#+begin_") (setf in-block t))
                    (t
                     (multiple-value-bind (level todo priority title tags) (heading-split line keywords)
                       (when level
                         (let ((planning (and (second rest) (planning-line-p (second rest))
                                              (planning-parse (second rest)))))
                           (push (list :source source :line number :level level :todo todo
                                       :priority priority :title title :tags tags
                                       :done (and todo (string= todo done))
                                       :scheduled (cdr (assoc "SCHEDULED" planning :test #'string=))
                                       :deadline (cdr (assoc "DEADLINE" planning :test #'string=))
                                       :closed (cdr (assoc "CLOSED" planning :test #'string=)))
                                 entries)))))))
    (nreverse entries)))

(defun agenda-rank (entry)
  (case (getf entry :priority) (#\A 0) (#\C 2) (t 1)))

(defun agenda-sort (items key)
  "Stable sort of ITEMS by the priority of their entries, A first."
  (stable-sort (copy-list items) #'< :key (lambda (item) (agenda-rank (funcall key item)))))

(defun agenda-entry-text (entry)
  (format nil "~{~A~^ ~}"
          (remove nil (list (getf entry :todo)
                            (and (getf entry :priority) (format nil "[#~A]" (getf entry :priority)))
                            (getf entry :title)
                            (and (getf entry :tags) (format nil ":~{~A~^:~}:" (getf entry :tags)))))))

(defun entry-day (entry key)
  (let ((stamp (getf entry key)))
    (and stamp (multiple-value-bind (y m d) (stamp-date stamp) (and y (date-day y m d))))))

(defun agenda-day-items (entries day today warning-days)
  "Return (label . entry) for the open ENTRIES that belong on DAY of an agenda starting at TODAY (day numbers)."
  (let ((items '()))
    (dolist (entry entries)
      (unless (getf entry :done)
        (let ((scheduled (entry-day entry :scheduled))
              (deadline (entry-day entry :deadline)))
          (when scheduled
            (cond ((= scheduled day)
                   (push (cons (let ((time (stamp-time (getf entry :scheduled))))
                                 (if time (format nil "~A Sched.:" time) "Scheduled:"))
                               entry)
                         items))
                  ((and (= day today) (< scheduled today))
                   (push (cons (format nil "Sched.~Dx:" (- today scheduled)) entry) items))))
          (when deadline
            (cond ((= deadline day)
                   (push (cons "Deadline:" entry) items))
                  ((and (= day today) (< today deadline (1+ (+ today warning-days))))
                   (push (cons (format nil "In ~D d.:" (- deadline today)) entry) items))
                  ((and (= day today) (< deadline today))
                   (push (cons (format nil "~D d. ago:" (- today deadline)) entry) items)))))))
    (agenda-sort (nreverse items) #'cdr)))

(defun agenda-category (source)
  "A short name for SOURCE, a file name or a buffer name."
  (let* ((slash (position-if (lambda (c) (member c '(#\/ #\\))) source :from-end t))
         (name (if slash (subseq source (1+ slash)) source))
         (dot (position #\. name :from-end t)))
    (if (and dot (plusp dot)) (subseq name 0 dot) name)))

(defun agenda-line (source label text)
  (format nil "  ~10A ~11A ~A" (concatenate 'string (agenda-category source) ":") label text))

(defun agenda-week-records (entries today span warning-days)
  "Return a list of (line-text . entry-or-nil) for the week view starting at day number TODAY."
  (let ((records '())
        (weekdays #("Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "Sunday"))
        (months #("January" "February" "March" "April" "May" "June" "July" "August" "September"
                  "October" "November" "December")))
    (dotimes (i span)
      (let ((day (+ today i)))
        (multiple-value-bind (year month date) (day-date day)
          (push (cons (format nil "~A ~2D ~A ~D"
                              (aref weekdays (nth-value 6 (decode-universal-time (+ (* day 86400) 43200) 0)))
                              date (aref months (1- month)) year)
                      nil)
                records))
        (dolist (item (agenda-day-items entries day today warning-days))
          (let ((entry (cdr item)))
            (push (cons (agenda-line (getf entry :source) (car item) (agenda-entry-text entry)) entry)
                  records)))))
    (nreverse records)))

(defun agenda-todo-records (entries)
  "Return (line-text . entry) for every entry in a not-done todo state, highest priority first."
  (mapcar (lambda (entry)
            (cons (agenda-line (getf entry :source) "" (agenda-entry-text entry)) entry))
          (agenda-sort (remove-if-not (lambda (entry) (and (getf entry :todo) (not (getf entry :done))))
                                      entries)
                       #'identity)))

(defun agenda-tag-records (entries tag)
  (mapcar (lambda (entry)
            (cons (agenda-line (getf entry :source) "" (agenda-entry-text entry)) entry))
          (agenda-sort (remove-if-not (lambda (entry) (member tag (getf entry :tags) :test #'string-equal))
                                      entries)
                       #'identity)))

(defun split-text-lines (text)
  "The lines of TEXT as a list of strings, without line terminators."
  (loop :with start := 0
        :for end := (position #\newline text :start start)
        :collect (string-right-trim '(#\return) (subseq text start end))
        :while end
        :do (setf start (1+ end))))

;;; END pure agenda code

(defun agenda-source-files ()
  (loop :for item :in *org-agenda-files*
        :for path := (uiop:parse-native-namestring item)
        :append (if (uiop:directory-pathname-p path)
                    (mapcar #'uiop:native-namestring (uiop:directory-files path "*.org"))
                    (list (uiop:native-namestring path)))))

(defun agenda-entries ()
  "The entries of the agenda files, or of the current buffer when none are configured."
  (if *org-agenda-files*
      (loop :for file :in (agenda-source-files)
            :append (agenda-parse (uiop:read-file-lines file) *org-todo-keywords* file))
      (let ((buffer (current-buffer)))
        (agenda-parse (split-text-lines (buffer-text buffer)) *org-todo-keywords*
                      (or (buffer-filename buffer) (buffer-name buffer))))))

(define-major-mode org-agenda-mode nil
    (:name "Org-Agenda"
     :keymap *org-agenda-mode-keymap*))

(define-key *org-agenda-mode-keymap* "Return" 'org-agenda-goto)
(define-key *org-agenda-mode-keymap* "q" 'org-agenda-quit)

(defun show-agenda (title records)
  "Show RECORDS, a list of (line-text . entry-or-nil), in the agenda buffer."
  (let ((buffer (or (get-buffer "*Org Agenda*") (make-buffer "*Org Agenda*"))))
    (with-buffer-read-only buffer nil
      (erase-buffer buffer)
      (insert-string (buffer-point buffer)
                     (format nil "~A~%~{~A~^~%~}" title (mapcar #'car records)))
      (setf (buffer-value buffer 'agenda-targets)
            (coerce (cons nil (mapcar #'cdr records)) 'vector)))
    (change-buffer-mode buffer 'org-agenda-mode)
    (setf (buffer-read-only-p buffer) t)
    (switch-to-buffer buffer)
    (buffer-start (current-point))
    buffer))

(define-command org-agenda-week () ()
  "Show the next days of scheduled items and deadlines."
  (multiple-value-bind (year month day) (today)
    (show-agenda (format nil "Week agenda from ~4,'0D-~2,'0D-~2,'0D" year month day)
                 (agenda-week-records (agenda-entries) (date-day year month day)
                                      *org-agenda-span* *org-deadline-warning-days*))))

(define-command org-agenda-todo-list () ()
  "List every open todo entry, highest priority first."
  (show-agenda "Global TODO list" (agenda-todo-records (agenda-entries))))

(define-command org-agenda-tags () ()
  "List the entries that carry a tag."
  (let ((tag (prompt-for-string "Tag: ")))
    (show-agenda (format nil "Entries tagged :~A:" tag) (agenda-tag-records (agenda-entries) tag))))

(define-command org-agenda-goto () ()
  "Jump to the entry on the current agenda line."
  (let* ((targets (buffer-value (current-buffer) 'agenda-targets))
         (index (1- (line-number-at-point (current-point))))
         (entry (and targets (< index (length targets)) (aref targets index))))
    (unless entry
      (editor-error "No entry on this line"))
    (let* ((source (getf entry :source))
           (buffer (or (get-buffer source) (find-file-buffer source))))
      (switch-to-buffer buffer)
      (move-to-line (current-point) (getf entry :line)))))

(define-command org-agenda-quit () ()
  "Leave the agenda buffer."
  (let ((agenda (current-buffer)))
    (switch-to-buffer (or (find-if-not (lambda (buffer) (eq buffer agenda)) (buffer-list)) agenda))
    (delete-buffer agenda)))
