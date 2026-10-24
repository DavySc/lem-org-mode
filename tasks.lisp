;;; Tasks: changing the state of a TODO entry (CLOSED, log entries and notes, repeating tasks, clock, statistics
;;; cookies), notes, effort, and the commands around them. The pure rules are in tasks-model.lisp.

(in-package :lem-org-mode)

(defvar *org-log-done* :time
  "NIL: do not record when a task is done. :time adds a CLOSED timestamp; :note also asks for a closing note.")

(defvar *org-log-into-drawer* t
  "True: log entries go into the :LOGBOOK: drawer, with the clock lines. NIL: right under the heading.")

(defvar *org-log-repeat* :time
  "What to log when a repeating task is done: NIL, :time or :note.")

(defvar *org-todo-repeat-to-state* nil
  "The state a repeating task returns to: a keyword, or NIL for the first keyword of its sequence.")

(defun entry-line-strings (heading)
  "The lines of the entry under HEADING, a point on its heading line: up to the next heading of any level."
  (let ((all (following-lines heading)))
    (subseq all 0 (or (position-if #'org-heading-string-p all :start 1) (length all)))))

(defun heading-keyword-at (heading)
  (nth-value 1 (heading-split (line-string heading) (org-keywords))))

(defun clock-stamp-now ()
  (format-clock-stamp (now)))

(defun now-minutes ()
  (destructuring-bind (year month day hour minute) (now)
    (declare (ignore year month day))
    (+ (* 60 hour) minute)))

(defun org-insert-log (heading lines)
  "Put the log entry LINES on the entry at HEADING: on top of its :LOGBOOK: drawer or right after its head."
  (if *org-log-into-drawer*
      (org-edit-logbook heading (lambda (logbook) (logbook-add logbook lines "LOGBOOK")))
      (let ((entry (entry-line-strings heading)))
        (replace-line-span heading (head-length entry) 0 lines))))

(defun todo-log-kind (sequences old new)
  "What changing from OLD to NEW should log, according to the logging flags of the keywords: :note, :time or NIL."
  (let ((enter (and new (nth-value 0 (sequences-log sequences new))))
        (leave (and old (nth-value 1 (sequences-log sequences old)))))
    (cond ((or (eq enter :note) (eq leave :note)) :note)
          ((or enter leave) :time))))

(defun org-todo-log (heading old new kind note)
  (when kind
    (org-insert-log heading (log-entry-lines (log-state-text new old (clock-stamp-now)) note))))

(defun org-todo-closing-note (heading note)
  (org-insert-log heading (log-entry-lines (format nil "- CLOSING NOTE ~A" (clock-stamp-now)) note)))

(defun property-value-in-entry (heading name)
  "The value of the property NAME in the :PROPERTIES: drawer of the entry at HEADING, or NIL."
  (let* ((entry (entry-line-strings heading))
         (head (subseq entry 0 (head-length entry)))
         (prefix (format nil ":~A:" name)))
    (loop :for line :in head
          :for s := (string-left-trim " 	" line)
          :when (and (> (length s) (length prefix)) (string-equal prefix s :end2 (length prefix)))
            :return (string-trim " 	" (subseq s (length prefix))))))

(defun org-todo-repeat (heading old new note)
  "A repeating task was completed: move its timestamps on, put it back to a todo state and log the completion."
  (let* ((sequences (org-todo-sequences))
         (entry (entry-line-strings heading))
         (today-day (multiple-value-call #'date-day (today)))
         (target (or (property-value-in-entry heading "REPEAT_TO_STATE")
                     *org-todo-repeat-to-state*
                     (sequences-first-todo sequences old)))
         (stamp (clock-stamp-now))
         (reset (property-value-in-entry heading "RESET_CHECK_BOXES")))
    (let ((moved (entry-apply-repeat entry today-day (now-minutes))))
      (when reset
        (setf moved (mapcar (lambda (line)
                              (multiple-value-bind (indent bullet box) (list-item-parse line)
                                (declare (ignore bullet))
                                (if (and indent box (char/= box #\Space)) (item-set-checkbox line #\Space) line)))
                            moved)))
      (replace-lines heading (length entry) moved))
    (org-set-planning heading "CLOSED" nil)
    (org-set-todo-keyword heading target)
    (org-set-property heading "LAST_REPEAT" stamp)
    (when *org-log-repeat*
      (org-todo-log heading old new :time (and (eq *org-log-repeat* :note) note)))))

(defun org-update-cookies (point)
  "Recompute the statistics cookies and checkbox states around the line of POINT."
  (let* ((buffer (point-buffer point))
         (lines (split-text-lines (buffer-text buffer)))
         (index (1- (line-number-at-point point))))
    (with-todo-buffer (point)
      (dolist (change (cookies-update lines index (org-keywords) (sequences-done-keywords (org-todo-sequences))))
        (replace-lines (line-point-in buffer (1+ (car change))) 1 (list (cdr change)))))))

(defun line-point-in (buffer number)
  (move-to-line (copy-point (buffer-start-point buffer) :temporary) number))

(defun org-update-all-cookies (buffer)
  "Recompute every statistics cookie of BUFFER. Return how many lines changed."
  (let ((lines (split-text-lines (buffer-text buffer))))
    (with-todo-buffer ((buffer-start-point buffer))
      (let ((changes (cookies-update-all lines (org-keywords) (sequences-done-keywords (org-todo-sequences)))))
        (dolist (change changes)
          (replace-lines (line-point-in buffer (1+ (car change))) 1 (list (cdr change))))
        (length changes)))))

(defun org-todo-change (point new &key note)
  "Change the todo state of the entry at POINT to NEW (a keyword, or NIL for none): CLOSED timestamp, log entries,
repeating tasks, the running clock and statistics cookies are looked after. Return the new state."
  (with-todo-buffer (point)
    (let* ((heading (heading-point point))
           (sequences (org-todo-sequences))
           (old (heading-keyword-at heading))
           (done-new (sequences-done-p sequences new))
           (kind (todo-log-kind sequences old new)))
      (cond
        ((equal old new))
        ((and done-new (not (sequences-done-p sequences old)) (entry-repeats-p (entry-line-strings heading)))
         (org-todo-repeat heading old new note)
         (setf new (heading-keyword-at heading)))
        (t
         (org-set-todo-keyword heading new)
         (org-todo-update-closed heading)
         (org-todo-log heading old new kind note)
         (when (and done-new (not (sequences-done-p sequences old)) (eq *org-log-done* :note))
           (org-todo-closing-note heading note))))
      (when done-new (org-clock-stop-for-done heading))
      (org-update-cookies heading)
      new)))

(defun todo-note-needed-p (point new)
  "Whether changing the entry at POINT to NEW should ask for a note."
  (with-todo-buffer (point)
    (let* ((heading (heading-point point))
           (sequences (org-todo-sequences))
           (old (heading-keyword-at heading)))
      (and (not (equal old new))
           (or (eq (todo-log-kind sequences old new) :note)
               (and (sequences-done-p sequences new) (not (sequences-done-p sequences old))
                    (eq *org-log-done* :note)
                    (not (entry-repeats-p (entry-line-strings heading))))
               (and (sequences-done-p sequences new) (eq *org-log-repeat* :note)
                    (entry-repeats-p (entry-line-strings heading))))))))

(defun org-todo-step (point direction)
  "Move the entry at POINT to the next (1) or previous (-1) state of its sequence, asking for a note when needed."
  (with-todo-buffer (point)
    (let* ((heading (heading-point point))
           (new (sequences-next (org-todo-sequences) (heading-keyword-at heading) direction))
           (note (and (todo-note-needed-p point new) (prompt-for-string "Note for this state change: "))))
      (org-todo-change point new :note note))))

(define-command org-todo () ()
  "Cycle the todo state of the current heading. Logging, CLOSED, repeaters and statistics cookies are handled."
  (org-todo-step (current-point) 1))

(define-command org-todo-previous () ()
  "Cycle the todo state of the current heading backward."
  (org-todo-step (current-point) -1))

(defun todo-state-from-input (sequences input)
  "The keyword that INPUT names: a keyword in any case, the key of one, or none for blank (NIL, t)."
  (let ((input (string-trim " " input)))
    (cond ((or (string= input "") (string= input "-") (string= input " ")) (values nil t))
          ((and (= (length input) 1) (cdr (assoc (char input 0) (sequences-key-alist sequences))))
           (values (cdr (assoc (char input 0) (sequences-key-alist sequences))) t))
          (t (let ((keyword (find input (sequences-keywords sequences) :test #'string-equal)))
               (if keyword (values keyword t) (values nil nil)))))))

(define-command org-todo-select () ()
  "Choose the todo state of the current heading by keyword or fast-selection key (empty for none)."
  (let* ((point (current-point))
         (sequences (with-todo-buffer (point) (org-todo-sequences))))
    (multiple-value-bind (state ok)
        (todo-state-from-input sequences
                               (prompt-for-string
                                (format nil "State (~{~A~^ ~}, empty for none): "
                                        (mapcar (lambda (keyword)
                                                  (let ((key (car (rassoc keyword (sequences-key-alist sequences) :test #'string=))))
                                                    (if key (format nil "~A(~A)" keyword key) keyword)))
                                                (sequences-keywords sequences)))))
      (unless ok (editor-error "Not a todo keyword"))
      (org-todo-change point state :note (and (todo-note-needed-p point state)
                                              (prompt-for-string "Note for this state change: "))))))

(defun org-insert-todo-heading-command ()
  (org-insert-heading-command)
  (let ((point (current-point)))
    (org-set-todo-keyword point (sequences-first-todo (with-todo-buffer (point) (org-todo-sequences))))
    (line-end point)))

(define-command org-insert-todo-heading () ()
  "Insert a heading with the first todo keyword after the current subtree."
  (org-insert-todo-heading-command))

;;; Notes

(defun org-add-note (point text)
  "Add the note TEXT to the entry at POINT, with the time."
  (org-insert-log (heading-point point)
                  (log-entry-lines (log-note-text (clock-stamp-now)) text)))

(define-command org-add-note-command () ()
  "Add a time-stamped note to the logbook of the current entry."
  (org-add-note (current-point) (prompt-for-string "Note: ")))

;;; Statistics cookies

(define-command org-update-statistics-cookies () ()
  "Recompute all statistics cookies, [1/3] and [33%], in the buffer."
  (message "~D cookie~:P updated" (org-update-all-cookies (current-buffer))))

;;; Effort

(defun org-set-effort (point effort)
  (org-set-property point "Effort" effort))

(define-command org-set-effort-command () ()
  "Set the Effort property of the current entry, for example 1:30."
  (org-set-effort (current-point) (prompt-for-string "Effort (H:MM): ")))

;;; Priorities by letter

(define-command org-priority () ()
  "Set the priority of the current heading to a letter (empty to remove it)."
  (let* ((answer (string-upcase (string-trim " " (prompt-for-string "Priority (A-Z, empty for none): ")))))
    (org-edit-heading-line
     (current-point)
     (lambda (line)
       (multiple-value-bind (level todo priority title tags) (heading-split line (org-keywords))
         (declare (ignore priority))
         (unless level (editor-error "Not on a heading"))
         (heading-join level todo (and (plusp (length answer)) (char answer 0)) title tags))))))

(define-key *org-mode-keymap* "C-c C-x t" 'org-todo-select)
(define-key *org-mode-keymap* "C-c C-z" 'org-add-note-command)
(define-key *org-mode-keymap* "C-c #" 'org-update-statistics-cookies)
(define-key *org-mode-keymap* "C-c C-x e" 'org-set-effort-command)
(define-key *org-mode-keymap* "C-c ," 'org-priority)
(define-key *org-mode-keymap* "M-Shift-Return" 'org-insert-todo-heading)
