;;; Timestamps in the text: insert one (C-c .), change the part under the cursor (S-Up / S-Down / S-Left / S-Right),
;;; time ranges (C-c C-y), and the Shift-arrow keys that choose between timestamps, priorities and todo states.

(in-package :lem-org-mode)

;;; BEGIN pure planning code

(defun timestamp-field-at (text column start)
  "Which part of the timestamp that begins at START in TEXT the character at COLUMN belongs to: :year :month :day
:hour :minute or :repeater (a name of a day or a repeater counts as :day)."
  (let ((offset (- column start)))
    (cond ((<= offset 4) :year)
          ((<= offset 7) :month)
          ((<= offset 10) :day)
          (t (let ((colon (position #\: text :start (min (length text) (+ start 11)))))
               (cond ((null colon) :day)
                     ((<= (- colon 2) column (1- colon)) :hour)
                     ((<= (1+ colon) column (+ colon 2)) :minute)
                     (t :day)))))))

(defun timestamp-adjust (ts field delta)
  "TS with FIELD (:year :month :day :hour :minute) moved by DELTA. Minutes move in steps of 5 as in Org."
  (ecase field
    (:year (timestamp-add ts delta :y))
    (:month (timestamp-add ts delta :m))
    (:day (timestamp-add ts delta :d))
    (:hour (if (timestamp-minutes ts)
               (timestamp-add ts delta :h)
               (timestamp-add ts delta :d)))
    (:minute (if (timestamp-minutes ts)
                 (let* ((total (+ (* (timestamp-day ts) 1440) (timestamp-minutes ts) (* 5 delta)))
                        (new (timestamp-with-day ts (floor total 1440))))
                   (setf (getf new :hour) (floor (mod total 1440) 60) (getf new :minute) (mod total 60))
                   new)
                 (timestamp-add ts delta :d)))))

(defun timestamp-range-duration (first second)
  "The text for the time between the timestamps FIRST and SECOND: 2d 3:30, 3:30, or 5d when neither has a time."
  (let* ((minutes (- (+ (* 1440 (timestamp-day second)) (or (timestamp-minutes second) 0))
                     (+ (* 1440 (timestamp-day first)) (or (timestamp-minutes first) 0))))
         (days (floor (abs minutes) 1440))
         (rest (mod (abs minutes) 1440)))
    (concatenate 'string
                 (if (minusp minutes) "-" "")
                 (if (plusp days) (format nil "~Dd " days) "")
                 (if (or (plusp rest) (and (plusp days) (timestamp-minutes first)))
                     (format nil "~D:~2,'0D" (floor rest 60) (mod rest 60))
                     ""))))

(defun line-with-range-duration (line)
  "LINE, which holds <a>--<b> or [a]--[b], with ` => duration' after the range. Text after the range is replaced."
  (loop :for (a b) :on (timestamps-in-text line)
        :when (and b (= (+ 2 (third a)) (second b)) (string= "--" line :start2 (third a) :end2 (+ 2 (third a))))
          :do (return-from line-with-range-duration
                (concatenate 'string (subseq line 0 (third b)) " => " (timestamp-range-duration (first a) (first b)))))
  line)

;;; END pure planning code

(defun timestamp-at-point (point)
  "Return (values timestamp start end) of the timestamp on POINT's line that contains or follows POINT."
  (let* ((line (line-string point)) (column (point-charpos point))
         (found (timestamps-in-text line))
         (hit (or (find-if (lambda (f) (<= (second f) column (third f))) found)
                  (find-if (lambda (f) (>= (second f) column)) found))))
    (and hit (values (first hit) (second hit) (third hit)))))

(defun replace-timestamp-at (point ts start end)
  (let ((line (line-string point)))
    (replace-lines (line-start (copy-point point :temporary)) 1
                   (list (concatenate 'string (subseq line 0 start) (timestamp-format ts) (subseq line end))))))

(defun org-timestamp-change (point delta)
  "Change the part of the timestamp at POINT that the cursor is on by DELTA. Return true when there was a timestamp."
  (multiple-value-bind (ts start end) (timestamp-at-point point)
    (when ts
      (let* ((line (line-string point))
             (column (point-charpos point))
             (field (if (< column start) :day (timestamp-field-at line (min column (1- end)) start)))
             (new (timestamp-adjust ts field delta)))
        (replace-timestamp-at point new start end)
        (setf (point-charpos point) (min column (1- (length (line-string point)))))
        t))))

(defun prompt-for-timestamp (&key active (initial nil))
  "Ask for a date and time; return a timestamp object."
  (let ((today-day (today-day)))
    (multiple-value-bind (day hour minute end-hour end-minute)
        (date-input-parse (prompt-for-string (format nil "Date (e.g. +3d, fri, 2026-10-22 14:30)~@[ [~A]~]: " initial)) today-day)
      (timestamp-with-day (list :active active :hour hour :minute minute :end-hour end-hour :end-minute end-minute) day))))

(defun org-insert-timestamp (point ts)
  "Insert the timestamp TS at POINT, or replace the one on the line that POINT is in."
  (multiple-value-bind (old start end) (timestamp-at-point point)
    (if (and old (<= start (point-charpos point) end))
        (progn (setf (getf ts :repeater) (getf old :repeater) (getf ts :warning) (getf old :warning))
               (replace-timestamp-at point ts start end))
        (insert-string point (timestamp-format ts)))))

(define-command org-time-stamp () ()
  "Insert an active timestamp, or change the one at point."
  (org-insert-timestamp (current-point) (prompt-for-timestamp :active t)))

(define-command org-time-stamp-inactive () ()
  "Insert an inactive timestamp, or change the one at point."
  (org-insert-timestamp (current-point) (prompt-for-timestamp :active nil)))

(define-command org-evaluate-time-range () ()
  "Put the length of the time range <a>--<b> on the line after it, as => 2d 3:30."
  (let* ((point (current-point)) (line (line-string point)) (new (line-with-range-duration line)))
    (when (string= new line) (editor-error "No time range on this line"))
    (replace-lines (line-start (copy-point point :temporary)) 1 (list new))))

(define-command org-shift-up () ()
  "On a timestamp change the part under the cursor, otherwise raise the priority."
  (unless (org-timestamp-change (current-point) 1)
    (org-cycle-priority (current-point) 1)))

(define-command org-shift-down () ()
  "On a timestamp change the part under the cursor, otherwise lower the priority."
  (unless (org-timestamp-change (current-point) -1)
    (org-cycle-priority (current-point) -1)))

(define-command org-shift-right () ()
  "On a timestamp go one day on, on a heading go to the next todo state."
  (cond ((multiple-value-bind (ts start end) (timestamp-at-point (current-point))
           (when ts (replace-timestamp-at (current-point) (timestamp-adjust ts :day 1) start end) t)))
        ((org-heading-level (current-point)) (org-todo-step (current-point) 1))
        (t (editor-error "Nothing to change here"))))

(define-command org-shift-left () ()
  "On a timestamp go one day back, on a heading go to the previous todo state."
  (cond ((multiple-value-bind (ts start end) (timestamp-at-point (current-point))
           (when ts (replace-timestamp-at (current-point) (timestamp-adjust ts :day -1) start end) t)))
        ((org-heading-level (current-point)) (org-todo-step (current-point) -1))
        (t (editor-error "Nothing to change here"))))

(define-key *org-mode-keymap* "C-c ." 'org-time-stamp)
(define-key *org-mode-keymap* "C-c !" 'org-time-stamp-inactive)
(define-key *org-mode-keymap* "C-c C-y" 'org-evaluate-time-range)
(define-key *org-mode-keymap* "Shift-Up" 'org-shift-up)
(define-key *org-mode-keymap* "Shift-Down" 'org-shift-down)
(define-key *org-mode-keymap* "Shift-Right" 'org-shift-right)
(define-key *org-mode-keymap* "Shift-Left" 'org-shift-left)
