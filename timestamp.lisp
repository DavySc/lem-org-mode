;;; Timestamps: dates, parsing and formatting of <2026-10-22 Thu 14:30-15:30 +1w -3d>, shifting, repeaters,
;;; and reading a date the way a person types it ("fri", "+2w", "feb 3", "14:30").
;;;
;;; Everything here is pure (no buffers). Dates are (year month day) triples or day numbers, days since 1900-01-01.

(in-package :lem-org-mode)

;;; BEGIN pure timestamp code

(defun leap-year-p (year)
  (and (zerop (mod year 4)) (or (plusp (mod year 100)) (zerop (mod year 400)))))

(defun days-in-month (year month)
  (case month
    ((1 3 5 7 8 10 12) 31)
    ((4 6 9 11) 30)
    (t (if (leap-year-p year) 29 28))))

(defun date-day (year month day)
  "Days since 1900-01-01, so that dates can be compared and subtracted."
  (floor (encode-universal-time 0 0 12 day month year 0) 86400))

(defun day-date (days)
  "The inverse of `date-day': (values year month day)."
  (multiple-value-bind (s mi h day month year) (decode-universal-time (+ (* days 86400) 43200) 0)
    (declare (ignore s mi h))
    (values year month day)))

(defun day-weekday (days)
  "0 for Monday to 6 for Sunday."
  (mod (+ days 0) 7))

(defparameter *day-names* #("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))
(defparameter *month-names* #("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))

(defun day-name (days)
  (aref *day-names* (nth-value 6 (decode-universal-time (+ (* days 86400) 43200) 0))))

(defun date-shift (year month day count unit)
  "The date COUNT UNITs after YEAR-MONTH-DAY; UNIT is :d :w :m or :y. A day beyond the end of the month is
moved to the last day of it."
  (ecase unit
    ((:d :w) (day-date (+ (date-day year month day) (* count (if (eq unit :w) 7 1)))))
    ((:m :y)
     (let* ((months (+ (* year 12) (1- month) (* count (if (eq unit :y) 12 1))))
            (y (floor months 12))
            (m (1+ (mod months 12))))
       (values y m (min day (days-in-month y m)))))))

;;; Timestamp objects: (:active BOOL :year :month :day :hour :minute :end-hour :end-minute :repeater :warning)
;;; A repeater is (kind count unit) with kind :cumulate (+), :catch-up (++) or :restart (.+); a warning period
;;; (kind count unit) with kind :all (-) or :first (--).

(defun interval-parse (token)
  "Parse +1w, ++2d, .+3m, -3d or --1w into (kind count unit); NIL when TOKEN is none of them."
  (let* ((sign-end (position-if (lambda (c) (digit-char-p c)) token))
         (prefix (and sign-end (subseq token 0 sign-end)))
         (kind (cdr (assoc prefix '(("+" . :cumulate) ("++" . :catch-up) (".+" . :restart) ("-" . :all) ("--" . :first))
                           :test #'equal)))
         (unit-pos (and sign-end (position-if-not #'digit-char-p token :start sign-end)))
         (unit (and unit-pos (= unit-pos (1- (length token)))
                    (cdr (assoc (char token unit-pos) '((#\h . :h) (#\d . :d) (#\w . :w) (#\m . :m) (#\y . :y)))))))
    (and kind unit (list kind (parse-integer token :start sign-end :end unit-pos) unit))))

(defun interval-format (interval)
  (destructuring-bind (kind count unit) interval
    (format nil "~A~D~(~A~)" (ecase kind (:cumulate "+") (:catch-up "++") (:restart ".+") (:all "-") (:first "--"))
            count unit)))

(defun time-token-parse (token)
  "Parse 9:30 or 09:30-10:45 into (hour minute end-hour end-minute); the end may be NIL."
  (let* ((dash (position #\- token))
         (first (subseq token 0 dash))
         (colon (position #\: first)))
    (flet ((hm (text colon)
             (let ((h (parse-integer text :end colon :junk-allowed t))
                   (m (parse-integer text :start (1+ colon) :junk-allowed t)))
               (and h m (<= 0 h 24) (<= 0 m 59) (cons h m)))))
      (when (and colon (plusp colon) (every (lambda (c) (or (digit-char-p c) (char= c #\:))) first))
        (let ((start (hm first colon))
              (end (and dash (let* ((text (string-left-trim "-" (subseq token dash))) (c (position #\: text)))
                               (and c (hm text c))))))
          (and start (list (car start) (cdr start) (car end) (cdr end))))))))

(defun timestamp-parse (string &optional (start 0))
  "Parse the Org timestamp that begins at START in STRING, <...> (active) or [...] (inactive).
Return (values timestamp end) where END is the position after the closing bracket, or NIL."
  (when (and (< start (length string)) (find (char string start) "<["))
    (let* ((active (char= (char string start) #\<))
           (close (position (if active #\> #\]) string :start start)))
      (when (and close (>= (- close start) 11))
        (let* ((words (split-words (subseq string (1+ start) close)))
               (date (first words)))
          (when (and date (= (length date) 10) (char= (char date 4) #\-) (char= (char date 7) #\-))
            (let ((year (parse-integer date :end 4 :junk-allowed t))
                  (month (parse-integer date :start 5 :end 7 :junk-allowed t))
                  (day (parse-integer date :start 8 :junk-allowed t)))
              (when (and year month day (<= 1 month 12) (<= 1 day 31))
                (let ((ts (list :active active :year year :month month :day day
                                :hour nil :minute nil :end-hour nil :end-minute nil
                                :repeater nil :warning nil)))
                  (dolist (word (rest words))
                    (let ((time (time-token-parse word))
                          (interval (interval-parse word)))
                      (cond (time (setf (getf ts :hour) (first time) (getf ts :minute) (second time)
                                        (getf ts :end-hour) (third time) (getf ts :end-minute) (fourth time)))
                            ((and interval (member (first interval) '(:cumulate :catch-up :restart)))
                             (setf (getf ts :repeater) interval))
                            ((and interval (member (first interval) '(:all :first)))
                             (setf (getf ts :warning) interval))
                            ((position #\/ word)
                             (let ((interval (interval-parse (subseq word 0 (position #\/ word)))))
                               (when interval (setf (getf ts :repeater) interval)))))))
                  (values ts (1+ close)))))))))))

(defun timestamp-format (ts)
  "The text of the timestamp TS, with the name of the day."
  (let ((year (getf ts :year)) (month (getf ts :month)) (day (getf ts :day)))
    (format nil "~A~4,'0D-~2,'0D-~2,'0D ~A~@[ ~A~]~@[ ~A~]~@[ ~A~]~A"
            (if (getf ts :active) "<" "[") year month day (day-name (date-day year month day))
            (and (getf ts :hour)
                 (format nil "~2,'0D:~2,'0D~@[-~A~]" (getf ts :hour) (getf ts :minute)
                         (and (getf ts :end-hour) (format nil "~2,'0D:~2,'0D" (getf ts :end-hour) (getf ts :end-minute)))))
            (and (getf ts :repeater) (interval-format (getf ts :repeater)))
            (and (getf ts :warning) (interval-format (getf ts :warning)))
            (if (getf ts :active) ">" "]"))))

(defun timestamps-in-text (text)
  "Every timestamp in TEXT as (timestamp start end), in order."
  (let ((found '()) (i 0))
    (loop :while (< i (length text))
          :do (if (find (char text i) "<[")
                  (multiple-value-bind (ts end) (timestamp-parse text i)
                    (if ts
                        (progn (push (list ts i end) found) (setf i end))
                        (incf i)))
                  (incf i)))
    (nreverse found)))

(defun timestamp-day (ts)
  (date-day (getf ts :year) (getf ts :month) (getf ts :day)))

(defun timestamp-minutes (ts)
  "Minutes after midnight, or NIL for a timestamp without a time."
  (and (getf ts :hour) (+ (* 60 (getf ts :hour)) (getf ts :minute))))

(defun timestamp-with-day (ts day)
  (multiple-value-bind (y m d) (day-date day)
    (let ((new (copy-list ts)))
      (setf (getf new :year) y (getf new :month) m (getf new :day) d)
      new)))

(defun timestamp-add (ts count unit)
  "TS moved by COUNT UNITs (:h :d :w :m :y); hours carry into the date."
  (if (eq unit :h)
      (let* ((total (+ (* (timestamp-day ts) 1440) (or (timestamp-minutes ts) 0) (* count 60)))
             (day (floor total 1440)) (minutes (mod total 1440))
             (new (timestamp-with-day ts day)))
        (setf (getf new :hour) (floor minutes 60) (getf new :minute) (mod minutes 60))
        new)
      (multiple-value-bind (y m d) (date-shift (getf ts :year) (getf ts :month) (getf ts :day) count unit)
        (let ((new (copy-list ts)))
          (setf (getf new :year) y (getf new :month) m (getf new :day) d)
          new))))

(defun timestamp-before-p (ts day minutes)
  "True when TS lies at or before DAY, MINUTES (NIL means the end of the day) - by day and, with times, by time."
  (let ((d (timestamp-day ts)))
    (or (< d day)
        (and (= d day) (timestamp-minutes ts) minutes (<= (timestamp-minutes ts) minutes)))))

(defun timestamp-next-repeat (ts today &optional now-minutes)
  "The timestamp that follows TS after a completed repeat: + adds one interval, ++ adds until the date lies after
TODAY (a day number, with NOW-MINUTES for timestamps with times), .+ counts from TODAY."
  (destructuring-bind (kind count unit) (getf ts :repeater)
    (when (<= count 0) (editor-error "A repeater needs a positive interval"))
    (ecase kind
      (:cumulate (timestamp-add ts count unit))
      (:catch-up (let ((new (timestamp-add ts count unit)))
                   (loop :while (timestamp-before-p new today now-minutes)
                         :do (setf new (timestamp-add new count unit)))
                   new))
      (:restart (let ((base (timestamp-with-day ts today)))
                  (if (eq unit :h)
                      (let ((new (copy-list base)))
                        (setf (getf new :hour) (floor (or now-minutes 0) 60) (getf new :minute) (mod (or now-minutes 0) 60))
                        (timestamp-add new count unit))
                      (timestamp-add base count unit)))))))

(defun timestamp-occurrences (ts first-day last-day)
  "The days between FIRST-DAY and LAST-DAY (inclusive) on which TS falls: its own day, and with a repeater the
days it repeats on."
  (let ((start (timestamp-day ts)) (repeater (getf ts :repeater)))
    (if (null repeater)
        (and (<= first-day start last-day) (list start))
        (destructuring-bind (kind count unit) repeater
          (declare (ignore kind))
          (if (<= count 0)
              (and (<= first-day start last-day) (list start))
              (let ((days '()) (current ts))
                (loop :for day := (timestamp-day current)
                      :while (<= day last-day)
                      :do (when (>= day first-day) (push day days))
                          (setf current (timestamp-add current count (if (eq unit :h) :d unit))))
                (nreverse days)))))))

;;; Reading a date typed by a person

(defparameter *weekday-words*
  '(("mon" . 0) ("monday" . 0) ("tue" . 1) ("tuesday" . 1) ("wed" . 2) ("wednesday" . 2) ("thu" . 3) ("thursday" . 3)
    ("fri" . 4) ("friday" . 4) ("sat" . 5) ("saturday" . 5) ("sun" . 6) ("sunday" . 6)))

(defparameter *month-words*
  '(("jan" . 1) ("january" . 1) ("feb" . 2) ("february" . 2) ("mar" . 3) ("march" . 3) ("apr" . 4) ("april" . 4)
    ("may" . 5) ("jun" . 6) ("june" . 6) ("jul" . 7) ("july" . 7) ("aug" . 8) ("august" . 8)
    ("sep" . 9) ("sept" . 9) ("september" . 9) ("oct" . 10) ("october" . 10) ("nov" . 11) ("november" . 11)
    ("dec" . 12) ("december" . 12)))

(defun date-input-parse (input today &optional default-time)
  "Read INPUT, a date as a person types it, relative to TODAY (a day number). Return (values day hour minute
end-hour end-minute); HOUR and the rest are NIL unless the input has a time (or DEFAULT-TIME, (hour minute)).
Understood: empty or . (today), today, tomorrow, yesterday, +3d, -2w, ++1m (from today), a weekday name (the next
one), 2026-10-22, 10-22, 3/15 or 15 (the next such date), feb 3, 3 feb 2027, and a time such as 14:30 or 9am anywhere
in the input."
  (let* ((words (split-words (string-downcase (string-trim " " input))))
         (time nil) (day nil) (rest '()))
    (dolist (word words)
      (let ((parsed (or (time-token-parse word)
                        (let ((n (and (> (length word) 2) (member (subseq word (- (length word) 2)) '("am" "pm") :test #'string=)
                                      (parse-integer word :end (- (length word) 2) :junk-allowed t))))
                          (and n (<= 1 n 12)
                               (list (mod (+ (mod n 12) (if (string= (subseq word (- (length word) 2)) "pm") 12 0)) 24) 0 nil nil))))))
        (if (and parsed (null time)) (setf time parsed) (push word rest))))
    (setf rest (nreverse rest))
    (multiple-value-bind (ty tm td) (day-date today)
      (flet ((next-date (month day-of-month)
               (let ((candidate (date-day ty month (min day-of-month (days-in-month ty month)))))
                 (if (< candidate today)
                     (date-day (1+ ty) month (min day-of-month (days-in-month (1+ ty) month)))
                     candidate)))
             (fail () (editor-error "Cannot read the date: ~A" input)))
        (declare (ignorable tm td))
        (let ((text (format nil "~{~A~^ ~}" rest)))
          (setf day
                (cond
                  ((member text '("" "." "today" "now") :test #'string=) today)
                  ((string= text "tomorrow") (1+ today))
                  ((string= text "yesterday") (1- today))
                  ;; +3d -2w ++1m .+1d
                  ((and (member (char text 0) '(#\+ #\- #\.))
                        (let ((interval (interval-parse text)))
                          (when interval
                            (let ((n (second interval)) (unit (third interval)))
                              (multiple-value-bind (y m d)
                                  (date-shift ty tm td (if (char= (char text 0) #\-) (- n) n) (if (eq unit :h) :d unit))
                                (date-day y m d)))))))
                  ((assoc text *weekday-words* :test #'string=)
                   (let* ((target (cdr (assoc text *weekday-words* :test #'string=)))
                          (delta (mod (- target (day-weekday today)) 7)))
                     (+ today (if (zerop delta) 7 delta))))
                  ((and (= (length text) 10) (char= (char text 4) #\-) (char= (char text 7) #\-))
                   (let ((y (parse-integer text :end 4 :junk-allowed t)) (m (parse-integer text :start 5 :end 7 :junk-allowed t))
                         (d (parse-integer text :start 8 :junk-allowed t)))
                     (unless (and y m d (<= 1 m 12) (<= 1 d (days-in-month y m))) (fail))
                     (date-day y m d)))
                  ((and (find #\- text) (= (count #\- text) 1) (every (lambda (c) (or (digit-char-p c) (char= c #\-))) text))
                   (let* ((dash (position #\- text)) (m (parse-integer text :end dash :junk-allowed t))
                          (d (parse-integer text :start (1+ dash) :junk-allowed t)))
                     (unless (and m d (<= 1 m 12) (<= 1 d (days-in-month ty m))) (fail))
                     (next-date m d)))
                  ((and (find #\/ text) (every (lambda (c) (or (digit-char-p c) (char= c #\/))) text))
                   (let* ((slash (position #\/ text)) (m (parse-integer text :end slash :junk-allowed t))
                          (d (parse-integer text :start (1+ slash) :junk-allowed t)))
                     (unless (and m d (<= 1 m 12) (<= 1 d (days-in-month ty m))) (fail))
                     (next-date m d)))
                  ((and (every #'digit-char-p text) (<= 1 (length text) 2))
                   (let ((d (parse-integer text)))
                     (unless (<= 1 d 31) (fail))
                     ;; the next date with this day of month
                     (loop :for k :from 0 :below 14
                           :do (multiple-value-bind (y m) (date-shift ty tm 1 k :m)
                                 (when (and (<= d (days-in-month y m)) (>= (date-day y m d) today))
                                   (return (date-day y m d))))
                           :finally (fail))))
                  (t
                   ;; month name forms: "feb 3", "3 feb", "feb 3 2027", "3 feb 2027"
                   (let* ((month-word (find-if (lambda (w) (assoc w *month-words* :test #'string=)) rest))
                          (numbers (remove month-word rest :test #'equal)))
                     (unless (and month-word numbers (every (lambda (w) (every #'digit-char-p w)) numbers)) (fail))
                     (let* ((month (cdr (assoc month-word *month-words* :test #'string=)))
                            (d (parse-integer (first numbers)))
                            (year (and (second numbers) (parse-integer (second numbers)))))
                       (unless (<= 1 d (days-in-month (or year ty) month)) (fail))
                       (if year (date-day year month d) (next-date month d))))))))
        (let ((time (or time default-time)))
          (values day (first time) (second time) (third time) (fourth time)))))))

;;; END pure timestamp code
