;;; Agenda: day / week / month / year views of scheduled items, deadlines and timestamps (repeaters, ranges, time
;;; grid, log mode), the global todo list, tag / property / todo matching, text search, stuck projects, filters,
;;; custom commands, and the agenda buffer with commands that act on the entry under the cursor.
;;;
;;; The model (parsing, matching, building and laying out the lines) is pure and tested without Lem; the buffer
;;; commands are at the end.

(in-package :lem-org-mode)

;;; BEGIN pure agenda code

(defvar *org-agenda-files* '()
  "Files (or directories, whose .org files are used) that the agenda reads. When empty the current buffer is used.")

(defvar *org-agenda-span* 7
  "Length of the agenda: a number of days, or :day :week :fortnight :month :year.")

(defvar *org-agenda-start-on-weekday* 1
  "The weekday (0 Sunday, 1 Monday ...) on which a week agenda starts; NIL starts it today.")

(defvar *org-deadline-warning-days* 14
  "A deadline is shown on today's agenda when it is at most this many days away (unless it has its own -Nd).")

(defvar *org-agenda-skip-done* t
  "True: scheduled items, deadlines and timestamps of finished entries are left out of the agenda.")

(defvar *org-agenda-use-time-grid* t
  "True: a day with timed items also shows the grid lines of `*org-agenda-grid-times*'.")

(defvar *org-agenda-grid-times* '(800 1000 1200 1400 1600 1800 2000)
  "Times of the agenda time grid, as HHMM numbers.")

(defvar *org-tags-exclude-from-inheritance* '()
  "Tags that child entries do not inherit.")

(defvar *org-agenda-custom-commands* '()
  "Extra agenda commands for `org-agenda'. Each is (KEY DESCRIPTION TYPE MATCH [SETTINGS]) with TYPE agenda, alltodo,
todo, tags, tags-todo, search or stuck, or (KEY DESCRIPTION (BLOCK ...)) where a BLOCK is (TYPE MATCH [SETTINGS]).
SETTINGS is a plist: :span :start :files :log.")

(defvar *org-stuck-projects* '("+LEVEL=2/-DONE" ("TODO" "NEXT" "NEXTACTION") nil "")
  "What `org-agenda-list-stuck-projects' calls a project: (PROJECT-MATCH NON-STUCK-TODO-KEYWORDS NON-STUCK-TAGS REGEXP).
A project is stuck when nothing under it is in one of those states, has one of those tags or matches the regexp.")

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

(defun line-starts-with-ci-p (line prefix)
  (let ((s (string-left-trim '(#\space #\tab) line)))
    (and (>= (length s) (length prefix)) (string-equal prefix s :end2 (length prefix)))))

(defun split-text-lines (text)
  "The lines of TEXT as a list of strings, without line terminators."
  (loop :with start := 0
        :for end := (position #\newline text :start start)
        :collect (string-right-trim '(#\return) (subseq text start end))
        :while end
        :do (setf start (1+ end))))

;;; Regular expressions come from cl-ppcre when it is loaded (it is in Lem); without it a pattern is a substring.

(defun regex-search-p (pattern text &optional (case-insensitive t))
  (let ((scan (and (find-package "PPCRE") (find-symbol "SCAN" "PPCRE"))))
    (if scan
        (and (funcall scan (if case-insensitive (concatenate 'string "(?i)" pattern) pattern) text) t)
        (and (search pattern text :test (if case-insensitive #'char-equal #'char=)) t))))

;;; Parsing entries

(defun file-keyword-values (lines name)
  "The values of the #+NAME: lines of LINES."
  (loop :for line :in lines
        :for s := (string-left-trim " 	" line)
        :when (and (> (length s) (+ 3 (length name))) (string= "#+" s :end2 2)
                   (string-equal name s :start2 2 :end2 (+ 2 (length name)))
                   (char= (char s (+ 2 (length name))) #\:))
          :collect (string-trim " 	" (subseq s (+ 3 (length name))))))

(defun entry-property-alist (head)
  "The properties of the :PROPERTIES: drawer in the HEAD lines of an entry as (NAME . value), names in upper case."
  (let ((start (position-if (lambda (l) (string-equal (string-trim " 	" l) ":PROPERTIES:")) head)))
    (when start
      (loop :for line :in (nthcdr (1+ start) head)
            :for s := (string-trim " 	" line)
            :until (string-equal s ":END:")
            :when (and (> (length s) 2) (char= (char s 0) #\:) (position #\: s :start 1))
              :collect (let ((colon (position #\: s :start 1)))
                         (cons (string-upcase (subseq s 1 colon))
                               (string-trim " 	" (subseq s (1+ colon)))))))))

(defun parse-clock-line (line)
  "For a CLOCK line with a finished interval return (start-timestamp end-timestamp minutes), else NIL."
  (when (clock-line-p line)
    (let ((stamps (timestamps-in-text line)))
      (when (= (length stamps) 2)
        (let ((a (first (first stamps))) (b (first (second stamps))))
          (list a b (max 0 (- (+ (* 1440 (timestamp-day b)) (or (timestamp-minutes b) 0))
                              (+ (* 1440 (timestamp-day a)) (or (timestamp-minutes a) 0))))))))))

(defun active-timestamps-with-ranges (text)
  "The active timestamps in TEXT: (values singles ranges), a range being (first last) for <a>--<b>."
  (let ((found (remove-if-not (lambda (f) (getf (first f) :active)) (timestamps-in-text text)))
        (singles '()) (ranges '()))
    (loop :while found
          :do (let ((f (pop found)))
                (if (and found
                         (= (+ 2 (third f)) (second (first found)))
                         (string= "--" text :start2 (third f) :end2 (+ 2 (third f))))
                    (push (list (first f) (first (pop found))) ranges)
                    (push (first f) singles))))
    (values (nreverse singles) (nreverse ranges))))

(defun body-timestamps (heading-line body)
  "The active timestamps of an entry's title and text, outside drawers, blocks and planning lines, as
(values singles ranges)."
  (let ((singles '()) (ranges '()) (in-drawer nil) (in-block nil))
    (flet ((scan (text)
             (multiple-value-bind (s r) (active-timestamps-with-ranges text)
               (setf singles (append singles s) ranges (append ranges r)))))
      (scan heading-line)
      (dolist (line body)
        (let ((s (string-trim " 	" line)))
          (cond (in-block (when (line-starts-with-ci-p s "#+end_") (setf in-block nil)))
                (in-drawer (when (string-equal s ":END:") (setf in-drawer nil)))
                ((line-starts-with-ci-p s "#+begin_") (setf in-block t))
                ((and (> (length s) 2) (char= (char s 0) #\:) (char= (char s (1- (length s))) #\:)
                      (not (find #\space s)))
                 (unless (string-equal s ":END:") (setf in-drawer t)))
                ((planning-line-p line))
                ((clock-line-p line))
                (t (scan line))))))
    (values singles ranges)))

(defun heading-indexes-of (v keywords)
  "The indexes of the heading lines in the vector V, leaving out heading-like lines inside #+begin_ blocks."
  (let ((indexes '()) (in-block nil))
    (dotimes (i (length v))
      (let ((line (aref v i)))
        (cond (in-block (when (line-starts-with-ci-p line "#+end_") (setf in-block nil)))
              ((line-starts-with-ci-p line "#+begin_") (setf in-block t))
              ((heading-split line keywords) (push i indexes)))))
    (nreverse indexes)))

(defun agenda-parse (lines keywords source &key (done (last keywords)) file-category)
  "Return the entries of the Org text LINES as property lists. Heading-like lines inside #+begin_ blocks are skipped.
KEYWORDS are all todo keywords, DONE those that finish a task. An entry has :source :line :level :todo :priority
:title :tags (its own) :all-tags (with inherited and file tags) :done :scheduled :deadline :closed (the text) and
:scheduled-ts :deadline-ts (parsed), :timestamps and :ranges (plain active timestamps), :clocks, :props, :category,
:effort (minutes), :path (titles of the ancestors), :body (the text lines) and :subtree-end (the number of the line
after the subtree)."
  (let* ((v (coerce lines 'vector)) (n (length v))
         (file-tags (loop :for value :in (file-keyword-values lines "FILETAGS") :append (parse-tag-input value)))
         (default-category (or file-category (first (file-keyword-values lines "CATEGORY")) (agenda-category source)))
         (heading-indexes (heading-indexes-of v keywords))
         (levels (mapcar (lambda (i) (heading-split (aref v i) keywords)) heading-indexes))
         (stack '()) (entries '()))
    (loop :for (index . later) :on heading-indexes
          :for (level . later-levels) :on levels
          :do (multiple-value-bind (level todo priority title tags) (heading-split (aref v index) keywords)
                (let* ((next (first later))
                       (body (coerce (subseq v (1+ index) (or next n)) 'list))
                       (head-lines (cons (aref v index) body))
                       (head (subseq head-lines 0 (head-length head-lines)))
                       (planning (and (second head) (planning-line-p (second head)) (planning-parse (second head))))
                       (props (entry-property-alist head))
                       (scheduled (cdr (assoc "SCHEDULED" planning :test #'string=)))
                       (deadline (cdr (assoc "DEADLINE" planning :test #'string=)))
                       (subtree-next (loop :for j :in later :for l :in later-levels
                                           :when (<= l level) :return j))
                       (inherited-tags nil))
                  (setf stack (remove-if (lambda (e) (>= (getf e :level) level)) stack))
                  (setf inherited-tags (loop :for e :in (reverse stack)
                                             :append (remove-if (lambda (tag) (member tag *org-tags-exclude-from-inheritance*
                                                                                      :test #'string=))
                                                                (getf e :tags))))
                  (multiple-value-bind (singles ranges) (body-timestamps (aref v index) body)
                    (push (list :source source :line (1+ index) :level level :todo todo
                                :priority priority :title title :tags tags
                                :all-tags (remove-duplicates (append tags inherited-tags file-tags) :test #'string= :from-end t)
                                :done (and todo (member todo done :test #'string=) t)
                                :scheduled scheduled :deadline deadline
                                :closed (cdr (assoc "CLOSED" planning :test #'string=))
                                :scheduled-ts (and scheduled (timestamp-parse scheduled))
                                :deadline-ts (and deadline (timestamp-parse deadline))
                                :timestamps singles :ranges ranges
                                :clocks (remove nil (mapcar #'parse-clock-line body))
                                :props props
                                :category (or (cdr (assoc "CATEGORY" props :test #'string=))
                                              (some (lambda (e) (getf e :category-prop)) stack)
                                              default-category)
                                :effort (effort-minutes (cdr (assoc "EFFORT" props :test #'string=)))
                                :path (mapcar (lambda (e) (getf e :title)) (reverse stack))
                                :body body
                                :subtree-end (1+ (or subtree-next n)))
                          entries)
                    (push (list :level level :tags tags :title title
                                :category-prop (cdr (assoc "CATEGORY" props :test #'string=)))
                          stack)))))
    (nreverse entries)))

(defun priority-rank (priority)
  "0 for the highest priority up; entries without one rank as the default priority."
  (- (char-code (or priority *org-priority-default*)) (char-code *org-priority-highest*)))

(defun agenda-rank (entry)
  (priority-rank (getf entry :priority)))

(defun agenda-sort (items key)
  "Stable sort of ITEMS by the priority of their entries, highest first."
  (stable-sort (copy-list items) #'< :key (lambda (item) (agenda-rank (funcall key item)))))

(defun agenda-entry-text (entry)
  (format nil "~{~A~^ ~}"
          (remove nil (list (getf entry :todo)
                            (and (getf entry :priority) (format nil "[#~A]" (getf entry :priority)))
                            (getf entry :title)
                            (and (getf entry :tags) (format nil ":~{~A~^:~}:" (getf entry :tags)))))))

(defun entry-day (entry key)
  (let ((ts (getf entry (case key (:scheduled :scheduled-ts) (:deadline :deadline-ts) (t key)))))
    (cond ((and ts (listp ts) (getf ts :year)) (timestamp-day ts))
          ((stringp (getf entry key))
           (multiple-value-bind (y m d) (stamp-date (getf entry key)) (and y (date-day y m d)))))))

(defun agenda-category (source)
  "A short name for SOURCE, a file name or a buffer name."
  (let* ((slash (position-if (lambda (c) (member c '(#\/ #\\))) source :from-end t))
         (name (if slash (subseq source (1+ slash)) source))
         (dot (position #\. name :from-end t)))
    (if (and dot (plusp dot)) (subseq name 0 dot) name)))

;;; Matching tags, properties and todo states:  +work-home+PRIORITY="A"|boss/TODO|NEXT

(defvar *match-today* 0 "The day number that <today> stands for in matchers.")

(defun match-split (string)
  "Split STRING at the first / outside quotes and braces: (values tags-part todo-part-or-NIL)."
  (let ((quoted nil) (brace 0))
    (loop :for i :from 0 :below (length string)
          :for c := (char string i)
          :do (cond ((char= c #\") (setf quoted (not quoted)))
                    (quoted)
                    ((char= c #\{) (incf brace))
                    ((char= c #\}) (decf brace))
                    ((and (char= c #\/) (zerop brace))
                     (return-from match-split (values (subseq string 0 i) (subseq string (1+ i)))))))
    (values string nil)))

(defun match-parse-tags (string)
  "Parse the tag / property part into a list of OR alternatives, each a list of (sign . term); a term is (:tag name),
(:tag-regex re) or (:prop name operator value) with VALUE (:string s), (:number n) or (:regex re)."
  (let ((i 0) (n (length string)) (groups '()) (current '()))
    (labels ((peek () (and (< i n) (char string i)))
             (name-char-p (c) (or (alphanumericp c) (find c "_@#%.")))
             (read-word () (let ((start i)) (loop :while (and (< i n) (name-char-p (char string i))) :do (incf i))
                             (subseq string start i)))
             (read-braced () (incf i)
               (let ((start i))
                 (loop :while (and (< i n) (char/= (char string i) #\})) :do (incf i))
                 (prog1 (subseq string start i) (incf i))))
             (read-op () (let ((op (find-if (lambda (o) (and (<= (+ i (length o)) n)
                                                             (string= o string :start2 i :end2 (+ i (length o)))))
                                            '("<>" "<=" ">=" "==" "!=" "=" "<" ">"))))
                           (when op (incf i (length op)) op)))
             (read-value ()
               (cond ((eql (peek) #\")
                      (incf i)
                      (let ((start i))
                        (loop :while (and (< i n) (char/= (char string i) #\")) :do (incf i))
                        (prog1 (list :string (subseq string start i)) (incf i))))
                     ((eql (peek) #\{) (list :regex (read-braced)))
                     (t (let ((start i))
                          (loop :while (and (< i n) (or (alphanumericp (char string i)) (find (char string i) ".:_")))
                                :do (incf i))
                          (let ((text (subseq string start i)))
                            (if (and (plusp (length text)) (every #'digit-char-p text))
                                (list :number (parse-integer text))
                                (list :string text)))))))
             (finish () (when current (push (nreverse current) groups) (setf current '()))))
      (loop :while (< i n)
            :do (let ((c (peek)))
                  (cond ((char= c #\|) (incf i) (finish))
                        ((find c "& ") (incf i))
                        (t (let ((sign #\+))
                             (when (find c "+-") (setf sign c) (incf i))
                             (if (eql (peek) #\{)
                                 (push (cons sign (list :tag-regex (read-braced))) current)
                                 (let ((word (read-word)))
                                   (when (zerop (length word))
                                     (editor-error "Cannot read the match near ~S" (subseq string (min i n))))
                                   (let ((op (read-op)))
                                     (push (cons sign (if op
                                                          (list :prop word op (read-value))
                                                          (list :tag word)))
                                           current)))))))))
      (finish)
      (nreverse groups))))

(defun match-parse-todo (string)
  "Parse the part after / into (values includes excludes only-open)."
  (let ((string (string-trim " " string)) (includes '()) (excludes '()) (open nil))
    (if (string= string "!")
        (setf open t)
        (let ((i 0) (n (length string)))
          (loop :while (< i n)
                :do (let ((c (char string i)))
                      (if (find c "|& ")
                          (incf i)
                          (let ((negative (char= c #\-)))
                            (when (find c "+-") (incf i))
                            (let ((start i))
                              (loop :while (and (< i n) (not (find (char string i) "|&-+ "))) :do (incf i))
                              (let ((word (subseq string start i)))
                                (when (plusp (length word))
                                  (if negative (push word excludes) (push word includes)))))))))))
    (values (nreverse includes) (nreverse excludes) open)))

(defun match-date-value (text)
  "The day number of a date in a matcher: <2026-10-22>, <today>, <+1w>, <-3d>."
  (let ((inner (string-trim "<>[] " text)))
    (cond ((string-equal inner "today") *match-today*)
          ((and (plusp (length inner)) (find (char inner 0) "+-"))
           (let ((interval (interval-parse inner)))
             (and interval
                  (multiple-value-bind (y m d) (day-date *match-today*)
                    (multiple-value-bind (y2 m2 d2)
                        (date-shift y m d (if (char= (char inner 0) #\-) (- (second interval)) (second interval))
                                    (if (eq (third interval) :h) :d (third interval)))
                      (date-day y2 m2 d2))))))
          (t (let ((ts (timestamp-parse (format nil "<~A>" inner)))) (and ts (timestamp-day ts)))))))

(defun entry-special-value (entry name)
  "(values value kind) of the property NAME of ENTRY; KIND is :string, :number, :date or :priority."
  (cond ((string-equal name "TODO") (values (getf entry :todo) :string))
        ((string-equal name "PRIORITY") (values (or (getf entry :priority) *org-priority-default*) :priority))
        ((string-equal name "LEVEL") (values (getf entry :level) :number))
        ((string-equal name "CATEGORY") (values (getf entry :category) :string))
        ((string-equal name "ITEM") (values (getf entry :title) :string))
        ((string-equal name "FILE") (values (getf entry :source) :string))
        ((string-equal name "SCHEDULED") (values (entry-day entry :scheduled) :date))
        ((string-equal name "DEADLINE") (values (entry-day entry :deadline) :date))
        ((string-equal name "CLOSED")
         (values (let ((c (getf entry :closed)))
                   (and c (multiple-value-bind (y m d) (stamp-date (substitute #\< #\[ c)) (and y (date-day y m d)))))
                 :date))
        ((string-equal name "TIMESTAMP")
         (values (let ((ts (first (getf entry :timestamps)))) (and ts (timestamp-day ts))) :date))
        ((string-equal name "EFFORT") (values (getf entry :effort) :number))
        (t (values (cdr (assoc (string-upcase name) (getf entry :props) :test #'string=)) :string))))

(defun compare-values (op a b)
  (cond ((member op '("=" "==") :test #'string=) (equal a b))
        ((member op '("<>" "!=") :test #'string=) (not (equal a b)))
        ((string= op "<") (< a b))
        ((string= op ">") (> a b))
        ((string= op "<=") (<= a b))
        ((string= op ">=") (>= a b))))

(defun compare-strings (op a b)
  (cond ((member op '("=" "==") :test #'string=) (string= a b))
        ((member op '("<>" "!=") :test #'string=) (string/= a b))
        ((string= op "<") (and (string< a b) t))
        ((string= op ">") (and (string> a b) t))
        ((string= op "<=") (and (string<= a b) t))
        ((string= op ">=") (and (string>= a b) t))))

(defun match-prop-p (entry name op value)
  (multiple-value-bind (actual kind) (entry-special-value entry name)
    (if (null actual)
        (and (member op '("<>" "!=") :test #'string=) t)
        (ecase (first value)
          (:regex (let ((hit (regex-search-p (second value) (princ-to-string (if (characterp actual) (string actual) actual)))))
                    (if (member op '("<>" "!=") :test #'string=) (not hit) hit)))
          (:number (let ((x (if (numberp actual) actual (parse-integer (princ-to-string actual) :junk-allowed t))))
                     (and x (compare-values op x (second value)))))
          (:string (let ((text (second value)))
                     (case kind
                       (:date (let ((day (match-date-value text))) (and day (compare-values op actual day))))
                       (:priority (compare-values op (char-code actual) (char-code (char text 0))))
                       (:number (let ((x (or (effort-minutes text) (parse-integer text :junk-allowed t))))
                                  (and x (compare-values op actual x))))
                       (t (compare-strings op (princ-to-string actual) text)))))))))

(defun match-term-p (entry term)
  (ecase (first term)
    (:tag (and (member (second term) (getf entry :all-tags) :test #'string-equal) t))
    (:tag-regex (and (some (lambda (tag) (regex-search-p (second term) tag nil)) (getf entry :all-tags)) t))
    (:prop (match-prop-p entry (second term) (third term) (fourth term)))))

(defun match-compile (string &optional (today *match-today*))
  "A predicate on entries for the matcher STRING, such as \"+work-home/TODO|NEXT\"."
  (multiple-value-bind (tag-part todo-part) (match-split string)
    (let ((groups (match-parse-tags (string-trim " " tag-part))))
      (multiple-value-bind (includes excludes open) (if todo-part (match-parse-todo todo-part) (values nil nil nil))
        (lambda (entry)
          (let ((*match-today* today))
            (and (or (null groups)
                     (some (lambda (group)
                             (every (lambda (signed)
                                      (let ((hit (match-term-p entry (cdr signed))))
                                        (if (char= (car signed) #\-) (not hit) hit)))
                                    group))
                           groups))
                 (or (null todo-part)
                     (let ((todo (getf entry :todo)))
                       (and todo
                            (or (not open) (not (getf entry :done)))
                            (or (null includes) (member todo includes :test #'string=))
                            (not (member todo excludes :test #'string=))
                            t))))))))))

;;; Filters (the / command in the agenda): (:tag name include-p), (:regexp text include-p), (:effort op minutes)

(defun filter-parse (string)
  "Read +tag -tag =regexp <30 >1:00 words into a filter."
  (let ((filter '()))
    (dolist (word (split-words string) (nreverse filter))
      (let ((sign (char word 0)) (rest (subseq word 1)))
        (case sign
          (#\+ (push (list :tag rest t) filter))
          (#\- (push (list :tag rest nil) filter))
          (#\= (push (list :regexp rest t) filter))
          (#\< (push (list :effort "<" (or (effort-minutes rest) 0)) filter))
          (#\> (push (list :effort ">" (or (effort-minutes rest) 0)) filter))
          (t (push (list :tag word t) filter)))))))

(defun filter-entry-p (filter entry)
  (every (lambda (rule)
           (ecase (first rule)
             (:tag (let ((has (and (member (second rule) (getf entry :all-tags) :test #'string-equal) t)))
                     (if (third rule) has (not has))))
             (:regexp (regex-search-p (second rule) (agenda-entry-text entry)))
             (:effort (let ((effort (getf entry :effort)))
                        (and effort (if (string= (second rule) "<") (< effort (third rule)) (> effort (third rule))))))))
         filter))

;;; Items on a day

(defun warning-days (ts default)
  (let ((warning (getf ts :warning)))
    (if warning
        (* (second warning) (ecase (third warning) (:d 1) (:h 0) (:w 7) (:m 30) (:y 365)))
        default)))

(defun item-time-text (ts)
  (and ts (getf ts :hour)
       (format nil "~D:~2,'0D~@[-~A~]" (getf ts :hour) (getf ts :minute)
               (and (getf ts :end-hour) (format nil "~D:~2,'0D" (getf ts :end-hour) (getf ts :end-minute))))))

(defun make-item (label entry type ts)
  (list :label label :entry entry :type type :time (and ts (timestamp-minutes ts))
        :end-time (and ts (getf ts :end-hour) (+ (* 60 (getf ts :end-hour)) (getf ts :end-minute)))
        :time-text (item-time-text ts)))

(defun entry-day-items (entry day today warning-days log)
  "The items of ENTRY for DAY (see `agenda-day-items')."
  (let ((items '()) (sts (getf entry :scheduled-ts)) (dts (getf entry :deadline-ts)))
    (unless (or (member "ARCHIVE" (getf entry :all-tags) :test #'string=)
                (and *org-agenda-skip-done* (getf entry :done)))
      (when sts
        (let* ((delay (if (getf sts :warning) (warning-days sts 0) 0))
               (shifted (timestamp-with-day sts (+ (timestamp-day sts) delay))))
          (cond ((timestamp-occurrences shifted day day)
                 (push (make-item "Scheduled:" entry :scheduled sts) items))
                ((and (= day today) (< (timestamp-day shifted) today))
                 (push (make-item (format nil "Sched.~Dx:" (- today (timestamp-day shifted))) entry :scheduled nil)
                       items)))))
      (when dts
        (cond ((timestamp-occurrences dts day day)
               (push (make-item "Deadline:" entry :deadline dts) items))
              ((and (= day today) (< today (timestamp-day dts) (+ today 1 (warning-days dts warning-days))))
               (push (make-item (format nil "In ~D d.:" (- (timestamp-day dts) today)) entry :deadline nil) items))
              ((and (= day today) (< (timestamp-day dts) today))
               (push (make-item (format nil "~D d. ago:" (- today (timestamp-day dts))) entry :deadline nil) items))))
      (dolist (ts (getf entry :timestamps))
        (when (timestamp-occurrences ts day day)
          (push (make-item "" entry :timestamp ts) items)))
      (dolist (range (getf entry :ranges))
        (let ((first-day (timestamp-day (first range))) (last-day (timestamp-day (second range))))
          (when (<= first-day day last-day)
            (push (make-item (format nil "(~D/~D):" (1+ (- day first-day)) (1+ (- last-day first-day)))
                             entry :range
                             (cond ((= day first-day) (first range)) ((= day last-day) (second range))))
                  items)))))
    (when log
      (when (and (getf entry :closed) (eql (entry-day entry :closed) day))
        (push (make-item "Closed:" entry :closed
                          (timestamp-parse (substitute #\< #\[ (substitute #\> #\] (getf entry :closed)))))
              items))
      (dolist (clock (getf entry :clocks))
        (when (= (timestamp-day (first clock)) day)
          (push (make-item (format nil "Clocked: (~A)" (string-left-trim " " (format-clock-duration (third clock))))
                           entry :clock (first clock))
                items))))
    (nreverse items)))

(defun agenda-day-items (entries day today warning-days &key log)
  "The items for DAY of an agenda seen from TODAY (day numbers): plists with :label :entry :type :time :time-text.
Scheduled items and deadlines that are overdue, and deadlines that approach, appear on today's line only.
Timed items come first by time, then by priority."
  (let ((items (loop :for entry :in entries :append (entry-day-items entry day today warning-days log))))
    (stable-sort (stable-sort items #'< :key (lambda (item) (agenda-rank (getf item :entry))))
                 #'< :key (lambda (item) (or (getf item :time) 100000)))))

(defun agenda-line (source label text &optional (time "") category)
  (format nil "  ~10A ~11A ~11A ~A" (concatenate 'string (or category (agenda-category source)) ":") time label text))

(defun iso-week (day)
  (let* ((thursday (+ (- day (day-weekday day)) 3))
         (year (day-date thursday)))
    (1+ (floor (- thursday (date-day year 1 1)) 7))))

(defun agenda-grid-lines (items)
  "Grid lines (minutes . text) for a day whose ITEMS include timed ones, leaving out times that an item occupies."
  (when (and *org-agenda-use-time-grid* (some (lambda (item) (getf item :time)) items))
    (loop :for hhmm :in *org-agenda-grid-times*
          :for minutes := (+ (* 60 (floor hhmm 100)) (mod hhmm 100))
          :unless (some (lambda (item)
                          (let ((start (getf item :time)))
                            (and start (<= start minutes) (< minutes (max (1+ start) (or (getf item :end-time) start))))))
                        items)
            :collect (cons minutes (agenda-line "" "" "----------------" (format nil "~D:~2,'0D......" (floor minutes 60) (mod minutes 60)) "")))))

(defun day-header (day)
  (multiple-value-bind (year month date) (day-date day)
    (format nil "~A ~2D ~A ~D"
            (aref #("Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "Sunday") (day-weekday day))
            date
            (aref #("January" "February" "March" "April" "May" "June" "July" "August" "September" "October" "November" "December")
                  (1- month))
            year)))

(defun agenda-span-range (start span)
  "Return (values first-day count) for an agenda that starts at day START with SPAN (a number of days or a keyword).
Months and years start on their first day."
  (multiple-value-bind (year month) (day-date start)
    (case span
      (:day (values start 1))
      (:week (values start 7))
      (:fortnight (values start 14))
      (:month (values (date-day year month 1) (days-in-month year month)))
      (:year (values (date-day year 1 1) (if (leap-year-p year) 366 365)))
      (t (values start (max 1 span))))))

(defun agenda-shift-start (start span direction)
  "The start day after moving the agenda forward (1) or backward (-1) by one SPAN."
  (multiple-value-bind (year month) (day-date start)
    (case span
      (:month (multiple-value-bind (y m) (date-shift year month 1 direction :m) (date-day y m 1)))
      (:year (date-day (+ year direction) 1 1))
      (t (+ start (* direction (nth-value 1 (agenda-span-range start span))))))))

(defun agenda-default-start (today span)
  "The first day shown by an agenda for TODAY: the start of the week for a week agenda."
  (if (and (member span '(:week 7)) *org-agenda-start-on-weekday*)
      (- today (mod (- (day-weekday today) (mod (1- *org-agenda-start-on-weekday*) 7)) 7))
      today))

(defun agenda-title (start span)
  (let ((name (case span (:day "Day") (:week "Week") (:fortnight "Fortnight") (:month "Month") (:year "Year")
                (t (if (eql span 7) "Week" "Span")))))
    (if (member span '(:week 7))
        (format nil "~A-agenda (W~D):" name (iso-week start))
        (format nil "~A-agenda:" name))))

(defun agenda-week-records (entries start span today warning-days &key log filter)
  "Return a list of (line-text . entry-or-nil) for the agenda that starts at day START and lasts SPAN, seen from TODAY."
  (let ((records '()) (entries (if filter (remove-if-not (lambda (e) (filter-entry-p filter e)) entries) entries)))
    (multiple-value-bind (first count) (agenda-span-range start span)
      (dotimes (i count)
        (let* ((day (+ first i))
               (items (agenda-day-items entries day today warning-days :log log))
               (pending-grid (agenda-grid-lines items)))
          (push (cons (if (= day today) (format nil "~A  <today>" (day-header day)) (day-header day)) nil) records)
          (flet ((flush-grid (upto)
                   (loop :while (and pending-grid (or (null upto) (<= (car (first pending-grid)) upto)))
                         :do (push (cons (cdr (pop pending-grid)) nil) records))))
            (dolist (item items)
              (when (getf item :time) (flush-grid (getf item :time)))
              (let ((entry (getf item :entry)))
                (push (cons (agenda-line (getf entry :source) (getf item :label) (agenda-entry-text entry)
                                         (or (getf item :time-text) "") (getf entry :category))
                            entry)
                      records)))
            (flush-grid nil)))))
    (nreverse records)))

(defun agenda-entry-record (entry)
  (cons (agenda-line (getf entry :source) "" (agenda-entry-text entry) "" (getf entry :category)) entry))

(defun agenda-visible-entries (entries filter)
  (remove-if (lambda (entry)
               (or (member "ARCHIVE" (getf entry :all-tags) :test #'string=)
                   (and filter (not (filter-entry-p filter entry)))))
             entries))

(defun agenda-todo-records (entries &key keywords filter)
  "Return (line-text . entry) for every entry in a not-done todo state (or in one of KEYWORDS), highest priority first."
  (mapcar #'agenda-entry-record
          (agenda-sort (remove-if-not (lambda (entry)
                                        (and (getf entry :todo)
                                             (if keywords
                                                 (member (getf entry :todo) keywords :test #'string=)
                                                 (not (getf entry :done)))))
                                      (agenda-visible-entries entries filter))
                       #'identity)))

(defun agenda-match-records (entries match &key todo-only filter (today 0))
  "Return (line-text . entry) for the entries that the matcher MATCH selects (and, with TODO-ONLY, are open tasks)."
  (let ((predicate (match-compile match today)))
    (mapcar #'agenda-entry-record
            (agenda-sort (remove-if-not (lambda (entry)
                                          (and (funcall predicate entry)
                                               (or (not todo-only) (and (getf entry :todo) (not (getf entry :done))))))
                                        (agenda-visible-entries entries filter))
                         #'identity))))

(defun agenda-tag-records (entries tag &key filter)
  (agenda-match-records entries (concatenate 'string "+" tag) :filter filter))

(defun search-terms (string)
  "Split a search STRING into (sign kind text) terms: +word -word \"a phrase\" {regexp}."
  (let ((terms '()) (i 0) (n (length string)))
    (loop :while (< i n)
          :do (let ((c (char string i)))
                (if (char= c #\space)
                    (incf i)
                    (let ((sign #\+))
                      (when (find c "+-") (setf sign c) (incf i))
                      (cond ((and (< i n) (char= (char string i) #\"))
                             (let ((close (or (position #\" string :start (1+ i)) n)))
                               (push (list sign :phrase (subseq string (1+ i) close)) terms)
                               (setf i (min n (1+ close)))))
                            ((and (< i n) (char= (char string i) #\{))
                             (let ((close (or (position #\} string :start (1+ i)) n)))
                               (push (list sign :regex (subseq string (1+ i) close)) terms)
                               (setf i (min n (1+ close)))))
                            (t (let ((end (or (position #\space string :start i) n)))
                                 (when (> end i) (push (list sign :word (subseq string i end)) terms))
                                 (setf i end))))))))
    (nreverse terms)))

(defun entry-search-text (entry)
  (format nil "~{~A~^~%~}" (cons (agenda-entry-text entry) (getf entry :body))))

(defun agenda-search-records (entries string &key filter)
  "Entries whose heading or text matches every term of STRING."
  (let ((terms (search-terms string)))
    (mapcar #'agenda-entry-record
            (remove-if-not (lambda (entry)
                             (let ((text (entry-search-text entry)))
                               (every (lambda (term)
                                        (let ((hit (if (eq (second term) :regex)
                                                       (regex-search-p (third term) text)
                                                       (and (search (third term) text :test #'char-equal) t))))
                                          (if (char= (first term) #\-) (not hit) hit)))
                                      terms)))
                           (agenda-visible-entries entries filter)))))

(defun entry-descendants (entries entry)
  "The entries in the subtree of ENTRY (same source), excluding ENTRY."
  (remove-if-not (lambda (other)
                   (and (equal (getf other :source) (getf entry :source))
                        (> (getf other :line) (getf entry :line))
                        (< (getf other :line) (getf entry :subtree-end))))
                 entries))

(defun agenda-stuck-records (entries &optional (setting *org-stuck-projects*) (today 0))
  "Projects (by the matcher in SETTING) that have no next action under them."
  (destructuring-bind (match todo-keywords tags regexp) setting
    (let ((projects (remove-if-not (match-compile match today) (agenda-visible-entries entries nil))))
      (mapcar #'agenda-entry-record
              (remove-if (lambda (project)
                           (some (lambda (child)
                                   (or (and (getf child :todo) (member (getf child :todo) todo-keywords :test #'string=)
                                            (not (getf child :done)))
                                       (some (lambda (tag) (member tag (getf child :all-tags) :test #'string-equal)) tags)
                                       (and regexp (plusp (length regexp)) (regex-search-p regexp (entry-search-text child)))))
                                 (entry-descendants entries project)))
                         projects)))))

(defun agenda-view-records (spec entries today)
  "Return (values title records) for the view SPEC, a plist with :type (:agenda :alltodo :todo :tags :tags-todo :search
:stuck), :match, :start, :span, :log and :filter."
  (let ((filter (getf spec :filter)) (match (or (getf spec :match) "")))
    (ecase (getf spec :type)
      (:agenda
       (let* ((span (or (getf spec :span) *org-agenda-span*))
              (start (or (getf spec :start) (agenda-default-start today span))))
         (values (agenda-title start span)
                 (agenda-week-records entries start span today *org-deadline-warning-days*
                                      :log (getf spec :log) :filter filter))))
      (:alltodo (values "Global TODO list" (agenda-todo-records entries :filter filter)))
      (:todo (values (format nil "TODO entries: ~A" match)
                     (agenda-todo-records entries :keywords (split-words (substitute #\space #\| match)) :filter filter)))
      (:tags (values (format nil "Headlines with TAGS match: ~A" match)
                     (agenda-match-records entries match :filter filter :today today)))
      (:tags-todo (values (format nil "Global list of TODO items of type: ~A" match)
                          (agenda-match-records entries match :todo-only t :filter filter :today today)))
      (:search (values (format nil "Search words: ~A" match) (agenda-search-records entries match :filter filter)))
      (:stuck (values "List of stuck projects" (agenda-stuck-records entries *org-stuck-projects* today))))))

;;; END pure agenda code

;;; The agenda buffer

(defun agenda-source-files (&optional (files *org-agenda-files*))
  (loop :for item :in files
        :for path := (uiop:parse-native-namestring item)
        :append (if (uiop:directory-pathname-p path)
                    (mapcar #'uiop:native-namestring (uiop:directory-files path "*.org"))
                    (list (uiop:native-namestring path)))))

(defun buffer-for-file (file)
  (find file (buffer-list) :key #'buffer-filename :test #'equal))

(defun parse-agenda-text (lines source buffer)
  "The entries of LINES; those read from a BUFFER remember it in :buffer."
  (let ((sequences (if buffer
                       (buffer-todo-sequences buffer)
                       (todo-sequences-from-lines lines (todo-sequences-from-setting *org-todo-keywords*)))))
    (let ((entries (agenda-parse lines (sequences-keywords sequences) source :done (sequences-done-keywords sequences))))
      (if buffer
          (mapcar (lambda (entry) (list* :buffer buffer entry)) entries)
          entries))))

(defun agenda-entries (&optional (files *org-agenda-files*) (origin (current-buffer)))
  "The entries of the agenda files (an open buffer wins over the file on disk), or of the buffer ORIGIN when no files
are configured."
  (if files
      (loop :for file :in (agenda-source-files files)
            :for buffer := (buffer-for-file file)
            :append (cond (buffer (parse-agenda-text (split-text-lines (buffer-text buffer)) file buffer))
                          ((probe-file file) (parse-agenda-text (uiop:read-file-lines file) file nil))))
      (parse-agenda-text (split-text-lines (buffer-text origin))
                         (or (buffer-filename origin) (buffer-name origin)) origin)))

(define-major-mode org-agenda-mode nil
    (:name "Org-Agenda"
     :keymap *org-agenda-mode-keymap*))

(defun agenda-buffer (title records &optional spec)
  "The agenda buffer holding RECORDS, a list of (line-text . entry-or-nil), under TITLE."
  (let ((buffer (or (get-buffer "*Org Agenda*") (make-buffer "*Org Agenda*"))))
    (with-buffer-read-only buffer nil
      (erase-buffer buffer)
      (insert-string (buffer-point buffer)
                     (format nil "~A~%~{~A~^~%~}" title (mapcar #'car records)))
      (setf (buffer-value buffer 'agenda-targets)
            (coerce (cons nil (mapcar #'cdr records)) 'vector)
            (buffer-value buffer 'agenda-spec) spec))
    (change-buffer-mode buffer 'org-agenda-mode)
    (setf (buffer-read-only-p buffer) t)
    buffer))

(defun show-agenda (title records &optional spec)
  (let ((buffer (agenda-buffer title records spec)))
    (org-switch-to-buffer buffer)
    (buffer-start (current-point))
    buffer))

(defun today-day ()
  (multiple-value-call #'date-day (today)))

(defun agenda-block-records (spec)
  "Return (values title records) for SPEC, a view spec or a plist with :blocks (view specs) and :title."
  (if (getf spec :blocks)
      (let ((records '()))
        (dolist (block (getf spec :blocks))
          (multiple-value-bind (title block-records)
              (agenda-block-records (if (getf spec :origin) (list* :origin (getf spec :origin) block) block))
            (setf records (append records (list (cons title nil)) block-records (list (cons "" nil))))))
        (values (or (getf spec :title) "Agenda") records))
      (agenda-view-records spec (agenda-entries (or (getf spec :files) *org-agenda-files*)
                                                (or (getf spec :origin) (current-buffer)))
                           (today-day))))

(defun agenda-refresh-buffer (buffer)
  "Rebuild the agenda in BUFFER from its stored spec, keeping the cursor line."
  (let ((spec (buffer-value buffer 'agenda-spec)))
    (unless spec (editor-error "This is not an agenda buffer"))
    (let ((line (line-number-at-point (buffer-point buffer))))
      (multiple-value-bind (title records) (agenda-block-records spec)
        (agenda-buffer title records spec))
      (move-to-line (buffer-point buffer) (min line (buffer-nlines buffer)))
      buffer)))

(defun show-agenda-spec (spec)
  "Show the agenda described by SPEC, reading the current buffer when no agenda files are set."
  (let ((spec (if (getf spec :origin) spec (list* :origin (current-buffer) spec))))
    (multiple-value-bind (title records) (agenda-block-records spec)
      (show-agenda title records spec))))

(define-command org-agenda-week () ()
  "Show the week's scheduled items, deadlines and timestamps."
  (show-agenda-spec (list :type :agenda :span :week)))

(define-command org-agenda-day () ()
  "Show today's agenda."
  (show-agenda-spec (list :type :agenda :span :day)))

(define-command org-agenda-todo-list () ()
  "List every open todo entry, highest priority first."
  (show-agenda-spec (list :type :alltodo)))

(define-command org-agenda-tags () ()
  "List the entries that match a tags / property / todo expression such as +work-home/TODO."
  (show-agenda-spec (list :type :tags :match (prompt-for-string "Match: "))))

(define-command org-agenda-tags-todo () ()
  "Like org-agenda-tags, for open todo entries only."
  (show-agenda-spec (list :type :tags-todo :match (prompt-for-string "Match (todo entries only): "))))

(define-command org-agenda-todo-keyword () ()
  "List the entries in given todo states, for example NEXT|WAIT."
  (show-agenda-spec (list :type :todo :match (prompt-for-string "Todo keyword(s): "))))

(define-command org-agenda-search () ()
  "Search the agenda files for words, +word -word \"a phrase\" or {regexp}."
  (show-agenda-spec (list :type :search :match (prompt-for-string "Search words: "))))

(define-command org-agenda-list-stuck-projects () ()
  "List the projects that have no next action."
  (show-agenda-spec (list :type :stuck)))

;;; Custom commands

(defun custom-command-spec (command)
  "The view spec for COMMAND, an element of `*org-agenda-custom-commands*'."
  (destructuring-bind (key description type &optional match settings) command
    (declare (ignore key))
    (flet ((one (type match settings)
             (append (list :type (or (cdr (assoc (symbol-name type)
                                                 '(("AGENDA" . :agenda) ("ALLTODO" . :alltodo) ("TODO" . :todo) ("TAGS" . :tags)
                                                   ("TAGS-TODO" . :tags-todo) ("SEARCH" . :search) ("STUCK" . :stuck))
                                                 :test #'string=))
                                     (editor-error "Unknown agenda type ~A" type))
                           :match (if (stringp match) match ""))
                     settings)))
      (if (and (listp type) type)
          (list :blocks (mapcar (lambda (block) (destructuring-bind (b-type &optional b-match b-settings) block
                                                  (one b-type b-match b-settings)))
                                type)
                :title description)
          (one type match (if (listp match) match settings))))))

(defun agenda-menu-text ()
  (format nil "Agenda command: a)genda d)ay t)odo T)odo-state m)atch M)atch-todo s)earch #)stuck~{ ~A)~A~}: "
          (loop :for c :in *org-agenda-custom-commands* :append (list (first c) (second c)))))

(define-command org-agenda () ()
  "Ask which agenda to show: a week, a day, the todo list, a match, a search, stuck projects or a custom command."
  (let* ((answer (prompt-for-string (agenda-menu-text)))
         (custom (find answer *org-agenda-custom-commands* :key #'first :test #'string=)))
    (cond (custom (show-agenda-spec (custom-command-spec custom)))
          ((string= answer "a") (org-agenda-week))
          ((string= answer "d") (org-agenda-day))
          ((string= answer "t") (org-agenda-todo-list))
          ((string= answer "T") (org-agenda-todo-keyword))
          ((string= answer "m") (org-agenda-tags))
          ((string= answer "M") (org-agenda-tags-todo))
          ((string= answer "s") (org-agenda-search))
          ((string= answer "#") (org-agenda-list-stuck-projects))
          (t (editor-error "No agenda command ~A" answer)))))

;;; Agenda files

(define-command org-agenda-file-to-front () ()
  "Add the file of the current buffer to the agenda files."
  (let ((file (buffer-filename (current-buffer))))
    (unless file (editor-error "This buffer has no file"))
    (setf *org-agenda-files* (cons file (remove file *org-agenda-files* :test #'equal)))
    (message "~A is an agenda file" (file-namestring file))))

(define-command org-remove-file () ()
  "Remove the file of the current buffer from the agenda files."
  (let ((file (buffer-filename (current-buffer))))
    (setf *org-agenda-files* (remove file *org-agenda-files* :test #'equal))
    (message "Removed from the agenda files")))

;;; Commands in the agenda buffer

(defun agenda-entry-at-point (&optional (point (current-point)))
  (let* ((targets (buffer-value (point-buffer point) 'agenda-targets))
         (index (1- (line-number-at-point point))))
    (and targets (< index (length targets)) (aref targets index))))

(defun agenda-entry-buffer (entry)
  (let ((source (getf entry :source)))
    (or (getf entry :buffer)
        (buffer-for-file source)
        (and (probe-file source) (find-file-buffer source))
        (get-buffer source)
        (editor-error "Cannot find ~A" source))))

(defun call-at-entry (entry function)
  "Call FUNCTION with a point on the heading of ENTRY in its buffer."
  (let* ((buffer (agenda-entry-buffer entry))
         (point (move-to-line (copy-point (buffer-start-point buffer) :temporary) (getf entry :line))))
    (funcall function point)))

(define-command org-agenda-goto () ()
  "Jump to the entry on the current agenda line."
  (let ((entry (agenda-entry-at-point)))
    (unless entry (editor-error "No entry on this line"))
    (org-switch-to-buffer (agenda-entry-buffer entry))
    (move-to-line (current-point) (getf entry :line))))

(define-command org-agenda-quit () ()
  "Leave the agenda buffer."
  (let ((agenda (current-buffer)))
    (org-switch-to-buffer (or (find-if-not (lambda (buffer) (eq buffer agenda)) (buffer-list)) agenda))
    (delete-buffer agenda)))

(define-command org-agenda-redo () ()
  "Rebuild the agenda from the files."
  (agenda-refresh-buffer (current-buffer)))

(defun agenda-update-spec (function)
  (let* ((buffer (current-buffer))
         (spec (copy-list (or (buffer-value buffer 'agenda-spec) (editor-error "This is not an agenda buffer")))))
    (setf (buffer-value buffer 'agenda-spec) (funcall function spec))
    (agenda-refresh-buffer buffer)))

(defun agenda-spec-span (spec) (or (getf spec :span) *org-agenda-span*))
(defun agenda-spec-start (spec)
  (or (getf spec :start) (agenda-default-start (today-day) (agenda-spec-span spec))))

(define-command org-agenda-later () ()
  "Show the following period."
  (agenda-update-spec (lambda (spec)
                        (setf (getf spec :start) (agenda-shift-start (agenda-spec-start spec) (agenda-spec-span spec) 1))
                        spec)))

(define-command org-agenda-earlier () ()
  "Show the previous period."
  (agenda-update-spec (lambda (spec)
                        (setf (getf spec :start) (agenda-shift-start (agenda-spec-start spec) (agenda-spec-span spec) -1))
                        spec)))

(define-command org-agenda-goto-today () ()
  "Show the period that contains today."
  (agenda-update-spec (lambda (spec) (remf spec :start) spec)))

(defun agenda-set-span (span)
  (agenda-update-spec (lambda (spec) (setf (getf spec :span) span) (remf spec :start) spec)))

(define-command org-agenda-day-view () () "Show one day." (agenda-set-span :day))
(define-command org-agenda-week-view () () "Show a week." (agenda-set-span :week))
(define-command org-agenda-fortnight-view () () "Show two weeks." (agenda-set-span :fortnight))
(define-command org-agenda-month-view () () "Show a month." (agenda-set-span :month))
(define-command org-agenda-year-view () () "Show a year." (agenda-set-span :year))

(define-command org-agenda-log-mode () ()
  "Toggle showing closed entries and clocked time in the agenda."
  (agenda-update-spec (lambda (spec) (setf (getf spec :log) (not (getf spec :log))) spec)))

(define-command org-agenda-filter () ()
  "Filter the agenda: +tag -tag =regexp <effort >effort."
  (let ((filter (filter-parse (prompt-for-string "Filter (+tag -tag =regexp <30 >1:00): "))))
    (agenda-update-spec (lambda (spec) (setf (getf spec :filter) filter) spec))))

(define-command org-agenda-filter-clear () ()
  "Remove the agenda filter."
  (agenda-update-spec (lambda (spec) (remf spec :filter) spec)))

(defmacro define-agenda-entry-command (name doc (point) &body body)
  `(define-command ,name () ()
     ,doc
     (let ((entry (agenda-entry-at-point)))
       (unless entry (editor-error "No entry on this line"))
       (call-at-entry entry (lambda (,point) ,@body))
       (agenda-refresh-buffer (current-buffer)))))

(define-agenda-entry-command org-agenda-todo "Change the todo state of the entry on this line." (point)
  (org-todo-step point 1))

(define-agenda-entry-command org-agenda-priority-up "Raise the priority of the entry on this line." (point)
  (org-cycle-priority point 1))

(define-agenda-entry-command org-agenda-priority-down "Lower the priority of the entry on this line." (point)
  (org-cycle-priority point -1))

(define-agenda-entry-command org-agenda-set-tags "Set the tags of the entry on this line." (point)
  (let ((heading (heading-point point)))
    (multiple-value-bind (level todo priority title tags) (heading-split (line-string heading) (org-keywords))
      (declare (ignore level todo priority title))
      (org-set-tags point (parse-tag-input (prompt-for-string "Tags: " :initial-value (format nil "~{~A~^:~}" tags)))))))

(define-agenda-entry-command org-agenda-schedule "Schedule the entry on this line." (point)
  (prompt-for-planning point "SCHEDULED"))

(define-agenda-entry-command org-agenda-deadline "Set the deadline of the entry on this line." (point)
  (prompt-for-planning point "DEADLINE"))

(define-agenda-entry-command org-agenda-add-note "Add a note to the entry on this line." (point)
  (org-add-note point (prompt-for-string "Note: ")))

(define-agenda-entry-command org-agenda-clock-in "Start the clock on the entry on this line." (point)
  (org-clock-in point (now)))

(define-agenda-entry-command org-agenda-set-effort "Set the effort of the entry on this line." (point)
  (org-set-effort point (prompt-for-string "Effort (H:MM): ")))

(define-command org-agenda-clock-out () ()
  "Stop the running clock."
  (let ((entry (agenda-entry-at-point)))
    (unless entry (editor-error "No entry on this line"))
    (org-clock-out (buffer-start-point (agenda-entry-buffer entry)) (now))
    (agenda-refresh-buffer (current-buffer))))

(define-command org-agenda-save-buffers () ()
  "Save the buffers of the agenda files."
  (let ((count 0))
    (dolist (file (agenda-source-files))
      (let ((buffer (buffer-for-file file)))
        (when (and buffer (buffer-modified-p buffer))
          (save-buffer buffer)
          (incf count))))
    (message "Saved ~D file~:P" count)))

(define-command org-agenda-next-item () ()
  "Move to the next agenda line that holds an entry."
  (let ((point (current-point)))
    (loop :while (line-offset point 1)
          :do (when (agenda-entry-at-point point) (return)))))

(define-command org-agenda-previous-item () ()
  "Move to the previous agenda line that holds an entry."
  (let ((point (current-point)))
    (loop :while (line-offset point -1)
          :do (when (agenda-entry-at-point point) (return)))))

(define-key *org-agenda-mode-keymap* "Return" 'org-agenda-goto)
(define-key *org-agenda-mode-keymap* "Tab" 'org-agenda-goto)
(define-key *org-agenda-mode-keymap* "q" 'org-agenda-quit)
(define-key *org-agenda-mode-keymap* "x" 'org-agenda-quit)
(define-key *org-agenda-mode-keymap* "r" 'org-agenda-redo)
(define-key *org-agenda-mode-keymap* "g" 'org-agenda-redo)
(define-key *org-agenda-mode-keymap* "f" 'org-agenda-later)
(define-key *org-agenda-mode-keymap* "b" 'org-agenda-earlier)
(define-key *org-agenda-mode-keymap* "." 'org-agenda-goto-today)
(define-key *org-agenda-mode-keymap* "d" 'org-agenda-day-view)
(define-key *org-agenda-mode-keymap* "w" 'org-agenda-week-view)
(define-key *org-agenda-mode-keymap* "v d" 'org-agenda-day-view)
(define-key *org-agenda-mode-keymap* "v w" 'org-agenda-week-view)
(define-key *org-agenda-mode-keymap* "v t" 'org-agenda-fortnight-view)
(define-key *org-agenda-mode-keymap* "v m" 'org-agenda-month-view)
(define-key *org-agenda-mode-keymap* "v y" 'org-agenda-year-view)
(define-key *org-agenda-mode-keymap* "l" 'org-agenda-log-mode)
(define-key *org-agenda-mode-keymap* "v l" 'org-agenda-log-mode)
(define-key *org-agenda-mode-keymap* "/" 'org-agenda-filter)
(define-key *org-agenda-mode-keymap* "|" 'org-agenda-filter-clear)
(define-key *org-agenda-mode-keymap* "t" 'org-agenda-todo)
(define-key *org-agenda-mode-keymap* "+" 'org-agenda-priority-up)
(define-key *org-agenda-mode-keymap* "-" 'org-agenda-priority-down)
(define-key *org-agenda-mode-keymap* ":" 'org-agenda-set-tags)
(define-key *org-agenda-mode-keymap* "C-c C-s" 'org-agenda-schedule)
(define-key *org-agenda-mode-keymap* "C-c C-d" 'org-agenda-deadline)
(define-key *org-agenda-mode-keymap* "z" 'org-agenda-add-note)
(define-key *org-agenda-mode-keymap* "I" 'org-agenda-clock-in)
(define-key *org-agenda-mode-keymap* "O" 'org-agenda-clock-out)
(define-key *org-agenda-mode-keymap* "e" 'org-agenda-set-effort)
(define-key *org-agenda-mode-keymap* "n" 'org-agenda-next-item)
(define-key *org-agenda-mode-keymap* "p" 'org-agenda-previous-item)
(define-key *org-agenda-mode-keymap* "C-x C-s" 'org-agenda-save-buffers)
