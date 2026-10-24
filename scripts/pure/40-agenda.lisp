;;; Agenda: parsing, matching, views

(defvar *a-doc*
  (list "#+CATEGORY: home" "#+FILETAGS: :file:"
        "* Projects :proj:"
        "** TODO [#A] Write book :writing:"
        "SCHEDULED: <2026-10-22 Thu 10:00-11:00 +1w>"
        ":PROPERTIES:" ":Effort: 2:00" ":CATEGORY: books" ":END:"
        "** NEXT Outline"
        "DEADLINE: <2026-10-30 Fri -3d>"
        "** DONE Idea" "CLOSED: [2026-10-20 Tue 09:00]"
        "* TODO Dentist"
        "<2026-10-23 Fri 14:30> appointment"
        "* TODO Trip" "<2026-10-26 Mon>--<2026-10-28 Wed>"
        "* Empty project" ":PROPERTIES:" ":ID: x" ":END:"
        "* TODO Stuck project" "** WAIT sub"))
(defvar *a-kw* '("TODO" "NEXT" "WAIT" "DONE"))
(defvar *a-entries* (agenda-parse *a-doc* *a-kw* "/tmp/notes.org" :done '("DONE")))
(defun a-find (title) (find title *a-entries* :key (lambda (e) (getf e :title)) :test #'string=))
(defvar *a-today* (date-day 2026 10 22))

(check "titles" '("Projects" "Write book" "Outline" "Idea" "Dentist" "Trip" "Empty project" "Stuck project" "sub")
       (mapcar (lambda (e) (getf e :title)) *a-entries*))
(check "tags are inherited" '("writing" "proj" "file") (getf (a-find "Write book") :all-tags))
(check "own tags only" '("writing") (getf (a-find "Write book") :tags))
(check "category from property and file" '("books" "home")
       (list (getf (a-find "Write book") :category) (getf (a-find "Dentist") :category)))
(check "category inherited from the parent entry" "c"
       (let ((e (agenda-parse (list "* A" ":PROPERTIES:" ":CATEGORY: c" ":END:" "** B") *a-kw* "x.org"))) (getf (second e) :category)))
(check "effort" 120 (getf (a-find "Write book") :effort))
(check "scheduled ts" '(10 0 11 0 (:cumulate 1 :w))
       (let ((ts (getf (a-find "Write book") :scheduled-ts)))
         (list (getf ts :hour) (getf ts :minute) (getf ts :end-hour) (getf ts :end-minute) (getf ts :repeater))))
(check "path" '("Projects") (getf (a-find "Outline") :path))
(check "plain timestamp" '(23) (mapcar (lambda (ts) (getf ts :day)) (getf (a-find "Dentist") :timestamps)))
(check "range is not two timestamps" '(0 1) (list (length (getf (a-find "Trip") :timestamps)) (length (getf (a-find "Trip") :ranges))))
(check "closed" "[2026-10-20 Tue 09:00]" (getf (a-find "Idea") :closed))
(check "subtree end" '(14 10 14)
       (list (getf (a-find "Projects") :subtree-end) (getf (a-find "Write book") :subtree-end) (getf (a-find "Idea") :subtree-end)))
(check "done flag" '(nil nil t) (mapcar (lambda (n) (getf (a-find n) :done)) '("Write book" "Outline" "Idea")))

(defun item-summary (i) (list (getf i :label) (getf (getf i :entry) :title) (getf i :time-text)))
(defun labels-on (day &key log (today *a-today*))
  (mapcar #'item-summary (agenda-day-items *a-entries* day today 14 :log log)))
(check "today: scheduled item with its time (the deadline is outside its own 3 day warning)" '(("Scheduled:" "Write book" "10:00-11:00"))
       (labels-on *a-today*))
(check "repeater shows a week later" '(("Scheduled:" "Write book" "10:00-11:00")) (labels-on (+ *a-today* 7)))
(check "timestamp with time" '(("" "Dentist" "14:30")) (labels-on (+ *a-today* 1)))
(check "range days" '((("(1/3):" "Trip" nil)) (("(2/3):" "Trip" nil)) (("(3/3):" "Trip" nil)))
       (list (labels-on (date-day 2026 10 26)) (labels-on (date-day 2026 10 27)) (labels-on (date-day 2026 10 28))))
(check "deadline day" '(("Deadline:" "Outline" nil)) (labels-on (date-day 2026 10 30)))
(check "deadline with its own -3d warns only 3 days ahead" '(("In 3 d.:" "Outline" nil))
       (remove-if-not (lambda (x) (string= (second x) "Outline")) (labels-on (date-day 2026 10 27) :today (date-day 2026 10 27))))
(check "no warning outside its own period" nil
       (remove-if-not (lambda (x) (string= (second x) "Outline")) (labels-on (date-day 2026 10 25) :today (date-day 2026 10 25))))
(check "log mode shows closed" '(("Closed:" "Idea" "9:00")) (labels-on (date-day 2026 10 20) :log t))
(check "done entries are hidden otherwise" nil (labels-on (date-day 2026 10 20)))

(defun day-lines (day)
  (mapcar #'car (agenda-week-records *a-entries* day :day *a-today* 14)))
(check "grid appears on a day with timed items" t
       (let ((lines (day-lines (+ *a-today* 1)))) (and (some (lambda (l) (search "----------------" l)) lines) (some (lambda (l) (search "14:30" l)) lines) t)))
(check "grid is omitted for a day without timed items" nil (some (lambda (l) (search "-------" l)) (day-lines (+ *a-today* 2))))
(check "grid line order" t
       (let* ((lines (day-lines (+ *a-today* 1)))
              (grid-12 (position-if (lambda (l) (search "12:00" l)) lines))
              (item (position-if (lambda (l) (search "Dentist" l)) lines))
              (grid-16 (position-if (lambda (l) (search "16:00" l)) lines)))
         (and grid-12 item grid-16 (< grid-12 item grid-16))))
(check "month span" 31 (nth-value 1 (agenda-span-range *a-today* :month)))
(check "month starts on the first" (date-day 2026 10 1) (agenda-span-range *a-today* :month))
(check "year span" 365 (nth-value 1 (agenda-span-range *a-today* :year)))
(check "shift month" (date-day 2026 11 1) (agenda-shift-start *a-today* :month 1))
(check "shift week back" (- *a-today* 7) (agenda-shift-start *a-today* :week -1))
(check "week starts on monday" (date-day 2026 10 19) (agenda-default-start *a-today* :week))
(check "iso week" 43 (iso-week *a-today*))
(check "title" "Week-agenda (W43):" (agenda-title (date-day 2026 10 19) :week))

;;; matching
(defun titles (records) (mapcar (lambda (r) (getf (cdr r) :title)) records))
(defun match (string &key todo-only) (titles (agenda-match-records *a-entries* string :todo-only todo-only :today *a-today*)))
(check "match tag" '("Write book") (match "writing"))
(check "match inherited tag" '("Write book" "Outline" "Idea") (match "+proj+LEVEL=2"))
(check "match minus" '("Outline" "Idea") (match "proj+LEVEL=2-writing"))
(check "match or" '("Write book" "Dentist") (match "writing|ITEM=\"Dentist\""))
(check "match todo part" '("Outline") (match "proj/NEXT"))
(check "match todo or" '("Write book" "Outline") (match "proj/TODO|NEXT"))
(check "match todo exclude" '("Write book" "Outline") (match "proj/-DONE"))
(check "match open todos" '("Write book" "Outline" "Dentist" "Trip" "Stuck project" "sub") (match "/!"))
(check "match priority" '("Write book") (match "PRIORITY=\"A\""))
(check "match priority compare" '("Write book") (match "PRIORITY<\"B\""))
(check "match category" '("Write book") (match "CATEGORY=\"books\""))
(check "match effort" '("Write book") (match "Effort>\"1:00\""))
(check "match effort number" nil (match "Effort>200"))
(check "match level" '("Projects" "Dentist" "Trip" "Empty project" "Stuck project") (match "LEVEL=1"))
(when (find-package "PPCRE")
  (check "match regex on tag" '("Write book") (match "{writ.*}")))
(check "match date" '("Write book") (match "SCHEDULED<=\"<today>\""))
(check "match deadline relative" '("Outline") (match "DEADLINE<\"<+2w>\"&DEADLINE>\"<today>\""))
(check "match todo only" '("Write book" "Outline") (match "proj" :todo-only t))
(check "match property" '("Empty project") (match "ID=\"x\""))
(check-error "bad match" (match-compile "+"))

;;; filters
(check "filter tag" '("Write book") (titles (agenda-todo-records *a-entries* :filter (filter-parse "+writing"))))
(check "filter minus tag" '("Outline" "Dentist" "Trip" "Stuck project" "sub") (titles (agenda-todo-records *a-entries* :filter (filter-parse "-writing"))))
(check "filter effort" '("Write book") (titles (agenda-todo-records *a-entries* :filter (filter-parse ">1:00"))))
(check "filter regexp" '("Dentist") (titles (agenda-todo-records *a-entries* :filter (filter-parse "=dent"))))

;;; todo list, keyword list, search, stuck
(check "todo list order" '("Write book" "Outline" "Dentist" "Trip" "Stuck project" "sub") (titles (agenda-todo-records *a-entries*)))
(check "todo keyword list" '("Outline") (titles (agenda-todo-records *a-entries* :keywords '("NEXT"))))
(check "done todo keyword list" '("Idea") (titles (agenda-todo-records *a-entries* :keywords '("DONE"))))
(check "search word" '("Dentist") (titles (agenda-search-records *a-entries* "appointment")))
(check "search negative" '("Trip") (titles (agenda-search-records *a-entries* "2026-10-26 -appointment")))
(check "search phrase" '("Dentist") (titles (agenda-search-records *a-entries* "\"14:30> appointment\"")))
(check "stuck projects" '("Dentist" "Trip" "Stuck project")
       (titles (agenda-stuck-records *a-entries* '("+LEVEL=1/-DONE" ("NEXT") nil "") *a-today*)))
(check "next action keeps a project from being stuck" '("Dentist" "Trip")
       (titles (agenda-stuck-records *a-entries* '("+LEVEL=1/-DONE" ("WAIT") nil "") *a-today*)))
(check "view records agenda" 8
       (length (nth-value 1 (agenda-view-records (list :type :agenda :span :day :start *a-today*) *a-entries* *a-today*))))
