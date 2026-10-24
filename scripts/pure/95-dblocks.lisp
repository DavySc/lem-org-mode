;;; Dynamic blocks: clock tables, column view

(defvar *db-doc*
  (list "* Project A" "** Task 1" ":LOGBOOK:" "CLOCK: [2026-10-20 Tue 09:00]--[2026-10-20 Tue 10:30] =>  1:30"
        "CLOCK: [2026-10-22 Thu 14:00]--[2026-10-22 Thu 14:45] =>  0:45" ":END:"
        "** Task 2" ":PROPERTIES:" ":Effort: 2:00" ":END:" ":LOGBOOK:" "CLOCK: [2026-10-22 Thu 08:00]--[2026-10-22 Thu 09:00] =>  1:00" ":END:"
        "* Project B" ":PROPERTIES:" ":Effort: 1:00" ":END:" "** Sub" ":PROPERTIES:" ":Effort: 0:30" ":END:"))
(defvar *db-entries* (agenda-parse *db-doc* '("TODO" "DONE") "x.org"))
(defun db-find (title) (find title *db-entries* :key (lambda (e) (getf e :title)) :test #'string=))

(check "dblock lines" '(t nil t) (list (dblock-begin-line-p "#+BEGIN: clocktable :x 1") (dblock-begin-line-p "#+begin_src sh") (dblock-end-line-p "#+END:")))
(check "dblock parse" '("clocktable" (("scope" . "file") ("maxlevel" . "2"))) (multiple-value-list (dblock-parse-begin "#+BEGIN: clocktable :scope file :maxlevel 2")))
(check "dblock param" '("file" "2" nil) (let ((p '(("scope" . "file") ("maxlevel" . "2")))) (list (dblock-param p "scope") (dblock-param p "maxlevel") (dblock-param p "tstart"))))

(defun minutes-of (title &optional from to)
  (entry-total-clock *db-entries* (db-find title) from to))
(check "own clock time" 60 (minutes-of "Task 2"))
(check "task time adds up" 135 (minutes-of "Task 1"))
(check "project time adds up its children" 195 (minutes-of "Project A"))
(check "time in a range" 45 (minutes-of "Task 1" (* 1440 (date-day 2026 10 21)) (* 1440 (date-day 2026 10 23))))
(check "clock clipped at midnight" 30
       (clock-minutes-clipped (list (timestamp-parse "[2026-10-20 Tue 23:30]") (timestamp-parse "[2026-10-21 Wed 00:30]") 60)
                              (* 1440 (date-day 2026 10 21)) nil))

(defvar *d22* (date-day 2026 10 22))
(check "range today" (list (* 1440 *d22*) (* 1440 (1+ *d22*))) (multiple-value-list (clocktable-range "today" *d22*)))
(check "range this week" (list (* 1440 (date-day 2026 10 19)) (* 1440 (date-day 2026 10 26))) (multiple-value-list (clocktable-range "thisweek" *d22*)))
(check "range this month" (list (* 1440 (date-day 2026 10 1)) (* 1440 (date-day 2026 11 1))) (multiple-value-list (clocktable-range "thismonth" *d22*)))
(check "range last month" (list (* 1440 (date-day 2026 9 1)) (* 1440 (date-day 2026 10 1))) (multiple-value-list (clocktable-range "lastmonth" *d22*)))
(check "range year" (list (* 1440 (date-day 2026 1 1)) (* 1440 (date-day 2027 1 1))) (multiple-value-list (clocktable-range "2026" *d22*)))
(check "range month name" (list (* 1440 (date-day 2026 2 1)) (* 1440 (date-day 2026 3 1))) (multiple-value-list (clocktable-range "2026-02" *d22*)))
(check "range day" (list (* 1440 (date-day 2026 10 20)) (* 1440 (date-day 2026 10 21))) (multiple-value-list (clocktable-range "2026-10-20" *d22*)))
(check "range week" (list (* 1440 (date-day 2026 10 19)) (* 1440 (date-day 2026 10 26))) (multiple-value-list (clocktable-range "2026-W43" *d22*)))
(check "no range" '(nil nil) (multiple-value-list (clocktable-range nil *d22*)))
(check "bounds from tstart and tend" (list (* 1440 (date-day 2026 10 20)) (* 1440 (date-day 2026 10 22)))
       (multiple-value-list (clocktable-bounds '(("tstart" . "2026-10-20") ("tend" . "2026-10-22")) *d22*)))

(defvar *db-rows* (clocktable-rows *db-entries* :maxlevel 2))
(check "rows" '((1 "Project A" 195) (2 "Task 1" 135) (2 "Task 2" 60)) (mapcar (lambda (r) (subseq r 0 3)) *db-rows*))
(check "rows maxlevel 1" '("Project A") (mapcar #'second (clocktable-rows *db-entries* :maxlevel 1)))
(check "total counts top entries once" 195 (clocktable-total *db-entries* *db-rows* nil nil))
(check "table lines" '("| Headline     | Time   |" "|--------------+--------|" "| *Total time* | *3:15* |" "|--------------+--------|")
       (subseq (clocktable-lines *db-rows* 195) 0 4))
(check "table shows the tree" t (let ((lines (clocktable-lines *db-rows* 195))) (and (search "\\_" (nth 5 lines)) (search "Task 1" (nth 5 lines)) (search "3:15" (nth 2 lines)) t)))
(check "tag filter" nil (clocktable-rows *db-entries* :tags '("nothing")))
(check "subtree scope" '("Task 1" "Task 2") (mapcar #'second (clocktable-rows *db-entries* :scope-entry (db-find "Project A"))))

(check "columns parse" '(("ITEM" "ITEM" nil 25) ("Effort" "Est" ":" nil)) (columns-parse "%25ITEM %Effort(Est){:}"))
(check "column summary time" "1:30" (column-summary ":" '("1:00" "0:30" "")))
(check "column summary number" "5" (column-summary "+" '("2" "3")))
(check "column summary max" "9" (column-summary "max" '("2" "9" "3")))
(check "column summary mean" "2.5" (column-summary "mean" '("2" "3")))
(check "column view rows with a summary" '(("Project A" "2:00") ("Task 1" "") ("Task 2" "2:00") ("Project B" "0:30") ("Sub" "0:30"))
       (columnview-rows *db-entries* (columns-parse "%ITEM %Effort{:}")))
(check "column view lines" 2 (length (subseq (columnview-lines *db-entries* (columns-parse "%ITEM %TODO")) 0 2)))
(check "clock line duration is fixed" "CLOCK: [2026-10-22 Thu 08:00]--[2026-10-22 Thu 09:00] =>  1:00"
       (clock-line-recompute "CLOCK: [2026-10-22 Thu 08:00]--[2026-10-22 Thu 09:00] => 9:99"))
(check "open clock is left alone" "CLOCK: [2026-10-22 Thu 08:00]" (clock-line-recompute "CLOCK: [2026-10-22 Thu 08:00]"))
