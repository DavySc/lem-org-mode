(in-package :cl-user)

(smoke-check "table formula"
             '(("2" "3" "6") ("4" "5" "20"))
             (lem-org-mode::table-recalculate-rows '(("2" "3" "") ("4" "5" "")) '("$3=$1*$2")))

(smoke-check "clock table total"
             t
             (let* ((entries (lem-org-mode::agenda-parse (list "* A" ":LOGBOOK:" "CLOCK: [2026-10-20 Tue 09:00]--[2026-10-20 Tue 10:30] =>  1:30" ":END:")
                                                         '("TODO" "DONE") "x.org"))
                    (rows (lem-org-mode::clocktable-rows entries)))
               (and (= 90 (lem-org-mode::clocktable-total entries rows nil nil)) t)))

(smoke-check "structure template"
             (list "#+begin_quote" "" "#+end_quote")
             (lem-org-mode::structure-template-lines "quote" (list "") 0))

(smoke-check "list item move"
             (list "- b" "- a" "- c")
             (first (multiple-value-list (lem-org-mode::list-move-item (list "- a" "- b" "- c") 1 -1))))
