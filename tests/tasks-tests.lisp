(in-package :lem-org-mode/tests)

(defun today-text ()
  (multiple-value-bind (y m d) (lem-org-mode::today) (lem-org-mode::format-timestamp y m d)))

(deftest test-todo-sequences-from-buffer
  (let ((buffer (make-org-buffer (lines "#+TODO: TODO NEXT | DONE CANCELED" "* a"))))
    (let ((point (line-point buffer 2)))
      (setf (lem:current-buffer) buffer)
      (lem-org-mode:org-todo-change point "NEXT")
      (ok (string= "* NEXT a" (line-string (line-point buffer 2))))
      (ok (string= "NEXT" (lem-org-mode:org-cycle-todo (line-point buffer 2) -1)) "backward returns the old state")
      (ok (string= "* TODO a" (line-string (line-point buffer 2))))
      (lem-org-mode:org-todo-change point "CANCELED")
      (ok (string= "* CANCELED a" (line-string (line-point buffer 2))))
      (ok (search "CLOSED:" (text-of buffer)) "any done state closes the task")
      (lem-org-mode:org-todo-change point "TODO")
      (ok (not (search "CLOSED:" (text-of buffer))) "leaving a done state removes CLOSED"))))

(deftest test-todo-logging
  (let ((buffer (make-org-buffer (lines "#+TODO: TODO(t) | DONE(d@/!)" "* TODO a"))))
    (setf (lem:current-buffer) buffer)
    (lem-org-mode:org-todo-change (line-point buffer 2) "DONE" :note "all good")
    (let ((text (text-of buffer)))
      (ok (search ":LOGBOOK:" text))
      (ok (search "- State \"DONE\"       from \"TODO\"" text))
      (ok (search "  all good" text) "the note follows the entry"))
    (lem-org-mode:org-todo-change (line-point buffer 2) "TODO")
    (ok (search "- State \"TODO\"       from \"DONE\"" (text-of buffer)) "leaving DONE logs a timestamp (d@/!)")))

(deftest test-repeating-task
  (let ((buffer (make-org-buffer (lines "* TODO water plants" "SCHEDULED: <2026-10-22 Thu +1w>"))))
    (setf (lem:current-buffer) buffer)
    (lem-org-mode:org-todo-change (line-point buffer 1) "DONE")
    (let ((text (text-of buffer)))
      (ok (search "* TODO water plants" text) "back to TODO")
      (ok (search "SCHEDULED: <2026-10-29 Thu +1w>" text) "moved a week")
      (ok (not (search "CLOSED" text)))
      (ok (search ":LAST_REPEAT:" text))
      (ok (search "- State \"DONE\"       from \"TODO\"" text)))))

(deftest test-statistics-cookies
  (let ((buffer (make-org-buffer (lines "* Project [0/0]" "** TODO a" "** TODO b" "* Shopping [%]" "- [ ] milk" "- [ ] eggs"))))
    (setf (lem:current-buffer) buffer)
    (lem-org-mode:org-todo-change (line-point buffer 2) "DONE")
    (ok (string= "* Project [1/2]" (line-string (line-point buffer 1))))
    (lem-org-mode:org-toggle-checkbox (line-point buffer 7))
    (ok (string= "* Shopping [50%]" (line-string (line-point buffer 5))))
    (lem-org-mode:org-update-all-cookies buffer)
    (ok (string= "* Project [1/2]" (line-string (line-point buffer 1))))))

(deftest test-nested-checkbox-state
  (let ((buffer (make-org-buffer (lines "- [ ] parent [0/2]" "  - [ ] a" "  - [ ] b"))))
    (setf (lem:current-buffer) buffer)
    (lem-org-mode:org-toggle-checkbox (line-point buffer 2))
    (ok (string= "- [-] parent [1/2]" (line-string (line-point buffer 1))))
    (lem-org-mode:org-toggle-checkbox (line-point buffer 3))
    (ok (string= "- [X] parent [2/2]" (line-string (line-point buffer 1))))))

(deftest test-add-note
  (let ((buffer (make-org-buffer (lines "* a"))))
    (setf (lem:current-buffer) buffer)
    (lem-org-mode:org-add-note (line-point buffer 1) "remember this")
    (ok (search (lines ":LOGBOOK:") (text-of buffer)))
    (ok (search "- Note taken on [" (text-of buffer)))
    (ok (search "  remember this" (text-of buffer)))))
