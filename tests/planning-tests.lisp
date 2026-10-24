(in-package :lem-org-mode/tests)

(deftest test-timestamp-change-at-point
  (let ((buffer (make-org-buffer (lines "* TODO a" "SCHEDULED: <2026-10-22 Thu 14:30>"))))
    (setf (lem:current-buffer) buffer)
    (let ((point (line-point buffer 2)))
      (character-offset point 20)           ; on the day
      (ok (lem-org-mode::org-timestamp-change point 1))
      (ok (search "<2026-10-23 Fri 14:30>" (text-of buffer)))
      (setf (point-charpos point) 28)       ; on the hour
      (lem-org-mode::org-timestamp-change point 2)
      (ok (search "<2026-10-23 Fri 16:30>" (text-of buffer)))
      (setf (point-charpos point) 12)       ; on the year
      (lem-org-mode::org-timestamp-change point 1)
      (ok (search "<2027-10-23 Sat 16:30>" (text-of buffer))))
    (ok (not (lem-org-mode::org-timestamp-change (line-point buffer 1) 1)) "no timestamp on the heading line")))

(deftest test-shift-keys
  (let ((buffer (make-org-buffer (lines "* TODO a" "SCHEDULED: <2026-10-22 Thu>"))))
    (setf (lem:current-buffer) buffer)
    (move-to-line (current-point) 2)
    (character-offset (current-point) 15)
    (lem-org-mode::org-shift-right)
    (ok (search "<2026-10-23 Fri>" (text-of buffer)))
    (lem-org-mode::org-shift-left)
    (ok (search "<2026-10-22 Thu>" (text-of buffer)))
    (move-to-line (current-point) 1)
    (lem-org-mode::org-shift-right)
    (ok (search "* DONE a" (text-of buffer)))
    (lem-org-mode::org-shift-left)
    (ok (search "* TODO a" (text-of buffer)))
    (lem-org-mode::org-shift-up)
    (ok (search "* TODO [#C] a" (text-of buffer)) "on a heading Shift-Up raises the priority")))

(deftest test-insert-timestamp
  (let ((buffer (make-org-buffer (lines "meet at "))))
    (setf (lem:current-buffer) buffer)
    (move-to-line (current-point) 1)
    (line-end (current-point))
    (lem-org-mode::org-insert-timestamp (current-point)
                                        (lem-org-mode::timestamp-with-day (list :active t :hour 9 :minute 0)
                                                                          (lem-org-mode::date-day 2026 10 22)))
    (ok (search "meet at <2026-10-22 Thu 09:00>" (text-of buffer)))))
