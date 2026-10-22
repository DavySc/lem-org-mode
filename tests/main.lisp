(defpackage :lem-org-mode/tests
  (:use :cl :rove :lem :lem-org-mode))
(in-package :lem-org-mode/tests)

(defun make-org-buffer (text)
  (let ((buffer (make-buffer nil :temporary t)))
    (insert-string (buffer-point buffer) text)
    buffer))

(defun buffer-text (buffer)
  (points-to-string (buffer-start-point buffer) (buffer-end-point buffer)))

(defun line-point (buffer line)
  (move-to-line (copy-point (buffer-start-point buffer) :temporary) line))

(deftest test-mode-registration
  (ok (eq (get-file-mode "notes.org") 'org-mode))
  (ok (subtypep 'org-mode 'lem/language-mode:language-mode)))

(deftest test-heading-level
  (let ((buffer (make-org-buffer (format nil "* One~%** Two~%not a heading~%*bold*~%***~%"))))
    (ok (eql 1 (org-heading-level (line-point buffer 1))))
    (ok (eql 2 (org-heading-level (line-point buffer 2))))
    (ok (null (org-heading-level (line-point buffer 3))))
    (ok (null (org-heading-level (line-point buffer 4))))
    (ok (null (org-heading-level (line-point buffer 5))))))

(deftest test-subtree-bounds
  (let ((buffer (make-org-buffer (format nil "* A~%body~%** A1~%* B~%tail"))))
    (multiple-value-bind (start end) (org-subtree-bounds (line-point buffer 1))
      (ok (= 1 (line-number-at-point start)))
      (ok (= 4 (line-number-at-point end))))
    (multiple-value-bind (start end) (org-subtree-bounds (line-point buffer 4))
      (ok (= 4 (line-number-at-point start)))
      (ok (point= end (buffer-end-point buffer))))
    (ok (null (org-subtree-bounds (line-point buffer 2))))))

(deftest test-cycle-todo
  (let ((buffer (make-org-buffer (format nil "** write tests~%"))))
    (org-cycle-todo (line-point buffer 1))
    (ok (string= (format nil "** TODO write tests~%") (buffer-text buffer)))
    (org-cycle-todo (line-point buffer 1))
    (ok (string= (format nil "** DONE write tests~%") (buffer-text buffer)))
    (org-cycle-todo (line-point buffer 1))
    (ok (string= (format nil "** write tests~%") (buffer-text buffer)))))

(deftest test-shift-subtree
  (let ((buffer (make-org-buffer (format nil "* A~%** A1~%* B~%"))))
    (org-shift-subtree (line-point buffer 1) 1)
    (ok (string= (format nil "** A~%*** A1~%* B~%") (buffer-text buffer)))
    (org-shift-subtree (line-point buffer 1) -1)
    (ok (string= (format nil "* A~%** A1~%* B~%") (buffer-text buffer)))
    (ok (signals (org-shift-subtree (line-point buffer 1) -1) 'editor-error))))

(deftest test-move-subtree
  (let ((buffer (make-org-buffer (format nil "* A~%a~%* B~%b"))))
    (let ((point (line-point buffer 1)))
      (org-move-subtree point :down)
      (ok (string= (format nil "* B~%b~%* A~%a") (buffer-text buffer)))
      (ok (= 3 (line-number-at-point point))))
    (let ((point (line-point buffer 3)))
      (org-move-subtree point :up)
      (ok (string= (format nil "* A~%a~%* B~%b") (buffer-text buffer)))
      (ok (= 1 (line-number-at-point point))))
    (ok (signals (org-move-subtree (line-point buffer 3) :down) 'editor-error))))

(deftest test-table-format
  (ok (equal (org-table-format-rows (list '("Name" "Qty") :rule '("apple" "3") '("kiwi" "12.5")))
             (list "| Name  |  Qty |"
                   "|-------+------|"
                   "| apple |    3 |"
                   "| kiwi  | 12.5 |"))))

(deftest test-table-edit
  (let ((buffer (make-org-buffer (format nil "|a|bb~%|ccc|d|~%"))))
    (let ((point (line-point buffer 1)))
      (org-table-edit point :align)
      (ok (string= (format nil "| a   | bb |~%| ccc | d  |~%") (buffer-text buffer)))
      (setf (point-charpos point) 2)
      (org-table-edit point :next-cell)
      (ok (= 1 (line-number-at-point point)))
      (ok (= 8 (point-charpos point)))
      (org-table-edit point :insert-column)
      (ok (string= (format nil "| a   |   | bb |~%| ccc |   | d  |~%") (buffer-text buffer)))
      (org-table-edit point :move-row-down)
      (ok (string= (format nil "| ccc |   | d  |~%| a   |   | bb |~%") (buffer-text buffer)))
      (ok (= 2 (line-number-at-point point))))
    (ok (signals (org-table-edit (line-point buffer 1) :previous-cell) 'editor-error))))

(deftest test-list-items
  (let ((buffer (make-org-buffer (format nil "1. a~%   - [ ] sub~%2. b~%"))))
    (let ((point (line-point buffer 1)))
      (org-insert-item point)
      (ok (string= (format nil "1. a~%   - [ ] sub~%2. ~%3. b~%") (buffer-text buffer)))
      (ok (= 3 (line-number-at-point point))))
    (let ((point (line-point buffer 2)))
      (setf (point-charpos point) 5)
      (org-toggle-checkbox point)
      (ok (string= "   - [X] sub" (line-string point)))
      (ok (= 5 (point-charpos point))))
    (ok (signals (org-toggle-checkbox (line-point buffer 1)) 'editor-error)
        "toggling needs an item")))

(deftest test-heading-attributes
  (let ((buffer (make-org-buffer (format nil "* TODO write~%body~%* next~%"))))
    (org-cycle-priority (line-point buffer 2) 1)
    (ok (string= "* TODO [#C] write" (line-string (line-point buffer 1))))
    (org-set-tags (line-point buffer 1) '("work"))
    (ok (string= ":work:" (subseq (line-string (line-point buffer 1)) (- 77 6))))
    (org-set-planning (line-point buffer 1) "SCHEDULED" "<2026-10-22 Thu>")
    (ok (string= "SCHEDULED: <2026-10-22 Thu>" (line-string (line-point buffer 2))))
    (org-set-property (line-point buffer 1) "ID" "7")
    (ok (equal (list ":PROPERTIES:" ":ID: 7" ":END:" "body")
               (loop :for n :from 3 :to 6 :collect (line-string (line-point buffer n)))))
    (ok (string= "* next" (line-string (line-point buffer 7))))))

(deftest test-babel-results
  (let ((buffer (make-org-buffer (format nil "#+begin_src sql :engine postgres~%select 1;~%#+end_src~%"))))
    (ok (src-block-bounds (line-point buffer 2)))
    (ok (null (src-block-bounds (buffer-end-point buffer))) "no block after the end")))

(deftest test-clocking
  (let ((buffer (make-org-buffer (format nil "* TODO a~%SCHEDULED: <2026-10-22 Thu>~%body~%* b~%"))))
    (org-clock-in (line-point buffer 1) '(2026 10 22 10 0))
    (ok (string= (format nil "* TODO a~%SCHEDULED: <2026-10-22 Thu>~%:LOGBOOK:~%CLOCK: [2026-10-22 Thu 10:00]~%:END:~%body~%* b~%")
                 (buffer-text buffer)))
    (ok (= 90 (org-clock-out (line-point buffer 1) '(2026 10 22 11 30))))
    (ok (string= "CLOCK: [2026-10-22 Thu 10:00]--[2026-10-22 Thu 11:30] =>  1:30" (line-string (line-point buffer 4))))
    (org-clock-in (line-point buffer 1) '(2026 10 22 12 0))
    (org-clock-in (line-point buffer 8) '(2026 10 22 12 30))
    (ok (string= "CLOCK: [2026-10-22 Thu 12:00]--[2026-10-22 Thu 12:30] =>  0:30" (line-string (line-point buffer 4))))
    (ok (string= ":LOGBOOK:" (line-string (line-point buffer 9))))
    (org-clock-cancel (line-point buffer 1))
    (ok (null (org-clock-running (buffer-point buffer))))
    (ok (signals (org-clock-out (line-point buffer 1) '(2026 10 22 13 0)) 'editor-error))))
