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
