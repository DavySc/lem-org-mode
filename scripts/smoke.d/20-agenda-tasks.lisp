(in-package :cl-user)

(smoke-check "todo sequence from #+TODO and CLOSED"
             (list "* NEXT a" t)
             (let ((b (smoke-buffer (format nil "#+TODO: TODO NEXT | DONE~%* TODO a~%"))))
               (lem-org-mode:org-todo-change (smoke-line b 2) "NEXT")
               (let ((first (lem:line-string (smoke-line b 2))))
                 (lem-org-mode:org-todo-change (smoke-line b 2) "DONE")
                 (list first (and (search "CLOSED:" (smoke-text b)) t)))))

(smoke-check "repeating task moves its date"
             t
             (let ((b (smoke-buffer (format nil "* TODO water~%SCHEDULED: <2026-10-22 Thu +1w>~%"))))
               (lem-org-mode:org-todo-change (smoke-line b 1) "DONE")
               (and (search "SCHEDULED: <2026-10-29 Thu +1w>" (smoke-text b)) (search "* TODO water" (smoke-text b)) t)))

(smoke-check "statistics cookie follows children"
             "* P [1/2]"
             (let ((b (smoke-buffer (format nil "* P [0/0]~%** TODO a~%** TODO b~%"))))
               (lem-org-mode:org-todo-change (smoke-line b 2) "DONE")
               (lem:line-string (smoke-line b 1))))

(smoke-check "agenda match view"
             t
             (let ((b (smoke-buffer (format nil "* TODO a :x:~%* TODO b :y:~%"))))
               (setf (lem:current-buffer) b)
               (multiple-value-bind (title records)
                   (lem-org-mode::agenda-block-records (list :type :tags :match "x" :origin b))
                 (declare (ignore title))
                 (and (= 1 (length records)) (string= "a" (getf (cdr (first records)) :title))))))
