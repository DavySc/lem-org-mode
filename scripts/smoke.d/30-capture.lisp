(in-package :cl-user)

(smoke-check "capture expand"
             (list "* TODO <2026-10-22 Thu> " 7)
             (multiple-value-bind (text cursor)
                 (lem-org-mode::capture-expand "* TODO %t %?" (list :time '(2026 10 22 9 0 0)))
               (list text cursor)))

(smoke-check "capture insert under a headline"
             (list "* Tasks" "** TODO x")
             (first (multiple-value-list (lem-org-mode::capture-insert (list "* Tasks") '(:olp ("Tasks")) 'entry "* TODO x" '("TODO" "DONE")))))

(smoke-check "refile moves a subtree"
             (format nil "* A~%** x~%* B~%")
             (let ((b (smoke-buffer (format nil "* A~%* B~%** x~%"))))
               (lem-org-mode:org-refile (smoke-line b 3)
                                        (find "A" (lem-org-mode::agenda-entries nil b) :key (lambda (e) (getf e :title)) :test #'string=))
               (smoke-text b)))
