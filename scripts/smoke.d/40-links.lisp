(in-package :cl-user)

(smoke-check "links in a line"
             '((:bracket "a" "b") (:plain "https://x.org" nil))
             (mapcar (lambda (l) (list (getf l :kind) (getf l :target) (getf l :description)))
                     (lem-org-mode::links-in-line "see [[a][b]] or https://x.org.")))

(smoke-check "link classification" '(:file "a.org" "*H") (multiple-value-list (lem-org-mode::link-classify "file:a.org::*H")))

(smoke-check "heading search finds the line"
             1
             (lem-org-mode::lines-find-search (list "x" "** TODO Heading :t:" "y") "*heading" '("TODO" "DONE")))

(smoke-check "id property is made once"
             t
             (let* ((b (smoke-buffer (format nil "* E~%")))
                    (one (lem-org-mode::org-id-get-create (smoke-line b 1)))
                    (two (lem-org-mode::org-id-get-create (smoke-line b 1))))
               (and (string= one two) (= 36 (length one)))))
