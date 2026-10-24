
;;; live sessions
(check "session request" (format nil "value~C/tmp/a.py~CEND~%" #\tab #\tab) (babel-session-request "value" "/tmp/a.py" "END"))
(check "session output" '("a" "a
" "" "x") (list (babel-session-output (format nil "a~%END~%") "END") (babel-session-output (format nil "a~%~%END~%") "END")
                (babel-session-output (format nil "~%END~%") "END") (babel-session-output "x" "END")))
(check "replace text" "a-1-b-1" (babel-replace-text "a-%V-b-%V" "%V" "1"))
(check "session js var" '("var n = 1;" "const n = 1;") (list (babel-assignment :js "n" 1 t) (babel-assignment :js "n" 1)))
(check "session servers" '(:python :js :ruby :shell) (mapcar #'car *babel-session-servers*))
