(in-package :cl-user)

(smoke-check "inline src result"
             (format nil "x src_sh{echo 5} {{{results(=5=)}}}~%")
             (let ((b (smoke-buffer (format nil "x src_sh{echo 5}~%"))))
               (lem-org-mode:org-babel-execute (let ((p (smoke-line b 1))) (lem:character-offset p 4) p))
               (smoke-text b)))

(smoke-check "#+header overrides the begin_src line"
             (format nil "#+header: :results output~%#+begin_src sh :results value~%echo hi~%#+end_src~%~%#+RESULTS:~%: hi~%")
             (let ((b (smoke-buffer (format nil "#+header: :results output~%#+begin_src sh :results value~%echo hi~%#+end_src~%"))))
               (lem-org-mode:org-babel-execute (smoke-line b 3))
               (smoke-text b)))

(smoke-check "var index"
             (format nil "#+NAME: t~%| a | 1 |~%| b | 2 |~%#+begin_src sh :var x=t[1,1] :results output~%echo $x~%#+end_src~%~%#+RESULTS:~%: 2~%")
             (let ((b (smoke-buffer (format nil "#+NAME: t~%| a | 1 |~%| b | 2 |~%#+begin_src sh :var x=t[1,1] :results output~%echo $x~%#+end_src~%"))))
               (lem-org-mode:org-babel-execute (smoke-line b 5))
               (smoke-text b)))
