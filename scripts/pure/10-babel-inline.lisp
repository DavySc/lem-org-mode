;;; Babel: #+header lines, inline blocks, indexes, output files

(defvar *kw2* '("TODO" "DONE"))
(check "#+header lines override the begin_src line"
       '(("results" . "output") ("var" . "x=2"))
       (let ((r (find :src (babel-scan (list "#+header: :results output :var x=2" "#+begin_src sh :results value :var x=1" "echo" "#+end_src") *kw2*)
                      :key (lambda (r) (getf r :kind)))))
         (remove-if-not (lambda (e) (member (car e) '("results" "var") :test #'string=)) (getf r :own))))
(check "#+header keeps #+name" "n"
       (getf (first (babel-scan (list "#+name: n" "#+header: :tangle yes" "#+begin_src sh" "x" "#+end_src") *kw2*)) :name))

(check "inline src"
       '((:kind :inline-src :col 5 :col-end 32 :language "python" :header ":var x=1" :code "x + 1"))
       (inline-babel-find "Some src_python[:var x=1]{x + 1} text"))
(check "inline src without header" "1+1" (getf (first (inline-babel-find "a src_sh{1+1}")) :code))
(check "inline src with braces in code" "{a}" (getf (first (inline-babel-find "src_js{{a}}")) :code))
(check "src_ inside a word is not inline" nil (inline-babel-find "mysrc_python{1}"))
(check "inline call" '(:inline-call 5 "sq[:results raw](4)")
       (let ((f (first (inline-babel-find "Call call_sq[:results raw](4) here"))))
         (list (getf f :kind) (getf f :col) (getf f :text))))
(check "inline call with end header" "sq(4)[:results raw]" (getf (first (inline-babel-find "call_sq(4)[:results raw]")) :text))
(check "two inline elements" '(:inline-src :inline-call) (mapcar (lambda (f) (getf f :kind)) (inline-babel-find "src_sh{a} and call_x(1)")))

(check "inline scan records with inherited args"
       '(:inline-src 2 "output replace" "results")
       (let ((r (find :inline-src (babel-scan (list "#+PROPERTY: header-args :results output" "* H" "text src_sh{echo 1} more") *kw2*)
                      :key (lambda (r) (getf r :kind)))))
         (list (getf r :kind) (getf r :begin) (header-arg (getf r :args) "results") (header-arg (getf r :args) "exports"))))
(check "inline scan skips blocks" nil
       (remove-if-not (lambda (r) (eq (getf r :kind) :inline-src))
                      (babel-scan (list "#+begin_example" "src_sh{x}" "#+end_example") *kw2*)))
(check "inline result text" "=2="  (inline-result-text 2))
(check "inline result list" "=(1 2)=" (inline-result-text '(1 2)))
(check "inline result table" "=((1 2) (3 4))=" (inline-result-text '((1 2) (3 4))))
(check "inline result added" "a src_sh{echo 2} {{{results(=2=)}}} b"
       (inline-replace-result "a src_sh{echo 2} b" 16 "2"))
(check "inline result replaced" "a src_sh{echo 3} {{{results(=3=)}}} b"
       (inline-replace-result "a src_sh{echo 3} {{{results(=2=)}}} b" 16 "3"))
(check "inline result removed" "a src_sh{echo 3} b" (inline-replace-result "a src_sh{echo 3} {{{results(=2=)}}} b" 16 nil))

(defvar *tbl* '(("a" 1 2) ("b" 3 4) ("c" 5 6)))
(check "index single" '("b" 3 4) (value-index *tbl* "1"))
(check "index cell" 4 (value-index *tbl* "1,2"))
(check "index column" '(2 4 6) (value-index *tbl* "*,2"))
(check "index negative" '("c" 5 6) (value-index *tbl* "-1"))
(check "index range" '(("a" 1) ("b" 3)) (value-index *tbl* "0:1,0:1"))
(check "index flat list" "y" (value-index '("x" "y" "z") "1"))
(check "index out of range" nil (value-index '("x") "5"))
(check "reference parse" '("data" "1,2") (multiple-value-list (reference-parse "data[1,2]")))
(check "reference without index" '("data" nil) (multiple-value-list (reference-parse "data")))
(check "call is not an index" '("f(a[1])" nil) (multiple-value-list (reference-parse "f(a[1])")))
(check "reference file" '("n.org" "tbl") (multiple-value-list (reference-file-split "n.org:tbl")))
(check "reference no file" nil (reference-file-split "tbl"))

(check "output file explicit" "a.png" (babel-output-file '(("file" . "a.png")) nil "h"))
(check "output file with dir" "img/a.png" (babel-output-file '(("file" . "a.png") ("output-dir" . "img")) nil "h"))
(check "output file from ext and name" "fig.svg" (babel-output-file '(("file-ext" . "svg")) "fig" "h"))
(check "output file from ext and hash" "h.svg" (babel-output-file '(("file-ext" . "svg")) nil "h"))
(check "no output file" nil (babel-output-file '(("results" . "value")) "n" "h"))
(check ":sep splits" '(("a" "b") ("c" "d")) (text-to-rows (format nil "a;b~%c;d") "\";\""))
(check "tab text" '(("a" "b")) (text-to-rows (format nil "a	b")))
