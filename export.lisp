;;; Export: HTML and Markdown.
;;;
;;; `export-parse' turns the lines of an Org document into a list of nodes; `export-html' and
;;; `export-markdown' render those nodes. Both are pure so they can be tested without Lem.

(in-package :lem-org-mode)

;;; BEGIN pure export code

(defun html-escape (string)
  (with-output-to-string (out)
    (loop :for c :across string
          :do (case c
                (#\& (write-string "&amp;" out))
                (#\< (write-string "&lt;" out))
                (#\> (write-string "&gt;" out))
                (#\" (write-string "&quot;" out))
                (t (write-char c out))))))

(defun word-char-p (c)
  (or (alphanumericp c) (char= c #\_)))

(defun inline-closing-position (string marker start)
  "Position of the MARKER that closes an emphasis whose text begins at START, or NIL."
  (loop :for j :from start :below (length string)
        :when (and (char= (char string j) marker)
                   (> j start)
                   (not (member (char string (1- j)) '(#\space #\tab)))
                   (or (= (1+ j) (length string))
                       (not (word-char-p (char string (1+ j))))))
          :return j))

(defun export-link (target text format)
  (let ((url (if (and (> (length target) 5) (string= "file:" target :end2 5)) (subseq target 5) target)))
    (ecase format
      (:html (format nil "<a href=\"~A\">~A</a>" (html-escape url) text))
      (:markdown (format nil "[~A](~A)" text url)))))

(defun export-inline (string format)
  "Render the Org markup in STRING (links, *bold*, /italic/, _underline_, =code=, ~code~, +strike+)."
  (with-output-to-string (out)
    (let ((i 0) (n (length string)))
      (flet ((plain (text) (if (eq format :html) (html-escape text) text)))
        (loop :while (< i n)
              :do (let ((c (char string i)))
                    (cond
                      ((and (char= c #\[) (< (1+ i) n) (char= (char string (1+ i)) #\[)
                            (search "]]" string :start2 i))
                       (let* ((end (search "]]" string :start2 i))
                              (inner (subseq string (+ i 2) end))
                              (split (search "][" inner))
                              (target (if split (subseq inner 0 split) inner))
                              (text (if split (subseq inner (+ split 2)) inner)))
                         (write-string (export-link target (export-inline text format) format) out)
                         (setf i (+ end 2))))
                      ((and (find c "*/_=~+")
                            (or (= i 0) (not (word-char-p (char string (1- i)))))
                            (< (1+ i) n)
                            (not (member (char string (1+ i)) '(#\space #\tab)))
                            (inline-closing-position string c (1+ i)))
                       (let* ((end (inline-closing-position string c (1+ i)))
                              (inner (subseq string (1+ i) end))
                              (verbatim (find c "=~"))
                              (body (if verbatim (plain inner) (export-inline inner format))))
                         (write-string
                          (ecase format
                            (:html (format nil (ecase c
                                                 (#\* "<b>~A</b>") (#\/ "<i>~A</i>") (#\_ "<u>~A</u>")
                                                 ((#\= #\~) "<code>~A</code>") (#\+ "<del>~A</del>"))
                                           body))
                            (:markdown (format nil (ecase c
                                                     (#\* "**~A**") (#\/ "*~A*") (#\_ "~A")
                                                     ((#\= #\~) "`~A`") (#\+ "~~~~~A~~~~"))
                                               body)))
                          out)
                         (setf i (1+ end))))
                      (t (write-string (plain (string c)) out) (incf i)))))))))

(defun list-flat-items (lines)
  "Turn the LINES of a list into (indent bullet checkbox text) items; continuation lines join the item text."
  (let ((items '()))
    (dolist (line lines)
      (multiple-value-bind (indent bullet checkbox content) (list-item-parse line)
        (cond (indent
               (push (list indent bullet checkbox (string-trim " " (subseq line content))) items))
              ((and items (string/= (string-trim " " line) ""))
               (setf (fourth (first items))
                     (format nil "~A ~A" (fourth (first items)) (string-trim " " line)))))))
    (nreverse items)))

(defun list-tree (items)
  "Group flat ITEMS (indent bullet checkbox text) into nodes (bullet checkbox text children)."
  (when items
    (let ((indent (first (first items))) (result '()))
      (loop :while items
            :do (let* ((item (pop items))
                       (children (loop :while (and items (> (first (first items)) indent))
                                       :collect (pop items))))
                  (push (list (second item) (third item) (fourth item) (list-tree children)) result)))
      (nreverse result))))

(defun line-indent (line)
  (or (position #\space line :test-not #'char=) (length line)))

(defun org-blank-line-p (line)
  (string= (string-trim '(#\space #\tab) line) ""))

(defun ordered-item-p (line)
  (let ((bullet (nth-value 1 (list-item-parse line))))
    (and bullet (digit-char-p (char bullet 0)) t)))

(defun list-region-end (lines start)
  "Index after the last line of the list that begins at START in the vector LINES."
  (let* ((base (line-indent (aref lines start))) (n (length lines)) (j start))
    (loop :while (< j n)
          :do (let ((line (aref lines j)))
                (cond ((org-blank-line-p line)
                       (let ((next (and (< (1+ j) n) (aref lines (1+ j)))))
                         (if (and next (not (org-blank-line-p next))
                                  (or (list-item-parse next) (> (line-indent next) base)))
                             (incf j)
                             (return))))
                      ((and (list-item-parse line) (= (line-indent line) base)
                            (not (eq (ordered-item-p line) (ordered-item-p (aref lines start)))))
                       (return))
                      ((or (and (list-item-parse line) (>= (line-indent line) base))
                           (> (line-indent line) base))
                       (incf j))
                      (t (return)))))
    j))

(defun block-name (line)
  "For \"#+begin_src sql\" return \"src\" and the rest of the line."
  (let* ((s (string-left-trim '(#\space #\tab) line))
         (words (split-words (subseq s 8))))
    (values (string-downcase (first words)) (rest words))))

(defun keyword-line (line)
  "Return (values name value) for a #+NAME: value line."
  (let* ((s (string-left-trim '(#\space #\tab) line))
         (colon (and (>= (length s) 3) (string= "#+" s :end2 2) (position #\: s))))
    (when (and colon (> colon 2))
      (values (string-upcase (subseq s 2 colon)) (string-trim " " (subseq s (1+ colon)))))))

(defun drawer-start-p (line)
  (let ((s (string-trim '(#\space #\tab) line)))
    (and (> (length s) 2) (char= (char s 0) #\:) (char= (char s (1- (length s))) #\:)
         (notany (lambda (c) (member c '(#\space #\tab))) s)
         (not (string-equal s ":END:")))))

(defun paragraph-break-p (line keywords)
  (or (org-blank-line-p line)
      (heading-split line keywords)
      (table-line-p line)
      (list-item-parse line)
      (line-starts-with-ci-p line "#+")
      (and (>= (length line) 2) (string= ": " line :end2 2))
      (drawer-start-p line)))

(defun export-parse (lines keywords)
  "Return (values nodes meta) for the Org text LINES. META is an alist such as ((\"TITLE\" . \"Notes\"))."
  (let* ((lines (coerce lines 'vector)) (n (length lines)) (i 0)
         (nodes '()) (meta '()) (exports "code"))
    (flet ((emit (node) (push node nodes))
           (find-end (name from)
             (or (position-if (lambda (line) (and (line-starts-with-ci-p line "#+end_")
                                                  (string-equal name (string-trim " " (subseq (string-left-trim " " line) 6)))))
                              lines :start from)
                 n)))
      (loop :while (< i n)
            :do (let ((line (aref lines i)))
                  (cond
                    ((org-blank-line-p line) (incf i))
                    ((line-starts-with-ci-p line "#+begin_")
                     (multiple-value-bind (name args) (block-name line)
                       (let* ((end (find-end name (1+ i)))
                              (inner (coerce (subseq lines (1+ i) end) 'list)))
                         (cond ((string= name "src")
                                (multiple-value-bind (language header) (src-header line)
                                  (setf exports (or (cdr (assoc "exports" header :test #'string=)) "code"))
                                  (when (member exports '("code" "both") :test #'string=)
                                    (emit (list :src language inner)))))
                               ((string= name "quote") (emit (list :quote (export-parse inner keywords))))
                               ((string= name "center") (emit (list :center (export-parse inner keywords))))
                               ((string= name "verse") (emit (list :verse inner)))
                               ((string= name "comment"))
                               ((string= name "export") (emit (list :raw (first args) inner)))
                               (t (emit (list :example inner))))
                         (setf i (1+ end)))))
                    ((keyword-line line)
                     (multiple-value-bind (name value) (keyword-line line)
                       (cond ((string= name "RESULTS")
                              (let ((end (or (position-if-not
                                              (lambda (l) (or (table-line-p l)
                                                              (and (>= (length l) 2) (string= ": " l :end2 2))))
                                              lines :start (1+ i))
                                             n)))
                                (when (member exports '("results" "both") :test #'string=)
                                  (let ((results (coerce (subseq lines (1+ i) end) 'list)))
                                    (if (and results (table-line-p (first results)))
                                        (emit (list :table (mapcar #'table-split-line results)))
                                        (emit (list :example (mapcar (lambda (l) (subseq l 2)) results))))))
                                (setf i end)))
                             (t (when (member name '("TITLE" "AUTHOR" "DATE" "EMAIL") :test #'string=)
                                  (push (cons name value) meta))
                                (incf i)))))
                    ((or (string= (string-trim " " line) "#")
                         (line-starts-with-ci-p line "# "))
                     (incf i))
                    ((heading-split line keywords)
                     (multiple-value-bind (level todo priority title tags) (heading-split line keywords)
                       (emit (list :heading level todo priority title tags))
                       (incf i)
                       (when (and (< i n) (planning-line-p (aref lines i)))
                         (emit (list :planning (string-trim " " (aref lines i))))
                         (incf i))
                       (when (and (< i n) (string-equal (string-trim " " (aref lines i)) ":PROPERTIES:"))
                         (setf i (let ((end (position-if (lambda (l) (string-equal (string-trim " " l) ":END:"))
                                                         lines :start i)))
                                   (if end (1+ end) n))))))
                    ((drawer-start-p line)
                     (setf i (let ((end (position-if (lambda (l) (string-equal (string-trim " " l) ":END:"))
                                                     lines :start i)))
                               (if end (1+ end) (1+ i)))))
                    ((table-line-p line)
                     (let ((end (or (position-if-not #'table-line-p lines :start i) n)))
                       (emit (list :table (map 'list #'table-split-line (subseq lines i end))))
                       (setf i end)))
                    ((and (>= (length line) 2) (string= ": " line :end2 2))
                     (let ((end (or (position-if-not (lambda (l) (and (>= (length l) 2) (string= ": " l :end2 2)))
                                                     lines :start i)
                                    n)))
                       (emit (list :example (map 'list (lambda (l) (subseq l 2)) (subseq lines i end))))
                       (setf i end)))
                    ((and (>= (length (string-trim " " line)) 5)
                          (every (lambda (c) (char= c #\-)) (string-trim " " line)))
                     (emit (list :rule))
                     (incf i))
                    ((list-item-parse line)
                     (let* ((end (list-region-end lines i))
                            (flat (list-flat-items (coerce (subseq lines i end) 'list))))
                       (emit (list :list (digit-char-p (char (second (first flat)) 0)) (list-tree flat)))
                       (setf i end)))
                    (t
                     (let ((end (or (position-if (lambda (l) (paragraph-break-p l keywords)) lines :start (1+ i)) n)))
                       (emit (list :paragraph
                                   (format nil "~{~A~^ ~}"
                                           (map 'list (lambda (l) (string-trim " " l)) (subseq lines i end)))))
                       (setf i end)))))))
    (values (nreverse nodes) (nreverse meta))))

(defun table-header-count (rows)
  "How many leading rows of ROWS come before the first horizontal rule, or 0."
  (let ((rule (position :rule rows)))
    (if (and rule (plusp rule)) rule 0)))

(defun join-lines (lines)
  (format nil "~{~A~^~%~}" lines))

(defun html-list (ordered tree)
  (let ((tag (if ordered "ol" "ul")))
    (format nil "<~A>~%~{~A~%~}</~A>" tag
            (mapcar (lambda (item)
                      (destructuring-bind (bullet checkbox text children) item
                        (declare (ignore bullet))
                        (format nil "<li>~@[<code>[~A]</code> ~]~A~@[~%~A~]</li>"
                                (and checkbox (string checkbox))
                                (export-inline text :html)
                                (and children (html-list (digit-char-p (char (first (first children)) 0)) children)))))
                    tree)
            tag)))

(defun html-node (node keywords)
  (ecase (first node)
    (:heading
     (destructuring-bind (level todo priority title tags) (rest node)
       (let ((h (min 6 (1+ level))))
         (format nil "<h~D>~@[<span class=\"todo ~A\">~:*~A</span> ~]~@[<span class=\"priority\">[#~A]</span> ~]~A~@[ <span class=\"tag\">~{<span>~A</span>~^ ~}</span>~]</h~D>"
                 h todo priority (export-inline title :html) tags h))))
    (:planning (format nil "<p class=\"planning\"><i>~A</i></p>" (html-escape (second node))))
    (:paragraph (format nil "<p>~A</p>" (export-inline (second node) :html)))
    (:list (html-list (second node) (third node)))
    (:table
     (let* ((rows (second node)) (head (table-header-count rows)))
       (flet ((row (cells tag)
                (format nil "<tr>~{<~A>~A</~A>~}</tr>"
                        (loop :for cell :in cells :collect tag :collect (export-inline cell :html) :collect tag))))
         (format nil "<table>~%~@[<thead>~%~{~A~%~}</thead>~%~]<tbody>~%~{~A~%~}</tbody>~%</table>"
                 (and (plusp head) (mapcar (lambda (r) (row r "th")) (subseq rows 0 head)))
                 (mapcar (lambda (r) (row r "td"))
                         (remove :rule (nthcdr (if (plusp head) head 0) rows)))))))
    (:src (format nil "<pre class=\"src src-~A\"><code>~A</code></pre>" (or (second node) "text")
                  (html-escape (join-lines (third node)))))
    (:example (format nil "<pre class=\"example\">~A</pre>" (html-escape (join-lines (second node)))))
    (:verse (format nil "<pre class=\"verse\">~A</pre>" (html-escape (join-lines (second node)))))
    (:quote (format nil "<blockquote>~%~A~%</blockquote>" (html-nodes (second node) keywords)))
    (:center (format nil "<div class=\"center\">~%~A~%</div>" (html-nodes (second node) keywords)))
    (:rule "<hr>")
    (:raw (if (equal (second node) "html") (join-lines (third node)) ""))))

(defun html-nodes (nodes keywords)
  (join-lines (remove "" (mapcar (lambda (node) (html-node node keywords)) nodes) :test #'string=)))

(defun export-html (lines keywords)
  "Render the Org text LINES as a complete HTML document."
  (multiple-value-bind (nodes meta) (export-parse lines keywords)
    (let ((title (cdr (assoc "TITLE" meta :test #'string=)))
          (author (cdr (assoc "AUTHOR" meta :test #'string=))))
      (format nil "<!DOCTYPE html>~%<html>~%<head>~%<meta charset=\"utf-8\">~%<title>~A</title>~%~
<style>body{max-width:50em;margin:2em auto;padding:0 1em;font-family:sans-serif;line-height:1.5}~
pre{background:#f4f4f4;padding:.6em;overflow:auto}table{border-collapse:collapse}~
td,th{border:1px solid #ccc;padding:.2em .6em}.todo{color:#c00;font-weight:bold}~
.TODO{color:#c00}.DONE{color:#080}.tag{font-size:smaller;color:#666}.planning{color:#666}</style>~%~
</head>~%<body>~%~@[<h1 class=\"title\">~A</h1>~%~]~@[<p class=\"author\">~A</p>~%~]~A~%</body>~%</html>~%"
              (html-escape (or title "")) (and title (html-escape title)) (and author (html-escape author))
              (html-nodes nodes keywords)))))

(defun markdown-list (ordered tree depth)
  (join-lines
   (loop :for item :in tree
         :for number :from 1
         :collect (destructuring-bind (bullet checkbox text children) item
                    (declare (ignore bullet))
                    (format nil "~A~A ~@[[~A] ~]~A~@[~%~A~]"
                            (make-string (* 3 depth) :initial-element #\space)
                            (if ordered (format nil "~D." number) "-")
                            (and checkbox (string checkbox))
                            (export-inline text :markdown)
                            (and children
                                 (markdown-list (digit-char-p (char (first (first children)) 0)) children (1+ depth))))))))

(defun markdown-node (node offset keywords)
  (ecase (first node)
    (:heading
     (destructuring-bind (level todo priority title tags) (rest node)
       (declare (ignore tags))
       (format nil "~A ~@[**~A** ~]~@[[#~A] ~]~A" (make-string (min 6 (+ level offset)) :initial-element #\#)
               todo priority (export-inline title :markdown))))
    (:planning (format nil "*~A*" (second node)))
    (:paragraph (export-inline (second node) :markdown))
    (:list (markdown-list (second node) (third node) 0))
    (:table
     (let* ((rows (remove :rule (second node))) (head (max 1 (table-header-count (second node)))))
       (flet ((row (cells) (format nil "| ~{~A~^ | ~} |" (mapcar (lambda (c) (export-inline c :markdown)) cells))))
         (join-lines (append (mapcar #'row (subseq rows 0 (min head (length rows))))
                             (list (format nil "|~{~A~}" (loop :repeat (length (first rows)) :collect " --- |")))
                             (mapcar #'row (nthcdr (min head (length rows)) rows)))))))
    (:src (format nil "```~A~%~A~%```" (or (second node) "") (join-lines (third node))))
    ((:example :verse) (format nil "```~%~A~%```" (join-lines (second node))))
    (:quote (join-lines (mapcar (lambda (line) (if (string= line "") ">" (format nil "> ~A" line)))
                                (split-text-lines (markdown-nodes (second node) offset keywords)))))
    (:center (markdown-nodes (second node) offset keywords))
    (:rule "---")
    (:raw (if (member (second node) '("md" "markdown") :test #'equal) (join-lines (third node)) ""))))

(defun markdown-nodes (nodes offset keywords)
  (format nil "~{~A~^~%~%~}"
          (remove "" (mapcar (lambda (node) (markdown-node node offset keywords)) nodes) :test #'string=)))

(defun export-markdown (lines keywords)
  "Render the Org text LINES as Markdown."
  (multiple-value-bind (nodes meta) (export-parse lines keywords)
    (let ((title (cdr (assoc "TITLE" meta :test #'string=))))
      (format nil "~@[# ~A~%~%~]~A~%" title (markdown-nodes nodes (if title 1 0) keywords)))))

;;; END pure export code

(defun export-to-file (buffer type function)
  "Write FUNCTION's rendering of BUFFER to a file with extension TYPE next to it, or to a new buffer when it has no file."
  (let ((text (funcall function (split-text-lines (buffer-text buffer)) *org-todo-keywords*))
        (file (buffer-filename buffer)))
    (cond (file
           (let ((target (make-pathname :type type :defaults file)))
             (with-open-file (out target :direction :output :if-exists :supersede :external-format :utf-8)
               (write-string text out))
             (message "Exported to ~A" target)
             target))
          (t
           (let* ((name (format nil "*Org ~A Export*" (string-upcase type)))
                  (export (or (get-buffer name) (make-buffer name))))
             (erase-buffer export)
             (insert-string (buffer-point export) text)
             (switch-to-buffer export)
             export)))))

(define-command org-export-html () ()
  "Export the buffer to an HTML file next to it."
  (export-to-file (current-buffer) "html" #'export-html))

(define-command org-export-markdown () ()
  "Export the buffer to a Markdown file next to it."
  (export-to-file (current-buffer) "md" #'export-markdown))
