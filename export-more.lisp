;;; More export backends: plain text (ASCII), LaTeX, Org (the document after includes, macros and filters)
;;; and iCalendar; subtree export; and the export dispatcher.

(in-package :lem-org-mode)

;;; BEGIN pure export-more code

(defvar *org-ascii-text-width* 72
  "The line width of the plain text export.")

(defvar *org-latex-class* "article"
  "The LaTeX document class when the file has no #+LATEX_CLASS:.")

(defvar *org-latex-packages* '("graphicx" "longtable" "hyperref" "amsmath" "amssymb" "ulem" "capt-of")
  "LaTeX packages that every exported document loads.")

(defvar *org-icalendar-include-todo* nil
  "True: entries with a todo keyword and a date also become VTODO items in the iCalendar export.")

;;; Plain text

(defun wrap-words (text width &optional (indent 0) (first-indent indent))
  "TEXT broken into lines of at most WIDTH columns. The first line is indented by FIRST-INDENT, the others by INDENT."
  (let ((words (split-words text)) (lines '()) (current (make-string-output-stream)) (column first-indent) (first t))
    (write-string (make-string first-indent :initial-element #\space) current)
    (dolist (word words)
      (let ((need (+ (length word) (if (> column (if first first-indent indent)) 1 0))))
        (when (and (> (+ column need) width) (> column (if first first-indent indent)))
          (push (get-output-stream-string current) lines)
          (setf first nil column indent)
          (write-string (make-string indent :initial-element #\space) current))
        (when (> column (if first first-indent indent)) (write-char #\space current) (incf column))
        (write-string word current)
        (incf column (length word))))
    (push (get-output-stream-string current) lines)
    (nreverse lines)))

(defun ascii-fill (text width indent)
  "TEXT (which may hold line breaks from \\\\) filled to WIDTH, every line indented by INDENT."
  (loop :for part :in (split-text-lines text)
        :append (if (blank-string-p part) (list "") (wrap-words part width indent))))

(defun ascii-list (ordered tree depth)
  (loop :for item :in tree
        :for number :from 1
        :append (destructuring-bind (bullet checkbox text children) item
                  (declare (ignore bullet))
                  (let* ((marker (if ordered (format nil "~D. " number) "- "))
                         (indent (* 3 depth))
                         (text (let ((split (search " :: " text)))
                                 (if split (format nil "~A :: ~A" (subseq text 0 split) (subseq text (+ split 4))) text)))
                         (rendered (export-inline (format nil "~@[[~A] ~]~A" (and checkbox (string checkbox)) text) :ascii)))
                    (let ((lines (wrap-words (substitute #\space #\newline rendered) *org-ascii-text-width*
                                             (+ indent (length marker)) (+ indent (length marker)))))
                      (append (list (concatenate 'string (make-string indent :initial-element #\space) marker
                                                 (string-left-trim " " (first lines))))
                              (rest lines)
                              (and children (ascii-list (digit-char-p (char (first (first children)) 0)) children (1+ depth)))))))))

(defun ascii-heading-underline (level)
  (case level (1 #\=) (2 #\-) (3 #\~) (t #\^)))

(defun ascii-node (node)
  "The lines for NODE in the plain text export."
  (let* ((affiliated (and (eq (first node) :affiliated) (second node)))
         (node (node-core node)))
    (ecase (first node)
      ((:heading :properties :footnote-def :toc) nil)
      (:planning (and (option :planning) (list (second node))))
      (:paragraph (ascii-fill (export-inline (second node) :ascii) *org-ascii-text-width* 0))
      (:list (ascii-list (second node) (third node) 0))
      (:table (append (and (caption-text affiliated) (list (format nil "Table ~D: ~A" (next-counter :table) (caption-text affiliated))))
                      (org-table-format-rows (mapcar (lambda (row) (if (eq row :rule) row (mapcar (lambda (c) (export-inline c :ascii)) row)))
                                                     (second node)))))
      ((:src :example) (mapcar (lambda (l) (concatenate 'string "     " l)) (if (eq (first node) :src) (third node) (second node))))
      (:verse (mapcar (lambda (l) (concatenate 'string "     " (export-inline l :ascii))) (second node)))
      (:quote (mapcar (lambda (l) (if (string= l "") l (concatenate 'string "     " l)))
                      (ascii-nodes (second node))))
      (:center (mapcar (lambda (l) (let ((pad (max 0 (floor (- *org-ascii-text-width* (length l)) 2))))
                                     (concatenate 'string (make-string pad :initial-element #\space) l)))
                       (ascii-nodes (second node))))
      (:block (ascii-nodes (third node)))
      (:rule (list (make-string *org-ascii-text-width* :initial-element #\-)))
      (:raw (and (member (second node) '("ascii" "text") :test #'equal) (third node))))))

(defun ascii-nodes (nodes)
  (let ((blocks (remove nil (mapcar #'ascii-node nodes))))
    (loop :for block :in blocks :for first := t :then nil
          :append (if first block (cons "" block)))))

(defun ascii-section (section)
  (if (not (section-included-p section))
      nil
      (let* ((level (section-level section))
             (text (format nil "~@[~A ~]~@[~A ~]~A~@[   :~{~A~^:~}:~]"
                           (section-number section) (and (option :todo) (section-todo section))
                           (export-inline (heading-plain-text (section-title section)) :ascii)
                           (and (option :tags) (section-tags section))))
             (title-line (string-left-trim " " text)))
        (append (list title-line (make-string (length title-line) :initial-element (ascii-heading-underline level)) "")
                (let ((body (ascii-nodes (section-body section)))) (and body (append body (list ""))))
                (loop :for child :in (section-children section) :append (ascii-section child))))))

(defun ascii-toc (entries depth)
  (loop :for entry :in entries
        :append (cons (format nil "~A~@[~A ~]~A" (make-string (* 2 depth) :initial-element #\space)
                              (section-number (car entry)) (heading-plain-text (section-title (car entry))))
                      (ascii-toc (cdr entry) (1+ depth)))))

(defun export-ascii (lines keywords &key file mtime reader)
  "Render the Org text LINES as plain text."
  (multiple-value-bind (lines ctx) (prepare-document lines keywords :ascii :file file :mtime mtime :reader reader)
    (let ((*export-ctx* ctx))
      (multiple-value-bind (nodes meta) (export-parse lines keywords)
        (setf (export-ctx-meta ctx) meta (export-ctx-options ctx) (export-options meta)
              (export-ctx-definitions ctx)
              (loop :for node :in nodes :for core := (node-core node)
                    :when (eq (first core) :footnote-def) :collect (cons (second core) (format nil "~{~A~^ ~}" (third core)))))
        (multiple-value-bind (preamble sections) (export-sections nodes)
          (assign-section-numbers sections ctx)
          (let* ((title (meta-get meta "TITLE")) (author (meta-get meta "AUTHOR"))
                 (head (append (and title (option :title)
                                    (list (export-inline title :ascii) (make-string (length title) :initial-element #\=) ""))
                               (and author (option :author) (list (format nil "Author: ~A" author) ""))
                               (and (option :toc) sections
                                    (append (list "Table of Contents" "-----------------")
                                            (ascii-toc (toc-entries sections (let ((toc (option :toc))) (if (integerp toc) toc (or (option :levels) 3)))) 0)
                                            (list "")))))
                 (body (append (let ((pre (ascii-nodes preamble))) (and pre (append pre (list ""))))
                               (loop :for s :in sections :append (ascii-section s))))
                 (notes (export-ctx-footnotes ctx))
                 (footnotes (and (plusp (length notes))
                                 (append (list "Footnotes" "---------" "")
                                         (loop :for note :across notes
                                               :append (destructuring-bind (label number text) note
                                                         (declare (ignore label))
                                                         (wrap-words (format nil "[~D] ~A" number (export-inline (if (listp text) (format nil "~{~A~^ ~}" text) (or text "")) :ascii))
                                                                     *org-ascii-text-width* 4 0)))))))
            (format nil "~{~A~%~}" (append head body footnotes))))))))

;;; Org

(defun export-org (lines keywords &key file mtime reader)
  "The Org text LINES after includes, setup files, macros and subtree filters have been applied."
  (format nil "~{~A~%~}" (values (prepare-document lines keywords :org :file file :mtime mtime :reader reader))))

;;; LaTeX

(defun latex-section-command (class level numbered)
  (let* ((names (cond ((member class '("report" "book") :test #'string-equal) '("chapter" "section" "subsection" "subsubsection" "paragraph" "subparagraph"))
                      (t '("section" "subsection" "subsubsection" "paragraph" "subparagraph"))))
         (name (nth (min (1- level) (1- (length names))) names)))
    (format nil "\\~A~:[*~;~]" name numbered)))

(defun latex-list (ordered tree depth)
  (let ((description (and tree (every (lambda (item) (search " :: " (third item))) tree)))
        (environment (if ordered "enumerate" "itemize")))
    (format nil "\\begin{~A}~%~{~A~%~}\\end{~A}"
            (if description "description" environment)
            (mapcar (lambda (item)
                      (destructuring-bind (bullet checkbox text children) item
                        (declare (ignore bullet))
                        (let ((split (and description (search " :: " text))))
                          (format nil "\\item~A ~A~A~A"
                                  (if split (format nil "[~A]" (export-inline (subseq text 0 split) :latex)) "")
                                  (if checkbox (format nil "~A " (case checkbox (#\X "$\\boxtimes$") (#\- "$\\boxminus$") (t "$\\square$"))) "")
                                  (export-inline (if split (subseq text (+ split 4)) text) :latex)
                                  (if children
                                      (format nil "~%~A" (latex-list (digit-char-p (char (first (first children)) 0)) children (1+ depth)))
                                      "")))))
                    tree)
            (if description "description" environment))))

(defun latex-table (rows affiliated)
  (let* ((alignments (table-column-alignments rows)) (head (table-header-count rows)) (caption (caption-text affiliated))
         (body (with-output-to-string (out)
                 (format out "\\begin{tabular}{~{~A~}}~%\\hline~%" (mapcar (lambda (a) (if (eq a :right) "r" "l")) alignments))
                 (loop :for row :in rows :for index :from 0
                       :do (if (eq row :rule)
                               (format out "\\hline~%")
                               (format out "~{~A~^ & ~} \\\\~%" (mapcar (lambda (c) (export-inline c :latex)) row))))
                 (when (or (zerop head) (not (eq (car (last rows)) :rule))) (format out "\\hline~%"))
                 (format out "\\end{tabular}"))))
    (if caption
        (format nil "\\begin{table}[htbp]~%\\caption{~A}~%\\centering~%~A~%\\end{table}" (export-inline caption :latex) body)
        body)))

(defun latex-node (node)
  (let* ((affiliated (and (eq (first node) :affiliated) (second node)))
         (node (node-core node)))
    (ecase (first node)
      ((:heading :properties :footnote-def :toc) "")
      (:planning "")
      (:paragraph (let* ((text (second node)) (links (links-in-line text)) (caption (caption-text affiliated)))
                    (if (and links (null (cdr links)) (zerop (getf (first links) :start)) (= (getf (first links) :end) (length text))
                             (eq :image (resolve-link (getf (first links) :target) :latex)) (null (getf (first links) :description)))
                        (format nil "\\begin{figure}[htbp]~%\\centering~%~A~@[~%\\caption{~A}~]~%\\end{figure}"
                                (export-inline text :latex) (and caption (export-inline caption :latex)))
                        (export-inline text :latex))))
      (:list (latex-list (second node) (third node) 0))
      (:table (latex-table (second node) affiliated))
      (:src (format nil "\\begin{verbatim}~%~{~A~%~}\\end{verbatim}" (third node)))
      (:example (format nil "\\begin{verbatim}~%~{~A~%~}\\end{verbatim}" (second node)))
      (:verse (format nil "\\begin{verse}~%~{~A~^ \\\\~%~}~%\\end{verse}" (mapcar (lambda (l) (export-inline l :latex)) (second node))))
      (:quote (format nil "\\begin{quote}~%~A~%\\end{quote}" (latex-nodes (second node))))
      (:center (format nil "\\begin{center}~%~A~%\\end{center}" (latex-nodes (second node))))
      (:block (format nil "\\begin{~A}~%~A~%\\end{~A}" (second node) (latex-nodes (third node)) (second node)))
      (:rule "\\rule{\\linewidth}{0.5pt}")
      (:raw (if (equal (second node) "latex") (join-lines (third node)) "")))))

(defun latex-nodes (nodes)
  (format nil "~{~A~^~%~%~}" (remove "" (mapcar #'latex-node nodes) :test #'string=)))

(defun latex-section (section class)
  (if (not (section-included-p section))
      ""
      (let ((numbered (and (section-number section) t)))
        (format nil "~A{~A~A}~%\\label{~A}~@[~%~%~A~]~@[~%~%~A~]"
                (latex-section-command class (section-level section) numbered)
                (if (and (section-todo section) (option :todo)) (format nil "\\textbf{~A} " (section-todo section)) "")
                (export-inline (heading-plain-text (section-title section)) :latex)
                (section-id section)
                (let ((body (latex-nodes (section-body section)))) (and (plusp (length body)) body))
                (let ((children (format nil "~{~A~^~%~%~}" (remove "" (mapcar (lambda (c) (latex-section c class)) (section-children section)) :test #'string=))))
                  (and (plusp (length children)) children))))))

(defun export-latex (lines keywords &key body-only file mtime reader)
  "Render the Org text LINES as a LaTeX document (or just the body with BODY-ONLY)."
  (multiple-value-bind (lines ctx) (prepare-document lines keywords :latex :file file :mtime mtime :reader reader)
    (let ((*export-ctx* ctx))
      (multiple-value-bind (nodes meta) (export-parse lines keywords)
        (setf (export-ctx-meta ctx) meta (export-ctx-options ctx) (export-options meta)
              (export-ctx-definitions ctx)
              (loop :for node :in nodes :for core := (node-core node)
                    :when (eq (first core) :footnote-def) :collect (cons (second core) (format nil "~{~A~^ ~}" (third core)))))
        (multiple-value-bind (preamble sections) (export-sections nodes)
          (assign-section-numbers sections ctx)
          (let* ((class (or (meta-get meta "LATEX_CLASS") *org-latex-class*))
                 (class-options (or (meta-get meta "LATEX_CLASS_OPTIONS") "[11pt]"))
                 (title (meta-get meta "TITLE")) (author (meta-get meta "AUTHOR")) (date (meta-get meta "DATE"))
                 (body (format nil "~{~A~^~%~%~}"
                               (remove "" (list (latex-nodes preamble)
                                                (format nil "~{~A~^~%~%~}" (remove "" (mapcar (lambda (s) (latex-section s class)) sections) :test #'string=)))
                                       :test #'string=))))
            (if body-only
                (format nil "~A~%" body)
                (format nil "\\documentclass~A{~A}~%\\usepackage[utf8]{inputenc}~%\\usepackage[T1]{fontenc}~%~{\\usepackage{~A}~%~}~{~A~%~}~A\\begin{document}~%~%~A~A~A~%~%\\end{document}~%"
                        class-options class
                        *org-latex-packages*
                        (append (meta-values meta "LATEX_HEADER") (meta-values meta "LATEX_HEADER_EXTRA"))
                        (format nil "\\author{~A}~%\\date{~A}~%\\title{~A}~%\\hypersetup{pdfcreator={~A}}~%"
                                (latex-escape (or author "")) (latex-escape (or date "\\today"))
                                (export-inline (or title "") :latex) *org-export-creator*)
                        (if (and title (option :title)) (format nil "\\maketitle~%~%") "")
                        (if (and (option :toc) sections) (format nil "\\tableofcontents~%~%") "")
                        body))))))))

;;; iCalendar

(defun ical-escape (text)
  (with-output-to-string (out)
    (loop :for c :across text
          :do (case c
                (#\\ (write-string "\\\\" out)) (#\, (write-string "\\," out)) (#\; (write-string "\\;" out))
                (#\newline (write-string "\\n" out))
                (t (write-char c out))))))

(defun ical-fold (line)
  "LINE folded to at most 75 characters per physical line, continuation lines starting with a blank."
  (if (<= (length line) 75)
      line
      (with-output-to-string (out)
        (write-string (subseq line 0 75) out)
        (loop :for start :from 75 :by 74 :below (length line)
              :do (format out "~C~C ~A" #\return #\newline (subseq line start (min (length line) (+ start 74))))))))

(defun ical-date (ts) (format nil "~4,'0D~2,'0D~2,'0D" (getf ts :year) (getf ts :month) (getf ts :day)))
(defun ical-datetime (day minutes)
  (multiple-value-bind (y m d) (day-date day)
    (format nil "~4,'0D~2,'0D~2,'0DT~2,'0D~2,'0D00" y m d (floor minutes 60) (mod minutes 60))))

(defun ical-rrule (repeater)
  (destructuring-bind (kind count unit) repeater
    (declare (ignore kind))
    (format nil "RRULE:FREQ=~A;INTERVAL=~D" (ecase unit (:h "HOURLY") (:d "DAILY") (:w "WEEKLY") (:m "MONTHLY") (:y "YEARLY")) count)))

(defun ical-event-lines (uid summary ts &key description categories (end-ts nil) now)
  "The lines of one VEVENT for the timestamp TS (and END-TS for a range)."
  (let* ((timed (timestamp-minutes ts))
         (start-day (timestamp-day ts))
         (end-day (if end-ts (timestamp-day end-ts) start-day)))
    (append (list "BEGIN:VEVENT"
                  (format nil "UID:~A" uid)
                  (format nil "DTSTAMP:~A" (ical-datetime (date-day (first now) (second now) (third now)) (+ (* 60 (fourth now)) (fifth now))))
                  )
            (if timed
                (list (format nil "DTSTART:~A" (ical-datetime start-day timed))
                      (format nil "DTEND:~A"
                              (cond (end-ts (ical-datetime end-day (or (timestamp-minutes end-ts) timed)))
                                    ((getf ts :end-hour) (ical-datetime start-day (+ (* 60 (getf ts :end-hour)) (getf ts :end-minute))))
                                    (t (ical-datetime start-day (min 1439 (+ timed 60)))))))
                (list (format nil "DTSTART;VALUE=DATE:~A" (ical-date ts))
                      (multiple-value-bind (y m d) (day-date (1+ end-day))
                        (format nil "DTEND;VALUE=DATE:~4,'0D~2,'0D~2,'0D" y m d))))
            (and (getf ts :repeater) (list (ical-rrule (getf ts :repeater))))
            (list (format nil "SUMMARY:~A" (ical-escape summary)))
            (and description (plusp (length description)) (list (format nil "DESCRIPTION:~A" (ical-escape description))))
            (and categories (list (format nil "CATEGORIES:~{~A~^,~}" (mapcar #'ical-escape categories))))
            (list "END:VEVENT"))))

(defun ical-todo-lines (uid summary due &key categories now status)
  (append (list "BEGIN:VTODO" (format nil "UID:~A" uid)
                (format nil "DTSTAMP:~A" (ical-datetime (date-day (first now) (second now) (third now)) (+ (* 60 (fourth now)) (fifth now)))))
          (and due (list (if (timestamp-minutes due)
                             (format nil "DUE:~A" (ical-datetime (timestamp-day due) (timestamp-minutes due)))
                             (format nil "DUE;VALUE=DATE:~A" (ical-date due)))))
          (list (format nil "SUMMARY:~A" (ical-escape summary)) (format nil "STATUS:~A" status))
          (and categories (list (format nil "CATEGORIES:~{~A~^,~}" (mapcar #'ical-escape categories))))
          (list "END:VTODO")))

(defun export-icalendar (entries &key (now '(2026 1 1 0 0)) (include-todo nil) (name "Org"))
  "An iCalendar (.ics) document for ENTRIES from `agenda-parse': one event per active timestamp, range, SCHEDULED
date and DEADLINE, with repeaters as RRULE. With INCLUDE-TODO open tasks that have a date also become VTODO items."
  (let ((lines (list "BEGIN:VCALENDAR" "VERSION:2.0" "PRODID:-//lem-org-mode//EN" "CALSCALE:GREGORIAN"
                     (format nil "X-WR-CALNAME:~A" (ical-escape name))))
        (counter 0))
    (dolist (entry entries)
      (unless (or (member "ARCHIVE" (getf entry :all-tags) :test #'string=) (and *org-agenda-skip-done* (getf entry :done)))
        (let ((title (getf entry :title)) (categories (getf entry :all-tags)))
          (flet ((uid (kind) (format nil "~A-~A-~D@lem-org-mode" (babel-hash (format nil "~A~A~A" (getf entry :source) (getf entry :line) kind))
                                     kind (incf counter))))
            (when (getf entry :scheduled-ts)
              (setf lines (append lines (ical-event-lines (uid "S") (format nil "S: ~A" title) (getf entry :scheduled-ts)
                                                          :categories categories :now now))))
            (when (getf entry :deadline-ts)
              (setf lines (append lines (ical-event-lines (uid "DL") (format nil "DL: ~A" title) (getf entry :deadline-ts)
                                                          :categories categories :now now))))
            (dolist (ts (getf entry :timestamps))
              (setf lines (append lines (ical-event-lines (uid "T") title ts :categories categories :now now))))
            (dolist (range (getf entry :ranges))
              (setf lines (append lines (ical-event-lines (uid "R") title (first range) :end-ts (second range)
                                                                                      :categories categories :now now))))
            (when (and include-todo (getf entry :todo) (or (getf entry :deadline-ts) (getf entry :scheduled-ts)))
              (setf lines (append lines (ical-todo-lines (uid "TODO") title (or (getf entry :deadline-ts) (getf entry :scheduled-ts))
                                                         :categories categories :now now :status "NEEDS-ACTION"))))))))
    (setf lines (append lines (list "END:VCALENDAR")))
    (with-output-to-string (out)
      (dolist (line lines)
        (format out "~A~C~C" (ical-fold line) #\return #\newline)))))

;;; Subtrees

(defun subtree-export-lines (subtree keywords)
  "Turn the SUBTREE lines (the heading first) into a document: the heading text becomes #+TITLE:, EXPORT_ properties become
keywords, and sub-headings move up so that the children of the subtree are at level 1."
  (let* ((head (subseq subtree 0 (head-length subtree)))
         (props (entry-property-alist head))
         (title (or (cdr (assoc "EXPORT_TITLE" props :test #'string=))
                    (nth-value 3 (heading-split (first subtree) keywords))))
         (level (heading-split (first subtree) keywords))
         (body (nthcdr (length head) subtree))
         (keywords-lines (loop :for (name . value) :in props
                               :when (and (> (length name) 7) (string= "EXPORT_" name :end2 7)
                                          (not (string= name "EXPORT_TITLE")) (not (string= name "EXPORT_FILE_NAME")))
                                 :collect (format nil "#+~A: ~A" (subseq name 7) value))))
    (append (list (format nil "#+TITLE: ~A" title)) keywords-lines
            (lines-shift-levels body (- level) keywords))))

;;; END pure export-more code

(defun subtree-export-file-name (heading)
  (property-value-in-entry heading "EXPORT_FILE_NAME"))

(defun org-export-subtree-lines (point)
  "The document made from the subtree at POINT (see `subtree-export-lines')."
  (let ((heading (heading-point point)))
    (subtree-export-lines (subtree-line-strings heading) (org-keywords))))

(defun export-run (buffer format-key &key subtree body-only to-buffer open)
  "Export BUFFER (or its subtree at point) in the format FORMAT-KEY (:html :markdown :ascii :latex :org :ics)."
  (let* ((point (buffer-point buffer))
         (heading (and subtree (heading-point point)))
         (lines (if subtree (org-export-subtree-lines point) (split-text-lines (buffer-text buffer))))
         (file (buffer-filename buffer))
         (*org-export-reader* (export-file-reader buffer))
         (mtime (and file (probe-file file) (file-time-list file)))
         (keywords (org-keywords buffer))
         (text (ecase format-key
                 (:html (export-html lines keywords :body-only body-only :file file :mtime mtime))
                 (:markdown (export-markdown lines keywords :file file :mtime mtime))
                 (:ascii (export-ascii lines keywords :file file :mtime mtime))
                 (:latex (export-latex lines keywords :body-only body-only :file file :mtime mtime))
                 (:org (export-org lines keywords :file file :mtime mtime))
                 (:ics (export-icalendar (agenda-entries nil buffer) :now (now) :include-todo *org-icalendar-include-todo*
                                                                      :name (if file (pathname-name file) (buffer-name buffer))))))
         (type (ecase format-key (:html "html") (:markdown "md") (:ascii "txt") (:latex "tex") (:org "org.txt") (:ics "ics")))
         (custom (and heading (subtree-export-file-name heading))))
    (if (and file (not to-buffer))
        (let ((target (if custom
                          (merge-pathnames custom (or (buffer-directory buffer) ""))
                          (make-pathname :type type :defaults file))))
          (ensure-directories-exist target)
          (with-open-file (out target :direction :output :if-exists :supersede :external-format :utf-8)
            (write-string text out))
          (message "Exported to ~A" target)
          (when open (open-url-externally (uiop:native-namestring target)))
          target)
        (let* ((name (format nil "*Org ~A Export*" (string-upcase type)))
               (export (or (get-buffer name) (make-buffer name))))
          (erase-buffer export)
          (insert-string (buffer-point export) text)
          (org-switch-to-buffer export)
          export))))

(define-command org-export-ascii () () "Export the buffer to a plain text file." (export-run (current-buffer) :ascii))
(define-command org-export-latex () () "Export the buffer to a LaTeX file." (export-run (current-buffer) :latex))
(define-command org-export-org () () "Export the buffer to an Org file with includes and macros expanded." (export-run (current-buffer) :org))
(define-command org-icalendar-export-to-ics () () "Export the dated entries of the buffer to an iCalendar file." (export-run (current-buffer) :ics))

(define-command org-icalendar-combine-agenda-files () ()
  "Write one iCalendar file with the dated entries of all agenda files."
  (let* ((file (prompt-for-string "Write to: " :initial-value "agenda.ics"))
         (text (export-icalendar (agenda-entries) :now (now) :include-todo *org-icalendar-include-todo* :name "Org agenda")))
    (with-open-file (out file :direction :output :if-exists :supersede :external-format :utf-8)
      (write-string text out))
    (message "Wrote ~A" file)))

(defun export-dispatch (buffer answer)
  "Run the export described by ANSWER: a format letter (h html, m markdown, a ascii, l latex, o org, i ics) followed by
options (s subtree, b body only, B to a buffer, v open the file afterwards)."
  (let* ((key (and (plusp (length answer)) (char answer 0)))
         (format (case key (#\h :html) (#\m :markdown) (#\a :ascii) (#\l :latex) (#\o :org) (#\i :ics)
                   (t (editor-error "No export format ~A (h m a l o i)" answer))))
         (options (subseq answer 1)))
    (export-run buffer format :subtree (find #\s options) :body-only (find #\b options)
                              :to-buffer (find #\B options) :open (find #\v options))))

(define-command org-export-dispatch () ()
  "Export the buffer: h)tml m)arkdown a)scii l)atex o)rg i)calendar, then options s)ubtree b)ody-only B)uffer v)iew. For example hs."
  (export-dispatch (current-buffer) (prompt-for-string "Export [h m a l o i] + [s b B v]: ")))

(define-command org-export-html-subtree () ()
  "Export the subtree at point to HTML."
  (export-run (current-buffer) :html :subtree t))

(define-key *org-mode-keymap* "C-c C-e a" 'org-export-ascii)
(define-key *org-mode-keymap* "C-c C-e l" 'org-export-latex)
(define-key *org-mode-keymap* "C-c C-e o" 'org-export-org)
(define-key *org-mode-keymap* "C-c C-e i" 'org-icalendar-export-to-ics)
(define-key *org-mode-keymap* "C-c C-e s" 'org-export-html-subtree)
(define-key *org-mode-keymap* "C-c C-e e" 'org-export-dispatch)
