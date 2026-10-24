;;; Publishing: turn a directory of Org files into a web site (or Markdown, text, LaTeX, iCalendar files), copy the
;;; attachments, build a sitemap, and skip what has not changed.
;;;
;;;   (setf lem-org-mode:*org-publish-project-alist*
;;;         '(("notes" :base-directory "~/org/" :base-extension "org" :publishing-directory "~/public_html/"
;;;            :recursive t :publishing-function org-html-publish-to-html
;;;            :auto-sitemap t :sitemap-title "Notes" :with-toc nil)
;;;           ("static" :base-directory "~/org/img/" :base-extension "png\\|jpg\\|css"
;;;            :publishing-directory "~/public_html/img/" :recursive t :publishing-function org-publish-attachment)
;;;           ("website" :components ("notes" "static"))))
;;;
;;; M-x org-publish asks for a project; org-publish-all does all of them.

(in-package :lem-org-mode)

(defvar *org-publish-project-alist* '()
  "Projects for `org-publish': (NAME . PLIST). Keys: :base-directory :base-extension (a regexp such as \"org\\\\|md\")
:publishing-directory :recursive :publishing-function (org-html-publish-to-html org-md-publish-to-md
org-ascii-publish-to-txt org-latex-publish-to-latex org-icalendar-publish-to-ics org-publish-attachment)
:exclude (regexp) :include (list of files) :components (names of other projects) :auto-sitemap :sitemap-filename
:sitemap-title :sitemap-style (list or tree) :sitemap-sort-files (alphabetically chronologically anti-chronologically)
:sitemap-sort-folders (first last ignore) :with-toc :section-numbers :headline-levels :with-author :with-creator
:html-head :html-head-extra :html-preamble :html-postamble :language :author :email :force.")

(defvar *org-publish-timestamp-directory* "~/.org-timestamps/"
  "Where the record of what has been published is kept, one file per project.")

;;; BEGIN pure publish code

(defun publish-project (name alist)
  "The plist of the project NAME in ALIST (as `*org-publish-project-alist*'), or NIL."
  (cdr (assoc name alist :test #'string=)))

(defun publish-expand-components (name alist &optional seen)
  "The names of the projects that publishing NAME means: itself, or the (nested) :components."
  (when (member name seen :test #'string=) (editor-error "Project ~A includes itself" name))
  (let ((plist (publish-project name alist)))
    (unless (assoc name alist :test #'string=) (editor-error "No publishing project ~A" name))
    (if (getf plist :components)
        (loop :for component :in (getf plist :components)
              :append (publish-expand-components component alist (cons name seen)))
        (list name))))

(defun extension-regexp-match-p (extension spec)
  "Whether EXTENSION matches SPEC, a regexp or alternatives such as \"org\\\\|md\" (a plain | works too)."
  (let* ((normalized (with-output-to-string (out)
                       (loop :with i := 0
                             :while (< i (length spec))
                             :do (if (and (char= (char spec i) #\\) (< (1+ i) (length spec)) (char= (char spec (1+ i)) #\|))
                                     (progn (write-char #\| out) (incf i 2))
                                     (progn (write-char (char spec i) out) (incf i))))))
         (alternatives (loop :with start := 0
                             :for bar := (position #\| normalized :start start)
                             :collect (subseq normalized start bar)
                             :while bar :do (setf start (1+ bar)))))
    (some (lambda (alternative) (regex-search-p (concatenate 'string "^(?:" alternative ")$") extension nil)) alternatives)))

(defun publish-select-files (files base-directory extension-spec exclude include)
  "The FILES (absolute names under BASE-DIRECTORY) that a project publishes: those whose extension matches
EXTENSION-SPEC and that EXCLUDE (a regexp on the relative name) does not match, plus the relative names in INCLUDE."
  (let ((base (string-right-trim "/" base-directory)))
    (flet ((relative (file) (string-left-trim "/" (subseq file (min (length file) (length base))))))
      (remove-duplicates
       (append
        (loop :for file :in files
              :for dot := (position #\. file :from-end t)
              :for extension := (if (and dot (> dot (or (position #\/ file :from-end t) -1))) (subseq file (1+ dot)) "")
              :when (and (extension-regexp-match-p extension extension-spec)
                         (not (and exclude (regex-search-p exclude (relative file) nil))))
                :collect file)
        (loop :for name :in include :collect (concatenate 'string base "/" name)))
       :test #'string= :from-end t))))

(defun publish-output-path (file base-directory publishing-directory extension)
  "Where the published version of FILE goes: the same relative path below PUBLISHING-DIRECTORY, with EXTENSION
(NIL keeps the old one)."
  (let* ((base (string-right-trim "/" base-directory))
         (relative (string-left-trim "/" (subseq file (min (length file) (length base)))))
         (dot (position #\. relative :from-end t))
         (renamed (if (and extension dot (> dot (or (position #\/ relative :from-end t) -1)))
                      (concatenate 'string (subseq relative 0 dot) "." extension)
                      relative)))
    (concatenate 'string (string-right-trim "/" publishing-directory) "/" renamed)))

(defun publish-function-kind (function)
  "The kind of publishing function: :html :markdown :ascii :latex :ics :org or :attachment."
  (let ((name (string-downcase (if (symbolp function) (symbol-name function) (princ-to-string function)))))
    (cond ((search "attachment" name) :attachment)
          ((search "html" name) :html)
          ((search "md" name) :markdown)
          ((search "ascii" name) :ascii)
          ((search "latex" name) :latex)
          ((search "ics" name) :ics)
          ((search "org" name) :org)
          (t (editor-error "Unknown publishing function ~A" function)))))

(defun publish-extension (kind)
  (ecase kind (:html "html") (:markdown "md") (:ascii "txt") (:latex "tex") (:ics "ics") (:org "org") (:attachment nil)))

(defun publish-extra-lines (plist)
  "Keyword lines that carry the export settings of a project into each file it publishes."
  (let ((options '()))
    (flet ((option (key name)
             (when (loop :for (k) :on plist :by #'cddr :thereis (eq k key))
               (let ((v (getf plist key)))
                 (push (format nil "~A:~A" name (cond ((eq v t) "t") ((null v) "nil") (t v))) options)))))
      (option :with-toc "toc") (option :section-numbers "num") (option :headline-levels "H")
      (option :with-author "author") (option :with-creator "creator") (option :with-date "date")
      (option :with-title "title") (option :with-tags "tags") (option :with-todo-keywords "todo")
      (option :with-sub-superscript "^") (option :with-planning "p") (option :with-emphasize "*"))
    (append (and options (list (format nil "#+OPTIONS: ~{~A~^ ~}" (nreverse options))))
            (and (getf plist :html-head) (list (format nil "#+HTML_HEAD: ~A" (getf plist :html-head))))
            (and (getf plist :html-head-extra) (list (format nil "#+HTML_HEAD_EXTRA: ~A" (getf plist :html-head-extra))))
            (and (getf plist :language) (list (format nil "#+LANGUAGE: ~A" (getf plist :language))))
            (and (getf plist :author) (list (format nil "#+AUTHOR: ~A" (getf plist :author))))
            (and (getf plist :email) (list (format nil "#+EMAIL: ~A" (getf plist :email)))))))

(defun file-title (lines default)
  "The #+TITLE: of the Org text LINES, or DEFAULT."
  (or (first (file-keyword-values lines "TITLE")) default))

(defun sitemap-lines (entries title &key (style :list))
  "The Org text of a sitemap. ENTRIES are (relative-name . title) in the order to show them. STYLE :tree puts the
entries below the directories they are in."
  (let ((out '()) (shown '()))
    (dolist (entry entries)
      (if (eq style :list)
          (push (format nil "- [[file:~A][~A]]" (car entry) (cdr entry)) out)
          (let ((directories (butlast (split-path (car entry)))))
            (loop :for depth :from 1 :to (length directories)
                  :for path := (subseq directories 0 depth)
                  :do (unless (member path shown :test #'equal)
                        (push path shown)
                        (push (format nil "~A- ~A" (make-string (* 2 (1- depth)) :initial-element #\space) (car (last path))) out)))
            (push (format nil "~A- [[file:~A][~A]]" (make-string (* 2 (length directories)) :initial-element #\space)
                          (car entry) (cdr entry))
                  out))))
    (append (list (format nil "#+TITLE: ~A" title) "") (nreverse out))))

(defun split-path (path)
  (loop :with start := 0
        :for slash := (position #\/ path :start start)
        :collect (subseq path start slash)
        :while slash :do (setf start (1+ slash))))

(defun sitemap-sort (entries how &key (folders :ignore))
  "Sort the sitemap ENTRIES, each (relative-name title modification-time), by HOW: :alphabetically, :chronologically
or :anti-chronologically. FOLDERS :first or :last groups files that are in directories."
  (let* ((sorted (stable-sort (copy-list entries)
                              (ecase how
                                (:alphabetically (lambda (a b) (string< (string-downcase (first a)) (string-downcase (first b)))))
                                (:chronologically (lambda (a b) (< (third a) (third b))))
                                (:anti-chronologically (lambda (a b) (> (third a) (third b)))))))
         (in-folder (remove-if-not (lambda (e) (find #\/ (first e))) sorted))
         (top (remove-if (lambda (e) (find #\/ (first e))) sorted)))
    (ecase folders
      (:ignore sorted)
      (:first (append in-folder top))
      (:last (append top in-folder)))))

;;; END pure publish code

(defun publish-list-files (directory recursive)
  "The names of the files in DIRECTORY (and below, when RECURSIVE)."
  (let ((directory (uiop:ensure-directory-pathname (expand-user-path directory))))
    (append (mapcar #'uiop:native-namestring (uiop:directory-files directory))
            (and recursive
                 (loop :for sub :in (uiop:subdirectories directory)
                       :unless (char= #\. (char (car (last (pathname-directory sub))) 0))
                         :append (publish-list-files (uiop:native-namestring sub) t))))))

(defun publish-cache-file (project-name)
  (merge-pathnames (format nil "~A.cache" project-name) (uiop:ensure-directory-pathname (expand-user-path *org-publish-timestamp-directory*))))

(defun publish-read-cache (project-name)
  (let ((file (publish-cache-file project-name)))
    (when (probe-file file)
      (handler-case (with-open-file (in file) (let ((*read-eval* nil)) (read in)))
        (error () nil)))))

(defun publish-write-cache (project-name table)
  (let ((file (publish-cache-file project-name)))
    (ensure-directories-exist file)
    (with-open-file (out file :direction :output :if-exists :supersede)
      (let ((*print-readably* nil)) (prin1 table out)))))

(defun publish-sitemap (name plist files base)
  "Write the sitemap file of the project and return its name."
  (let* ((filename (or (getf plist :sitemap-filename) "sitemap.org"))
         (path (concatenate 'string (string-right-trim "/" base) "/" filename))
         (entries (loop :for file :in files
                        :unless (string= (uiop:native-namestring (merge-pathnames file)) (uiop:native-namestring (merge-pathnames path)))
                          :collect (let ((relative (string-left-trim "/" (subseq file (length (string-right-trim "/" base))))))
                                     (list relative
                                           (file-title (uiop:read-file-lines file) (pathname-name file))
                                           (file-write-date file)))))
         (sorted (sitemap-sort entries (or (getf plist :sitemap-sort-files) :alphabetically)
                               :folders (or (getf plist :sitemap-sort-folders) :ignore)))
         (text (format nil "~{~A~%~}" (sitemap-lines (mapcar (lambda (e) (cons (first e) (second e))) sorted)
                                                    (or (getf plist :sitemap-title) (format nil "Sitemap for project ~A" name))
                                                    :style (or (getf plist :sitemap-style) :tree)))))
    (unless (and (probe-file path) (string= text (uiop:read-file-string path)))
      (with-open-file (out path :direction :output :if-exists :supersede :external-format :utf-8)
        (write-string text out)))
    path))

(defun publish-file (file plist kind output)
  "Write the published version of FILE to OUTPUT."
  (ensure-directories-exist output)
  (if (eq kind :attachment)
      (uiop:copy-file file output)
      (let* ((lines (append (publish-extra-lines plist) (uiop:read-file-lines file)))
             (default (todo-sequences-from-setting *org-todo-keywords*))
             (sequences (todo-sequences-from-lines lines default))
             (keywords (sequences-keywords sequences))
             (directory (uiop:pathname-directory-pathname file))
             (*org-export-reader* (lambda (name)
                                    (let ((path (merge-pathnames (expand-user-path name) directory)))
                                      (and (probe-file path) (uiop:read-file-lines path)))))
             (mtime (file-time-list file))
             (text (ecase kind
                     (:html (export-html lines keywords :file file :mtime mtime))
                     (:markdown (export-markdown lines keywords :file file :mtime mtime))
                     (:ascii (export-ascii lines keywords :file file :mtime mtime))
                     (:latex (export-latex lines keywords :file file :mtime mtime))
                     (:org (export-org lines keywords :file file :mtime mtime))
                     (:ics (export-icalendar (agenda-parse lines keywords file :done (sequences-done-keywords sequences))
                                             :now (now) :name (pathname-name file))))))
        (with-open-file (out output :direction :output :if-exists :supersede :external-format :utf-8)
          (write-string text out)))))

(defun publish-one-project (name plist &key force)
  "Publish the project NAME. Return (values published skipped)."
  (let* ((base (expand-user-path (or (getf plist :base-directory) (editor-error "Project ~A has no :base-directory" name))))
         (target (expand-user-path (or (getf plist :publishing-directory) (editor-error "Project ~A has no :publishing-directory" name))))
         (kind (publish-function-kind (or (getf plist :publishing-function) 'org-html-publish-to-html)))
         (extension (publish-extension kind))
         (all (publish-list-files base (getf plist :recursive)))
         (files (publish-select-files all base (or (getf plist :base-extension) "org") (getf plist :exclude) (getf plist :include)))
         (cache (publish-read-cache name))
         (new-cache '()) (published 0) (skipped 0))
    (when (getf plist :auto-sitemap)
      (let ((sitemap (publish-sitemap name plist (remove-if (lambda (f) (uiop:string-suffix-p f (or (getf plist :sitemap-filename) "sitemap.org"))) files) base)))
        (pushnew (uiop:native-namestring sitemap) files :test #'string=)))
    (dolist (file files)
      (let* ((output (publish-output-path file base target extension))
             (modified (file-write-date file))
             (cached (cdr (assoc file cache :test #'string=))))
        (if (and (not force) cached (<= modified cached) (probe-file output))
            (progn (incf skipped) (push (cons file cached) new-cache))
            (progn (publish-file file plist kind output)
                   (incf published)
                   (push (cons file (get-universal-time)) new-cache)))))
    (publish-write-cache name new-cache)
    (values published skipped)))

(defun org-publish-project (name &key force)
  "Publish the project NAME (and the projects it is made of). Return (values published skipped)."
  (let ((published 0) (skipped 0))
    (dolist (project (publish-expand-components name *org-publish-project-alist*))
      (multiple-value-bind (p s) (publish-one-project project (publish-project project *org-publish-project-alist*)
                                                       :force (or force (getf (publish-project project *org-publish-project-alist*) :force)))
        (incf published p) (incf skipped s)))
    (values published skipped)))

(defun org-publish-all-projects (&key force)
  (let ((published 0) (skipped 0))
    (dolist (project (remove-if (lambda (name) (getf (publish-project name *org-publish-project-alist*) :components))
                                (mapcar #'car *org-publish-project-alist*)))
      (multiple-value-bind (p s) (org-publish-project project :force force)
        (incf published p) (incf skipped s)))
    (values published skipped)))

(defun publish-report (published skipped)
  (message "Published ~D file~:P~@[, ~D unchanged~]" published (and (plusp skipped) skipped)))

(define-command org-publish () ()
  "Publish a project of `*org-publish-project-alist*'. Add ! to its name to publish everything again."
  (unless *org-publish-project-alist* (editor-error "No publishing projects (set *org-publish-project-alist*)"))
  (let* ((names (mapcar #'car *org-publish-project-alist*))
         (answer (prompt-for-string (format nil "Project (~{~A~^ ~}, all): " names)))
         (force (uiop:string-suffix-p answer "!"))
         (name (string-right-trim "!" answer)))
    (multiple-value-bind (published skipped)
        (if (string= name "all") (org-publish-all-projects :force force) (org-publish-project name :force force))
      (publish-report published skipped))))

(define-command org-publish-all () ()
  "Publish all projects."
  (multiple-value-bind (published skipped) (org-publish-all-projects)
    (publish-report published skipped)))

(defun project-of-file (file)
  "The name of the first project whose base directory contains FILE."
  (loop :for (name . plist) :in *org-publish-project-alist*
        :for base := (and (getf plist :base-directory) (expand-user-path (getf plist :base-directory)))
        :when (and base (uiop:string-prefix-p (string-right-trim "/" (uiop:native-namestring (truename base))) (uiop:native-namestring (truename file))))
          :return name))

(define-command org-publish-current-file () ()
  "Publish the file of the current buffer with the project it belongs to."
  (let* ((file (or (buffer-filename (current-buffer)) (editor-error "This buffer has no file")))
         (name (or (project-of-file file) (editor-error "No project contains this file")))
         (plist (publish-project name *org-publish-project-alist*))
         (kind (publish-function-kind (or (getf plist :publishing-function) 'org-html-publish-to-html)))
         (base (expand-user-path (getf plist :base-directory)))
         (output (publish-output-path file base (expand-user-path (getf plist :publishing-directory)) (publish-extension kind))))
    (publish-file file plist kind output)
    (message "Published ~A" output)))

(define-command org-publish-current-project () ()
  "Publish the project that the current file belongs to."
  (let* ((file (or (buffer-filename (current-buffer)) (editor-error "This buffer has no file")))
         (name (or (project-of-file file) (editor-error "No project contains this file"))))
    (multiple-value-bind (published skipped) (org-publish-project name)
      (publish-report published skipped))))

(define-key *org-mode-keymap* "C-c C-e P" 'org-publish)
