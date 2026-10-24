;;; Links: [[target][description]] and plain URLs, opening them (C-c C-o), storing (C-c l) and inserting (C-c C-l)
;;; them, IDs, custom ids, dedicated targets, link abbreviations, and footnotes.

(in-package :lem-org-mode)

(defvar *org-url-open-function* 'open-url-externally
  "The function that opens a URL or an external file, called with the string.")

(defvar *org-file-apps* '(("pdf" . :external) ("png" . :external) ("jpg" . :external) ("jpeg" . :external)
                          ("gif" . :external) ("svg" . :external) ("odt" . :external) ("docx" . :external)
                          ("xlsx" . :external) ("mp4" . :external) ("mkv" . :external) ("mp3" . :external)
                          ("html" . :external) ("htm" . :external))
  "File extensions that file: links open outside the editor. Anything else is opened in Lem.")

(defvar *org-link-shell-confirm* t
  "True: ask before running a shell: link.")

(defvar *org-id-link-to-org-use-id* nil
  "NIL: `org-store-link' makes file links. T or `create-if-interactive': it makes an id: link (and creates the ID).")

(defvar *org-stored-links* '()
  "The links made by `org-store-link', newest first, as (link . description).")

(defvar *org-footnote-section* "Footnotes"
  "The heading under which new footnote definitions are put.")

(defvar *org-id-locations* (make-hash-table :test 'equal)
  "ID -> file name, filled by `org-id-update-id-locations' and when IDs are found.")

;;; BEGIN pure links code

(defvar *org-link-abbrev-alist* '()
  "Link abbreviations: an alist of (NAME . FORMAT). A link [[NAME:tag]] opens FORMAT with %s replaced by tag
(%h by the URL-encoded tag). #+LINK: NAME FORMAT lines of a buffer add to these.")

;;; Finding links in text

(defun url-scheme-end (text start)
  "If a URL scheme such as https: begins at START in TEXT return the index after the colon."
  (let ((end (position-if-not (lambda (c) (or (alphanumericp c) (find c "+.-"))) text :start start)))
    (and end (> end start) (< end (length text)) (char= (char text end) #\:)
         (alpha-char-p (char text start))
         (member (subseq text start end)
                 '("http" "https" "ftp" "mailto" "file" "id" "news" "shell" "doi" "help" "elisp" "irc" "ssh")
                 :test #'string-equal)
         (1+ end))))

(defun links-in-line (line)
  "The links in LINE as plists (:start :end :target :description :kind), KIND :bracket, :plain or :angle."
  (let ((found '()) (i 0) (n (length line)))
    (loop :while (< i n)
          :do (let ((c (char line i)))
                (cond
                  ((and (char= c #\[) (< (1+ i) n) (char= (char line (1+ i)) #\[))
                   (let* ((close (search "]]" line :start2 (+ i 2)))
                          (inner (and close (subseq line (+ i 2) close)))
                          (sep (and inner (search "][" inner))))
                     (if (and close (plusp (length inner)) (not (find #\newline inner)))
                         (progn (push (list :start i :end (+ close 2) :kind :bracket
                                            :target (if sep (subseq inner 0 sep) inner)
                                            :description (and sep (subseq inner (+ sep 2))))
                                      found)
                                (setf i (+ close 2)))
                         (incf i))))
                  ((and (char= c #\<) (url-scheme-end line (1+ i)))
                   (let ((close (position #\> line :start i)))
                     (if close
                         (progn (push (list :start i :end (1+ close) :kind :angle :target (subseq line (1+ i) close) :description nil)
                                      found)
                                (setf i (1+ close)))
                         (incf i))))
                  ((and (alpha-char-p c) (or (zerop i) (not (alphanumericp (char line (1- i)))))
                        (url-scheme-end line i)
                        (< (url-scheme-end line i) n)
                        (not (char= (char line (url-scheme-end line i)) #\space)))
                   (let* ((end (or (position-if (lambda (ch) (find ch " 	<>\"")) line :start i) n))
                          (end (loop :while (and (> end i) (find (char line (1- end)) ".,;:!?)]}'")) :do (decf end) :finally (return end))))
                     (push (list :start i :end end :kind :plain :target (subseq line i end) :description nil) found)
                     (setf i (max end (1+ i)))))
                  (t (incf i)))))
    (nreverse found)))

(defun link-at-column (line column)
  "The link of LINE that contains COLUMN, or NIL."
  (find-if (lambda (link) (and (<= (getf link :start) column) (< column (getf link :end)))) (links-in-line line)))

(defun link-format (target &optional description)
  (if (and description (plusp (length description)))
      (format nil "[[~A][~A]]" target description)
      (format nil "[[~A]]" target)))

;;; What a target means

(defun link-expand-abbreviation (target abbreviations)
  "TARGET with a leading abbreviation name replaced, as defined by ABBREVIATIONS, an alist (name . format)."
  (let ((colon (position #\: target)))
    (if colon
        (let ((entry (assoc (subseq target 0 colon) abbreviations :test #'string=)))
          (if entry
              (let ((tag (subseq target (1+ colon))) (format (cdr entry)))
                (let ((at (search "%s" format)))
                  (cond (at (concatenate 'string (subseq format 0 at) tag (subseq format (+ at 2))))
                        ((search "%h" format)
                         (let ((at (search "%h" format)))
                           (concatenate 'string (subseq format 0 at) (url-encode tag) (subseq format (+ at 2)))))
                        (t (concatenate 'string format tag)))))
              target))
        target)))

(defun url-encode (string)
  (with-output-to-string (out)
    (loop :for c :across string
          :do (if (or (alphanumericp c) (find c "-._~"))
                  (write-char c out)
                  (loop :for byte :across (sb-ext:string-to-octets (string c) :external-format :utf-8)
                        :do (format out "%~2,'0X" byte))))))

(defun link-abbreviations-from-lines (lines)
  "(name . format) from the #+LINK: lines of LINES."
  (loop :for value :in (file-keyword-values lines "LINK")
        :for space := (position #\space value)
        :when space :collect (cons (subseq value 0 space) (string-trim " " (subseq value space)))))

(defun link-classify (target)
  "Return (values type path search) for TARGET. TYPE is :file :url :id :shell :heading :custom-id :coderef :fuzzy.
For files, SEARCH is the part after ::; for a URL PATH is the whole target."
  (let ((target (string-trim " " target)))
    (cond
      ((zerop (length target)) (values :fuzzy "" nil))
      ((url-scheme-end target 0)
       (let* ((colon (position #\: target)) (scheme (string-downcase (subseq target 0 colon))) (rest (subseq target (1+ colon))))
         (cond ((string= scheme "file")
                (let ((sep (search "::" rest)))
                  (values :file (subseq rest 0 sep) (and sep (subseq rest (+ sep 2))))))
               ((string= scheme "id") (values :id rest nil))
               ((string= scheme "shell") (values :shell rest nil))
               ((string= scheme "elisp") (values :unsupported target nil))
               (t (values :url target nil)))))
      ((char= (char target 0) #\*) (values :heading (subseq target 1) nil))
      ((char= (char target 0) #\#) (values :custom-id (subseq target 1) nil))
      ((and (char= (char target 0) #\() (char= (char target (1- (length target))) #\)))
       (values :coderef (subseq target 1 (1- (length target))) nil))
      ((or (char= (char target 0) #\/) (char= (char target 0) #\~)
           (and (> (length target) 1) (string= "./" target :end2 2))
           (and (> (length target) 2) (string= "../" target :end2 3)))
       (let ((sep (search "::" target)))
         (values :file (subseq target 0 sep) (and sep (subseq target (+ sep 2))))))
      (t (values :fuzzy target nil)))))

(defun link-search-kind (search)
  "Interpret the SEARCH of a file link: (:line N), (:heading text), (:custom-id id), (:regexp re) or (:text s)."
  (cond ((null search) nil)
        ((and (plusp (length search)) (every #'digit-char-p search)) (list :line (parse-integer search)))
        ((char= (char search 0) #\*) (list :heading (subseq search 1)))
        ((char= (char search 0) #\#) (list :custom-id (subseq search 1)))
        ((and (> (length search) 1) (char= (char search 0) #\/) (char= (char search (1- (length search))) #\/))
         (list :regexp (subseq search 1 (1- (length search)))))
        (t (list :text search))))

(defun normalize-space (text)
  "TEXT with runs of blanks reduced to one space."
  (format nil "~{~A~^ ~}" (split-words text)))

(defun heading-link-text (line keywords)
  "The text of a heading as used in links: without stars, todo keyword, priority, tags and statistics cookie."
  (multiple-value-bind (level todo priority title tags) (heading-split line keywords)
    (declare (ignore todo priority tags))
    (when level
      (let ((title (or title "")))
        (multiple-value-bind (start end) (cookie-find title)
          (normalize-space (if start (concatenate 'string (subseq title 0 start) (subseq title end)) title)))))))

(defun lines-find-heading (lines text keywords)
  "The index of the heading whose link text is TEXT (exactly, else as a prefix, ignoring case), or NIL."
  (let ((wanted (normalize-space text)))
    (or (position-if (lambda (l) (let ((h (heading-link-text l keywords))) (and h (string-equal h wanted)))) lines)
        (position-if (lambda (l) (let ((h (heading-link-text l keywords)))
                                   (and h (>= (length h) (length wanted)) (string-equal wanted h :end2 (length wanted)))))
                     lines))))

(defun lines-find-property (lines name value keywords)
  "The index of the heading whose property NAME equals VALUE."
  (loop :for i :from 0 :below (length lines)
        :when (and (heading-split (nth i lines) keywords)
                   (let* ((tail (nthcdr i lines)) (head (subseq tail 0 (head-length tail))))
                     (equal (cdr (assoc (string-upcase name) (entry-property-alist head) :test #'string=)) value)))
          :return i))

(defun lines-find-dedicated-target (lines text)
  "The index of a line with the dedicated target <<TEXT>> or the radio target <<<TEXT>>>."
  (let ((wanted (normalize-space text)))
    (position-if (lambda (l)
                   (loop :with from := 0
                         :for open := (search "<<" l :start2 from)
                         :while open
                         :do (let* ((triple (and (< (+ open 2) (length l)) (char= (char l (+ open 2)) #\<)))
                                    (start (+ open (if triple 3 2)))
                                    (close (search (if triple ">>>" ">>") l :start2 start)))
                               (when (and close (string-equal (normalize-space (subseq l start close)) wanted))
                                 (return t))
                               (setf from (+ open 2)))))
                 lines)))

(defun lines-find-name (lines text)
  (position-if (lambda (l) (and (line-starts-with-ci-p l "#+name:")
                                (string-equal (normalize-space (subseq (string-left-trim " " l) 7)) (normalize-space text))))
               lines))

(defun lines-find-fuzzy (lines text keywords)
  "The line a fuzzy link TEXT points to: a dedicated target, a #+NAME, a heading, or the first line containing the text."
  (or (lines-find-dedicated-target lines text)
      (lines-find-name lines text)
      (lines-find-heading lines text keywords)
      (let ((wanted (normalize-space text)))
        (position-if (lambda (l) (search wanted (normalize-space l) :test #'char-equal)) lines))))

(defun lines-find-search (lines search keywords)
  "The 0-based line index that the SEARCH part of a link designates in LINES, or NIL."
  (let ((kind (link-search-kind search)))
    (when kind
      (ecase (first kind)
        (:line (and (<= 1 (second kind) (length lines)) (1- (second kind))))
        (:heading (lines-find-heading lines (second kind) keywords))
        (:custom-id (lines-find-property lines "CUSTOM_ID" (second kind) keywords))
        (:regexp (position-if (lambda (l) (regex-search-p (second kind) l)) lines))
        (:text (lines-find-fuzzy lines (second kind) keywords))))))

;;; Making links

(defun make-uuid (&optional (random-fn #'random))
  "A random (version 4) UUID string."
  (flet ((hex (n) (format nil "~(~V,'0X~)" n (funcall random-fn (expt 16 n)))))
    (format nil "~A-~A-4~A-~A~A-~A" (hex 8) (hex 4) (hex 3)
            (char "89ab" (funcall random-fn 4)) (hex 3) (hex 12))))

(defun make-file-link (path heading-text)
  "The file link for PATH, with a ::*HEADING-TEXT search when HEADING-TEXT is given."
  (format nil "file:~A~@[::*~A~]" path heading-text))

;;; Footnotes

(defun footnote-references (line)
  "The footnote references in LINE as (start end label definition): [fn:1] [fn:name] [fn::inline] [fn:name:def]."
  (let ((found '()) (i 0))
    (loop :for open := (search "[fn:" line :start2 i)
          :while open
          :do (let ((close (let ((depth 0))
                             (loop :for j :from open :below (length line)
                                   :do (case (char line j) (#\[ (incf depth)) (#\] (decf depth) (when (zerop depth) (return j))))))))
                (if close
                    (let* ((inner (subseq line (+ open 4) close))
                           (colon (position #\: inner)))
                      (push (list open (1+ close) (if colon (subseq inner 0 colon) inner) (and colon (subseq inner (1+ colon)))) found)
                      (setf i (1+ close)))
                    (setf i (+ open 4)))))
    (nreverse found)))

(defun footnote-definition-label (line)
  "The label of a footnote definition line such as [fn:1] text, or NIL."
  (when (and (> (length line) 5) (string= "[fn:" line :end2 4))
    (let ((close (position #\] line)))
      (and close (> close 4) (subseq line 4 close)))))

(defun footnote-next-number (lines)
  "The smallest positive integer that no footnote reference or definition in LINES uses as a label."
  (let ((used '()))
    (dolist (line lines)
      (dolist (ref (footnote-references line))
        (let ((n (parse-integer (third ref) :junk-allowed t))) (when n (push n used))))
      (let ((label (footnote-definition-label line)))
        (when label (let ((n (parse-integer label :junk-allowed t))) (when n (push n used))))))
    (loop :for n :from 1 :unless (member n used) :return n)))

(defun footnote-definition-index (lines label)
  (position-if (lambda (l) (equal (footnote-definition-label l) label)) lines))

(defun footnote-reference-index (lines label)
  "The index of the first line with a reference to LABEL that is not the definition itself."
  (position-if (lambda (l) (and (not (equal (footnote-definition-label l) label))
                                (find label (footnote-references l) :key #'third :test #'string=)))
               lines))

;;; END pure links code

;;; Opening

(defun open-url-externally (url)
  "Open URL with the desktop's opener."
  (let ((command (cond ((uiop:os-windows-p) (list "cmd" "/c" "start" "" url))
                       ((uiop:os-macosx-p) (list "open" url))
                       (t (list "xdg-open" url)))))
    (handler-case (uiop:launch-program command :output nil :error-output nil)
      (error (e) (editor-error "Cannot open ~A: ~A" url e)))))

(defun buffer-link-abbreviations (buffer)
  (append (link-abbreviations-from-lines (split-text-lines (buffer-text buffer))) *org-link-abbrev-alist*))

(defun expand-user-path (path)
  (cond ((and (plusp (length path)) (char= (char path 0) #\~))
         (uiop:native-namestring (merge-pathnames (string-left-trim "/" (subseq path 1)) (user-homedir-pathname))))
        (t path)))

(defun resolve-link-path (path buffer)
  (let ((path (expand-user-path path)))
    (uiop:native-namestring (merge-pathnames path (or (buffer-directory buffer) "")))))

(defun goto-line-in-buffer (buffer line-index)
  (org-switch-to-buffer buffer)
  (move-to-line (current-point) (1+ line-index)))

(defun org-goto-search (buffer search)
  "Jump in BUFFER to the place SEARCH designates. Error when there is none."
  (with-todo-buffer ((buffer-start-point buffer))
    (let ((index (lines-find-search (split-text-lines (buffer-text buffer)) search (org-keywords))))
      (unless index (editor-error "No match for ~A" search))
      (goto-line-in-buffer buffer index))))

(defun buffer-has-id-p (buffer id)
  (lines-find-property (split-text-lines (buffer-text buffer)) "ID" id '("TODO" "DONE")))

(defun org-id-find-buffer (id)
  "The buffer that holds the entry with ID: the current buffer, another open Org buffer, the file recorded for the
ID, or one of the agenda files (which is opened)."
  (or (and (buffer-has-id-p (current-buffer) id) (current-buffer))
      (find-if (lambda (buffer)
                 (and (or (eq (buffer-major-mode buffer) 'org-mode) (null (buffer-filename buffer))
                          (uiop:string-suffix-p (buffer-filename buffer) ".org"))
                      (buffer-has-id-p buffer id)))
               (buffer-list))
      (let ((file (gethash id *org-id-locations*)))
        (and file (probe-file file) (find-file-buffer file)))
      (let ((file (find-if (lambda (file)
                             (and (probe-file file)
                                  (lines-find-property (uiop:read-file-lines file) "ID" id '("TODO" "DONE"))))
                           (agenda-source-files))))
        (and file (find-file-buffer file)))))

(defun org-open-id (id)
  (let ((buffer (org-id-find-buffer id)))
    (unless buffer (editor-error "Cannot find the entry with ID ~A" id))
    (when (buffer-filename buffer) (setf (gethash id *org-id-locations*) (buffer-filename buffer)))
    (with-todo-buffer ((buffer-start-point buffer))
      (goto-line-in-buffer buffer (lines-find-property (split-text-lines (buffer-text buffer)) "ID" id (org-keywords))))))

(defun file-extension-external-p (path)
  (let ((extension (pathname-type (uiop:parse-native-namestring path))))
    (and extension (eq :external (cdr (assoc (string-downcase extension) *org-file-apps* :test #'string=))))))

(defun org-open-file-link (path search buffer)
  (let ((file (resolve-link-path path buffer)))
    (cond ((and (file-extension-external-p file) (null search))
           (funcall *org-url-open-function* file))
          (t (let ((target (if (string= path "") buffer (find-file-buffer file))))
               (if search
                   (org-goto-search target search)
                   (org-switch-to-buffer target)))))))

(defun org-open-link (target buffer)
  "Open the link TARGET found in BUFFER."
  (let ((target (link-expand-abbreviation target (buffer-link-abbreviations buffer))))
    (multiple-value-bind (type path search) (link-classify target)
      (ecase type
        (:url (funcall *org-url-open-function* path))
        (:file (org-open-file-link path search buffer))
        (:id (org-open-id path))
        (:shell (when (or (not *org-link-shell-confirm*) (prompt-for-y-or-n-p (format nil "Run ~A? " path)))
                  (uiop:launch-program path :output nil :error-output nil)))
        (:heading (org-goto-search buffer (concatenate 'string "*" path)))
        (:custom-id (org-goto-search buffer (concatenate 'string "#" path)))
        (:coderef (org-goto-search buffer (format nil "/(~A)/" (string-trim "()" path))))
        (:fuzzy (org-goto-search buffer (if (and (every #'digit-char-p path) (plusp (length path))) (concatenate 'string "/" path "/") path)))
        (:unsupported (editor-error "Cannot open this kind of link: ~A" path))))))

(defun footnote-at-point (point)
  (let* ((line (line-string point)) (column (point-charpos point)))
    (find-if (lambda (ref) (and (<= (first ref) column) (< column (second ref))
                                (not (and (zerop (first ref)) (footnote-definition-label line)))))
             (footnote-references line))))

(defun org-open-footnote (point reference)
  (let* ((buffer (point-buffer point))
         (lines (split-text-lines (buffer-text buffer)))
         (index (footnote-definition-index lines (third reference))))
    (unless index (editor-error "No definition for footnote ~A" (third reference)))
    (goto-line-in-buffer buffer index)))

(defun org-open-at-point (point)
  "Open what is at POINT: a link, a footnote reference (go to its definition), a timestamp (the agenda of that day) or
a tag (the headlines with it)."
  (let* ((buffer (point-buffer point))
         (link (link-at-column (line-string point) (point-charpos point)))
         (footnote (and (null link) (footnote-at-point point))))
    (cond (link (org-open-link (getf link :target) buffer))
          (footnote (org-open-footnote point footnote))
          ((footnote-definition-label (line-string point))
           (let* ((lines (split-text-lines (buffer-text buffer)))
                  (index (footnote-reference-index lines (footnote-definition-label (line-string point)))))
             (unless index (editor-error "This footnote is not referenced"))
             (goto-line-in-buffer buffer index)))
          ((timestamp-at-point point)
           (let ((ts (timestamp-at-point point)))
             (show-agenda-spec (list :type :agenda :span :day :start (timestamp-day ts)))))
          ((let ((tags (and (org-heading-level point) (nth-value 4 (heading-split (line-string point) (org-keywords))))))
             (when tags
               (let ((token (let ((line (line-string point)) (column (point-charpos point)))
                              (let ((tag (find-if (lambda (tag)
                                                    (let ((at (search (format nil ":~A:" tag) line :from-end t)))
                                                      (and at (<= at column (+ at (length tag) 2)))))
                                                  tags)))
                                tag))))
                 (when token (show-agenda-spec (list :type :tags :match token)) t)))))
          (t (editor-error "No link at point")))))

(define-command org-open-at-point-command () ()
  "Open the link, footnote, timestamp or tag at point."
  (org-open-at-point (current-point)))

;;; Storing and inserting links

(defun org-link-to-point (point)
  "Return (values link description) for POINT: an id: or file: link to the entry, or file:path::LINE outside Org."
  (let* ((buffer (point-buffer point))
         (file (buffer-filename buffer)))
    (unless file (editor-error "This buffer is not visiting a file"))
    (if (or (eq (buffer-major-mode buffer) 'org-mode) (uiop:string-suffix-p file ".org"))
        (let* ((heading (enclosing-heading point))
               (text (and heading (with-todo-buffer (point) (heading-link-text (line-string heading) (org-keywords)))))
               (id (and heading *org-id-link-to-org-use-id* (org-id-get-create heading))))
          (cond (id (values (format nil "id:~A" id) text))
                (text (values (make-file-link file text) text))
                (t (values (format nil "file:~A" file) (file-namestring file)))))
        (values (format nil "file:~A::~D" file (line-number-at-point point)) (file-namestring file)))))

(defun org-store-link (point)
  "Remember a link to POINT for `org-insert-link' and the %a escape of capture templates. Return the link."
  (multiple-value-bind (link description) (org-link-to-point point)
    (setf *org-last-stored-link* (link-format link description))
    (push (cons link description) *org-stored-links*)
    link))

(define-command org-store-link-command () ()
  "Store a link to the current place."
  (message "Stored: ~A" (org-store-link (current-point))))

(defun org-insert-link-text (point link description)
  "Insert [[LINK][DESCRIPTION]] at POINT, replacing the link that POINT is on."
  (let* ((line (line-string point))
         (existing (link-at-column line (point-charpos point)))
         (text (link-format link description)))
    (if existing
        (replace-lines (line-start (copy-point point :temporary)) 1
                       (list (concatenate 'string (subseq line 0 (getf existing :start)) text (subseq line (getf existing :end)))))
        (insert-string point text))))

(define-command org-insert-link () ()
  "Insert a link, or edit the one at point. An empty answer for the link uses the last stored one."
  (let* ((point (current-point))
         (existing (link-at-column (line-string point) (point-charpos point)))
         (stored (first *org-stored-links*))
         (link (prompt-for-string "Link: " :initial-value (cond (existing (getf existing :target)) (stored (car stored)) (t ""))))
         (description (prompt-for-string "Description: "
                                         :initial-value (cond (existing (or (getf existing :description) ""))
                                                              ((and stored (string= link (car stored))) (or (cdr stored) ""))
                                                              (t "")))))
    (when (zerop (length link)) (editor-error "No link"))
    (org-insert-link-text point link description)))

(defun next-link-position (point direction)
  "Move through the buffer to the start of the next (1) or previous (-1) link. Return a point or NIL."
  (let ((p (copy-point point :temporary)) (column (point-charpos point)))
    (loop
      (let* ((links (links-in-line (line-string p)))
             (candidates (if (plusp direction)
                             (remove-if-not (lambda (l) (> (getf l :start) column)) links)
                             (reverse (remove-if-not (lambda (l) (< (getf l :start) column)) links)))))
        (when candidates
          (setf (point-charpos p) (getf (first candidates) :start))
          (return p))
        (unless (line-offset p direction) (return nil))
        (setf column (if (plusp direction) -1 most-positive-fixnum))))))

(define-command org-next-link () ()
  "Move to the next link."
  (let ((target (next-link-position (current-point) 1)))
    (unless target (editor-error "No further link"))
    (move-point (current-point) target)))

(define-command org-previous-link () ()
  "Move to the previous link."
  (let ((target (next-link-position (current-point) -1)))
    (unless target (editor-error "No earlier link"))
    (move-point (current-point) target)))

;;; IDs

(defun org-id-get-create (point)
  "The ID property of the entry at POINT; a new one is created when there is none."
  (let ((heading (heading-point point)))
    (or (property-value-in-entry heading "ID")
        (let ((id (make-uuid)))
          (org-set-property heading "ID" id)
          (setf (gethash id *org-id-locations*) (buffer-filename (point-buffer heading)))
          id))))

(define-command org-id-get-create-command () ()
  "Give the current entry an ID property if it has none, and show it."
  (message "ID: ~A" (org-id-get-create (current-point))))

(defun org-id-update-id-locations ()
  "Find the IDs in the agenda files and the open Org buffers. Return how many were found."
  (let ((count 0))
    (flet ((scan (lines file)
             (dolist (entry (agenda-parse lines '("TODO" "DONE") file))
               (let ((id (cdr (assoc "ID" (getf entry :props) :test #'string=))))
                 (when id (setf (gethash id *org-id-locations*) file) (incf count))))))
      (dolist (file (agenda-source-files))
        (when (probe-file file) (scan (uiop:read-file-lines file) file)))
      (dolist (buffer (buffer-list))
        (when (and (eq (buffer-major-mode buffer) 'org-mode) (buffer-filename buffer))
          (scan (split-text-lines (buffer-text buffer)) (buffer-filename buffer)))))
    count))

(define-command org-id-update-id-locations-command () ()
  "Scan the agenda files and open Org buffers for IDs."
  (message "~D ID~:P found" (org-id-update-id-locations)))

;;; Footnotes

(defun org-footnote-new (point)
  "Insert a reference to a new footnote at POINT and add its definition at the end of the buffer.
Return the label."
  (let* ((buffer (point-buffer point))
         (lines (split-text-lines (buffer-text buffer)))
         (label (format nil "~D" (footnote-next-number lines))))
    (insert-string point (format nil "[fn:~A]" label))
    (let* ((lines (split-text-lines (buffer-text buffer)))
           (section (position-if (lambda (l) (and (org-heading-string-p l)
                                                   (string-equal (heading-link-text l (org-keywords)) *org-footnote-section*)))
                                 lines))
           (end (copy-point (buffer-end-point buffer) :temporary)))
      (unless (start-line-p end) (insert-character end #\newline))
      (unless section
        (insert-string end (format nil "* ~A~%" *org-footnote-section*)))
      (insert-string end (format nil "[fn:~A] " label)))
    label))

(defun org-footnote-action (point)
  "On a reference go to its definition, on a definition go back to a reference, elsewhere create a new footnote
and put the cursor on its definition."
  (let ((ref (footnote-at-point point)))
    (cond (ref (org-open-footnote point ref))
          ((footnote-definition-label (line-string point)) (org-open-at-point point))
          (t (org-footnote-new point)
             (move-point point (buffer-end-point (point-buffer point)))))))

(define-command org-footnote-action-command () ()
  "Footnotes: go to the definition or reference at point, or create a new footnote."
  (org-footnote-action (current-point)))

(define-key *org-mode-keymap* "C-c C-o" 'org-open-at-point-command)
(define-key *org-mode-keymap* "C-c l" 'org-store-link-command)
(define-key *org-mode-keymap* "C-c C-l" 'org-insert-link)
(define-key *org-mode-keymap* "C-c C-x C-n" 'org-next-link)
(define-key *org-mode-keymap* "C-c C-x C-p" 'org-previous-link)
(define-key *org-mode-keymap* "C-c C-x f" 'org-footnote-action-command)
(define-key *org-mode-keymap* "C-c C-x i" 'org-id-get-create-command)
