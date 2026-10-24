;;; Tasks, the pure part: TODO keyword sequences (#+TODO: TODO(t) NEXT(n!) | DONE(d@) CANCELED(c@/!)), log entries,
;;; repeating tasks and statistics cookies ([1/3] and [33%]). Nothing here touches a buffer.

(in-package :lem-org-mode)

;;; BEGIN pure tasks-model code

;;; Keyword sequences. A sequence is (:todo (kw ...) :done (kw ...) :keys ((kw . char) ...) :log ((kw enter leave) ...)).
;;; ENTER and LEAVE are NIL, :time or :note: what to log on entering and on leaving the state.

(defun todo-spec-words (text)
  (remove "" (split-words text) :test #'string=))

(defun todo-word-parse (word)
  "Parse TODO(t!) into (values keyword key enter leave)."
  (let ((open (position #\( word)))
    (if (and open (char= (char word (1- (length word))) #\)))
        (let* ((inside (subseq word (1+ open) (1- (length word))))
               (key (and (plusp (length inside)) (not (find (char inside 0) "!@/")) (char inside 0)))
               (flags (if key (subseq inside 1) inside))
               (slash (position #\/ flags))
               (flag (lambda (s) (cond ((find #\@ s) :note) ((find #\! s) :time)))))
          (values (subseq word 0 open) key
                  (funcall flag (subseq flags 0 slash))
                  (if slash (funcall flag (subseq flags (1+ slash))) nil)))
        (values word nil nil nil))))

(defun todo-sequence-parse (words)
  "A sequence from the WORDS of a #+TODO: line. Without | the last keyword is the done state."
  (let* ((bar (position "|" words :test #'string=))
         (todo-words (if bar (subseq words 0 bar) (butlast words)))
         (done-words (if bar (subseq words (1+ bar)) (last words)))
         (todo '()) (done '()) (keys '()) (log '()))
    (flet ((add (word kind)
             (multiple-value-bind (keyword key enter leave) (todo-word-parse word)
               (if (eq kind :todo) (push keyword todo) (push keyword done))
               (when key (push (cons keyword key) keys))
               (when (or enter leave) (push (list keyword enter leave) log)))))
      (dolist (w todo-words) (add w :todo))
      (dolist (w done-words) (add w :done)))
    (list :todo (nreverse todo) :done (nreverse done) :keys (nreverse keys) :log (nreverse log))))

(defun todo-sequences-from-setting (setting)
  "Sequences from a user setting: a list of strings (one sequence, with an optional \"|\"), or a list of such lists."
  (cond ((null setting) (list (todo-sequence-parse '("TODO" "DONE"))))
        ((stringp (first setting)) (list (todo-sequence-parse setting)))
        (t (mapcar (lambda (entry)
                     (todo-sequence-parse (if (member (first entry) '(sequence type :sequence :type))
                                              (rest entry)
                                              entry)))
                   setting))))

(defun todo-sequences-from-lines (lines default)
  "The sequences declared by #+TODO:, #+SEQ_TODO: and #+TYP_TODO: lines in LINES, or DEFAULT when there are none."
  (let ((found '()))
    (dolist (line lines)
      (let* ((s (string-left-trim " 	" line))
             (colon (position #\: s)))
        (when (and colon (> (length s) 2) (string= "#+" s :end2 2)
                   (member (subseq s 2 colon) '("TODO" "SEQ_TODO" "TYP_TODO") :test #'string-equal))
          (let ((words (todo-spec-words (subseq s (1+ colon)))))
            (when words (push (todo-sequence-parse words) found))))))
    (if found (nreverse found) default)))

(defun sequences-keywords (sequences)
  "All keywords of SEQUENCES, todo states first within each sequence."
  (loop :for s :in sequences :append (getf s :todo) :append (getf s :done)))

(defun sequences-done-keywords (sequences)
  (loop :for s :in sequences :append (getf s :done)))

(defun sequences-done-p (sequences keyword)
  (and keyword (member keyword (sequences-done-keywords sequences) :test #'string=) t))

(defun sequences-first-todo (sequences &optional keyword)
  "The first todo keyword of the sequence that holds KEYWORD (or of the first sequence)."
  (let ((sequence (or (and keyword (find-if (lambda (s) (member keyword (append (getf s :todo) (getf s :done)) :test #'string=))
                                            sequences))
                      (first sequences))))
    (first (getf sequence :todo))))

(defun sequences-next (sequences current direction)
  "The state after CURRENT (a keyword or NIL for none) when cycling forward (DIRECTION 1) or backward (-1)."
  (let* ((sequence (and current (find-if (lambda (s) (member current (append (getf s :todo) (getf s :done)) :test #'string=))
                                         sequences)))
         (all (and sequence (append (getf sequence :todo) (getf sequence :done)))))
    (cond ((null current)
           (if (plusp direction)
               (first (getf (first sequences) :todo))
               (let ((last-sequence (car (last sequences))))
                 (car (last (append (getf last-sequence :todo) (getf last-sequence :done)))))))
          ((null sequence) (sequences-next sequences nil direction))
          (t (let ((position (+ (position current all :test #'string=) direction)))
               (and (< -1 position (length all)) (nth position all)))))))

(defun sequences-key-alist (sequences)
  "((char . keyword) ...) for fast selection."
  (loop :for s :in sequences :append (mapcar (lambda (entry) (cons (cdr entry) (car entry))) (getf s :keys))))

(defun sequences-log (sequences keyword)
  "(values enter leave) logging flags of KEYWORD."
  (let ((entry (loop :for s :in sequences :for e := (assoc keyword (getf s :log) :test #'string=) :when e :return e)))
    (values (second entry) (third entry))))

;;; Log entries

(defun log-state-text (new old stamp)
  "The first line of a state change entry."
  (format nil "- State ~12A from ~12A ~A" (format nil "~S" (or new "")) (format nil "~S" (or old "")) stamp))

(defun log-entry-lines (first-line note)
  "FIRST-LINE and, when NOTE is a non-empty string, the note lines under it."
  (if (and note (plusp (length note)))
      (cons (format nil "~A \\\\" first-line)
            (mapcar (lambda (line) (concatenate 'string "  " line)) (split-text-lines note)))
      (list first-line)))

(defun log-note-text (stamp)
  (format nil "- Note taken on ~A" stamp))

(defun logbook-add (logbook entry-lines drawer-name)
  "LOGBOOK (the lines of the drawer, or NIL) with ENTRY-LINES on top."
  (if logbook
      (append (list (first logbook)) entry-lines (rest logbook))
      (append (list (format nil ":~A:" drawer-name)) entry-lines (list ":END:"))))

;;; Repeating tasks

(defun planning-with-repeat (planning today now-minutes)
  "PLANNING, an alist (keyword . timestamp text), with SCHEDULED and DEADLINE shifted by their repeaters.
Return (values alist shifted-p)."
  (let ((shifted nil))
    (values (mapcar (lambda (entry)
                      (let ((ts (and (member (car entry) '("SCHEDULED" "DEADLINE") :test #'string=)
                                     (timestamp-parse (cdr entry)))))
                        (if (and ts (getf ts :repeater))
                            (progn (setf shifted t)
                                   (cons (car entry) (timestamp-format (timestamp-next-repeat ts today now-minutes))))
                            entry)))
                    planning)
            shifted)))

(defun text-with-repeat (text today now-minutes)
  "TEXT with every active timestamp that has a repeater shifted. Return (values text shifted-p)."
  (let ((out (make-string-output-stream)) (pos 0) (shifted nil))
    (dolist (found (timestamps-in-text text))
      (destructuring-bind (ts start end) found
        (when (and (getf ts :active) (getf ts :repeater))
          (setf shifted t)
          (write-string text out :start pos :end start)
          (write-string (timestamp-format (timestamp-next-repeat ts today now-minutes)) out)
          (setf pos end))))
    (write-string text out :start pos)
    (values (get-output-stream-string out) shifted)))

(defun entry-repeats-p (lines)
  "True when the entry LINES (heading first) holds an active timestamp with a repeater."
  (and (some (lambda (line) (some (lambda (found) (and (getf (first found) :active) (getf (first found) :repeater)))
                                  (timestamps-in-text line)))
             lines)
       t))

(defun entry-apply-repeat (lines today now-minutes)
  "The entry LINES (heading line first, up to the next heading) with all repeating active timestamps moved on."
  (mapcar (lambda (line) (values (text-with-repeat line today now-minutes))) lines))

;;; Statistics cookies

(defun cookie-find (text)
  "Find the first statistics cookie in TEXT. Return (values start end kind) with KIND :count ([1/3], [/]) or
:percent ([33%], [%]); NIL when there is none."
  (let ((from 0))
    (loop
      (let* ((open (position #\[ text :start from))
             (close (and open (position #\] text :start open))))
        (unless close (return nil))
        (let ((inside (subseq text (1+ open) close)))
          (cond ((and (position #\/ inside)
                      (every (lambda (c) (or (digit-char-p c) (char= c #\/))) inside)
                      (= (count #\/ inside) 1))
                 (return (values open (1+ close) :count)))
                ((and (plusp (length inside)) (char= (char inside (1- (length inside))) #\%)
                      (every #'digit-char-p (subseq inside 0 (1- (length inside)))))
                 (return (values open (1+ close) :percent)))))
        (setf from (1+ open))))))

(defun cookie-render (kind done total)
  (if (eq kind :percent)
      (format nil "[~D%]" (if (zerop total) 0 (floor (* 100 done) total)))
      (format nil "[~D/~D]" done total)))

(defun line-with-cookie (line done total)
  "LINE with its statistics cookie updated; LINE itself when it has none."
  (multiple-value-bind (start end kind) (cookie-find line)
    (if start
        (concatenate 'string (subseq line 0 start) (cookie-render kind done total) (subseq line end))
        line)))

(defun lines-heading-index (lines index keywords)
  "The index of the heading whose entry contains line INDEX, or NIL."
  (loop :for i :from index :downto 0
        :when (heading-split (nth i lines) keywords) :return i))

(defun heading-cookie-stats (lines index keywords done-keywords)
  "Count for the heading at INDEX of LINES: the direct child headings with a todo keyword (done: in DONE-KEYWORDS), or,
when there are none, the checkboxes of the entry's own list. Return (values done total)."
  (let* ((level (heading-split (nth index lines) keywords))
         (done 0) (total 0) (todo-children nil)
         (end (or (position-if (lambda (l) (let ((lv (heading-split l keywords))) (and lv (<= lv level))))
                               lines :start (1+ index))
                  (length lines))))
    (loop :for i :from (1+ index) :below end
          :do (multiple-value-bind (lv todo) (heading-split (nth i lines) keywords)
                (when (and lv (= lv (1+ level)) todo)
                  (setf todo-children t)
                  (incf total)
                  (when (member todo done-keywords :test #'string=) (incf done)))))
    (unless todo-children
      (let ((own-end (or (position-if (lambda (l) (heading-split l keywords)) lines :start (1+ index)) (length lines)))
            (top nil))
        (loop :for i :from (1+ index) :below own-end
              :do (multiple-value-bind (indent bullet box) (list-item-parse (nth i lines))
                    (declare (ignore bullet))
                    (when indent
                      (when (or (null top) (< indent top)) (setf top indent))
                      (when (and box (= indent top))
                        (incf total)
                        (when (char= box #\X) (incf done))))))))
    (values done total)))

(defun item-cookie-stats (lines index)
  "Count for the list item at INDEX: its direct sub-items with a checkbox. Return (values checked total)."
  (let* ((indent (list-item-parse (nth index lines)))
         (done 0) (total 0) (child-indent nil))
    (loop :for i :from (1+ index) :below (length lines)
          :do (let ((line (nth i lines)))
                (multiple-value-bind (ind bullet box) (list-item-parse line)
                  (declare (ignore bullet))
                  (cond ((and ind (<= ind indent)) (return))
                        ((and (null ind) (not (blank-string-p line)) (<= (line-indent line) indent)) (return))
                        ((blank-string-p line))
                        (ind (when (null child-indent) (setf child-indent ind))
                             (when (and box (= ind child-indent))
                               (incf total)
                               (when (char= box #\X) (incf done))))))))
    (values done total)))

(defun item-set-checkbox (line state)
  "LINE, a list item with a checkbox, with the box set to STATE (#\\Space, #\\- or #\\X)."
  (let ((box (position #\[ line :start (multiple-value-bind (indent bullet) (list-item-parse line)
                                         (+ indent (length bullet))))))
    (concatenate 'string (subseq line 0 (1+ box)) (string state) (subseq line (+ box 2)))))

(defun cookies-update (lines index keywords done-keywords)
  "Recompute the checkbox state and statistics cookie of every list item and heading that contains line INDEX
(plus the line itself). Return an alist (line-index . new-line) of the lines that change."
  (let ((changes '()) (current (copy-list lines)))
    (flet ((change (i new)
             (unless (string= new (nth i current))
               (setf (nth i current) new)
               (push (cons i new) changes))))
      ;; list items: the item itself and its parents
      (when (list-item-parse (nth index current))
        (let ((indent-limit (1+ (list-item-parse (nth index current)))))
          (loop :for i :from index :downto 0
                :do (let ((line (nth i current)))
                      (cond ((heading-split line keywords) (return))
                            ((and (list-item-parse line) (< (list-item-parse line) indent-limit))
                             (multiple-value-bind (done total) (item-cookie-stats current i)
                               (multiple-value-bind (ind bullet box) (list-item-parse line)
                                 (declare (ignore ind bullet))
                                 (let ((new line))
                                   (when (and box (plusp total))
                                     (setf new (item-set-checkbox new (cond ((= done total) #\X) ((zerop done) #\Space) (t #\-)))))
                                   (setf new (line-with-cookie new done total))
                                   (change i new))))
                             (setf indent-limit (list-item-parse line)))))))) 
      ;; headings: the entry's own heading and its ancestors
      (let ((heading (lines-heading-index current index keywords)))
        (loop :while heading
              :do (multiple-value-bind (done total) (heading-cookie-stats current heading keywords done-keywords)
                    (change heading (line-with-cookie (nth heading current) done total)))
                  (let ((level (heading-split (nth heading current) keywords)))
                    (setf heading (and (> level 1)
                                       (loop :for i :from (1- heading) :downto 0
                                             :when (let ((lv (heading-split (nth i current) keywords))) (and lv (< lv level)))
                                               :return i)))))))
    (nreverse changes)))

(defun cookies-update-all (lines keywords done-keywords)
  "`cookies-update' for every heading and item of LINES that carries a cookie."
  (let ((changes '()) (current (copy-list lines)))
    (loop :for i :from 0 :below (length current)
          :do (when (and (cookie-find (nth i current))
                         (or (heading-split (nth i current) keywords) (list-item-parse (nth i current))))
                (dolist (change (cookies-update current i keywords done-keywords))
                  (setf (nth (car change) current) (cdr change))
                  (setf changes (cons change (remove (car change) changes :key #'car))))))
    (sort changes #'< :key #'car)))

;;; Effort

(defun effort-minutes (text)
  "Minutes in an effort such as 1:30, 2h, 45min, 1d or 90."
  (let* ((text (string-trim " " (or text ""))) (colon (position #\: text)))
    (cond ((string= text "") nil)
          (colon (+ (* 60 (or (parse-integer text :end colon :junk-allowed t) 0))
                    (or (parse-integer text :start (1+ colon) :junk-allowed t) 0)))
          (t (let* ((n (parse-integer text :junk-allowed t))
                    (unit (string-downcase (string-left-trim "0123456789. " text))))
               (and n (cond ((string= unit "") n)
                            ((member unit '("h" "hour" "hours") :test #'string=) (* 60 n))
                            ((member unit '("min" "m" "minutes") :test #'string=) n)
                            ((member unit '("d" "day" "days") :test #'string=) (* 60 8 n))
                            ((member unit '("w" "week" "weeks") :test #'string=) (* 60 8 5 n)))))))))

;;; END pure tasks-model code
