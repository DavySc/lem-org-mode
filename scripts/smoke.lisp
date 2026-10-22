;;; Smoke test that runs inside a real Lem, without Rove.
;;;
;;;   lem.exe -q --eval "(load \"<repo>/scripts/smoke.lisp\")"
;;;
;;; Results go to scripts/smoke.log (one PASS/FAIL line per check, then DONE),
;;; because the GUI build prints nothing to stdout. Lem is left running; kill it
;;; once the log ends with DONE.

(in-package :cl-user)

(defparameter *smoke-root*
  (merge-pathnames "../" (make-pathname :name nil :type nil :defaults *load-truename*)))

(defparameter *smoke-log* (merge-pathnames "scripts/smoke.log" *smoke-root*))

(defun smoke-say (fmt &rest args)
  (with-open-file (s *smoke-log* :direction :output :if-exists :append :if-does-not-exist :create)
    (apply #'format s fmt args)
    (terpri s)))

(defmacro smoke-check (name expected form)
  `(handler-case
       (let ((actual ,form))
         (if (equal actual ,expected)
             (smoke-say "PASS ~A" ,name)
             (smoke-say "FAIL ~A: expected ~S, got ~S" ,name ,expected actual)))
     (serious-condition (e) (smoke-say "FAIL ~A: ~A" ,name e))))

(with-open-file (s *smoke-log* :direction :output :if-exists :supersede) (declare (ignore s)))

(handler-case
    (progn
      (asdf:load-asd (merge-pathnames "lem-org-mode.asd" *smoke-root*))
      (asdf:load-system "lem-org-mode")
      (smoke-say "PASS load"))
  (serious-condition (e) (smoke-say "FAIL load: ~A" e)))

(in-package :cl-user)

(defun smoke-buffer (text)
  (let ((buffer (lem:make-buffer nil :temporary t)))
    (lem:insert-string (lem:buffer-point buffer) text)
    buffer))

(defun smoke-text (buffer)
  (lem:points-to-string (lem:buffer-start-point buffer) (lem:buffer-end-point buffer)))

(defun smoke-line (buffer n)
  (lem:move-to-line (lem:copy-point (lem:buffer-start-point buffer) :temporary) n))

(smoke-check "file type" 'lem-org-mode:org-mode (lem:get-file-mode "notes.org"))

(smoke-check "heading level"
             '(1 2 nil)
             (let ((b (smoke-buffer (format nil "* A~%** B~%text~%"))))
               (list (lem-org-mode:org-heading-level (smoke-line b 1))
                     (lem-org-mode:org-heading-level (smoke-line b 2))
                     (lem-org-mode:org-heading-level (smoke-line b 3)))))

(smoke-check "todo cycle"
             (list (format nil "** TODO x~%") (format nil "** DONE x~%") (format nil "** x~%"))
             (let ((b (smoke-buffer (format nil "** x~%"))) (out '()))
               (dotimes (i 3)
                 (lem-org-mode:org-cycle-todo (smoke-line b 1))
                 (push (smoke-text b) out))
               (nreverse out)))

(smoke-check "demote/promote"
             (list (format nil "** A~%*** A1~%* B~%") (format nil "* A~%** A1~%* B~%"))
             (let ((b (smoke-buffer (format nil "* A~%** A1~%* B~%"))) (out '()))
               (lem-org-mode:org-shift-subtree (smoke-line b 1) 1)
               (push (smoke-text b) out)
               (lem-org-mode:org-shift-subtree (smoke-line b 1) -1)
               (push (smoke-text b) out)
               (nreverse out)))

(smoke-check "move subtree down and up"
             (list (format nil "* B~%b~%* A~%a") (format nil "* A~%a~%* B~%b"))
             (let ((b (smoke-buffer (format nil "* A~%a~%* B~%b"))) (out '()))
               (lem-org-mode:org-move-subtree (smoke-line b 1) :down)
               (push (smoke-text b) out)
               (lem-org-mode:org-move-subtree (smoke-line b 3) :up)
               (push (smoke-text b) out)
               (nreverse out)))

(smoke-say "DONE")
