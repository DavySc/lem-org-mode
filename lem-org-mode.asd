(defun lem-org-mode-compile-policy (thunk)
  "Favour compile speed: a cold build, needed after every update of the source, takes about 10 s instead of 12 s."
  (with-compilation-unit (:policy '(optimize (compilation-speed 3) (speed 0) (debug 0) (safety 1)))
    (funcall thunk)))

(defsystem "lem-org-mode"
  :depends-on ("lem/core")
  :serial t
  :around-compile lem-org-mode-compile-policy
  :components ((:file "org-mode")
               (:file "timestamp")
               (:file "tasks-model")
               (:file "tasks")
               (:file "planning")
               (:file "structure")
               (:file "capture")
               (:file "links")
               (:file "tables")
               (:file "dblocks")
               (:file "editing")
               (:file "agenda")
               (:file "babel-model")
               (:file "export")
               (:file "export-more")
               (:file "publish")
               (:file "babel")))

(defsystem "lem-org-mode/tests"
  :depends-on ("lem-org-mode" "rove")
  :components ((:module "tests"
                :components ((:file "main")
                               (:file "babel2")
                               (:file "tasks-tests")
                               (:file "agenda-tests")
                               (:file "planning-tests")
                               (:file "structure-tests")
                               (:file "capture-tests")
                               (:file "links-tests")
                               (:file "tables-tests")
                               (:file "editing-tests")
                               (:file "export-tests")
                               (:file "publish-tests")
                               (:file "prompt-tests"))))
  :perform (test-op (op c) (symbol-call :rove '#:run c)))
