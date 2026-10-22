(defsystem "lem-org-mode"
  :depends-on ("lem/core")
  :serial t
  :components ((:file "org-mode")
               (:file "agenda")
               (:file "babel-model")
               (:file "export")
               (:file "babel")))

(defsystem "lem-org-mode/tests"
  :depends-on ("lem-org-mode" "rove")
  :components ((:module "tests"
                :components ((:file "main"))))
  :perform (test-op (op c) (symbol-call :rove '#:run c)))
