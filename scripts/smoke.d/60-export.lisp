(in-package :cl-user)

(smoke-check "html export has numbered sections and a table of contents"
             t
             (let ((html (lem-org-mode:export-html (list "* One" "** Sub") '("TODO" "DONE"))))
               (and (search "section-number-3\">1.1</span>" html) (search "id=\"table-of-contents\"" html) t)))

(smoke-check "latex export"
             t
             (let ((tex (lem-org-mode:export-latex (list "* One" "*bold*") '("TODO" "DONE") :body-only t)))
               (and (search "\\section{One}" tex) (search "\\textbf{bold}" tex) t)))

(smoke-check "ascii export"
             t
             (let ((text (lem-org-mode:export-ascii (list "#+TITLE: T" "* One" "text") '("TODO" "DONE"))))
               (and (search "1 One" text) (search "text" text) t)))

(smoke-check "icalendar export"
             t
             (let ((ics (lem-org-mode:export-icalendar (lem-org-mode::agenda-parse (list "* Meet" "<2026-10-22 Thu 10:00>") '("TODO" "DONE") "x.org") :now '(2026 10 22 8 0))))
               (and (search "DTSTART:20261022T100000" ics) (search "BEGIN:VEVENT" ics) t)))

(smoke-check "publish selects files"
             '("/b/a.org")
             (lem-org-mode::publish-select-files '("/b/a.org" "/b/c.txt") "/b" "org" nil nil))
