;;;; utopian.asd

(asdf:defsystem "utopian"
  :description "Save warnings."
  :long-description "This is the part of Utopian that records the warnings. It has no dependencies."
  :author "Paul M. Rodriguez <pmr@ruricolist.com>"
  :license "MIT"
  :depends-on ()
  :serial t
  :components ((:file "package")
               (:file "collect")))

(asdf:defsystem "utopian/report"
  :description "Generate reports."
  :author "Paul M. Rodriguez <pmr@ruricolist.com>"
  :license "MIT"
  :depends-on ("utopian"
               "serapeum"
               "quri"
               ;; For the HTML.
               "spinneret"
               "spinneret/cl-markdown"
               "lass"
               "parenscript"
               ;; For warnings.
               "closer-mop"
               ;; For ignore regexes.
               "cl-ppcre"
               ;; For opening the browser.
               "trivial-open-browser")
  :serial t
  :components ((:file "report")))
