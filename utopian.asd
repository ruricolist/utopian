;;;; utopian.asd

(asdf:defsystem "utopian"
  :description "Observe warnings and generate reports."
  :author "Paul M. Rodriguez <pmr@ruricolist.com>"
  :license "MIT"
  :depends-on ("utopian/collect"
               "utopian/report")
  :serial t
  :components ((:file "package")))

(asdf:defsystem "utopian/collect"
  :description "Save warnings."
  :long-description "This is the part of Utopian that records the warnings. It has no dependencies."
  :author "Paul M. Rodriguez <pmr@ruricolist.com>"
  :license "MIT"
  :serial t
  :components ((:file "collect")))

(asdf:defsystem "utopian/report"
  :description "Generate reports."
  :author "Paul M. Rodriguez <pmr@ruricolist.com>"
  :license "MIT"
  :depends-on ("utopian/collect"
               "serapeum"
               ;; For the HTML.
               "spinneret"
               "spinneret/cl-markdown"
               "lass"
               "parenscript"
               ;; For warnings.
               "closer-mop")
  :serial t
  :components ((:file "report")))
