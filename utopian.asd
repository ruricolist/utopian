;;;; utopian.asd

(asdf:defsystem "utopian"
  :description "Observe warnings and generate reports."
  :author "Paul M. Rodriguez <pmr@ruricolist.com>"
  :license "MIT"
  :depends-on ("serapeum"
               ;; For the HTML.
               "spinneret"
               "spinneret/cl-markdown"
               "lass"
               "parenscript"
               ;; For warnings.
               "closer-mop")
  :serial t
  :components ((:file "package")
               (:file "utopian")))

