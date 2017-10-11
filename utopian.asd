;;;; utopian.asd

(asdf:defsystem "utopian"
  :description "Observe warnings and generate reports."
  :author "Paul M. Rodriguez <pmr@ruricolist.com>"
  :license "MIT"
  :depends-on ("serapeum"
               "spinneret"
               "spinneret/cl-markdown"
               "lass"
               "parenscript"
               "closer-mop"
               "quicklisp")
  :serial t
  :components ((:file "package")
               (:file "utopian")))

