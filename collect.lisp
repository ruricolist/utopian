;;;; utopian.lisp

(defpackage :utopian/collect
  (:use :cl)
  (:export
   #:export-report
   #:import-report
   #:load-system
   #:quickload
   #:delayed-symbol->symbol
   #:delayed-symbol.package
   #:delayed-symbol.name
   #:warning-info.class
   #:warning-info.source-file
   #:warning-info.severity
   #:warning-info.severity-level
   #:warning-info.string
   #:warning-report.system
   #:warning-report.warnings
   #:uninteresting-warning
   #:severity
   #:warning-info
   #:warning-report))

(in-package :utopian/collect)

;;; "utopian" goes here. Hacks and glory await!

(deftype severity ()
  '(member :note :style-warning :warning))

(defun severity-level (sev)
  (ecase sev
    (:note 0)
    (:style-warning 1)
    (:warning 2)))

(defgeneric condition-severity (condition)
  (:method ((c warning)) :warning)
  (:method ((c style-warning)) :style-warning)
  #+sbcl (:method ((c sb-ext:compiler-note)) :note))

(defun sort-warnings (warnings)
  (coerce
   (stable-sort (copy-seq warnings)
                #'>
                :key #'warning-info.severity-level)
   'list))

(defstruct
    (delayed-symbol
     ;; Ensure default constructor.
     :constructor
     (:conc-name delayed-symbol.)
     (:constructor delayed-symbol (package name)))
  "An unresolved symbol that can be freely written out and read in
without having to worry whether the package actually exists."
  (package (error "No package!")
   :type string
   :read-only t)
  (name (error "No name!")
   :type string
   :read-only t))

(defun symbol->delayed-symbol (symbol)
  (delayed-symbol
   (package-name (symbol-package symbol))
   (symbol-name symbol)))

(defun delayed-symbol->symbol (ds)
  (let ((package (delayed-symbol.package ds))
        (name (delayed-symbol.name ds)))
    (or (find-symbol name (find-package package))
        (error "No such symbol as ~a in ~a" name package))))

(defstruct (warning-info (:conc-name warning-info.))
  ;; We do not store the condition itself to ensure that instances can
  ;; be written and read.
  (class (error "No class!") :type delayed-symbol :read-only t)
  (severity (error "No severity!") :type severity :read-only t)
  (string (error "No string!") :type string :read-only t)
  ;; TODO Do better.
  (source-file (or *compile-file-pathname* *load-truename*)
   :type (or null pathname)
   :read-only t))

(defmethod warning-info.severity-level ((self warning-info))
  (severity-level (warning-info.severity self)))

(defstruct (warning-report (:conc-name warning-report.))
  (system (error "No system!")
   :type string
   :read-only t)
  (warnings (error "No warnings!")
   :type list
   :read-only t)
  (lisp-implementation-type
   (lisp-implementation-type)
   :type (or string null)
   :read-only t)
  (lisp-implementation-version
   (lisp-implementation-version)
   :type (or string null)
   :read-only t))

(defclass warning-collector ()
  ((warnings
    :initform (make-array 10 :adjustable t :fill-pointer 0)
    :type vector
    :reader warning-collector-warnings)))

(deftype uninteresting-warning ()
  `(or uiop:compile-warned-warning
       ,@(remove-if-not #'symbolp uiop:*usual-uninteresting-conditions*)
       (satisfies uiop-finds-uninteresting?)))

(defun uiop-finds-uninteresting? (c)
  (uiop:match-any-condition-p c uiop:*usual-uninteresting-conditions*))

(defun uninteresting? (c)
  (typep c 'uninteresting-warning))

(defgeneric collect-warning (self condition)
  (:method :around (self condition)
    (declare (ignore self))
    (unless (uninteresting? condition)
      (call-next-method))))

(defmethod collect-warning ((self warning-collector) (warning condition))
  (with-slots (warnings) self
    (let ((info
            (make-warning-info
             :class (symbol->delayed-symbol (type-of warning))
             :string (princ-to-string warning)
             :severity (condition-severity warning))))
      (vector-push-extend info warnings))))

(defmethod warning-collector-report ((self warning-collector) system)
  (with-slots (warnings) self
    (make-warning-report
     :warnings (sort-warnings (reverse warnings))
     :system system)))

(defmethod warning-collector-handler ((self warning-collector))
  (lambda (&rest args)
    (apply #'collect-warning self args)))

(declaim (inline make-warning-collector))
(defun make-warning-collector (&rest args &key &allow-other-keys)
  (apply #'make-instance 'warning-collector args))

(defun call/warning-report (fn system)
  (let* ((collector (make-warning-collector))
         (handler (warning-collector-handler collector)))
    (handler-bind ((warning handler)
                   #+sbcl (sb-ext:compiler-note handler))
      (funcall fn))
    (warning-collector-report collector system)))

(defmacro with-warning-report ((&key (system (error "No system."))) &body body)
  `(call/warning-report
    (lambda ()
      ,@body)
    ,system))

(defun export-report (report stream)
  (with-standard-io-syntax
    (prin1 report stream)))

(defun import-report (stream)
  (let ((report
          (with-standard-io-syntax
            (read stream))))
    (assert (typep report 'warning-report))
    report))

(defun load-system (system &rest args &key &allow-other-keys)
  (with-warning-report (:system (string system))
    (apply #'asdf:load-system system args)))

(defun quickload (system)
  (unless (find-package :quicklisp)
    (error "Quicklisp is not installed in this Lisp."))
  (with-warning-report (:system (string system))
    (uiop:symbol-call :ql :quickload
                      (list system)
                          :verbose t)))
