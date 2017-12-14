;;;; utopian.lisp

(defpackage :utopian/collect
  (:use #:cl #:utopian)
  (:export
   #:export-report
   #:import-report
   #:load-system
   #:quickload
   #:delayed-symbol->symbol
   #:delayed-symbol.package
   #:delayed-symbol.name
   #:warning-class
   #:warning-source-file
   #:warning-severity
   #:warning-severity-level
   #:warning-string
   #:uninteresting-warning
   #:severity
   #:warning-info
   #:system-report))

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
                :key #'warning-severity-level)
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

(defun current-source-file ()
  ;; TODO Do better.
  (or *compile-file-pathname* *load-truename*))

(defstruct (warning-info
            (:conc-name warning-))
  ;; We do not store the condition itself to ensure that instances can
  ;; be written and read.
  (class (error "No class!") :type delayed-symbol :read-only t)
  (severity (error "No severity!") :type severity :read-only t)
  (string (error "No string!") :type string :read-only t)
  ;; TODO Do better.
  (source-file (current-source-file)
   :type (or null pathname)
   :read-only t))

(defmethod warning-severity-level ((self warning-info))
  (severity-level (warning-severity self)))

(deftype string-designator ()
  '(or string symbol))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-keyword (x)
    (if (keywordp x) x
        (intern (string x) :keyword))))

(defun system-name (system)
  (make-keyword
   (if (typep system 'asdf:system)
       (asdf:component-name system)
       system)))

(defun lisp-env-plist ()
  "Gather Lisp-supplied environment info."
  (list
   :lisp-implementation-type (lisp-implementation-type)
   :lisp-implementation-version (lisp-implementation-version)
   :machine-instance (machine-instance)
   :machine-type (machine-type)
   :machine-version (machine-version)
   :short-site-name (short-site-name)
   :long-site-name (long-site-name)))

(defun os-env-plist ()
  (list
   :path (uiop:getenv "PATH")
   :ostype (uiop:getenv "OSTYPE")
   :hosttype (uiop:getenv "HOSTTYPE")
   :lang (uiop:getenv "LANG")))

(defun make-warning-report (system warnings)
  (list :system-name (system-name system)
        :warnings (sort-warnings (reverse warnings))
        :lisp-env (lisp-env-plist)
        :os-env (os-env-plist)))

(defun reports-dir ()
  (ensure-directories-exist
   (uiop:merge-pathnames*
    "utopian/systems/"
    uiop:*temporary-directory*)))

(defun escape-system-name (name)
  (substitute #\_ #\/ (string (system-name name))))

(defun system-report-file (system)
  (let ((system (system-name system)))
    (uiop:merge-pathnames*
     (make-pathname :name (escape-system-name system))
     (reports-dir))))

(defun save-report (report)
  (let* ((system (getf report :system-name))
         (file (system-report-file system)))
    (with-open-file (out file
                         :direction :output
                         :if-exists :supersede)
      (prin1 report out))
    file))

(defun reload-report (system &key (error t))
  (let* ((name (system-name system))
         (file (system-report-file name))
         (report
           (with-open-file (in file :direction :input)
             (read in))))
    (if (and (plist? report)
             (not (null report)))
        report
        (when error
          (error "Report for ~a was corrupt." name)))))

(defun plist? (x)
  (and (listp x)
       (evenp (length x))))

(defun report? (report)
  (and report (plist? report)))

(defvar *reports*
  (make-hash-table)
  "Reports for systems.")

(defun system-report (system-name)
  (reload-report system-name))

(defun (setf system-report) (report system-name)
  (assert (report? report))
  (assert (string= (getf report :system-name)
                   system-name))
  (prog1 report
    (save-report report)))

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
  (or (typep c 'uninteresting-warning)))

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
    (make-warning-report system warnings)))

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
    (setf (system-report system)
          (warning-collector-report collector system))
    (after-load-message system)
    (system-report-file system)))

(defmacro with-warning-report ((&key (system (error "No system."))) &body body)
  `(call/warning-report
    (lambda ()
      ,@body)
    ,system))

(defun utopian:export-report (report stream)
  (with-standard-io-syntax
    (prin1 report stream)))

(defun utopian:import-report (stream)
  (let ((report
          (with-standard-io-syntax
            (read stream))))
    (assert (report? report))
    report))

(defun utopian:load-system (system &rest args &key &allow-other-keys)
  (with-warning-report (:system (string system))
    (apply #'asdf:load-system system args)))

(defun utopian:quickload (system)
  (unless (find-package :quicklisp)
    (error "Quicklisp is not installed in this Lisp."))
  (with-warning-report (:system (string system))
    (uiop:symbol-call :ql :quickload
                      (list system)
                          :verbose t)))

(defun after-load-message (system)
  (let ((name (system-name system)))
    (format t "~&System ~a has been loaded.
To render a report of any warnings, load system ~a and evaluate:
    ~s"
            name
            :utopian/report
            `(utopian:report-html-file ,name))))
