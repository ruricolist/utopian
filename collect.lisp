;;;; utopian.lisp

(defpackage :utopian/collect
  (:use #:cl #:utopian)
  (:export
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

(defmacro let1 (var expr &body body)
  `(let ((,var ,expr))
     ,@body))

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
  (let ((file
          ;; TODO Do better.
          (or *compile-file-pathname* *load-truename*)))
    (and file
         (translate-logical-pathname file))))

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

(defun quicklisp-dist-root ()
  "Get the directory of the Quicklisp dist, if there is one, without
actually depending on Quicklisp."
  (let1 ql-dist (find-package :ql-dist)
    (and ql-dist
         (let1 dist
             (uiop:symbol-call ql-dist :find-dist "quicklisp")
           (and dist
                (uiop:symbol-call ql-dist :base-directory
                                  dist))))))

(defun quicklisp-dist-cache-root ()
  "Get the root directory for Quicklisp fasls."
  (let1 qroot (quicklisp-dist-root)
    (when qroot
      (uiop:merge-pathnames*
       (uiop:make-pathname*
        :directory
        (cons :relative
              (loop for tail on (pathname-directory qroot)
                    while (keywordp (first tail))
                    finally (return tail))))
       uiop:*user-cache*))))

(defun make-warning-report (system warnings)
  (list :system-name (system-name system)
        :warnings (sort-warnings (reverse warnings))
        :lisp-env (lisp-env-plist)
        :os-env (os-env-plist)
        :quicklisp-dist-root (quicklisp-dist-root)
        :quicklisp-dist-cache-root (quicklisp-dist-cache-root)))

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
  `(or uiop:compile-warned-warning))

(defparameter *useless-warning-types*
  '(("closer-mop" . "defmethod-without-generic-function")
    ("asdf/parse-defsystem" . "bad-system-name")))

(defun warning-useless? (warning)
  (let* ((class (class-of warning))
         (name (class-name class)))
    (and name
         (symbolp name)
         (let* ((package (package-name (symbol-package name)))
                (name (symbol-name name))
                (key (cons package name)))
           (find key
                 *useless-warning-types*
                 :test #'equalp)))))

(defun uninteresting? (c)
  (or (typep c 'uninteresting-warning)
      (uiop:match-any-condition-p c uiop:*usual-uninteresting-conditions*)
      (warning-useless? c)))

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
    (let ((report (warning-collector-report collector system)))
      (setf (system-report system) report)
      (after-load-message system (count-warnings report))
      (system-report-file system))))

(defun count-warnings (report)
  (length (getf report :warnings)))

(defmacro with-warning-report ((&key (system (error "No system."))) &body body)
  `(call/warning-report
    (lambda ()
      ,@body)
    ,system))

(defgeneric write-report (report dest)
  (:method ((system symbol) dest)
    (write-report (system-report system) dest))
  (:method ((system string) dest)
    (write-report (system-report system) dest))
  (:method (report (dest string))
    (let1 p (uiop:ensure-pathname dest :want-pathname t)
      (write-report report p)))
  (:method ((report list) (stream stream))
    (with-standard-io-syntax
      (prin1 report stream)))
  (:method ((report list) (file pathname))
    (with-open-file (stream file
                            :direction :output
                            :if-exists :error)
      (write-report report stream))))

(defgeneric read-report (source)
  (:method ((stream stream))
    (let ((report
            (with-standard-io-syntax
              (read stream))))
      (assert (report? report))
      report))
  (:method ((file pathname))
    (with-open-file (stream file :direction :input)
      (read-report stream)))
  (:method ((file string))
    (let1 p (uiop:ensure-pathname file :want-pathname t)
      (read-report p))))

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

(defun after-load-message (system warning-count)
  (let ((name (system-name system)))
    (format t "~&System ~a has been loaded with ~a warning~:p."
            name
            warning-count)
    (when (> warning-count 0)
      (format t "~%To render a report, load system ~a and evaluate: ~s"
              :utopian/report
              `(report-html-file ,name)))))

(declaim (notinline report-html-file))
(unless (fboundp 'report-html-file)
  (defun report-html-file (&rest args)
    (declare (ignore args))
    (error "The utopian/report system has not been loaded yet.")))
