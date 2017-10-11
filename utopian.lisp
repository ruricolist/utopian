;;;; utopian.lisp

(in-package #:utopian)

;;; "utopian" goes here. Hacks and glory await!

(deftype severity ()
  '(member :note :style-warning :warning))

(defun severity-level (sev)
  (ecase-of severity sev
    (:note 0)
    (:style-warning 1)
    (:warning 2)))

(defgeneric condition-severity (condition)
  (:method ((c warning)) :warning)
  (:method ((c style-warning)) :warning)
  #+sbcl (:method ((c sb-ext:compiler-note)) :note))

(defun sort-warnings (warnings)
  (~> warnings
      (sort-new #'> :key #'warning-info-condition-severity-level)
      (coerce 'list)))

(defconstructor delayed-symbol
  "An unresolved symbol that can be freely written out and read in
without having to worry whether the package actually exists."
  (package string)
  (name string))

(defun symbol->delayed-symbol (symbol)
  (delayed-symbol
   (package-name (symbol-package symbol))
   (symbol-name symbol)))

(defun delayed-symbol->symbol (ds)
  (let-match1 (delayed-symbol package name) ds
    (intern name (find-package package))))

(defstruct-read-only warning-info
  ;; We do not store the condition itself to ensure that instances can
  ;; be written and read.
  (condition-type :type delayed-symbol)
  (condition-severity :type severity)
  (string :type string)
  ;; TODO Do better.
  (source-file (or *compile-file-pathname* *load-truename*) :type (or null pathname)))

(defmethod warning-info-condition-severity-level ((self warning-info))
  (severity-level (warning-info-condition-severity self)))

(defstruct-read-only warning-report
  (system :type string)
  (warnings :type list))

(defclass warning-collector ()
  ((q :initform (queue) :type queue
      :reader warning-collector-q)))

(defun uninteresting? (c)
  (or
   (uiop:match-any-condition-p c uiop:*usual-uninteresting-conditions*)
   (typep c '(or uiop:compile-warned-warning
              c2mop::defmethod-without-generic-function
              asdf:bad-system-name))))

(defgeneric collect-warning (self condition)
  (:method :around (self condition)
    (declare (ignore self))
    (unless (uninteresting? condition)
      (call-next-method))))

(defmethods warning-collector (self q)
  (:method collect-warning (self (warning condition))
    (let ((info
            (make-warning-info
             :condition-type (symbol->delayed-symbol (type-of warning))
             :string (princ-to-string warning)
             :condition-severity (condition-severity warning))))
      (enq info q)))

  (:method warning-collector-report (self system)
    (make-warning-report :warnings (sort-warnings (qlist q))
                         :system system))

  (:method warning-collector-handler (self)
    (curry #'collect-warning self)))

(defsubst make-warning-collector (&rest args &key &allow-other-keys)
  (apply #'make 'warning-collector args))

(defun call/warning-report (fn system)
  (let* ((collector (make-warning-collector))
         (handler (warning-collector-handler collector)))
    (handler-bind ((warning handler)
                   #+sbcl (sb-ext:compiler-note handler))
      (funcall fn))
    (warning-collector-report collector system)))

(defmacro with-warning-report ((&key (system (required-argument :system))) &body body)
  `(call/warning-report
    (lambda ()
      ,@body)
    ,system))

(defun export-report (report stream)
  (with-standard-io-syntax
    (prin1 report stream)))

(defun import-report (stream)
  (assure warning-report
    (with-standard-io-syntax
      (read stream))))

(defun load-system/report (system &rest args &key &allow-other-keys)
  (with-warning-report (:system (string system))
    (apply #'asdf:load-system system args)))

(defun quickload-system/report (system &rest args &key &allow-other-keys)
  (with-warning-report (:system (string system))
    (apply #'ql:quickload (list system) :verbose t args)))

(defclass report-formatter ()
  ((report :initarg :report :type warning-report))
  (:default-initargs
   :report (required-argument :report)))

(defclass text-formatter (report-formatter)
  ())

(defgeneric render-text (formatter report &optional stream))
(defgeneric render-warning (formatter warning &optional stream))

(defclass html-report (text-formatter)
  ())

(eval-always
  (defvar *ids* 0)

  (defun genid (string)
    (fmt "~a~a" string (finc *ids*))))

(deftag accordion-card (body attrs &key title parent)
  (with-unique-names (header-id body-id)
    `(let ((,header-id (genid "heading"))
           (,body-id (genid "collapse")))
       (:div :class "card"
         ,@attrs
         (:div :class "card-header" :role "tab" :id ,header-id
           (:h5
             (:a :class "collapsed"
               :data-toggle "collapse"
               :href (fmt "#~a" ,body-id)
               :aria-expanded "false"
               :aria-controls ,body-id
               ,title)))
         (:div :id ,body-id
           :class "collapse"
           :role "tabpanel"
           :aria-labelledby ,header-id
           :data-parent (fmt "#~a" ,parent)
           (:div.card-body
            ,@body))))))

(defmethod render-text ((formatter html-report) (report warning-report) &optional (*html* *html*))
  (let* ((system (warning-report-system report))
         (warnings (warning-report-warnings report))

         (by-source-file (assort warnings :test #'equal :key #'warning-info-source-file))
         (by-source-file (dsu-sort by-source-file #'> :key #'length))

         (by-severity (assort warnings :key #'warning-info-condition-severity))
         (by-severity (dsu-sort by-severity #'> :key (op (warning-info-condition-severity-level (first _)))))

         (by-source-file-id "by-source-file")
         (by-severity-id "by-severity")
         (by-source-file-hash (fmt "#~a" by-source-file-id))
         (by-severity-hash (fmt "#~a" by-severity-id)))

    (with-html
      (:doctype)
      (:html
        (:head
          (:meta :name "viewport" :content "width=device-width, initial-scale=1, shrink-to-fit=no")
          (:title ("Report for system ~s" system))
          (:link
            :rel "stylesheet"
            :href "https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0-beta/css/bootstrap.min.css"
            :integrity "sha384-/Y6pD6FV/Vv2HJnA6t+vslU6fwYXjCFtcEpHbNJ0lyAFsXTsjBbfaDjzALeQsN6M"
            :crossorigin "anonymous"))

        (:body
          (:div.container

           (when (or by-source-file by-severity)
             (:ul
               (:li (:a :href by-source-file-hash "By file"))
               (:li (:a :href by-severity-hash "By severity"))))

           (:p ("~a warning~:p in ~a file~:p"
                (length warnings)
                (length by-source-file)))

           (when by-source-file
             (:h1 "By file")
             (:div :id by-source-file-id :role "tablist"
               (do-each (file-warnings by-source-file)
                 (let* ((file (warning-info-source-file (first file-warnings)))
                        (title
                          (fmt "File: ~a (~a notes)"
                               (or (remove-homedir file) "Unknown")
                               (length file-warnings))))
                   (accordion-card
                     :title title
                     :parent by-source-file-id
                     (:ul.list-group
                      (dolist (warning file-warnings)
                        (render-warning formatter warning))))))))

           (when by-severity
             (:h1 "By severity")
             (:div :id by-severity-id :role "tablist"
               (do-each (warnings by-severity)
                 (let ((severity (warning-info-condition-severity (first warnings))))
                   (accordion-card
                     :title (fmt "Severity: ~a (~a)"
                                 (severity-title severity)
                                 (length warnings))
                     :parent by-severity-id
                     (:ul.list-group
                      (dolist (warning warnings)
                        (render-warning formatter warning)))))))))

          (:script :src "https://code.jquery.com/jquery-3.2.1.slim.min.js" :integrity "sha384-KJ3o2DKtIkvYIK3UENzmM7KCkRr/rE9/Qpg6aAZGJwFDMVNA/GpGFF93hXpG5KkN" :crossorigin "anonymous")
          (:script :src "https://cdnjs.cloudflare.com/ajax/libs/popper.js/1.11.0/umd/popper.min.js" :integrity "sha384-b/U6ypiBEHpOf/4+1nzFpr53nxSS+GLCkfwBdFNTxtclqqenISfwAzpKaMNFNmj4" :crossorigin "anonymous")
          (:script :src "https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0-beta/js/bootstrap.min.js" :integrity "sha384-h0AbiXch4ZDo7tp9hKZ4TsHbi047NrKGLO3SEJAg45jXxnGIfYzk4Si90RDIqNm1" :crossorigin "anonymous"))))))

(defun severity-title (sev)
  (ecase-of severity sev
    (:note "Note")
    (:warning "Warning")
    (:style-warning "Style warning")))

(defun severity-class (sev)
  (ecase-of severity sev
    (:note "list-group-item-info")
    (:warning "list-group-item-danger")
    (:style-warning "list-group-item-warning")))

(defun pathname-file-url (file)
  (let ((file (truename file)))
    (fmt "file:///~{~a/~}~a~@[.~a~]"
         (rest (pathname-directory file))
         (pathname-name file)
         (pathname-type file))))

(defun remove-homedir (file)
  (if (pathnamep file)
      (let ((file (namestring file))
            (homedir (namestring (user-homedir-pathname))))
        (if (string^= homedir file)
            (string+ "~/" (subseq file (length homedir)))
            file))
      file))

(defmethod render-warning ((formatter html-report) (warning warning-info) &optional (*html* *html*))
  (let* ((file (warning-info-source-file warning))
         (type (warning-info-condition-type warning))
         (string (warning-info-string warning)))
    (with-html
      (:li.list-group-item
       :class (severity-class (warning-info-condition-severity warning))
       (:small (:code
                 (ematch type
                   ((delayed-symbol (or "CL" "COMMON-LISP")
                                    name)
                    name)
                   ((delayed-symbol "KEYWORD" name)
                    (fmt ":~a" name))
                   ((delayed-symbol package name)
                    (fmt "~a:~a" package name)))))
       (:a :href (if file (pathname-file-url file) "#")
         :title (and file (fmt "In ~a" (remove-homedir file)))
         (:pre (:code string)))))))

(defun report-html (report &optional (stream *standard-output*))
  (render-text (make 'html-report :report report) report stream))

(defun report-html-file (report)
  (uiop:with-temporary-file (:stream s
                             :direction :output
                             :keep t
                             :pathname p)
    (report-html report s)
    (pathname-file-url p)))
