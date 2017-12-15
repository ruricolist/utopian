;;;; utopian.lisp

(defpackage :utopian/report
  (:use :cl :alexandria :serapeum :trivia :spinneret
    :utopian
    :utopian/collect)
  (:shadowing-import-from :serapeum :@)
  (:export :report-html-file))

(in-package :utopian/report)

;;; "utopian" goes here. Hacks and glory await!

(eval-always
  (defvar *ids* 0)

  (defun genid (string)
    "Generate a unique ID for embedding in HTML."
    (fmt "~a~a" string (finc *ids*))))

;; Pretty colors from http://clrs.cc/.
(def blue "#0074D9")
(def red "#FF4136")
(def yellow "#FFDC00")
(def orange "#FF851B")
(def black "#111111")

(def normalize.css
  (read-file-into-string
   (asdf:system-relative-pathname :utopian #p"normalize.css")))

(def custom-css
  (lass:compile-and-write
   '("*" :box-sizing "border-box")
   '("body"
     :margin "4em auto"
     :max-width "65em"
     :line-height "1.6"
     :font-size "18px"
     :color "222"
     :padding 0
     :font-family "sans-serif"
     :font-feature-settings "tnum")
   '("pre"
     :line-height "1.2")
   '("h1, h2, h3, h4, h5, h6"
     :line-height "1.2")
   '("ul"
     :list-style-type "none"
     :padding-left "0")
   '("ul ul"
     :padding-left "1em")
   '(".warning"
     :margin "1em auto")
   '(".warning a"
     :text-decoration "none"
     :color "inherit")
   '(".sc" :font-feature-settings "smcp")
   '(".pathname" :font-family "mono")
   `(".severity-warning figcaption" :color ,red)
   `(".severity-style-warning figcaption" :color ,orange)
   `(".severity-info figcaption" :color ,blue)))

(def html-report-css
  (fmt "~a~%~a"
       normalize.css
       custom-css))

(defun ignore-types (warnings types)
  (remove-if (lambda (w)
               (let ((sym
                       (delayed-symbol->symbol
                        (warning-class w))))
                 ;; TODO For development.
                 (or (subtypep sym 'uninteresting-warning)
                     (some (op (subtypep sym _))
                           types))))
             warnings))

(defun system-base (system)
  (asdf:system-relative-pathname system ""))

(defun system-fasl-base (system)
  (asdf:apply-output-translations (system-base system)))

(defun ignore-systems (warnings systems)
  (if (null systems) warnings
      (let ((system-bases
              (append (mapcar #'system-base systems)
                      (mapcar #'system-fasl-base systems))))
        (remove-if (lambda (w)
                     (when-let (file (warning-source-file w))
                       (some (lambda (base)
                               (or (uiop:subpathp file base)
                                   ;; TODO be more precise
                                   (and (uiop:subpathp file (uiop:xdg-cache-home))
                                        (string*= (namestring base)
                                                  (namestring file)))))
                             system-bases)))
                   warnings))))

(defun include-systems (warnings systems)
  (if (null systems) warnings
      (let ((system-bases
              (append (mapcar #'system-base systems)
                      (mapcar #'system-fasl-base systems))))
        (filter (lambda (w)
                  (let ((file (warning-source-file w)))
                    (or (null file)
                        (some (lambda (base)
                                (or (uiop:subpathp file base)
                                    ;; TODO be more precise
                                    (and (uiop:subpathp file (uiop:xdg-cache-home))
                                         (string*= (namestring base)
                                                   (namestring file)))))
                              system-bases))))
                warnings))))

(defun quicklisp-installed-systems ()
  (~>> "quicklisp"
       ql-dist:find-dist
       ql-dist:installed-releases
       (mappend #'ql-dist:provided-systems)
       (mapcar #'ql-dist:name)))

(defun find-system-or-dont (system)
  (ignore-errors (asdf:find-system system nil)))

(defun ignore-quicklisp-systems (warnings)
  (let* ((base
           (~> "quicklisp"
               ql-dist:find-dist
               ql-dist:base-directory))
         (cache-base
           (path-join uiop:*user-cache*
                      (uiop:make-pathname*
                       :directory
                       (cons :relative
                             (drop-while #'keywordp
                                         (pathname-directory base)))))))
    (remove-if (lambda (w)
                 (let ((file
                         (warning-source-file w)))
                   (unless (null file)
                     (or (uiop:subpathp file base)
                         (uiop:subpathp file cache-base)))))
               warnings)))

(defgeneric report-html (report &key &allow-other-keys))

(defmethod report-html ((report string) &rest args &key)
  (apply #'report-html (system-report report) args))

(defmethod report-html ((report symbol) &rest args &key)
  (apply #'report-html (system-report report) args))

(defmethod report-html ((report list)
                        &key ((:stream *html*) *html*) ignore-types
                             ignore-systems include-systems
                             (ignore-quicklisp-systems t)
                        &aux (*print-pretty* t))
  (nest
   (with-html)
   (destructuring-bind (&key system-name warnings &allow-other-keys) report)
   (let* ((warnings
            (~> warnings
                (ignore-types ignore-types)
                (ignore-systems ignore-systems)
                (include-systems include-systems)))
          (warnings
            (if ignore-quicklisp-systems
                (ignore-quicklisp-systems warnings)
                warnings))))
   (local
     (def by-source-file (assort warnings :test #'equal :key #'warning-source-file))
     (def by-source-file (sort-by-severity by-source-file))

     (def by-severity (assort warnings :key #'warning-severity))
     (def by-severity (dsu-sort (copy-seq by-severity) #'> :key (op (warning-severity-level (first _)))))

     (def by-source-file-id "by-source-file")
     (def by-severity-id "by-severity")
     (def by-source-file-hash (fmt "#~a" by-source-file-id))
     (def by-severity-hash (fmt "#~a" by-severity-id))

     (defun sort-by-severity (notes)
       "First all the files with warnings, then all the files with no
warnings but with style warnings, then all the files with notes, but
no warnings or style warnings."
       (dsu-sort (copy-seq notes)
                 (lambda (xs ys)
                   (nlet rec ((xs xs)
                              (ys ys))
                     (if (or (endp xs) (endp ys)) nil
                         (or (> (first xs) (first ys))
                             (and (= (first xs) (first ys))
                                  (rec (rest xs) (rest ys)))))))
                 :key (op (multiple-value-list (count-severities _)))))

     (defun count-severities (notes)
       (let ((warning-count 0)
             (style-warning-count 0)
             (note-count 0))
         (dolist (note notes)
           (ecase-of severity (warning-severity note)
             (:warning (incf warning-count))
             (:style-warning (incf style-warning-count))
             (:note (incf note-count))))
         (values warning-count style-warning-count note-count)))

     (defun quantify-notes (notes)
       (multiple-value-bind (warning-count style-warning-count note-count)
           (count-severities notes)
         ;; TODO Do better.
         (with-output-to-string (s)
           (when (plusp warning-count)
             (format s "~a warning~:p" warning-count))

           (when (plusp style-warning-count)
             (when (plusp warning-count)
               ;; There are warnings.
               (if (plusp note-count)
                   ;; There are also notes, so this is item 2 of 3.
                   (format s ", ")
                   (format s " and ")))
             (format s "~a style warning~:p" style-warning-count))

           (when (plusp note-count)
             (when (plusp (min style-warning-count warning-count))
               ;; Both nonzero.
               (format s ","))
             (when (plusp (logior style-warning-count warning-count))
               ;; Either one nonzero.
               (format s " and "))
             (format s "~a note~:p" note-count)))))

     (:doctype)
     (:html
       (:head
         (:meta :name "viewport" :content "width=device-width, initial-scale=1, shrink-to-fit=no")
         (:title ("Report for system ~s" system-name))
         (:style (:raw html-report-css)))

       (:body
         (:div :id "environment"
           (report-environment report))

         (:div

           (when (or by-source-file by-severity)
             (:ul
               (:li (:a :href by-source-file-hash "By file"))
               (:li (:a :href by-severity-hash "By severity"))))

           (:p
             (when warnings
               (fmt "There are ~a in ~a file~:p"
                    (quantify-notes warnings)
                    (length by-source-file))))

           (when by-source-file
             (:h1 "By file")
             (:ul :id by-source-file-id
               (do-each (file-warnings by-source-file)
                 (let* ((file (warning-source-file (first file-warnings))))
                   (:details
                     (:summary
                       (quantify-notes file-warnings)
                       " in "
                       (if file
                           ("file ~a"
                            (remove-homedir file))
                           "no file")
                       (:span.pathname file))
                     (:ul.list-group
                      (dolist (warning file-warnings)
                        (render-warning warning))))))))

           (when by-severity
             (:h1 "By severity")
             (:ul :id by-severity-id
               (do-each (warnings by-severity)
                 (let ((severity (warning-severity (first warnings))))
                   (:details
                     (:summary (fmt "~a ~a~:*~:p"
                                    (length warnings)
                                    (severity-title severity)))
                     (:ul.list-group
                      (dolist (warning warnings)
                        (render-warning warning))))))))))))))

(defun severity-title (sev)
  (ecase-of severity sev
    (:note "note")
    (:warning "warning")
    (:style-warning "style warning")))

(defun severity-class (sev)
  (ecase-of severity sev
    (:note "severity-note")
    (:warning "severity-warning")
    (:style-warning "severity-style-warning")))

(defun pathname-file-url (file)
  (let ((file (truename file)))
    (fmt "file:///~@[~a:/~]~{~a/~}~a~@[.~a~]"
         (and (uiop:os-windows-p)
              (pathname-device file))
         (mapcar #'quri:url-encode
                 (drop-while #'keywordp
                             (pathname-directory file)))
         (quri:url-encode (pathname-name file))
         (quri:url-encode (pathname-type file)))))

(defun remove-homedir (file)
  (if (pathnamep file)
      (let ((file (namestring file))
            (homedir (namestring (user-homedir-pathname))))
        (if (string^= homedir file)
            (string+ "~/" (subseq file (length homedir)))
            file))
      file))

(defun render-warning (warning &optional (*html* *html*))
  (check-type warning warning-info)
  (nest
   (with-html)
   (local
     (def file (warning-source-file warning))
     (def class (warning-class warning))
     (def string (trim-whitespace (warning-string warning)))
     (def severity (warning-severity warning))

     (defun show-string ()
       (:pre (:code string)))

     (defun maybe-link ()
       (if file
           (let ((url (pathname-file-url file)))
             (:a.file-link
              :href url
              :title (fmt "In ~a" (remove-homedir file))
              (show-string)))
           (show-string)))

     (defun delayed-symbol-string (class)
       (let ((package (delayed-symbol.package class))
             (name (delayed-symbol.name class)))
         (cond ((member package '("CL" "COMMON-LISP") :test #'equal)
                name)
               ((equal package "KEYWORD")
                (fmt ":~a" name))
               (t (fmt "~a:~a" package name)))))

     (defun teaser (string)
       (~> string
           collapse-whitespace
           (ellipsize 60)))

     (:li.warning
      :class (severity-class severity)
      (:figure
        (if (> (length string) 80)
            (:details (:summary (:code (teaser string)))
              (maybe-link))
            (maybe-link))
        (:figcaption
          (:small (:code (delayed-symbol-string class)))))))))

(defun report-environment (report)
  (with-html
    (flet ((th (string)
             (:th :scope "row" :style "text-align: left"
               string)))
      (destructuring-bind (&key lisp-env os-env &allow-other-keys)
          report
        (when (or lisp-env os-env)
          (:table
            (:caption "Environment")
            (doplist (k v lisp-env)
              (when v
                (unless (equal v "unspecified")
                  (:tr
                    (th (fmt "~@(~a~)" (substitute #\Space #\- (string k))))
                    (:td v)))))
            (doplist (k v os-env)
              (when v
                (:tr (th (fmt "$~:@(~a~)" k))
                  (:td :style "font-family: monospace"
                    v))))))))))

(defun utopian:report-html-file (report &rest args &key &allow-other-keys)
  (uiop:with-temporary-file (:stream s
                             :direction :output
                             :keep t
                             :pathname p)
    (apply #'report-html report :stream s args)
    (pathname-file-url p)))
