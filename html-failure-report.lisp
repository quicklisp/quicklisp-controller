;;;; html-failure-report.lisp

(in-package #:quicklisp-controller)

;;; Misc utils

(defun versions-and-such ()
  (format nil "~A / ASDF ~A"
          (run-output-line "sbcl" :version)
          (run-output-line "depcheck" :asdf-version)))

(defun test-system-p (system-name)
  (ppcre:scan "[.-]test" system-name))

(defun find-system-file (system-name)
  (gethash system-name (ensure-system-file-index)))

(defun source-system-files (source)
  (with-system-index
    (let ((result '()))
      (map-source-systems
       source
       (lambda (system-file-name system)
         (declare (ignore system))
         (push (find-system-file system-file-name) result)))
      result)))

(defun primary-system-file (source)
  (block nil
    (let ((files (source-system-files source)))
      (unless (rest files)
        (return (first files)))
      (let ((exact-match (find (name source)
                               files
                               :test 'string=
                               :key 'pathname-name)))
        (when exact-match
          (return exact-match)))
      (setf files (sort files #'<
                        :key (lambda (pathname)
                               (length (namestring pathname)))))
      (first files))))

(defun source-link (source)
  (let* ((primary (primary-system-file source))
         (system-info (project-info::system-file-info primary))
         (location (location source)))
    (or (getf system-info :homepage)
        (project-info::guess-website-by-location-pattern location))))

(defun substitute-if-matches (regex target substitution)
  (multiple-value-bind (start end anchor-starts anchor-ends)
      (ppcre:scan regex target)
    (labels ((anchor-substring (index)
               (subseq target
                       (aref anchor-starts index)
                       (aref anchor-ends index)))
             (maybe-substitute (object)
               (etypecase object
                 (string object)
                 (integer (anchor-substring object)))))
      (when (and start end)
        (format nil "~{~A~}" (mapcar #'maybe-substitute substitution))))))

(defun maybe-reconstitute (string patterns)
  "For each pattern in PATTERNS, check for a match against STRING. If
the pattern matches, its substitution data is interpolated and
returned. The first match 'wins'. A pattern should be a regular
expression; a substitution should be one or more strings or
integers. The integers in the substitution are substituted for the
corresponding register match in the regex. If there are no matches,
the string is returned unchanged."
  (dolist (pattern patterns string)
    (let ((substituted (substitute-if-matches (first pattern)
                                              string
                                              (rest pattern))))
      (when substituted
        (return substituted)))))

;;; Posting to S3

(defun upload-report-file (file key)
  (let ((zs3:*credentials* *failtail-credentials*))
    (zs3:put-file file *failtail-bucket* key
                  :public t
                  :content-type "text/html")))

(defun upload-report (base prefix)
  (let* ((*default-pathname-defaults* (truename base))
         (files (append (directory "**/*.html")
                        (directory "**/*.css"))))
    (dolist (file files)
      (let ((key (format nil "~A~A"
                         prefix
                         (enough-namestring file))))
        (upload-report-file file key)))))


(defgeneric failure-data (object))
(defgeneric failure-log-file (object))
(defgeneric system-name (object))
(defgeneric system-file-name (object))
(defgeneric failure-report-url (object))
(defgeneric failure-report-html-file (base object))

(defgeneric write-html-failure-report (object file))
(defgeneric write-html-failure-report-header (object stream))
(defgeneric write-html-failure-report-index (object stream))
(defgeneric write-html-failure-report-content (object stream))
(defgeneric write-html-failure-report-footer (object stream))

(defmethod failure-report-html-file (base object)
  (relative-to base (failure-report-url object)))

(defgeneric new-failure-p (object))

(defclass failing-system ()
  ((system-name
    :initarg :system-name
    :reader system-name)
   (system-file-name
    :initarg :system-file-name
    :reader system-file-name)
   (source
    :initarg :source
    :reader source)
   (failure-log-file
    :initarg :failure-log-file
    :reader failure-log-file)))

(defmethod print-object ((object failing-system) stream)
  (print-unreadable-object (object stream :type t)
    (write-string (system-name object) stream)))

(defmethod new-failure-p ((object failing-system))
  (let* ((dist (ql-dist:find-dist "quicklisp"))
         (system (ql-dist:find-system-in-dist (system-name object) dist)))
    (not (not system))))

(defmethod failure-data ((source upstream-source))
  (let ((result '()))
    (map-source-systems
     source
     (lambda (system-file-name system-name)
       (write-char #\. *trace-output*)
       (force-output *trace-output*)
       (let ((file (winfail-file "fail" source system-file-name system-name)))
         (when (probe-file file)
           (push (make-instance 'failing-system
                                :system-name system-name
                                :system-file-name
                                system-file-name
                                :source source
                                :failure-log-file file)
                 result)))))
    result))

(defmethod name ((object failing-system))
  (name (source object)))

(defmethod failure-report-url ((object failing-system))
   (format nil "failure-report/~A.html#~A"
           (name (source object))
           (encode-string-for-filesystem (system-name object))))


(defclass failing-source ()
  ((failure-data
    :initarg :failure-data
    :reader failure-data)
   (source
    :initarg :source
    :reader source)))

(defmethod print-object ((object failing-source) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~A (~D failing system~:P)"
            (name (source object))
            (length (failure-data object)))))

(defmethod new-failure-p ((object failing-source))
  (some #'new-failure-p (failure-data object)))

(defmethod failure-report-url ((object failing-source))
  (format nil "failure-report/~A.html"
          (encode-string-for-filesystem (name object))))

(defmethod name ((object failing-source))
  (name (source object)))

(defun find-failing-source (source)
  (let ((failing-systems (failure-data source)))
    (when failing-systems
      (make-instance 'failing-source
                     :failure-data failing-systems
                     :source source))))

(defclass failure-report ()
  ((failure-data
    :initarg :failure-data
    :reader failure-data)))

(defmethod print-object ((object failure-report) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "with ~A source failure~:P"
            (length (failure-data object)))))

(defmethod failure-report-url ((object failure-report))
  (format nil "failure-report.html"))

(defmethod name ((object failure-report))
  "Failure report")

(defmethod failure-data ((object (eql t)))
  (let ((sources
         (mapcan (lambda (source)
                   (let ((failing-source
                          (find-failing-source source)))
                     (when failing-source
                       (list failing-source))))
                 (all-of-type t))))
    (make-instance 'failure-report
                   :failure-data (sort sources
                                       #'string<
                                       :key #'name))))

(defparameter *log-lines-to-highlight*
  (mapcar 'ppcre:create-scanner
          '("^; caught (WARNING|ERROR):"
            " READ error during"
            "^Unhandled")))

(defparameter *failure-log-reconstitution-patterns*
  '(("(The ANSI Standard, Section )([0-9.]*)"
     0 "<a href='http://l1sp.org/cl/" 1 "'>" 1 "</a>")))

(defun failure-log-reconstitute-line (line)
  (maybe-reconstitute line *failure-log-reconstitution-patterns*))

(defun highlighted-log-line-p (line)
  (loop for scanner in *log-lines-to-highlight*
        thereis (ppcre:scan scanner line)))

(defun highlight-log-lines (input-stream output-stream)
  (loop for line = (read-line input-stream nil)
        while line
        do
        (setf line (cl-who:escape-string line))
        (setf line (failure-log-reconstitute-line line))
        (if (highlighted-log-line-p line)
            (progn
              (write-string "<strong>" output-stream)
              (write-string line output-stream)
              (write-string "</strong>" output-stream))
            (write-string line output-stream))
        (terpri output-stream)))

(defmethod write-html-failure-report-header (object stream)
  (format stream "<html><head><title>~A</title>~
                  <link rel=stylesheet href='style.css'></head><body>~%"
          (name object))
  (format stream "<h1>~A</h1>~%"
          (name object)))

(defmethod write-html-failure-report-footer (object stream)
  (format stream "</body></html>~%"))

(defmethod write-html-failure-report-index (object stream))

(defmethod write-html-failure-report-content (object stream))

(defmethod write-html-failure-report (object file)
  (with-open-file (stream file
                          :direction :output
                          :if-exists :supersede)
    (write-html-failure-report-header object stream)
    (write-html-failure-report-index object stream)
    (write-html-failure-report-content object stream)
    (write-html-failure-report-footer object stream)))

(defmethod write-html-failure-report-content ((system failing-system) stream)
  (format stream "<div class='failing-system'>")
  (format stream "<a name='~A'></a>"
          (encode-string-for-filesystem (system-name system)))
  (format stream "<h3>~A</h3>" (system-name system))
  (format stream "<pre>")
  (with-open-file (log-stream (failure-log-file system))
    (loop for line = (read-line log-stream nil)
        while line
        do
        (setf line (cl-who:escape-string line))
        (if (highlighted-log-line-p line)
            (progn
              (write-string "<strong>" stream)
              (write-string line stream)
              (write-string "</strong>" stream))
            (write-string line stream))
        (terpri stream)))
  (format stream "</pre>")
  (format stream "</div>~%"))

(defmethod write-html-failure-report-content ((source failing-source) stream)
  (dolist (system (failure-data source))
    (write-html-failure-report-content system stream)))

(defmethod write-html-failure-report-index ((source failing-source) stream)
  (format stream "<ul>")
  (dolist (system (failure-data source))
    (format stream "<li><a href='#~A'>~A</a>~@[ <i>new failure</i>~]</li>"
            (encode-string-for-filesystem (system-name system))
            (system-name system)
            (new-failure-p system)))
  (format stream "</ul>~%"))

(defmethod write-html-failure-report-index ((object failure-report) stream)
  (format stream "<ul>")
  (let* ((sources (failure-data object))
         (new (remove-if-not #'new-failure-p sources))
         (old (remove-if #'new-failure-p sources)))
    (flet ((show (sources)
             (dolist (source sources)
               (format stream "<li~@[ ~*class='new-failure'~]> ~A:</li>~%"
                       (new-failure-p source)
                       (name source))
               (format stream "<ul>")
               (dolist (system (failure-data source))
                 (format stream "<li~@[ ~*class='new-failure'~]> <a href='~A'>~A</a></li>~%"
                         (new-failure-p system)
                         (failure-report-url system)
                         (system-name system)))
               (format stream "</ul>~%"))))
      (show new)
      (format stream "<br><br>")
      (show old)))
  (format stream "</ul>"))

(defmethod write-html-failure-report-header :after ((object failing-source)
                                                    stream)
  (format stream "<ul><li> source: ~A" (location (source object)))
  (let ((link (source-link (source object))))
    (when link
      (format stream "<li> site: <a href='~A'>~A</a>~%" link link)))
  (format stream "</ul>~%")
  (format stream "<p>~A~%" (versions-and-such)))

(defmethod write-html-failure-report-header :after ((object failure-report)
                                                    stream)
  (format stream "<p>~A~%" (versions-and-such)))

(defun write-report (failure-report base)
  (dolist (source (failure-data failure-report))
    (let ((output (failure-report-html-file base source)))
      (ensure-directories-exist output)
      (write-html-failure-report source output)))
  (write-html-failure-report failure-report (failure-report-html-file base failure-report)))

