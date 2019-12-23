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

(defun source-system-names (source)
  (mapcar 'pathname-name (source-system-files source)))

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

(defgeneric source-link (source))

(defmethod source-link ((source upstream-source))
  (let* ((primary (primary-system-file source))
         (system-info (project-info:system-file-info primary))
         (location (location source)))
    (or (getf system-info :homepage)
        (project-info:guess-website-by-location-pattern location))))

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

(defun reconstitute (string patterns)
  (let ((result (maybe-reconstitute string patterns)))
    (and (not (eq result string))
         result)))

;;; Linking to VCS sources from log lines

(defun parse-vcs-source-log-line (log-line)
  "Return a plist of info about log-line."
  (when (and (search "dist/build-cache/" log-line)
             (not (search ".cache/common-lisp" log-line)))
    (ppcre:register-groups-bind (project-name full-path)
        ("dist/build-cache/(.*?)/(.*$)" log-line)
      (let* ((pos 0)
             (second-slash
              (dotimes (i 2 pos)
                (setf pos (position #\/ full-path :start (1+ pos)))))
             (end (and second-slash (position #\& full-path :start second-slash))))
        (let ((source (find-source project-name)))
          (when (and source second-slash)
            (let* ((path (subseq full-path (1+ second-slash) end))
                   (start (search path log-line))
                   (line-number nil))
              (ppcre:register-groups-bind ((#'parse-integer log-line-number)) ("Line: (\\d+)" log-line)
                (setf line-number log-line-number))
              (list :source source
                    :path path
                    :line-number line-number
                    :path-bounds (cons start (+ start (length path)))))))))))

(defparameter *location-base-substitutions*
  '(("(https://github.com/.*?/.*?)\\.git" 0 "/blob/")
    ("(https://.*gitlab.*)\\.git$" 0 "/blob/")
    ("(https://bitbucket.org/.*?)\\.git$" 0 "/src/")
    ("(http://dwim.hu/live/.*$)" 0)))

(defun location-base (location)
  (reconstitute location *location-base-substitutions*))

(defun source-branch (source)
  (typecase source
    (tagged-mixin
     (tag-data source))
    (git-source
     "master")
    (t
     nil)))

(defun source-file-link-base (source)
  (let ((base (location-base (location source))))
    (when base
      (format nil "~A~@[~A/~]" base (source-branch source)))))

(defun source-file-link (source path line-number)
  (let ((base (source-file-link-base source)))
    (when base
      (format nil "~A~A~@[#L~A~]" base path line-number))))

(defun link-subseq (line link bounds)
  (destructuring-bind (start . end)
      bounds
    (concatenate 'string
                 (subseq line 0 start)
                 "<a href='"
                 link
                 "'>"
                 (subseq line start end)
                 "</a>"
                 (subseq line end))))


;;; Posting to S3

(defun report-publishing-enabled-p ()
  (not (not (probe-file *failtail-credentials-file*))))

(defun content-type (file)
  (cond ((equalp (pathname-type file) "css")
         "text/css")
        ((equalp (pathname-type file) "rss")
         "application/rss+xml")
        (t
         "text/html")))

(defun upload-report-file (file key)
  (let ((zs3:*credentials* *failtail-credentials*))
    (zs3:put-file file *failtail-bucket* key
                  :public t
                  :content-type (content-type file))))

(defun upload-report (base prefix)
  (let* ((*default-pathname-defaults* (truename base))
         (files (append (directory "**/*.html")
                        (directory "**/*.css"))))
    (with-simple-restart (give-up "Stop uploading report")
      (dolist (file files)
        retry
        (let ((key (format nil "~A~A"
                           prefix
                           (enough-namestring file))))
          (with-simple-restart (try-again "Try uploading ~A again"
                                          (file-namestring file))
            (upload-report-file file key)))))))


(defgeneric failure-data (object))
(defgeneric failure-log-file (object))
(defgeneric system-name (object))
(defgeneric system-file-name (object))
(defgeneric failure-report-url (object))
(defgeneric full-failure-report-url (object)
  (:method (object)
    (format nil "http://~A/~A~A"
            *failtail-bucket*
            (report-prefix)
            (failure-report-url object))))
(defgeneric failure-report-html-file (base object))

(defgeneric stylesheet-path (object))
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
         (existing-system
          (ql-dist:find-system-in-dist (system-name object) dist)))
    (or (not (not existing-system))
        (< (days-old (source object)) 30))))

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
    :accessor failure-data)
   (source
    :initarg :source
    :reader source)
   (stylesheet-path
    :reader stylesheet-path
    :initform "../failure-report.css")))

(defmethod print-object ((object failing-source) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~A (~D failing system~:P)"
            (name (source object))
            (length (failure-data object)))))

(defmethod source-link ((source failing-source))
  (source-link (source source)))

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
    :accessor failure-data)
   (stylesheet-path
    :initform "failure-report.css"
    :reader stylesheet-path)))

(defmethod print-object ((object failure-report) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "with ~A source failure~:P"
            (length (failure-data object)))))

(defmethod failure-report-url ((object failure-report))
  (format nil "failure-report.html"))

(defmethod name ((object failure-report))
  "Failure report")

(defun parse-failure-file-name (failure-file)
  "Parse FAILURE-FILE's namestring into a FAILING-SYSTEM object; if no
source is found that matches the filename, return nil."
  ;; Syntax is:
  ;;   fail_<project>_<system-file-name>_<failing-system-escaped-name>.txt
  (ppcre:register-groups-bind (source-name system-file-name failing-system)
      ("^fail_(.*?)_(.*?)_(.*?)\\.txt"
       (file-namestring failure-file))
    ;; It's possible that the logfile belongs to a source that is no
    ;; longer part of Quicklisp (due to renaming, removal, or whatever
    ;; reason), so don't try to make a failing-source in that case.
    (let ((source (find-source source-name)))
      (when source
        (make-instance 'failing-system
                       :source (find-source source-name)
                       :failure-log-file failure-file
                       :system-file-name system-file-name
                       :system-name (decode-string-from-filesystem
                                     failing-system))))))

(defun failing-source-log-files ()
  (let* ((base (translate-logical-pathname "quicklisp-controller:dist;build-artifacts;"))
         (fail-wild (merge-pathnames "**/fail_*_*_*.txt" base)))
    (directory fail-wild)))

(defun failing-systems ()
  (remove nil
          (mapcar #'parse-failure-file-name
                  (failing-source-log-files))))

(defun failure-log-failure-report ()
  "Scan the failure log files of all projects to produce a failure report."
  (let ((systems (make-hash-table :test 'equal)))
    (flet ((fsource (source)
             (or (gethash (name source) systems)
                 (setf (gethash (name source) systems)
                       (make-instance 'failing-source
                                      :failure-data nil
                                      :source source)))))
      (let ((table (make-hash-table :test 'eq))
            (systems (failing-systems))
            (report (make-instance 'failure-report
                                   :failure-data '())))
        (dolist (system systems)
          (let ((key (fsource (source system))))
            (push system (gethash key table))))
        (maphash (lambda (failing-source failing-systems)
                   (setf (failure-data failing-source) failing-systems)
                   (push failing-source (failure-data report)))
                 table)
        report))))

(defmethod failure-data ((object (eql t)))
  (failure-log-failure-report))

(defparameter *log-lines-that-are-boring*
  (mapcar 'ppcre:create-scanner
          '("^WARNING:")))

(defparameter *log-lines-to-highlight*
  (mapcar 'ppcre:create-scanner
          '("^; caught (WARNING|ERROR):"
            " READ error during"
            "^Backtrace for"
            "^Unhandled")))

(defparameter *failure-log-reconstitution-patterns*
  '(("(^.*The ANSI Standard, Section )([0-9.]*)"
     0 "<a href='http://l1sp.org/cl/" 1 "'>" 1 "</a>")))

(defun failure-log-reconstitute-line (line)
  (maybe-reconstitute line *failure-log-reconstitution-patterns*))

(defun highlighted-log-line-p (line)
  (loop for scanner in *log-lines-to-highlight*
     thereis (ppcre:scan scanner line)))

(defun boring-log-line-p (line)
  (loop for scanner in *log-lines-that-are-boring*
       thereis (ppcre:scan scanner line)))


(defmethod write-html-failure-report-header (object stream)
  (format stream "<html><head><title>~A</title>~
                  <link rel=stylesheet href='~A'></head><body>~%"
          (name object)
          (stylesheet-path object))
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
         (setf line (failure-log-reconstitute-line (cl-who:escape-string line)))
         (let ((upstream-info (parse-vcs-source-log-line line)))
           (when upstream-info
             (destructuring-bind (&key source path path-bounds line-number &allow-other-keys)
                    upstream-info
                  (let ((link (source-file-link source path line-number)))
                    (when link
                      (setf line (link-subseq line link path-bounds)))))))
         (cond ((highlighted-log-line-p line)
                (write-string "<strong>" stream)
                (write-string line stream)
                (write-string "</strong>" stream)
                (terpri stream))
               ((boring-log-line-p line)
                (format stream "<span class='boring'>~A~%</span>" line))
               (t
                (write-line line stream))))
  (format stream "</pre>")
  (format stream "</div>~%")))

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
               (let ((link (source-link source)))
                 (format stream "<li~@[ ~*class='new-failure'~]> ~A:<br>"
                         (new-failure-p source)
                         (name source))
                 (if link
                     (format stream "<a class='source-link' href='~A'>~A</a>" link link)
                     (format stream "<span class='source-location'>~A</span>" (location (source source))))
                 (format stream "</li>~%")
                 (format stream "<ul>")
                 (dolist (system (failure-data source))
                   (format stream "<li~@[ ~*class='new-failure'~]> <a href='~A'>~A</a></li>~%"
                           (new-failure-p system)
                           (failure-report-url system)
                           (system-name system))))
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
  (copy (relative-to-system "failure-report.css") base)
  (dolist (source (failure-data failure-report))
    (let ((output (failure-report-html-file base source)))
      (ensure-directories-exist output)
      (write-html-failure-report source output)))
  (write-html-failure-report failure-report
                             (failure-report-html-file base failure-report)))

(defun publish-source-failure (source)
  (setf source (source-designator source))
  (let ((failing-source (find-failing-source source)))
    (in-anonymous-directory
      (let ((output (failure-report-html-file *default-pathname-defaults*
                                              failing-source)))
        (ensure-directories-exist output)
        (write-html-failure-report failing-source output)
        (let ((key (concatenate 'string (report-prefix)
                                (enough-namestring output))))
          (upload-report-file (relative-to-system "failure-report.css")
                              (format nil "~Afailure-report.css"
                                      (report-prefix)))
          (upload-report-file output key)
          (format nil "http://~A/~A" *failtail-bucket* key))))))

(defun report-prefix (&optional differentiator)
  "Generate a report prefix based on the current date and time."
  (multiple-value-bind (second minute hour day month year)
      (get-decoded-time)
    (declare (ignore second minute hour))
    (format nil "~4,'0D-~2,'0D-~2,'0D~@[-~A~]/"
            year month day
            differentiator)))

(defun failure-report-index-urls ()
  (flet ((indexp (name)
           (and (search "failure-report.html" name)
                (= 1 (count #\/ name)))))
    (mapcar
     (lambda (suffix)
       (format nil "http://~A/~A" *failtail-bucket* suffix))
     (sort
      (map 'list #'zs3:name
           (remove-if-not #'indexp
                          (zs3:all-keys *failtail-bucket*
                                        :credentials *failtail-credentials*)
                          :key #'zs3:name))
      #'string<))))

(defun write-failure-report-index (stream)
  (format stream "<html><head><title>Failure Reports</title></head><body><ul>")
  (dolist (url (last (failure-report-index-urls) 20))
    (format stream "<li><a href='~A'>~a</a></li>" url url))
  (format stream "</ul></body>"))

(defun publish-failure-report-index (output-file)
  (with-open-file (stream output-file :direction :output)
    (write-failure-report-index stream))
  (upload-report-file output-file "index.html"))

(defun publish-failure-report (&key report-prefix
                                 report-prefix-keyword
                                 failure-report)
  "Upload FAILURE-REPORT to the report S3 bucket. If FAILURE-REPORT is
  NIL, a fresh failure report is generated."
  (unless failure-report
    (setf failure-report (failure-data t)))
  (unless report-prefix
    (setf report-prefix (report-prefix report-prefix-keyword)))
  (in-anonymous-directory
    (write-report failure-report *default-pathname-defaults*)
    (upload-report *default-pathname-defaults* report-prefix)
    (publish-failure-report-index "index.html"))
  (format nil "http://~A/~Afailure-report.html"
          *failtail-bucket*
          report-prefix))
