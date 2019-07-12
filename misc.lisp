;;;; misc.lisp

(in-package #:quicklisp-controller)

(defun clear-fasl-cache ()
  (run "rm" "-rf"
       (translate-logical-pathname "quicklisp-controller:dist;fasls;")))

(defun system-from-release (system-name dist)
  (let* ((dist (ql-dist:dist dist))
         (system (ql-dist:find-system-in-dist system-name dist))
         (release (ql-dist:release system))
         (release-name (ql-dist:name release)))
    (if (equal system-name release-name)
        system-name
        (list system-name :from release-name))))

(defun =system-from-release (dist)
  (lambda (system-name)
    (system-from-release system-name dist)))

(defun write-dotfile (index-file dot-file)
  (let ((inlinks (make-string-table))
        (outlinks (make-string-table))
        (max-color 255)
        (max-size 400)
        (max-inlinks 0)
        (max-outlinks 0))
    (for-each-line (line index-file)
      (unless (ignorable-line-p line)
        (destructure-line (project system-file system &rest dependencies)
            line
          (declare (ignore project system-file))
          (setf (gethash system outlinks) dependencies)
          (dolist (dependency dependencies)
            (setf (gethash dependency inlinks)
                  (cons system (gethash dependency inlinks)))))))
    (print (list inlinks))
    (flet ((table-max (table)
             (let ((max 0))
               (maphash (lambda (k v)
                          (declare (ignore k))
                          (setf max (max (length v) max)))
                        table)
               max)))
      (setf max-inlinks (table-max inlinks)
            max-outlinks (table-max outlinks)))
    (labels ((iscale (value max1 max2)
               (format t "XXX iscale ~A ~A ~A~%" value max1 max2)
               (truncate (/ value max1) (/ max2)))
             (incount (system)
               (length (gethash system inlinks)))
             (outcount (system)
               (length (gethash system outlinks)))
             (color (system)
               (let ((red
                      (iscale (outcount system) max-outlinks max-color)))
                 (format nil "#0000~2,'0X" red)))
             (size (system)
               (+ 18 (iscale (incount system) max-inlinks max-size))))
      (with-open-file (output dot-file
                              :direction :output
                              :if-exists :supersede)
        (format output "digraph {~%")
        (maphash (lambda (system outs)
                   (format output "~S [fontsize=~D,fontcolor=~S];~%"
                           system
                           (size system)
                           (color system))
                   (dolist (outlink outs)
                     (format output "~S -> ~S;~%" system outlink)))
                 outlinks)
        (format output "}~%")))))

(defun save-bucket (bucket output-directory)
  (let ((fetched 0))
    (values
     (map 'list
          (lambda (key)
            (setf key (zs3:name key))
            (let* ((output-file (merge-pathnames key
                                                 output-directory))
                   (write-date (or (ignore-errors (file-write-date output-file))
                                   0)))
              (ensure-directories-exist output-file)
              (when (zs3:get-file bucket key output-file
                                  :when-modified-since write-date)
                (incf fetched))
              (probe-file output-file)))
          (zs3:all-keys bucket))
     fetched)))


(defclass sysindex ()
  ((needs
    :initarg :needs
    :accessor needs
    :initform (make-string-table)
    :documentation "A table of what each system needs.")
   (needed-by
    :initarg :needed-by
    :accessor needed-by
    :initform (make-string-table)
    :documentation "A table of what systems depend on a system.")))

(defun load-sysindex (systems-file)
  (let* ((sysindex (make-instance 'sysindex))
         (needs (needs sysindex))
         (needed-by (needed-by sysindex)))
    (for-each-line (line systems-file)
      (unless (ignorable-line-p line)
        (destructure-line (project system-file system &rest sysneeds)
            line
          (declare (ignore project system-file))
          (setf (gethash system needs) sysneeds)
          (dolist (need sysneeds)
            (setf (gethash need needed-by)
                  (cons system (gethash need needed-by)))))))
    sysindex))

(defun system-needs (name sysindex)
  (values (gethash name (needs sysindex))))

(defun system-needed-by (name sysindex)
  (values (gethash name (needed-by sysindex))))

(defun all-systems (sysindex)
  (alexandria:hash-table-keys (needs sysindex)))

(defun table-js (table)
  (with-output-to-string (stream)
    (format stream "var needs = {~%//")
    (maphash (lambda (system others)
               (format stream ",~%~S: [~{~S~^,~}]" system others))
             table)
    (format stream "~%};~%")))

(defun update-metadata (filename description)
  (dolist (source-file (directory "~/quicklisp-controller/projects/*/source.txt"))
    (let ((metafile (merge-pathnames filename source-file))
          (project-name (first (last (pathname-directory source-file)))))
      (unless (probe-file metafile)
        (format *query-io* "~&~A for ~A (hit enter to skip)~%"
                description
                project-name)
        (let ((response (read-line *query-io*)))
          (when (plusp (length response))
            (save-lines (list response) metafile)))))))

(defvar *last-source* nil)

(defun check-for-program (program)
  (let ((process (ignore-errors (run-program program nil :search t :wait t))))
    (unless process
      (error "Could not run program ~S -- not found" program))))

(defparameter *critical-programs*
  '("cvs" "git" "darcs" "hg" "svn" "depcheck" "system-file-magic"))

(defun check-critical-programs ()
  (dolist (program *critical-programs*)
    (check-for-program program)))

(defun crank (&optional (source *last-source*))
  (check-critical-programs)
  (unless (source-designator source)
    (warn "Not a known source -- ~S" source)
    (return-from crank nil))
  (setf *last-source* source)
  (update-system-file-index)
  (let ((wins (find-more-winning-systems source)))
    (list :fails (missing-components source)
          :wins wins)))

(defun source-pathname (project-name)
  (let ((directory `(:relative "quicklisp-controller"
                               "projects"
                               ,project-name)))
    (merge-pathnames (make-pathname :name "source"
                                    :type "txt"
                                    :directory directory)
                     (user-homedir-pathname))))

(defun dwimsource (name)
  (let* ((project (format nil "hu.dwim.~A" name))
         (source-file (source-pathname project)))
    (unless (probe-file source-file)
      (ensure-directories-exist source-file)
      (with-open-file (stream source-file :direction :output)
        (format stream "darcs http://dwim.hu/darcs/~A/" project)))
    (intern (string-upcase project))))


(defvar *output-lock* (bt:make-lock "output-lock"))

(defun call-with-skipping (fun &key (stream *standard-output*) parallel)
  (flet ((invoke-skip (condition)
           (when (find-restart 'skip)
             (bt:with-lock-held (*output-lock*)
               (when (boundp '*current-mapped-source*)
                 (format stream "~&* ~A~%" *current-mapped-source*)
                 (format stream ":: from ~A~%"
                         (find-source *current-mapped-source*)))
               (format stream "~&SKIPPING (~A)~%" condition))
             (invoke-restart 'skip))))
    (if (not parallel)
        (handler-bind ((error #'invoke-skip))
          (funcall fun))
        (lparallel:task-handler-bind ((error #'invoke-skip))
          (funcall fun)))))

(defun update-what-you-can (&key file parallel)
  (flet ((action (stream)
           (call-with-skipping
            (lambda ()
              (funcall (if parallel 'pmap-sources 'map-sources)
                       (lambda (source)
                         (bt:with-lock-held (*output-lock*)
                           (force-output stream)
                           (format t "~&Updating ~S from ~A~%"
                                   (project-name source)
                                   (location source)))
                         (update-source-cache source))))
            :stream stream
            :parallel parallel)))
    (if file
        (with-open-file (stream file :direction :output
                                :if-exists :rename-and-delete)
          (action (make-broadcast-stream *standard-output* stream)))
        (action *standard-output*))))

(defun ensure-what-wins-you-can ()
  (call-with-skipping
   (lambda ()
     (pmap-sources
      (lambda (source)
        (format t "~&Checking ~S~%" (project-name source))
        ;;(clear-fasl-cache)
        (ensure-winning-systems source))))))


(defun update-and-crank (&optional (source *last-source*))
  (setf *last-source* source)
  (setf source (source-designator source))
  (let* ((base (translate-logical-pathname "quicklisp-controller:dist;"))
         (build (format nil "~A/build-cache/~A"
                        (sb-ext:native-namestring base)
                        (project-name source)))
         (tar (format nil "~A/tar-cache/~A"
                      (sb-ext:native-namestring base)
                      (project-name source))))
    (update-source-cache source)
    (run "rm" "-rf" build)
    (run "rm" "-rf" tar)
    (crank source)))

(defun tail-file (file n)
  (let ((lines (make-array n :initial-element nil))
        (index 0))
    (with-open-file (stream file)
      (loop for line = (read-line stream nil)
           while line do
           (setf (aref lines index) line)
           (setf index (mod (1+ index) n))))
    (let ((start (if (aref lines index) index 0)))
      (loop for i = start then (mod (1+ i) n)
         repeat n
           for line = (aref lines i)
           when line do (write-line line)))))

(defun failtail (&optional (source *last-source*) (n 150))
  (setf *last-source* source)
  (setf source (source-designator source))
  (format t "::: ~A from ~A~%"
          (project-name source)
          (location source))
  (dolist (file (failing-system-files source))
    (format t "=================================~%")
    (format t " FAIL: ~S~%" (file-namestring file))
    (format t "=================================~%")
    (tail-file file n)))

(defvar *failtail-credentials-file*
  #p"quicklisp-controller:dist;failtail-credentials.txt")

(defvar *failtail-credentials*
  (zs3:file-credentials *failtail-credentials-file*))

(defvar *failtail-bucket*
  "report.quicklisp.org")

(defvar *failtail-prefix* "")

(defun date-string (&optional (time (get-universal-time)))
  (multiple-value-bind (second hour minute day month year)
      (decode-universal-time time nil)
    (format nil "~4,'0D-~2,'0D-~2,'0D" year month day)))

(defun failtail-string (source)
  (with-output-to-string (*standard-output*)
    (failtail source 10000)))

(defun fix-url (url)
  (ppcre:regex-replace-all "%2F" url "/"))

(defun post-failtail (source)
  (setf source (find-source source))
  (let ((key (format nil "~A~A/~A/failtail.txt"
                     *failtail-prefix*
                     (project-name source)
                     (date-string)))
        (string (failtail-string source)))
    (zs3:put-string string *failtail-bucket* key
                    :public t
                    :credentials *failtail-credentials*)
    (write-string string)
    (fix-url
     (zs3:resource-url :bucket *failtail-bucket*
                       :key key
                       :vhost :cname))))

(defun failtar (output &rest sources)
  (in-temporary-directory "failtar/"
    (ensure-directories-exist "fails/")
    (dolist (sd sources)
      (let ((source (source-designator sd)))
        (dolist (file (failing-system-files source))
          (copy file "fails/"))))
    (run "tar" "czvf" (pathname output) "fails/")))

(defparameter *project-name-guessers*
  '("/.*?/([^/]*)\\.git$"
    "github.com/.*?/(.*?)/"
    "//(.*?).googlecode.com"
    "//(.*?).git.sourceforge"
    "code.sf.net/p/([^/]+)/"
    "http://dwim.hu/.*?/([^/]+)$"
    "http://wcp\\.sdf-eu\\.org/software/(.*?)-"
    "code.google.com/p/(.*?)/"
    "bitbucket.org/.*/(.*)$"))

(defparameter *project-type-guessers*
  '((":pserver:" . "cvs")
    ("dwim\\.hu" . "darcs")
    ("github.com.*/tree/" . "branched-git")
    ("git" . "git")
    ("wcp\\.sdf-eu" . "wcpware-http")
    ("bitbucket.org" . "mercurial")
    ("svn|trunk" . "svn")
    ("https://" . "https")
    ("hg.code.sf.net" . "mercurial")
    ("(\\.tar\\.gz|\\.tgz)$" . "http")))

(defparameter *project-data-guessers*
  '("github.com/.*/tree/(.*)$"))

(defun guess-project-name (url)
  (dolist (pattern *project-name-guessers*)
    (ppcre:register-groups-bind (name) (pattern url)
      (return (string-downcase name)))))

(defun guess-project-type (url)
  (loop for (pattern . type) in *project-type-guessers*
        when (ppcre:scan pattern url)
     return type))

(defun guess-project-data (url)
  (dolist (pattern *project-data-guessers*)
    (ppcre:register-groups-bind (data) (pattern url)
      (return data))))

(defun maybe-rewrite-url (url type)
  (cond ((equal type "branched-git")
         (ppcre:regex-replace "/tree/.*$" url ".git"))
        (t
         url)))

(defun project-source-filename (project-name)
  (merge-pathnames (make-pathname :directory (list :relative project-name))
                   #p"~/src/quicklisp-projects/projects/source.txt"))

(defun add-project (url &key name type data)
  (let ((name (or name (guess-project-name url)))
        (type (or type (guess-project-type url)))
        (data (or data (guess-project-data url )))
        (*system-metadata-required-p* t))
    (setf url (maybe-rewrite-url url type))
    (tagbody
     :retry
       (unless type
         (error "Can't guess project type"))
       (unless name
         (error "Can't guess project name"))
       (when (equal name "")
         (error "Name can't be empty"))
       (let ((file (project-name-source-file name)))
         (restart-case
             (when (probe-file file)
               (error "Project already has a file"))
           (delete-and-retry (&optional v)
             (declare (ignore v))
             (delete-file file)
             (go :retry)))
         (ensure-directories-exist file)
         (with-open-file (stream file :direction :output)
           (format stream "~A ~A~@[ ~A~]~%" type url data))
         (return-from add-project (values name (update-and-crank name)))))))

(defun update-project (url &key name type)
  (let ((name (or name (guess-project-name url)))
        (type (or type (guess-project-type url))))
    (unless type
      (error "Can't guess project type"))
    (unless name
      (error "Can't guess project name"))
    (let ((file (project-source-filename name)))
      (ensure-directories-exist file)
      (with-open-file (stream file :direction :output
                              :if-exists :rename-and-delete)
        (format stream "~A ~A~%" type url))
      (update-and-crank name))))


(defun disable-source (source)
  (setf source (source-designator source))
  (rename-file (source-file source) "source-disabled.txt"))


;;; Sharing failure files to a directory

(defparameter *failure-share-directory*
  #p "~/Dropbox/Public/quicklisp/fails/")

(defun clear-shared-fails ()
  (dolist (file (directory (merge-pathnames "*.txt"
                                            *failure-share-directory*)))
    (delete-file file)))

(defun share-failure (source)
  (ensure-directories-exist *failure-share-directory*)
  (dolist (file (failing-system-files source))
    (copy file (merge-pathnames *failure-share-directory* file))))

;;; Sanity-checking a dist

;; FIXME: This should go into the client directly
(defmethod ql-dist:required-systems ((dist ql-dist:dist))
  (let ((table (make-string-table)))
    (dolist (system (ql-dist:provided-systems dist))
      (dolist (required-system (ql-dist:required-systems system))
        (push system (gethash required-system table))))
    (alexandria:hash-table-keys table)))

(defparameter *whitelisted-unprovided-systems*
  '("asdf"))

(defun unprovided-required-systems (dist)
  (let ((provided-systems (mapcar 'ql-dist:name
                                  (ql-dist:provided-systems dist)))
        (required-systems (ql-dist:required-systems dist)))
    (let ((unprovided (set-difference required-systems provided-systems
                                      :test 'equalp)))
      (set-difference unprovided *whitelisted-unprovided-systems*
                      :test 'string=))))

(defun unprovided-required-systems-report (dist)
  (let ((unprovided (unprovided-required-systems (ql-dist:dist dist))))
    (when unprovided
      (format t "UNPROVIDED (but required) systems: ~A~%"
              unprovided))))

(defun self-referential-systems (dist)
  (let ((provided-systems (ql-dist:provided-systems (ql-dist:dist dist))))
    (loop for system in provided-systems
          for required = (ql-dist:required-systems system)
          when (find (ql-dist:name system) required :test 'string=)
          collect (list (ql-dist:name system) required))))

(defun self-referential-systems-report (dist)
  (let ((self-referential (self-referential-systems dist)))
    (when self-referential
      (format t "SELF-REFERENTIAL systems: ~A~%"
              self-referential))))

(defun absent-dependencies-report (dist)
  (let* ((systems (provided-systems (dist dist)))
         (no-required-systems (remove-if-not #'required-systems systems))
         (ratio (/ (length no-required-systems) (length systems))))
    (unless (< 0.5 ratio )
      (format t "ONLY ~$% OF SYSTEMS WITH DEPENDENCIES!"
              (* ratio 100)))))

(defparameter *sanity-check-reports*
  '(unprovided-required-systems-report
    self-referential-systems-report
    absent-dependencies-report))

(defun sanity-check-report (dist)
  (fresh-line)
  (dolist (fun *sanity-check-reports*)
    (funcall fun dist)))


(defun rebuild-tools ()
  (with-posix-cwd (asdf:system-source-directory "quicklisp-controller")
    (run "make" "clean")
    (run "make" "install")))

(defun tool-versions-match ()
  (let ((sbcl-version (run-output-line "sbcl" :version))
        (depcheck-version (ignore-errors (run-output-line "depcheck"
                                                          :sbcl-version))))
    (string= sbcl-version depcheck-version
             :start1 (1+ (position #\Space sbcl-version)))))

;; Should have a set of hooks, and probably a ~/.quicklisp-controller-init.lisp
(defun preflight ()
  (ignore-errors (run "dig" "wandrian.net" "a"))
  (ignore-errors (run "dig" "method-combination.net" "a"))
  (unless (tool-versions-match)
    (rebuild-tools)))


;;; Rewriting source.txt files

(defun (setf first-line) (line file)
  (with-open-file (stream file :direction :output
                          :if-exists :rename-and-delete)
    (write-line line stream)))

(defun =location~ (substring)
  (lambda (source)
    (ppcre:scan substring (location source))))

(defun http-to-https (source)
  (let ((location (ppcre:regex-replace "http://" (location source) "https://")))
    (format nil "https ~A" location)))

(defun rewrite-sources (match-fun new-source-fun)
  "Rewrite any source that returns TRUE from MATCH-FUN with
NEW-SOURCE-FUN, which should return a single line for use in the
source's source.txt file. Useful for bulk-updating sources."
  (map-sources
   (lambda (source)
     (when (funcall match-fun source)
       (let ((new-source (funcall new-source-fun source)))
         (when new-source
           (setf (first-line (source-file source))
                 new-source)))))))


;;; Showing update failures from a recrank log

(defun starts-with (subseq seq)
  (and (<= (length subseq) (length seq))
       (every #'equal subseq seq)))

(defun crank-projects (crank-logfile)
  (with-open-file (stream crank-logfile)
    (loop for line = (read-line stream nil)
       while line
       when (and line (starts-with "* " line))
	 collect (subseq line 2))))

(defun crank-failures (crank-logfile)
  (dolist (project (crank-projects crank-logfile))
    (format t ";;; ~A~%" project)
    (with-simple-restart (skip "Skip ~A" project)
      (update-source-cache (find-source project)))))
