;;;; indexes.lisp

(in-package #:quicklisp-controller)

(defvar *index-file-format-version* 27)
(defparameter *s3-bucket* "beta.quicklisp.org")

(defun write-system-index (file)
  (with-open-file (stream file :direction :output
                          :if-exists :supersede)
    (format stream "# project system-file system-name [dependency1..dependencyN]~%")
    (map-sources
     (lambda (source)
       (let ((winners (find-winning-systems source)))
         (when winners
           (dolist (winner winners)
             (destructuring-bind (system-file system-name &rest dependencies)
                 winner
               (format stream "~A ~A ~A~{ ~A~}~%"
                       (project-name source)
                       system-file
                       system-name
                       (sort (copy-list dependencies) #'string<))))))))))

(defun write-release-index (file)
  (with-open-file (stream file :direction :output
                          :if-exists :supersede)
    (flet ((trim-prefix (file)
             (subseq file (1+ (position #\/ file)))))
      (format stream "# project url size file-md5 content-sha1 prefix ~
                      [system-file1..system-fileN]~%")
      (map-sources
       (lambda (source)
         (when (find-winning-systems source)
           (let* ((tarball (ensure-cached-release-tarball source))
                  (prefix (pathname-name tarball))
                  (project-name (project-name source))
                  (system-files (mapcar #'trim-prefix
                                        (winning-system-files source))))
             (format stream "~A http://~A/archive/~A/~A/~A ~A ~D ~A ~A~{ ~A~}~%"
                     project-name
                     *s3-bucket*
                     project-name
                     (dist-string (file-write-date tarball))
                     (file-namestring tarball)
                     (file-size tarball)
                     (file-md5 tarball)
                     (first (last (pathname-directory tarball)))
                     prefix
                     system-files))))))))

(defun s3-already-exists-p (content-sha1 bucket key)
  "If an object already exists in S3 with the given SHA1, return
true. If there's a SHA1 mismatch, signal an error."
  (multiple-value-bind (headers status-code)
      (zs3:head :bucket bucket :key key)
    (let ((s3-content-sha1 (cdr (assoc :x-amz-meta-content-sha1 headers))))
      (cond ((not (<= 200 status-code 299))
             nil)
            ((string= s3-content-sha1 content-sha1)
             t)
            (t
             (error "~A ~A exists but does not match sha1"
                    bucket key))))))

(defun upload-release-files ()
  (map-sources
   (lambda (source)
     (when (find-winning-systems source)
       (format *trace-output* "~&; uploading ~A~%" source)
       (let* ((tarball (ensure-cached-release-tarball source))
              (content-sha1 (first (last (pathname-directory tarball))))
              (key (format nil "archive/~A/~A/~A"
                           (project-name source)
                           (dist-string (file-write-date tarball))
                           (file-namestring tarball))))
         (unless (s3-already-exists-p content-sha1 *s3-bucket* key)
           (zs3:put-file tarball *s3-bucket* key :public t
                         :metadata (zs3:parameters-alist :content-sha1
                                                         content-sha1))))))))

(defun put-index-file (index-file)
  (let ((key (format nil "~A/~A/~A"
                     (pathname-name index-file)
                     (dist-string (file-write-date index-file))
                     (file-namestring index-file))))
    (zs3:put-file index-file *s3-bucket* key :public t :content-type "text/plain")))

(defun put-bootstrap-file (bootstrap-file)
  (put-index-file bootstrap-file)
  (zs3:put-file bootstrap-file "b.quicklisp.org"
                (file-namestring bootstrap-file)
                :public t
                :content-type "text/plain"))


(defclass distinfo ()
  ((name
    :initarg :name
    :accessor name)
   (version
    :initarg :version
    :accessor version)))

(defmethod print-object ((distinfo distinfo) stream)
  (print-unreadable-object (distinfo stream :type t)
    (format stream "~A ~A"
            (name distinfo)
            (version distinfo))))

(defun make-distinfo (name version)
  (make-instance 'distinfo :name name :version version))

(defgeneric archive-base-url (distinfo)
  (:method (distinfo)
    (format nil "http://beta.quicklisp.org/")))

(defgeneric system-index-key (distinfo)
  (:method (distinfo)
    (format nil "dist/~A/~A/systems.txt"
            (name distinfo)
            (version distinfo))))

(defgeneric release-index-key (distinfo)
  (:method (distinfo)
    (format nil "dist/~A/~A/releases.txt"
            (name distinfo)
            (version distinfo))))

(defgeneric system-index-url (distinfo)
  (:method (distinfo)
    (format nil "~A~A"
            (archive-base-url distinfo)
            (system-index-key distinfo))))

(defgeneric release-index-url (distinfo)
  (:method (distinfo)
    (format nil "~A~A"
            (archive-base-url distinfo)
            (release-index-key distinfo))))



(defgeneric distinfo-subscription-url (distinfo)
  (:method (distinfo)
    (format nil "http://~A/dist/~A.txt"
            *s3-bucket*
            (name distinfo))))

(defgeneric canonical-distinfo-key (distinfo)
  (:method (distinfo)
    (format nil "dist/~A/~A/distinfo.txt"
            (name distinfo)
            (version distinfo))))

(defgeneric canonical-distinfo-url (distinfo)
  (:method (distinfo)
    (format nil "~A~A"
            (archive-base-url distinfo)
            (canonical-distinfo-key distinfo))))

(defgeneric write-distinfo (distinfo file)
  (:method (distinfo file)
    (with-open-file (stream file :direction :output
                            :if-exists :supersede)
      (flet ((store (name value)
               (format stream "~A: ~A~%" name value)))
        (store "name" (name distinfo))
        (store "version" (version distinfo))
        (store "system-index-url" (system-index-url distinfo))
        (store "release-index-url" (release-index-url distinfo))
        (store "archive-base-url" (archive-base-url distinfo))
        (store "canonical-distinfo-url" (canonical-distinfo-url distinfo))
        (store "distinfo-subscription-url"
               (distinfo-subscription-url distinfo))))))

(defun upload-distinfo (distinfo)
  (let ((sysindex #p"quicklisp-controller:tmp;systems.txt")
        (relindex #p"quicklisp-controller:tmp;releases.txt")
        (distfile #p"quicklisp-controller:tmp;distinfo.txt")
        (primary-key (format nil "dist/~A.txt" (name distinfo))))
    (write-system-index sysindex)
    (write-release-index relindex)
    (write-distinfo distinfo distfile)
    (flet ((put (file bucket key)
             (zs3:put-file file bucket key :public t :content-type "text/plain")))
      (put distfile "beta.quicklisp.org" primary-key)
      (invalidate-quickstart-paths (list primary-key))
      (put distfile *s3-bucket* (canonical-distinfo-key distinfo))
      (put sysindex *s3-bucket* (system-index-key distinfo))
      (put relindex *s3-bucket* (release-index-key distinfo)))))


(defun make-mock-dist (name version directory)
  (let ((distinfo (make-distinfo name version))
        (*default-pathname-defaults* (pathname directory)))
    (ensure-directories-exist *default-pathname-defaults*)
    (open "enabled.txt" :direction :probe :if-does-not-exist :create)
    (write-system-index "systems.txt")
    (write-release-index "releases.txt")
    (write-distinfo distinfo "distinfo.txt")
    (ensure-directories-exist #p"archives/")
    (map-sources
     (lambda (source)
       (let ((tarball (ensure-cached-release-tarball source)))
         (copy tarball #p"archives/"))))))

(defun mail-mock-report ()
  (if *report-to-email*
      (with-output-to-mail (*standard-output* :to *report-to-email*
                                              :from *report-to-email*
                                              :subject "Quicklisp Mock Report Results")
        (mock-report :build nil :mail nil))
      (warn "~S unset, cowardly refusing to email any report"
            '*report-to-email*)))

(defun mock-report (&key (build t) (mail nil))
  (with-system-index
    (with-skipping
      (when build
        (let ((target-directory (ql-setup:qmerge "dists/mock/")))
          (run "rm" "-rf" target-directory)
          (make-mock-dist "mock" "9999-99-99" target-directory)))
      (ql-dist:show-update-report (ql-dist:find-dist "quicklisp")
                                  (ql-dist:find-dist "mock"))
      (print (multiple-value-list (system-differences "quicklisp" "mock")))))
  (when mail
    (mail-mock-report)))

(defun system-differences (old-dist new-dist)
  "Report on the systems that differ between old-dist and new-dist."
  (flet ((dist-table (dist)
           (let ((table (make-hash-table :test 'equalp)))
             (dolist (system (ql-dist:provided-systems (ql-dist:dist dist)))
               (setf (gethash (ql-dist:name system) table) system))
             table)))
    (let ((old-table (dist-table old-dist))
          (new-table (dist-table new-dist))
          (added '())
          (removed '()))
      (maphash (lambda (key system)
                 (declare (ignore system))
                 (if (gethash key old-table)
                     (remhash key old-table)
                     (push key added)))
               new-table)
      (maphash (lambda (key system)
                 (declare (ignore system))
                 (push key removed))
               old-table)
      (values added removed))))

(defun dist-differences (old-dist new-dist &key enumerator)
  "Report on the systems that differ between old-dist and new-dist."
  (flet ((dist-table (dist)
           (let ((table (make-hash-table :test 'equalp)))
             (dolist (object (funcall enumerator (ql-dist:dist dist)))
               (setf (gethash (ql-dist:name object) table) object))
             table)))
    (let ((old-table (dist-table old-dist))
          (new-table (dist-table new-dist))
          (added '())
          (removed '()))
      (maphash (lambda (key object)
                 (declare (ignore object))
                 (if (gethash key old-table)
                     (remhash key old-table)
                     (push key added)))
               new-table)
      (maphash (lambda (key object)
                 (declare (ignore object))
                 (push key removed))
               old-table)
      (values added removed))))
