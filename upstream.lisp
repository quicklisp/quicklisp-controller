;;;; upstream.lisp

(in-package #:quicklisp-controller)

(defclass upstream-source ()
  ((source-file
    :initarg :source-file
    :accessor source-file)
   (project-name
    :initarg :project-name
    :accessor project-name)
   (location
    :initarg :location
    :accessor location)))

(defgeneric github-issue (object)
  (:method (source)
    (setf source (source-designator source))
    (qlc-github-issues:matching-issue (name source))))

(defgeneric github-issue-number (object)
  (:method (source)
    (setf source (source-designator source))
    (getf (github-issue source) :number)))

(defmethod base-directory ((source upstream-source))
  (pathname (directory-namestring (source-file source))))

(defgeneric print-source (source stream)
  (:method (source stream)
    (format stream "~S ~S"
            (project-name source)
            (location source))))

(defmethod print-object ((source upstream-source) stream)
  (print-unreadable-object (source stream :type t)
    (print-source source stream)))

(defgeneric module-name (source)
  (:method (source)
    (project-name source)))

(defgeneric source-description (source)
  (:documentation "Return a line suitable for describing approximately
  how to get SOURCE yourself."))

(defgeneric release-tarball-prefix (source)
  (:method (source)
    (format nil "~A/" (project-name source))))

(defgeneric make-release-tarball (source output-file))

(defun release-maker (directory)
  (lambda (source)
    (let ((tarball (make-pathname :name (project-name source)
                                         :type "tgz"
                                         :defaults directory)))
      (unless (probe-file tarball)
        (format *trace-output* "~&; Creating ~A~%" tarball)
        (make-release-tarball source tarball)))))

(defgeneric create-source-cache (source)
  (:documentation "Unconditionaly get and cache data for building SOURCE."))

(defgeneric find-source-cache (source)
  (:documentation "Return the cached build data for SOURCE, or nil if
  no cached data is available."))

(defgeneric ensure-source-cache (source)
  (:documentation "Ensure data needed to build releases for SOURCE are
  cached. If cached data already exists for SOURCE, do not update
  it.")
  (:method (source)
    (or (find-source-cache source)
        (create-source-cache source))))

(defgeneric update-source-cache (source)
  (:documentation "Try to update cached data for SOURCE."))


;;;
;;; Loading & mapping sources from the quicklisp-projects directory.
;;;

(defun split-into-initargs (line initargs)
  (let ((values (split-spaces line)))
    (unless (eql (length values) (length initargs))
      (error "Initarg/value mismatch between ~S and ~S"
             initargs values))
    (mapcan #'list initargs values)))

(defgeneric source-location-initargs (source)
  (:method (source)
    (list :location)))

(defgeneric source-host (source)
  (:method (source)
    (puri:uri-host (puri:parse-uri (location source)))))

(defgeneric parse-location (source location-string)
  (:documentation "Update an instance by parsing its location value.")
  (:method (source location-string)
    (let ((initargs (split-into-initargs location-string
                                         (source-location-initargs source))))
      (apply #'reinitialize-instance source initargs))))

(defun pathname-project-name (pathname)
  (first (last (pathname-directory pathname))))

(defun make-source (class project location)
  (parse-location (make-instance class :project-name project)
                  location))

(defun load-source-file (project file)
  "Create a source from the description in FILE."
  (with-open-file (stream file)
    (let* ((line (read-line stream))
           (space (position #\Space line)))
      (unless space
        (error "Malformed source line in ~A" file))
      (let* ((source-name (subseq line 0 space))
             (source-class-name (format nil "~:@(~A-source~)" source-name))
             (source-class (find-symbol source-class-name
                                        (load-time-value *package*)))
             (location (subseq line (1+ space))))
        (when (not source-class)
          (error "No source class for ~S (loading ~S)" source-class-name file))
        (let ((source (make-source source-class project location)))
          (setf (source-file source) file)
          source)))))

(defvar *current-mapped-source* nil)

(defun map-source (fun source)
  (let ((*current-mapped-source* (project-name source)))
    (with-simple-restart (skip "Skip ~A source" *current-mapped-source*)
      (funcall fun source))))

(defun map-sources (fun)
  (with-simple-restart (abort "Give up entirely")
    (dolist (source-file
              (directory #p"quicklisp-controller:projects;projects;**;source.txt"))
      (let ((project-name (pathname-project-name source-file)))
        (map-source fun (load-source-file project-name source-file))))))

(defun collect-sources-if (fun)
  (let ((result '()))
    (map-sources (lambda (source)
		   (when (funcall fun source)
		     (push source result))))
    (sort result 'string< :key 'name)))


(defun source-bucket (source)
  "Return a string suitable for binning a source for parallel work."
  (subseq (string-digest (first-line-of (source-file source)))
          0 1))

(defun fasl-directory (source)
  (translate-logical-pathname
   (make-pathname :host "quicklisp-controller"
                  :directory (list :absolute "dist" "fasls"
                                   (source-bucket source)))))

(defun pmap-sources (fun &key (parallel-key 'source-bucket)
                           (test #'identity))
  (let ((dependency-tree (lparallel:make-ptree))
        (parallel-key-dependency (make-hash-table :test 'equal))
        (i 0)
        (result '()))
    (map-sources (lambda (source)
                   (let ((testp (funcall test source))
                         (pkey (funcall parallel-key source)))
                     (when testp
                       (lparallel:ptree-fn i (gethash pkey
                                                      parallel-key-dependency)
                                           (lambda (&optional arg)
                                             (declare (ignore arg))
                                             (multiple-value-bind (result error)
                                                 (ignore-errors
                                                  (map-source fun source))
                                               (when error
                                                 (format *trace-output* "; ERROR: ~A -> ~%;; ~A~%"
                                                         source error)
                                                 (push (cons source error)
                                                       result))))
                                           dependency-tree)
                       (setf (gethash pkey parallel-key-dependency)
                             (list i))
                       (incf i)))))
    (lparallel:ptree-fn 'everything (loop for j below i collect j)
                        (constantly nil) dependency-tree)
    (lparallel:call-ptree 'everything dependency-tree)
    (values nil result)))

(defun project-name-source-file (project-name)
  (merge-pathnames
   (make-pathname :defaults "" :directory (list :relative :back project-name))
   (translate-logical-pathname
    (make-pathname :host "quicklisp-controller"
                   :directory (list :absolute "projects" "projects" "stub")
                   :name "source"
                   :type "txt"))))

(defun fresh-cache-p (source)
  (not
   (not
    (probe-file
     (make-pathname :type nil :name "fresh-cache"
		    :defaults (project-name-source-file (name source)) )))))

(defun find-source (project-name)
  (let* ((name (string-downcase project-name))
         (file (probe-file (project-name-source-file name))))
    (when file
      (load-source-file name file))))

(defun edit-source (name)
  (let ((source (find-source name)))
    (when source
      (ed (source-file source)))))

(defun source-designator (source)
  (if (typep source 'upstream-source)
      source
      (find-source source)))

(defun map-sources-of-type (type fun)
  (map-sources (lambda (source)
                 (when (typep source type)
                   (funcall fun source)))))

(defun all-of-type (type)
  (let ((result '()))
    (map-sources-of-type type (lambda (source)
                                (push source result)))
    result))

(defun random-of-type (type)
  (random-element (coerce (all-of-type type) 'vector)))

(defgeneric command (source)
  (:method (source)
    nil))

(defun missing-commands ()
  (let ((missing '())
	(tried (make-string-table)))
    (map-sources
     (lambda (source)
       (let ((command (command source)))
	 (when command
	   (unless (gethash command tried)
	     (setf (gethash command tried) t)
	     (unless (ignore-errors (run command "--version"))
	       (push command missing)))))))
    missing))
