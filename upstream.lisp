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
  (let ((*current-mapped-source* (project-name source-file)))
    (with-simple-restart (skip "Skip ~A source" *current-mapped-source*)
      (funcall fun source))))

(defun map-sources (fun)
  (with-simple-restart (abort "Give up entirely")
    (dolist (source-file
              (directory #p"quicklisp-controller:projects;**;source.txt"))
      (let ((project-name (pathname-project-name source-file)))
        (map-source fun (load-source-file project-name source-file))))))

(defun pmap-sources (fun)
  (let ((dependency-tree (lparallel:make-ptree))
        (host-dependency (make-hash-table :test 'equal))
        (i 0))
    (map-sources (lambda (source)
                   (let ((host (source-host source)))
                     (lparallel:ptree-fn i (gethash host host-dependency)
                                         (lambda (&optional arg)
                                           (declare (ignore arg))
                                           (map-source fun source))
                                         dependency-tree)
                     (setf (gethash host host-dependency) (list i))
                     (incf i))))
    (lparallel:ptree-fn 'everything (loop for j below i collect j)
                        (constantly nil) dependency-tree)
    (lparallel:call-ptree 'everything dependency-tree)
    nil))

(defun find-source (name)
  (block nil
    (map-sources (lambda (source)
                   (when (string-equal (project-name source) name)
                     (return source))))))

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
