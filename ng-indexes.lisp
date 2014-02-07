;;;; ng-indexes.lisp

(in-package #:quicklisp-controller)

(defclass controller-dist-object ()
  ((name
    :initarg :name
    :reader name)))

(defclass controller-dist (controller-dist-object)
  ((provided-releases
    :initarg :provided-releases
    :initform nil
    :accessor provided-releases)
   (system-table
    :initarg :system-table
    :initform (make-string-table)
    :reader system-table)
   (release-table
    :initarg :release-table
    :initform (make-string-table)
    :reader release-table))
  (:default-initargs
   :name "quicklisp-controller"))

(defclass controller-release (controller-dist-object)
  ((source
    :initarg :source
    :reader source)
   (provided-systems
    :initarg :provided-systems
    :accessor provided-systems)
   (dist
    :initarg :dist
    :accessor dist)))

(defmethod print-object ((object controller-dist-object) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~S" (name object))))

(defclass controller-system (controller-dist-object)
  ((required-systems
    :initarg :required-systems
    :reader required-systems)
   (release
    :initarg :release
    :accessor release)))

(defmethod name ((source upstream-source))
  (project-name source))

(defun make-controller-system (winner-info &key release)
  (destructuring-bind (system-name &rest required-system-names)
      (rest winner-info)
    (make-instance 'controller-system
                   :release release
                   :name system-name
                   :required-systems required-system-names)))

(defun make-controller-dist ()
  (let ((dist (make-instance 'controller-dist)))
    (map-sources
     (lambda (source)
       (let ((winners (find-winning-systems source)))
         (when winners
           (let* ((release (make-instance 'controller-release
                                          :source source
                                          :name (project-name source)
                                          :dist dist))
                  (systems (mapcar 'make-controller-system winners)))
             (setf (dist release) dist)
             (setf (provided-systems release) systems)
             (dolist (system systems)
               (setf (release system) release)
               (setf (gethash (name system) (system-table dist)) system))
             (setf (gethash (name release) (release-table dist)) release)
             (push release (provided-releases dist)))))))
    dist))
