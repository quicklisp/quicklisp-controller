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
  ((system-file-name
    :initarg :system-file-name
    :reader system-file-name)
   (required-systems
    :initarg :required-systems
    :reader required-systems)
   (release
    :initarg :release
    :accessor release)))

(defmethod name ((source upstream-source))
  (project-name source))

(defun make-controller-system (winner-info &key release)
  (destructuring-bind (system-file-name system-name
                                        &rest required-system-names)
      winner-info
    (make-instance 'controller-system
                   :release release
                   :name system-name
                   :system-file-name system-file-name
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


(defun slashed-name-p (name)
  (position #\/ name))

(defun flat-unique (strings &key (test 'equalp))
  (remove-duplicates (alexandria:flatten strings) :test test))

(defun resolve-slashed-system (system-name required-system-name dist)
  (let ((system-table (system-table dist)))
    (labels ((lookup (name)
               (or (gethash name system-table)
                   (progn (warn "Unknown system ~S" name)
                          nil)))
             (maybe-required-systems (thing)
               (when thing
                 (required-systems thing)))
             (resolve (name)
               (let* ((unslashed (unslashify-system-name name))
                      (slashedp (slashed-name-p name))
                      (samep (and slashedp (equal system-name unslashed))))
                 (cond ((and slashedp samep)
                        (flat-unique
                         (mapcar #'resolve
                                 (maybe-required-systems (lookup name)))))
                       (slashedp
                        unslashed)
                       (t
                        name)))))
      (resolve required-system-name))))

(defmethod dist ((system controller-system))
  (dist (release system)))

(defun resolved-required-systems (system)
  (let ((dist (dist system))
        (name (name system)))
    (flat-unique
     (mapcar (lambda (required-system-name)
               (resolve-slashed-system name required-system-name dist))
             (required-systems system)))))


(defmethod provided-systems ((dist controller-dist))
  (loop for release in (provided-releases dist)
        appending (provided-systems release)))

(defun write-systems-index (dist file)
  (let ((systems (provided-systems dist)))
    (setf systems (sort (copy-list systems) #'string< :key 'name))
    (with-open-file (stream file :direction :output
                            :if-exists :supersede)
      (write-line *system-file-header* stream)
      (dolist (system systems)
        (unless (slashed-name-p (name system))
          (format stream "~A ~A ~A~{ ~A~}~%"
                  (name (release system))
                  (system-file-name system)
                  (name system)
                  (resolved-required-systems system)))))))
