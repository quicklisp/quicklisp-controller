;;;; system-file-magic.lisp

(defpackage #:system-file-magic
  (:use #:cl)
  (:shadowing-import-from #:asdf
                          #:system-source-file
                          #:map-systems
                          #:component-name
                          #:find-system))

(in-package #:system-file-magic)

(defun system-file-systems (name)
  (let* ((system (find-system name))
         (target (system-source-file system))
         (result '()))
    (map-systems
     (lambda (system)
       (when (equalp target (system-source-file system))
         (push (component-name system) result))))
    result))

(defun load-asdf-system-table (file)
  (setf file (translate-logical-pathname file))
  (let ((table (make-hash-table :test 'equalp)))
    (with-open-file (stream file)
      (loop for line = (read-line stream nil)
            while line do
            (let ((pathname
                   (merge-pathnames line
                                    file)))
              (setf (gethash (pathname-name pathname) table)
                    (truename pathname)))))
    table))

(defvar *systems* nil)

(defun system-finder (name)
  (when *systems*
    (gethash (string name) *systems*)))

(defun save-descriptions (system-names file)
  (dolist (system-name system-names)
    (let* ((system (asdf:find-system system-name))
           (description (ignore-errors (asdf:system-description system))))
      (when description
        (with-open-file (stream file :direction :output
                                :if-exists :append
                                :if-does-not-exist :create)
          (format stream "~S~%~%"
                  (list (asdf:component-name system)
                        description)))))))

(defun main (argv)
  (setf *package* (find-package :keyword))
  (sb-ext:disable-debugger)
  ;; (sb-posix:setenv "SBCL_HOME"
  ;;                  (load-time-value
  ;;                   (directory-namestring sb-int::*core-string*))
  ;;                  1)
  (destructuring-bind (index-file system-name output-file)
      (rest argv)
    (setf *systems* (load-asdf-system-table index-file))
    (setf asdf:*system-definition-search-functions*
          (list* #-asdf3 'asdf::sysdef-find-asdf
                 'system-finder
                 asdf:*system-definition-search-functions*))
    (with-open-file (stream output-file
                            :direction :output
                            :if-exists :supersede)
      (let ((broadcast (make-broadcast-stream stream *standard-output*))
            (system-names (system-file-systems system-name))
            (description-file (make-pathname :name "descriptions"
                                             :type "txt"
                                             :defaults index-file)))
        (save-descriptions system-names description-file)
        (format broadcast "~A~{ ~A~}~%"
                system-name
                system-names)))))
