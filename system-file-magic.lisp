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

(defun system-metadata-sexp (system project-name)
  (list :project project-name
        :system (asdf:component-name system)
        :description (asdf:system-description system)
        :long-description (asdf:system-long-description system)
        :license (asdf:system-license system)
        :author (asdf:system-author system)
        :depends-on (asdf:system-depends-on system)
        :homepage (asdf:system-homepage system)
        :bug-tracker (asdf:system-bug-tracker system)))

(defun save-system-metadata (system-names project-name file)
  (ensure-directories-exist file)
  (with-open-file (stream file :direction :output
                          :if-exists :rename-and-delete
                          :if-does-not-exist :create)
    (let ((*package* (find-package :cl)))
      (pprint (loop
                 for system-name in system-names
                 collecting (cons system-name
                                  (ignore-errors
                                    (system-metadata-sexp (asdf:find-system system-name)
                                                          project-name))))
              stream))))

(defun main (argv)
  (setf *package* (find-package :keyword))
  (sb-ext:disable-debugger)
  ;; (sb-posix:setenv "SBCL_HOME"
  ;;                  (load-time-value
  ;;                   (directory-namestring sb-int::*core-string*))
  ;;                  1)
  (destructuring-bind (index-file system-name output-file
                                  &optional project-name description-file)
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
            (system-names (reverse (system-file-systems system-name))))
        (when description-file
          (save-system-metadata system-names project-name description-file))
        (format broadcast "~A~{ ~A~}~%"
                system-name
                system-names)))))
