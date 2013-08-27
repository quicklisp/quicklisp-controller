;;;; fail-dwim.lisp

(in-package #:quicklisp-controller)

;;; Some ideas about guessing what problems are happening with a failure file.

(defparameter *component-not-found*
  (ppcre:create-scanner "component (.*?) not found"))

(defun missing-component (string)
  (ppcre:register-groups-bind (component) (*component-not-found* string)
    component))

(defun file-string (file)
  (with-output-to-string (stream)
    (dolist (line (file-lines file))
      (write-line line stream ))))

(defun fail-file-components (pathname)
  (let ((name (pathname-name pathname)))
    (destructuring-bind (fail project system-file system)
        (ppcre:split "_" name)
      (declare (ignore fail))
      (values project system-file system))))

(defun missing-components (source)
  (let ((components '()))
    (dolist (file (failing-system-files source) components)
      (let ((string (file-string file)))
        (let ((component (missing-component string)))
          (when component
            (multiple-value-bind (project system-file system)
                (fail-file-components file)
              (declare (ignore project system-file))
              (push (list system component) components))))))))


(defun apt-file-search (file)
  (with-run-output (stream ("apt-file" "search" file))
    (let ((scanner (ppcre:create-scanner "^(.*?): (.*)"))
          (result '()))
      (loop for line = (read-line stream nil)
            while line do
              (ppcre:register-groups-bind (key value) (scanner line)
                (setf result (acons key value result))))
      result)))

(defun failed-missing-libraries (failure-file)
  (let ((alternatives (ppcre:create-scanner ": Unable to load any"))
        (one-library (ppcre:create-scanner ": Unable to load foreign"))
        (one-library-name (ppcre:create-scanner "Error opening shared object \"(.*?)\"")))
    (with-open-file (stream failure-file)
      (loop
        (let ((line (read-line stream nil)))
          (unless line
            (return))
          (cond ((ppcre:scan alternatives line)
                 (return (read stream)))
                ((ppcre:scan one-library line)
                 (let ((next-line (read-line stream)))
                   (ppcre:register-groups-bind (library) (one-library-name next-line)
                     (return (list library)))))))))))

(defun missing-library-guess (library)
  (let ((alist (apt-file-search library)))
    (flet ((cdr-length (thing)
             (length (cdr thing))))
      (when alist
        (first (first (sort alist #'< :key #'cdr-length)))))))

(defun missing-library-package (failure-file)
  (let ((libraries (sort (failed-missing-libraries failure-file) #'< :key #'length)))
    (when libraries
      (missing-library-guess (first libraries)))))

(defun crawl-for-missing-libraries (wild)
  (let ((debian-packages '()))
    (dolist (file (directory wild) debian-packages)
      (let ((guess (missing-library-package file)))
        (when guess
          (pushnew guess debian-packages :test #'string=))))))

(defun missing-source-libraries (&optional (source *last-source*))
  (setf *last-source* source)
  (crawl-for-missing-libraries (build-relative "fail/fail_*.txt" source)))
