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
  (with-run-output (stream ("apt-file" "search" (format nil "/~A" file)))
    (let ((scanner (ppcre:create-scanner "^(.*?): (.*)"))
          (result '()))
      (loop for line = (read-line stream nil)
            while line do
              (ppcre:register-groups-bind (key value) (scanner line)
                (setf result (acons key value result))))
      result)))

(defun scan-group (scanner string)
  (multiple-value-bind (matchp groups)
      (ppcre:scan-to-strings scanner string)
    (when matchp
      (aref groups 0))))

(defun failed-missing-libraries (failure-file)
  (let ((alternatives (ppcre:create-scanner ": Unable to load any"))
        (one-library (ppcre:create-scanner ": Unable to load foreign"))
        (one-library-name (ppcre:create-scanner "Error opening shared object \"(.*?)\""))
	(header-name (ppcre:create-scanner "fatal error: (.*?): No such file")))
    (with-open-file (stream failure-file)
      (loop
	 (let ((line (read-line stream nil))
	       value)
	   (unless line
	     (return))
	   (cond ((ppcre:scan alternatives line)
		  (return (read stream)))
		 ((setf value (scan-group header-name line))
		  (return (list value)))
		 ((ppcre:scan one-library line)
		  (let ((next-line (read-line stream)))
		    (ppcre:register-groups-bind (library) (one-library-name next-line)
		      (return (list library)))))))))))

(defun missing-library-guess (library)
  (let ((alist (apt-file-search library)))
    (flet ((library-length (thing)
	     (+ (if (search "-dev" (car thing))
		    -1000
		    0)
		(length (car thing)))))
      (when alist
        (first (first (sort alist #'< :key #'library-length)))))))

(defun missing-library-package (failure-file)
  (let ((libraries (sort (remove-if-not 'stringp (failed-missing-libraries failure-file)) #'< :key #'length)))
    (when libraries
      (some #'missing-library-guess libraries))))

(defun crawl-for-missing-libraries (wild)
  (let ((debian-packages '()))
    (dolist (file (directory wild) debian-packages)
      (let ((guess (missing-library-package file)))
        (when guess
          (pushnew guess debian-packages :test #'string=))))))

(defun missing-source-libraries (&optional (source *last-source*))
  (setf *last-source* source)
  (crawl-for-missing-libraries (build-relative "fail/fail_*.txt" source)))

(defun apt-get-commands (packages &key run)
  (map nil (lambda (package)
	     (format t "apt-get --yes install ~A~%" package)
             (when run
               (run-program "/usr/bin/sudo"
                            (list "/usr/bin/apt-get" "--yes" "install" package)
                            :search nil
                            :wait t
                            :output t)))
       packages))

(defun foreign-library-loop (&key run)
  (let (source missing)
    (tagbody
     loop
       (format t "~&> ")
       (force-output)
       (setf source (read))
       (when (eql source :quit)
         (go end))
     retry
       (crank source)
       (setf missing (missing-source-libraries source))
       (unless missing
         (warn "No missing libraries for ~A, giving up" source)
         (go loop))
       (apt-get-commands missing :run run)
       (format t "~&Press enter to retry~%")
       (force-output)
       (unless (equal (read-line) "")
         (go loop))
       (go retry)
     end)))
