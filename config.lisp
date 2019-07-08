;;;; config.lisp

(in-package #:quicklisp-controller)

(defun config-value (name)
  (let* ((base "quicklisp-controller:config;value.txt")
	 (file (make-pathname :name name :defaults base)))
    (when (probe-file file)
      (with-open-file (stream file)
	(read-line stream)))))


(defvar *report-to-email*
  (config-value "report-to-email")
  "The email address to which reports are emailed.")

(defparameter githappy:*oauth2-token*
  (config-value "githappy-token"))

