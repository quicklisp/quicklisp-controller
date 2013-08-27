;;;; map-systems.lisp

(in-package #:quicklisp-controller)

(defun map-systems (file)
  (let ((table (asdf-systems-table)))
    (maphash (lambda (key value)
               (declare (ignore value))
               (ignore-errors
                 (run "sbcl"
                    "--noinform"
                    "--no-userinit"
                    "--non-interactive"
                    "--load" (probe-file #p "asdf.lisp")
                    "--load" (probe-file #p"map-systems-init.lisp")
                    "--eval"
                    (prin1-to-string `(defvar cl-user::*map-systems-system*
                                        ,key))
                    "--load" (truename file)
                    (translate-logical-pathname *system-file-index-file*))))
             table)))
