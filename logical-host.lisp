;;;; logical-host.lisp

(in-package #:quicklisp-controller)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ignore-errors (load-logical-pathname-translations "quicklisp-controller"))
  (unless (typep (ignore-errors (parse-namestring "quicklisp-controller:"))
                 'logical-pathname)
    (setf (logical-pathname-translations "quicklisp-controller")
          `(("projects;**;*.*.*"
             ,(merge-pathnames "quicklisp-controller/projects/**/"
                               (user-homedir-pathname)))
            ("**;*.*.*"
             ,(merge-pathnames "quicklisp-controller/**/"
                               (user-homedir-pathname)))))))
