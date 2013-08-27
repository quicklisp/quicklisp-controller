;;;; release.lisp

(in-package #:quicklisp-controller)

(defun release-maker (directory)
  (ensure-directories-exist directory)
  (lambda (source)
    (make-release-tarball source
                          (make-pathname :name (project-name source)
                                         :type "tgz"
                                         :defaults directory))))
