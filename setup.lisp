;;;; setup.lisp

(in-package #:quicklisp-controller)

(defun setup-directories (projects-pathname)
  (let ((home #p "~/quicklisp-controller/"))
    (unless (probe-file home)
      (sb-posix:mkdir home #o755)
      (with-posix-cwd home
        (sb-posix:symlink projects-pathname "projects")))))
