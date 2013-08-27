;;;; setup.lisp

(in-package #:quicklisp-controller)

(defun setup-directories (projects-pathname)
  (sb-posix:mkdir #p "~/quicklisp-controller/" #o755)
  (with-posix-cwd "~/quicklisp-controller/"
    (sb-posix:symlink projects-pathname "projects")))
