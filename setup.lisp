;;;; setup.lisp

(in-package #:quicklisp-controller)

(defun setup-directories (projects-pathname)
  (let ((home #p "~/quicklisp-controller/")
	(projects (truename projects-pathname)))
    (unless (probe-file home)
      (sb-posix:mkdir home #o755)
      (with-posix-cwd home
        (sb-posix:symlink (native-namestring projects) "projects")))))
