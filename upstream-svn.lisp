;;;; upstream-svn.lisp

(in-package #:quicklisp-controller)

(defclass svn-source (vcs-source)
  ()
  (:default-initargs
   :command "svn"))

(defmethod make-release-tarball ((source svn-source) output-file)
  (let ((prefix (release-tarball-prefix source))
        (checkout (ensure-source-cache source)))
    (in-temporary-directory "release/"
      (let ((output (merge-pathnames prefix)))
        (with-posix-cwd checkout
          (run (command source) "export" "." output))
        (run "tar" "cvf" "package.tar" (enough-namestring output)))
      (run "gzip" "-vn9" "package.tar")
      (copy "package.tar.gz" output-file))))
