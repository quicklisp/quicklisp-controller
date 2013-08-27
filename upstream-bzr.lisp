;;;; upstream-bzr.lisp

(in-package #:quicklisp-controller)

(defclass bzr-source (vcs-source)
  ()
  (:default-initargs
   :command "bzr"
   :checkout-subcommand "branch"
   :update-subcommand "merge"))

(defmethod make-release-tarball ((source bzr-source) output-file)
  (let* ((prefix (release-tarball-prefix source))
         (tar-name (string-right-trim "/" prefix))
        (checkout (ensure-source-cache source)))
    (in-temporary-directory prefix
      (let ((tempgz (make-pathname :name tar-name :type "tgz")))
        (with-posix-cwd checkout
          (run "bzr" "export" tempgz)
          (copy tempgz output-file))))))
