;;;; upstream-darcs.lisp

(in-package #:quicklisp-controller)

(defclass darcs-source (vcs-source)
  ()
  (:default-initargs
   :command "darcs"
   :checkout-subcommand "get"
   :checkout-subcommand-arguments '("--lazy")
   :update-subcommand "pull"
   :update-subcommand-arguments '("-a")))

(defmethod make-release-tarball ((source darcs-source) output-file)
  (let* ((prefix (release-tarball-prefix source))
         (distname (string-right-trim "/" prefix))
         (distball (format nil "~A.tar.gz" distname))
         (checkout (ensure-source-cache source)))
    (with-posix-cwd checkout
      (run "darcs" "dist" "-d" distname)
      (copy distball output-file))))
