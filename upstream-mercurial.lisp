;;;; upstream-mercurial.lisp

(in-package #:quicklisp-controller)

(defclass mercurial-source (vcs-source)
  ()
  (:default-initargs
   :command "hg"
   :checkout-subcommand "clone"
   :update-subcommand "pull"
   :update-subcommand-arguments (list "-u")))

(defmethod make-release-tarball ((source mercurial-source) output-file)
  (let ((prefix (release-tarball-prefix source))
        (checkout (ensure-source-cache source)))
    (with-posix-cwd checkout
      (run "hg" "archive" "-t" "tgz" "-p" prefix output-file))))
