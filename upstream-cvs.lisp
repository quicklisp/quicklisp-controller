;;;; upstream-cvs.lisp

(in-package #:quicklisp-controller)

(defclass cvs-source (vcs-source)
  ()
  (:default-initargs
   :command "cvs"))

(defclass cvs-oddmodule-source (cvs-source)
  ((module-name
    :initarg :module-name
    :accessor module-name
    :documentation "Some CVS sources have modules that don't
    correspond to their project name, e.g. Scott Burson's
    misc-extensions. Allow an explicit module name in the source.")))

(defmethod source-location-initargs ((source cvs-oddmodule-source))
  (list :location :module-name))

(defmethod source-host ((source cvs-source))
  (let* ((location (location source))
         (host-start (1+ (position #\@ location)))
         (host-end (position #\: location :start host-start)))
    (subseq location host-start host-end)))

(defmethod source-description ((source cvs-source))
  (format nil "cvs -d ~A co ~A"
          (location source)
          (module-name source)))

(defmethod vcs-checkout ((source cvs-source) checkout-directory)
  (let* ((pathname checkout-directory)
         (parent (parent-directory pathname))
         (enough (enough-namestring pathname parent)))
    (ensure-directories-exist parent)
    (with-posix-cwd parent
      (run "cvs" "-qz3" "-d" (location source)
           "co" "-d" (string-right-trim "/" enough) (module-name source)))))

(defmethod vcs-update-arguments ((source cvs-source) checkout-directory)
  (list "cvs" "-qz3" "-d" (location source)
        "update" "-dP"))

(defmethod make-release-tarball ((source cvs-source) output-file)
  (let ((prefix (release-tarball-prefix source))
        (checkout (ensure-source-cache source)))
    (with-posix-cwd checkout
      (in-temporary-directory "release/"
        ;; This garbage is because sourceforge CVS chokes on something like:
        ;;   cvs export -r HEAD -d foo/bar myproject
        ;; Work around it.
        (let ((output (merge-pathnames prefix))
              (ftso-sourceforge (format nil "XXX-FROBBLEDOBBLE-~D-XXX"
                                        (random 42))))
          (with-posix-cwd ".."
            (run "cvs" "-qz3" "export" "-r" "HEAD"
                 "-d" ftso-sourceforge
                 (module-name source))
            (run "mv" ftso-sourceforge output))
          (run "tar" "cvf" "package.tar" prefix)
          (run "gzip" "-vn9" "package.tar")
          (copy "package.tar.gz" output-file))))))
