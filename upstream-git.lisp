;;;; upstream-git.lisp

(in-package #:quicklisp-controller)

(defclass git-source (vcs-source)
  ((tarball-target
    :initarg :tarball-target
    :accessor tarball-target
    :initform "master"))
  (:default-initargs
   :command "git"
   :checkout-subcommand "clone"
   :checkout-subcommand-arguments '("--recursive")
   :update-subcommand "pull"))

(defclass tagged-git-source (tagged-mixin git-source)
  ((tag-data :reader tarball-target)))

(defclass branched-git-source (tagged-mixin git-source)
  ((tag-data :reader branch-name :reader tarball-target)))

(defmethod checkout-subcommand-arguments ((source branched-git-source))
  (append (call-next-method) (list "--branch" (branch-name source))))

(defmethod vcs-update ((source tagged-git-source) checkout-directory)
  (with-posix-cwd checkout-directory
    (run "git" "fetch" "--tags")
    (run "git" "checkout" (tag-data source))))

(defmethod vcs-update :after ((source git-source) checkout-directory)
  (with-posix-cwd checkout-directory
    (run "git" "submodule" "update" "--init" "--recursive")))

(defmethod cached-checkout-directory :around ((source git-source))
  ;; Older gits have problems with checking out to a directory with
  ;; a trailing slash, so trim it.
  (string-right-trim "/" (namestring (call-next-method))))

(defun commit-id (source)
  (let ((checkout (ensure-source-cache source)))
    (with-posix-cwd checkout
      (with-run-output (stream ("git" "rev-parse" "HEAD"))
        (read-line stream)))))

(defmethod tag-data :around ((source branched-git-source))
  (let ((tag (call-next-method))
        (commit (commit-id source)))
    (format nil "~A-~A"
            tag
            (subseq commit 0 8))))

(defgeneric target-ref (source)
  (:documentation "The ref to use when archiving.")
  (:method ((source git-source))
    "HEAD")
  (:method ((source branched-git-source))
    (format nil "refs/heads/~A" (tarball-target source)))
  (:method ((source tagged-git-source))
    (format nil "refs/tags/~A" (tarball-target source))))

(defmethod make-release-tarball ((source git-source) output-file)
  (let ((prefix (release-tarball-prefix source))
        (checkout (ensure-source-cache source)))
    (in-temporary-directory prefix
      (let ((temptar (merge-pathnames "package.tar"))
            (tempgz (merge-pathnames "package.tar.gz")))
        (with-posix-cwd checkout
          (with-binary-run-output temptar
            (run "git" "archive" :format "tar" :prefix prefix
                 (target-ref source)))
          (run "gzip" "-vn9" temptar)
          (copy tempgz output-file))))))

