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

(defclass git-at-commit-source (tagged-git-source)
  ((tag-data :reader commit :reader tarball-target)))

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

(defmethod source-cache-timestamp ((source git-source))
  (let ((unix-time-offset (load-time-value (encode-universal-time 0 0 0 1 1 1970 0))))
    (let ((checkout (ensure-source-cache source)))
      (with-posix-cwd checkout
	(with-run-output (stream ("git" "show" "-s" "--format=%ct"))
          (let ((unix-time-string (read-line stream)))
	    (+ (parse-integer unix-time-string) unix-time-offset)))))))

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
    (format nil "refs/tags/~A" (tarball-target source)))
  (:method ((source git-at-commit-source))
    (format nil "~A" (commit source))))

(defstruct (submodule (:type vector))
  name
  path
  sha1)

(defun git-submodules (git-path)
  (loop for line in
                 (run-output-lines "git" "-C" (truename git-path) "submodule"
                                   "--quiet"
                                   "foreach"
                                   "--recursive"
                                   "echo $name $sha1 $displaypath")
        for (name sha1 path) = (split-spaces line)
        collect (make-submodule :name (encode-string-for-filesystem name)
                                :path path
                                :sha1 sha1)))

(defun full-git-archive (git-path target-ref prefix output-file)
  "Create a tarball archive in OUTPUT-FILE of the full contents of the
git checkout GIT-PATH, including submodules. The repo is archived at
TARGET-REF, e.g. 'HEAD'. "
  (run "git" "-C" (truename git-path)
       "submodule" "update" "--init" "--recursive")
  (let ((submodules (git-submodules git-path)))
    (in-temporary-directory prefix
      (let* ((temp-base *default-pathname-defaults*))
        (with-posix-cwd git-path
          (run "git" "archive"
               :format "tar"
               :prefix (format nil "~A/" prefix)
               "-o" (make-pathname :name (string-right-trim "/" prefix)
                                   :type "tar"
                                   :defaults temp-base)
               target-ref)
          (dolist (submodule submodules)
            (with-posix-cwd (submodule-path submodule)
              (let ((output (make-pathname :name (submodule-name submodule)
                                           :type "tar"
                                           :defaults temp-base)))
                (run "git" "archive"
                     :format "tar"
                     "-o" output
                     :prefix (format nil "~A/~A/"
                                     prefix (submodule-path submodule))
                     (submodule-sha1 submodule))))))
        ;; Back in the temp directory
        (dolist (tarball (directory "*.tar"))
          (run "tar" "xf" tarball))
        (let ((combined "combined.tar")
              (combined-tgz "combined.tar.gz"))
          (run "tar" "cf" combined prefix)
          (run "gzip" "-vn9" "-S" ".gz" combined)
          (rename-file combined-tgz output-file))
        output-file))))

(defmethod make-release-tarball ((source git-source) output-file)
  (let ((prefix (release-tarball-prefix source))
        (checkout (ensure-source-cache source)))
    (full-git-archive checkout (target-ref source) prefix output-file)))

