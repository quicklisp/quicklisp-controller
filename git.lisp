;;;; git.lisp

(in-package #:quicklisp-controller)

;;; Stuff for automatically committing */source.txt files with
;;; appropriate github messages.

(defun call-in-projects-directory (fun)
  (with-posix-cwd
      (translate-logical-pathname "quicklisp-controller:projects;")
    (funcall fun)))

(defmacro in-projects-directory (&body body)
  `(call-in-projects-directory (lambda () ,@body)))

(defun clean-stage-p ()
  (in-projects-directory
    (null (run-output-lines "git" "status" "--porcelain"
                            "--untracked-files=no"))))

(defun commit-message (source)
  (setf source (source-designator source))
  (let ((issue-number (github-issue-number source)))
    (unless issue-number
      (error "No github issue info for substring ~S" (name source)))
    (format nil "Added ~A per issue #~A"
            (name source)
            issue-number)))

(defun commit-source (source &key commit-message unclean)
  (let* ((source (source-designator source))
         (file (source-file source))
         (message (or commit-message
                      (commit-message source))))
    (unless (or unclean (clean-stage-p))
      (error "Stage isn't clean"))
    (in-projects-directory
      (run "git" "add" (pathname file))
      (run "git" "commit" "-m" message))
    (list :committed (name source) :with message)))

(defun set-issue-label (source label)
  (setf source (source-designator source))
  (let ((issue-number (github-issue-number source)))
    (unless issue-number
      (error "Can't find issue number for ~A" source))
    (githappy:modify-repo-issue :owner "quicklisp"
                                :repo "quicklisp-projects"
                                :number issue-number
                                :body (githappy:js "labels" (list label)))))

(defun mark-canbuild (source)
  (set-issue-label source "canbuild"))

(defun mark-cantbuild (source)
  (set-issue-label source "cantbuild"))

(defun push-projects ()
  (with-posix-cwd (translate-logical-pathname "quicklisp-controller:projects;")
    (run "git" "push" "origin" "master")))

(defun pull-projects ()
  (with-posix-cwd (translate-logical-pathname "quicklisp-controller:projects;")
    (run "git" "pull")))
