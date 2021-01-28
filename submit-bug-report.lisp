;;;; submit-bug-report.lisp
;;;;
;;;; Semiautomatically create bug reports to send to projects for
;;;; systems that fail.
;;;;
;;;; This is based around the data in a failing-system object, defined
;;;; in html-failure-report.lisp
;;;;

(in-package #:quicklisp-controller)

(defun github-source-p (source)
  (search "/github.com/" (location source)))

(deftype github-source ()
  `(satisfies github-source-p))

(defun github-owner (failing-system)
  (nth-value 0 (github-owner-and-repo (location (source failing-system)))))

(defun github-repo (failing-system)
  (nth-value 1 (github-owner-and-repo (location (source failing-system)))))

(defun submit-github-issue (owner repo title body)
  (githappy::create-repo-issue
   :owner owner
   :repo repo
   :body (githappy:js "title" title "body" body)))
			       

(defun existing-bug-reports (source)
  (multiple-value-bind (owner repo)
      (github-owner-and-repo (location source))
    (unless owner
      (error "Not a github repo"))
    (let* ((response (githappy:repo-issues :owner owner :repo repo :per-page 100))
	   (json (githappy:json response)))
      (mapcan
       (lambda (issue)
	 (when (equal "quicklisp" (githappy:jref issue '("user" "login")))
	   (list (list :number (githappy:jref issue "number")
		       :title (githappy:jref issue "title")))))
       json))))

(defun blameless-for-failure-p (failing-source)
  (null (remove-if #'broken-by (failure-data failing-source))))

(defun bug-report-body (failing-source &key log-link)
  (with-output-to-string (s)
    (format s "Building with ~A for quicklisp dist creation.~%~%"
	    (versions-and-such))
    (format s "Trying to build commit id ~A~%~%" (commit-id (source failing-source)))
    (dolist (system (failure-data failing-source))
      (format s "*~A* fails to build" (system-name system))
      (if (broken-by system)
	  (format s " because of a failure in  _~A_.~%~%"
		  (system-name (broken-by system)))
	  (format s " with the following error:~%~%```~%~A~&```~%~%"
		  (failure-snippet system))))
    (when log-link
      (format s "[Full log here](~A)~%~%" log-link))))

(defun report-bug-stuff (source)
  (setf source (source-designator source))
  (let* ((failing-source (find-failing-source source))
	 (log-link (publish-source-failure source))
	 (body (bug-report-body failing-source :log-link log-link))
	 (title "Some systems failed to build for Quicklisp dist"))
    (list :title title
	  :body body)))

(defun report-bug-in (source)
  (setf source (source-designator source))
  (let* ((failing-source (find-failing-source source))
	 (log-link (publish-source-failure source))
	 (body (bug-report-body failing-source :log-link log-link)))
    (multiple-value-bind (owner repo)
	(github-owner-and-repo (location source))
      (format t "Posting bug report for ~A~%~%" source)
      (format t "~A" body)
      (let ((existing (existing-bug-reports (source failing-source))))
	(when existing
	  (format t "WARNING: BUGS ALREADY SUBMITTED BY quicklisp:~%~{  ~A~%~}~%"
		  existing)))
      (when (ql-util:press-enter-to-continue)
	(submit-github-issue owner repo "Some systems failed to build for Quicklisp dist" body)))))

