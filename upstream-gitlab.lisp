;;;; upstream-gitlab.lisp
;;;;
;;;; Fetch some basic info about a source from gitlab
;;;;

(in-package #:quicklisp-controller)

(defun gitlab-project-id (url)
  (ppcre:register-groups-bind (user project)
      ("gitlab.com/(.*?)/(.*?)(.git)?$" url)
    (when user
      (format nil "~A%2F~A" user project))))

(defun gitlab-project-release-json (url)
  (let* ((uri (format nil "https://gitlab.com/api/v4/projects/~A/releases"
		      (gitlab-project-id url)))
	 (request (make-instance 'githappy::request
				 :uri uri))
	 (response (githappy::submit request))
	 (json (yason:parse (githappy::utf8-string (githappy::body response)))))
    json))

(defun gitlab-latest-tag-info (url)
  (let ((json (gitlab-project-release-json url)))
    (when json
      (let ((tag-data (githappy:jref json '(0 "tag_name")))
	    (sources (githappy:jref json '(0 "assets" "sources"))))
	(dolist (source sources)
	  (when (equal (githappy:jref source "format") "tar.gz")
	    (return (list :tag tag-data
			  :url (githappy:jref source "url")))))))))


(defclass latest-gitlab-release-source (latest-github-release-source)
  ())

(defmethod github-info-plist ((source latest-gitlab-release-source))
  (gitlab-latest-tag-info (location source)))
