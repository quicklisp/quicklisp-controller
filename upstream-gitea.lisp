;;;; upstream-gitea.lisp
;;;;
;;;; Fetch some basic info about a source from a gitea/forgejo instance
;;;;

(in-package #:quicklisp-controller)

(defun gitea-project-data (url)
  (ppcre:register-groups-bind (host owner repo)
      ("//([^/]+?)/([^/]+?)/([^/]+?)(\\.git)?$" url)
    (values host owner repo)))

(defun gitea-latest-release-uri (repo-url)
  "Given a URL from a Gitea repo, give a URI to the latest release"
  (multiple-value-bind (host owner repo) (gitea-project-data repo-url)
    (format nil "https://~A/api/v1/repos/~A/~A/releases/latest"
		      host owner repo)))


(defun gitea-project-release-json (url)
  (let* ((uri (gitea-latest-release-uri url))
	 (request (make-instance 'githappy::request
				 :uri uri))
	 (response (githappy::submit request))
	 (json (yason:parse (githappy::utf8-string (githappy::body response)))))
    json))

(defun gitea-latest-release-info (url)
  (let ((json (gitea-project-release-json url)))
    (when json
      (list :tag (githappy:jref json "tag_name")
	    :url (githappy:jref json "tarball_url")))))

(defclass latest-gitea-release-source (latest-github-release-source)
  ())

(defmethod github-info-plist ((source latest-gitea-release-source))
  (gitea-latest-release-info (location source)))
