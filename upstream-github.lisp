;;;; github.lisp

(in-package #:quicklisp-controller)

(defun github-owner-and-repo (url)
  (ppcre:register-groups-bind (owner repo)
      ("//github.com/([^/]+?)/([^/]+?)(\\.git)?$" url)
    (values owner repo)))

(defun github-latest-release-info (url)
  (multiple-value-bind (owner repo)
      (github-owner-and-repo url)
    (unless (and owner repo)
      (error "Could not parse ~A as a github repo url" url))
    (let* ((response (githappy:repo-releases :owner owner :repo repo))
           (json (and response (githappy:json response))))
      (list :url (githappy:jref json '(0 "tarball_url"))
            :tag (githappy:jref json '(0 "tag_name"))))))

(defun github-latest-tag-info (url)
  (multiple-value-bind (owner repo)
      (github-owner-and-repo url)
    (unless (and owner repo)
      (error "Could not parse ~A as a github repo url" url))
    (let* ((response (githappy:repo-tags :owner owner :repo repo))
           (json (and response (githappy:json response))))
      (list :url (githappy:jref json '(0 "tarball_url"))
            :tag (githappy:jref json '(0 "name"))))))


(defclass latest-github-release-source (http-source)
  ((release-url
    :initarg :release-url
    :accessor release-url)
   (release-tag
    :initarg :release-tag
    :accessor release-tag)))

(defclass latest-github-tag-source (latest-github-release-source)
  ())

(defgeneric github-info-plist (source)
  (:method ((source symbol))
    (let ((real-source (source-designator source)))
      (unless real-source
        (error "~S does not designate a source" source))
      (github-info-plist real-source)))
  (:method ((source latest-github-release-source))
    (github-latest-release-info (location source)))
  (:method ((source latest-github-tag-source))
    (github-latest-tag-info (location source))))

(defgeneric initialize-release-info (source)
  (:method (source)
    (let ((info (github-info-plist source)))
      (setf (release-tag source) (getf info :tag)
            (release-url source) (getf info :url)))))

(defmethod slot-unbound ((class t) (source latest-github-release-source)
                         (slot-name (eql 'release-url)))
  (initialize-release-info source)
  (release-url source))

(defmethod slot-unbound ((class t) (source latest-github-release-source)
                         (slot-name (eql 'release-tag)))
  (initialize-release-info source)
  (release-tag source))

(defmethod release-tarball-prefix ((source latest-github-release-source))
  (format nil "~A-~A/" (name source) (release-tag source)))

(defmethod create-source-cache ((source latest-github-release-source))
  (let ((cached (cache-object-file source)))
    (ensure-directories-exist cached)
    (curl (release-url source) cached)
    (repack cached (release-tarball-prefix source) cached)
    (probe-file cached)))

