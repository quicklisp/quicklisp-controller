;;;; quicklisp-controller.asd

(asdf:defsystem #:quicklisp-controller
  :description "Build Quicklisp dists."
  :license "BSD"
  :author "Zachary Beane <zach@quicklisp.org>"
  :depends-on (#:quicklisp
               #:zs3
               #:zcdb
               #:cl-ppcre
               #:alexandria
               #:drakma
               #:yason
               #:function-cache
	       #:trivial-utf-8
               #:ironclad
               #:lparallel
	       #:cl-who
               #:ubiquitous
               #:githappy
               #:project-info
	       #:westbrook)
  :serial t
  :components ((:file "tarhash")
	       (:file "github-issues")
               (:file "package")
               (:file "config")
               (:file "logical-host")
               (:file "commands")
               (:file "utils")
               (:file "setup")
	       (:file "provenance")
               (:file "upstream")
	       (:file "system-file-magic-cache")
               (:file "dist-cache")
               (:file "upstream-vcs")
               (:file "upstream-http")
               (:file "upstream-file")
               (:file "upstream-cvs")
               (:file "upstream-darcs")
               (:file "upstream-git")
               (:file "upstream-github")
               (:file "upstream-mercurial")
               (:file "upstream-svn")
               (:file "upstream-bzr")
               (:file "upstream-misc")
               (:file "mail")
               (:file "indexes")
               (:file "misc")
               (:file "client-uploader")
               (:file "fail-dwim")
               (:file "map-systems")
               (:file "update-client-version")
               (:file "descriptions")
               (:file "ng-indexes")
	       (:file "git")
	       (:file "html-failure-report")
	       (:file "rss-failure-feeds")
               (:file "recrank")
               (:file "irepl")))

(defpackage #:quicklisp-controller-config
  (:use)
  (:export #:*base-directory*))

(defvar quicklisp-controller-config:*base-directory*
  (make-pathname :name nil
                 :type nil
                 :version nil
                 :defaults (or *load-truename*
                               *compile-file-truename*)))
