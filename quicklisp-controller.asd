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
               #:ironclad
               #:lparallel)
  :serial t
  :components ((:file "tarhash")
               (:file "package")
               (:file "config")
               (:file "logical-host")
               (:file "commands")
               (:file "utils")
               (:file "setup")
               (:file "upstream")
               (:file "dist-cache")
               (:file "upstream-vcs")
               (:file "upstream-http")
               (:file "upstream-file")
               (:file "upstream-cvs")
               (:file "upstream-darcs")
               (:file "upstream-git")
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
               (:file "ng-indexes")))
