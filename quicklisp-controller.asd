;;;; quicklisp-controller.asd

(asdf:defsystem #:quicklisp-controller
  :depends-on (#:quicklisp
               #:zs3
               #:zcdb
               #:cl-ppcre
               #:alexandria
               #:drakma
               #:ironclad)
  :serial t
  :components ((:file "config")
               (:file "tarhash")
               (:file "package")
               (:file "setup")
               (:file "logical-host")
               (:file "commands")
               (:file "utils")
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
               (:file "descriptions")))