;;;; upstream-misc.lisp

(in-package #:quicklisp-controller)

(defclass location-templated-source (upstream-source)
  ((location-template
    :initarg :location-template
    :accessor location-template)))

(defmethod location ((source location-templated-source))
  (format nil (location-template source) (project-name source)))

(defclass predictable-http-source (location-templated-source http-source)
  ())

(defclass xach-http-source (predictable-http-source) ()
  (:default-initargs
   :location-template "http://www.xach.com/lisp/~A.tgz"))


(defclass ediware-http-source (predictable-http-source) ()
  (:default-initargs
   :location-template "http://weitz.de/files/~A.tar.gz"))


(defclass froydware-http-source (predictable-http-source) ()
  (:default-initargs
   :location-template "http://method-combination.net/lisp/files/~A.tar.gz"))

(defclass svcware-http-source (predictable-http-source) ()
  (:default-initargs
   :location-template "http://homepage.mac.com/svc/~A/~:*~A.tar.gz"))


(defclass predictable-darcs-source (location-templated-source darcs-source)
  ())

(defclass hungarian-darcs-source (predictable-darcs-source) ()
  (:default-initargs
   :location-template "http://dwim.hu/darcs/~A"))

(defclass clnet-darcs-source (predictable-darcs-source) ()
  (:default-initargs
   :location-template "http://common-lisp.net/project/~A/darcs/~:*~A"))


(defclass kmr-git-source (location-templated-source git-source) ()
  (:default-initargs
   :location-template "http://git.b9.com/~A.git"))


;;; Walter C. Pelissero
;;; http://wcp.sdf-eu.org/

(defclass wcpware-http-source (http-source) ())

(defmethod make-release-tarball ((source wcpware-http-source) output-file)
  "WCP's projects are bzip2 archives that unpack into the current
directory. Fix them up to be tarballs with predictable output
directories."
  (let ((prefix (release-tarball-prefix source))
        (cached (ensure-source-cache source)))
    (in-temporary-directory prefix
      (sb-posix:mkdir prefix #o755)
      (with-posix-cwd prefix
        (run "tar" "xjvf" cached))
      (run "tar" :owner 0 :group 0 "-cvf" "wcp.tar" prefix)
      (run "gzip" "-vn9" "wcp.tar")
      (copy "wcp.tar.gz" output-file))))
