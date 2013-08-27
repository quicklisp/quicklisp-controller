;;;; client-uploader.lisp

(in-package #:quicklisp-controller)

(defparameter *quickstart-bucket* "beta.quicklisp.org")

(defun invalidate-quickstart-paths (paths)
  (let ((distributions (zs3:distributions-for-bucket *quickstart-bucket*))
        (paths (mapcar (lambda (path)
                         (if (char= (char path 0) #\/)
                             path
                             (format nil "/~A" path)))
                       paths)))
    (dolist (distribution distributions)
      (zs3:invalidate-paths distribution paths))))

(defun upload-client-files ()
  (with-posix-cwd "~/src/quicklisp-client/"
    (run "make")
    (let* ((version (first-line-of "version.txt"))
           (versioned-key (format nil "quickstart/quicklisp-~A.tgz" version)))
      (zs3:put-file "quicklisp.tar"
                    *quickstart-bucket* "quickstart/quicklisp.tar"
                    :public t)
      (zs3:put-file (file-namestring versioned-key)
                    *quickstart-bucket* versioned-key
                    :public t)
      (zs3:put-file "version.txt" *quickstart-bucket* "quickstart/version.txt"
                    :public t :content-type "text/plain")
      (invalidate-quickstart-paths (list "/quickstart/version.txt"
                                         "/quickstart/quicklisp.tar"
                                         versioned-key)))))

(defun upload-bootstrap-files ()
  (with-posix-cwd "~/src/quicklisp-bootstrap/"
    (zs3:put-file "asdf.lisp"
                  *quickstart-bucket*
                  "quickstart/asdf.lisp"
                  :public t
                  :content-type "text/plain")
    (zs3:put-file "quicklisp.lisp" *quickstart-bucket* "quicklisp.lisp"
                  :public t
                  :content-type "text/plain"
                  :content-disposition "attachment")
    (zs3:put-file "setup.lisp" *quickstart-bucket* "quickstart/setup.lisp"
                  :public t
                  :content-type "text/plain")
    (invalidate-quickstart-paths '("/quicklisp.lisp"
                                   "/quickstart/asdf.lisp"
                                   "/quickstart/setup.lisp"))))
