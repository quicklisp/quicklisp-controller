(defpackage #:qlc-github-issues
  (:use #:cl)
  (:export #:get-issues-plist
           #:matching-issue)
  (:import-from #:function-cache
                #:defcached)
  (:import-from #:drakma
                #:http-request))

(in-package #:qlc-github-issues)

(defvar *issues-json-url*
  "https://api.github.com/repos/quicklisp/quicklisp-projects/issues")

(defcached (get-issues-json :timeout 300) ()
  (multiple-value-bind (document status headers)
      (http-request *issues-json-url*)
    (declare (ignore headers))
    (unless (eql status 200)
      (error "Unexpected status from ~A: ~A" *issues-json-url* status))
    (trivial-utf-8:utf-8-bytes-to-string document)))

(defun clear-cache ()
  (function-cache:clear-cache *get-issues-json-cache*))

(defun get-raw-issues-data ()
  (yason:parse (get-issues-json)))

(defun get-issues-plist ()
  (let ((issues (get-raw-issues-data)))
    (loop for table in issues
          collect (list :title (gethash "title" table)
                        :number (gethash "number" table)
                        :body (gethash "body" table)))))

(defun matching-issue (substring)
  "Return the FIRST issue that matches SUBSTRING. Matches in titles
are preferred."
  (block found
    (dolist (issue (get-issues-plist))
      (when (search substring (getf issue :title)
                    :test 'equalp)
        (return-from found issue)))
    (dolist (issue (get-issues-plist))
      (when (search substring (getf issue :body)
                    :test 'equalp)
        (return-from found issue)))))





