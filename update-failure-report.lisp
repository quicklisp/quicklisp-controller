(in-package #:quicklisp-controller)

(defun latest-build-log-file ()
  (let ((logs (directory (asdf:system-relative-pathname "quicklisp-controller"
							#p"logs/**/*.txt"))))
    (first (last (sort logs #'string< :key #'namestring )))))

(defun collect-matching (fun stream)
  (loop for line = (next-line stream)
       while line
     when (funcall fun line)
     collect line))

(defun extract-failed-updates (file)
  (with-open-file (stream file)
    (mapcar (lambda (line)
	      (find-source (second (split-spaces line))))
	    (collect-matching (=line-matches "^[*]") (make-peekstream :stream stream)))))

(defun sources-that-failed-to-update ()
  (extract-failed-updates (latest-build-log-file)))
