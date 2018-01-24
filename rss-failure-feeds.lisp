;;;; rss-failure-feeds.lisp
;;;;
;;;; Building on the html failure reports, produces a set of feeds so
;;;; authors and other interested people can subscribe to failures.
;;;;
;;;; Each failing source becomes an item in a feed. There are many
;;;; different feeds - one for each source, one for each author (as
;;;; far as that can be determined from e.g. github username
;;;; patterns), and one for everything.
;;;;
;;;; The feeds don't have any state. They don't preserve old
;;;; items. They are created fresh for every run. (This will change if
;;;; it works poorly in feed readers.)
;;;;
;;;; Feeds are written into a directory based on a failure report
;;;; generated from a (failure-data t) call, then uploaded to the
;;;; failure report bucket all at once.
;;;;

(in-package #:quicklisp-controller)

;;; Grouping sources by author (or other criteria)

(defparameter *location-grouping-patterns*
  '("://github.com/(.*?)/"
    "://bitbucket.org/(.*?)/"
    "(kpe.io)"
    "(xach)"
    "(wcp).sdf-eu.org"
    "(nklein)"
    "gitlab.com/(.*?)/"
    "(hexstreamsoft)"
    "(marijnh)averbeke"
    "(maraist)"
    "(lichteblau)"
    "gitlab.common-lisp.net/(.*?)/"))

(defun scan-first-register (pattern target)
  "Scan TARGET with the regex PATTERN. If there is a match, return the
  first register grouping."
  (multiple-value-bind (start end starts ends)
      (ppcre:scan pattern target)
    (declare (ignore end))
    (when (and start (plusp (length starts)))
      (subseq target (aref starts 0) (aref ends 0)))))

(defun group-name (failing-source)
  (let ((location (location (source failing-source))))
    (dolist (pattern *location-grouping-patterns*)
      (let ((group (scan-first-register pattern location)))
	(when group
	  (return group))))))

;;; Producing feeds

(defun item-contents (failing-source)
  (with-output-to-string (stream)
    (let ((failures (failure-data failing-source)))
      (format stream "<p>Project <b>~A</b><p>~%" (name failing-source))
      (format stream "<p>Taken from <code>~A</code></p>~%"
	      (first-line-of (source-file (source failing-source))))
      (format stream "<p>~A</p>~%" (versions-and-such))
      (format stream "<p>~D failing system~:*~P: ~{~A~^, ~}</p>~%"
	      (length failures)
	      (mapcar 'system-name failures))
      (format stream "<p><a href='~A'>Full build log</a></p>~%"
	      (full-failure-report-url failing-source)))))

(defun item-title (failing-source)
  (format nil "~A" (name (source failing-source))))

(defclass failing-source-item (item)
  ((failing-source
    :reader failing-source
    :initarg :failing-source)))

(defun failing-source-item (failing-source)
  (make-instance 'failing-source-item
		 :failing-source failing-source
		 :title (item-title failing-source)
		 :description (item-contents failing-source)
		 :guid (full-failure-report-url failing-source)
		 :link (full-failure-report-url failing-source)))


(defun feed-link (group)
  (format nil "http://report.quicklisp.org/rss/~A.rss" group))

(defun generate-feeds (failure-report)
  (let ((feeds (make-string-table)))
    (labels ((ensure-feed (title link)
	       (let ((feed (gethash title feeds)))
		 (or feed
		     (setf (gethash title feeds)
			   (make-instance 'feed
					  :pub-date (get-universal-time)
					  :last-build-date (get-universal-time)
					  :title title
					  :link link
					  :description
					  (format nil "Quicklisp build failures for ~S"
						  title)))))))
      (let ((all (ensure-feed "all" (feed-link "all"))))
	(dolist (source (failure-data failure-report) feeds)
	  (let* ((item (failing-source-item source))
		 (group-name (group-name source)))
	    (push item (items all))
	    (when group-name
	      (push item (items (ensure-feed group-name (feed-link group-name)))))))))))

(defun write-feeds (failure-report output-directory)
  (let ((feeds (generate-feeds failure-report)))
    (let ((*default-pathname-defaults* (truename
					(ensure-directories-exist
					 output-directory))))
      (maphash
       (lambda (name feed)
	 (generate-to (make-pathname :type "rss" :name name) feed))
       feeds)
      *default-pathname-defaults*)))

(defun publish-feeds (feed-directory)
  (let ((files (directory (make-pathname :name :wild
					 :type "rss"
					 :defaults feed-directory))))
    (dolist (file files)
      (let ((key (format nil "feeds/~A.rss" (pathname-name file))))
	(upload-report-file file key)))))

