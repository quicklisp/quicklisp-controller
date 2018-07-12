;;;; recrank.lisp

(in-package #:quicklisp-controller)

(defun recrank (&key (update t) (report t) (feeds t)
		  (publish-failure-report t)
                  parallel
                (file #p"quicklisp:tmp;update-failures.txt"))
  (clear-fasl-cache)
  (preflight)
  (clear-dist-caches)
  (when update
    (update-what-you-can :file file :parallel parallel)
    (when (and file *report-to-email*)
      (unless (empty-file-p file)
	(mail-file file
		   :subject "Quicklisp update failures"
		   :from *report-to-email*
		   :to *report-to-email*))))
  (ensure-what-wins-you-can)
  (when report
    (with-skipping
      (mock-report :mail t))
    (when (and publish-failure-report (report-publishing-enabled-p))
      (let* ((report (failure-data t))
	     (url (publish-failure-report :failure-report report)))
	(when feeds
	  (in-anonymous-directory
	    (write-feeds report "feeds/")
	    (publish-feeds "feeds/")))
        (write-line url)))))

(defun recrank-to-file (file &rest args &key &allow-other-keys )
  (with-open-file (*command-output* file
                                    :direction :output
                                    :if-exists :supersede)
    (apply #'recrank :file file args)))
