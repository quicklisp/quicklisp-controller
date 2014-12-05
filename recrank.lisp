;;;; recrank.lisp

(in-package #:quicklisp-controller)

(defun recrank (&key (update t) (report t) (publish-failure-report t)
                (file #p"quicklisp:tmp;update-failures.txt"))
  (clear-fasl-cache)
  (when update
    (update-what-you-can :file file)
    (when (and file *report-to-email*)
      (mail-file file
                 :subject "Quicklisp update failures"
                 :from *report-to-email*
                 :to *report-to-email*)))
  (run "rm" "-rf"
       (native-namestring
        (translate-logical-pathname #p"quicklisp-controller:dist;")))
  (ensure-what-wins-you-can)
  (when report
    (with-skipping
      (mock-report :mail t))
    (when publish-failure-report
      (let ((url (publish-failure-report)))
        (write-line url)))))

(defun recrank-to-file (file &rest args &key &allow-other-keys )
  (with-open-file (*command-output* file
                                    :direction :output
                                    :if-exists :supersede)
    (apply #'recrank args)))
