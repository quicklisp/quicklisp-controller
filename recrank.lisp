;;;; recrank.lisp

(in-package #:quicklisp-controller)

(defun recrank (&key (update t) (report t)
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
      (mock-report :mail t))))

(defun recrank-to-file (file &rest args &key &allow-other-keys )
  (with-open-file (*command-output* file
                                    :direction :output
                                    :if-exists :supersede)
    (apply #'recrank args)))
