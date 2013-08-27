;;;; upstream-file.lisp

(in-package #:quicklisp-controller)

(defclass single-file-source (http-source) ())

(defgeneric single-file-name (source)
  (:method ((source single-file-source))
    (let ((slash (position #\/ (location source) :from-end t)))
      (string-downcase (subseq (location source) (1+ slash))))))

(defgeneric write-single-file-readme (source file)
  (:method (source file)
    (with-open-file (stream file :direction :output)
      (format stream "This is a packaging of ~A for quicklisp.~%~%"
              (project-name source))
      (format stream "It was downloaded from:~%~%    ~A~%~%"
              (location source))
      (format stream "For more information about quicklisp, see:~%~%")
      (format stream "    http://www.quicklisp.org/~%~%"))))


(defun single-file-asd-form (source)
  `(asdf:defsystem ,(make-symbol (string-upcase (project-name source)))
     :components ((:file ,(pathname-name (single-file-name source))))))

(defgeneric write-single-file-asd (source file)
  (:method (source file)
    (with-open-file (stream file :direction :output)
      (let ((*print-case* :downcase)
            (*package* (find-package :keyword)))
        (format stream ";;;; ~A, automatically created by quicklisp~%~%"
                (project-name source))
        (format stream "~S~%" (single-file-asd-form source))))))

(defmethod make-release-tarball ((source single-file-source)
                                 output-file)
  (let ((prefix (release-tarball-prefix source))
        (cached (ensure-source-cache source))
        (file (single-file-name source)))
    (in-temporary-directory "release/"
      (sb-posix:mkdir prefix #o755)
      (with-posix-cwd prefix
        (copy cached file)
        (write-single-file-readme source "README-QL.txt")
        (write-single-file-asd source (make-pathname :name (pathname-name file)
                                                     :type "asd")))
      (run "tar" "czvf" output-file prefix))))
