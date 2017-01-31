(require 'sb-posix)

(let* ((home (user-homedir-pathname))
       (homebin (merge-pathnames "bin/" home)))
  (load "~/quicklisp/setup.lisp")
  (sb-posix:setenv "PATH" (format nil "/bin:/usr/bin:/usr/local/bin:~A" homebin) 1))

(defvar *base-directory* *load-pathname*)
(defvar *log-directory* (merge-pathnames "logs/" *base-directory*))

(ql:quickload "quicklisp-controller")

(multiple-value-bind (second minute hour day month year)
    (get-decoded-time)
  (let* ((month-string (format nil "~4,'0D-~2,'0D" year month))
	 (file-name (format nil "~4,'0D~2,'0D~2,'0D~2,'0D~2,'0D~2,'0D"
			    year month day hour minute second))
	 (log-relative-pathname
	  (make-pathname :directory (list :relative month-string)
			 :name file-name
			 :type "txt"))
	 (log-file (merge-pathnames log-relative-pathname *log-directory*)))
    (ensure-directories-exist log-file)
    (quicklisp-controller::recrank-to-file  log-file)))
