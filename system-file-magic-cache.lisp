;;;; system-file-magic-cache.lisp

(in-package #:quicklisp-controller)

(defun system-file-magic (system-name)
  (ensure-system-file-index)
  (ensure-in-anonymous-directory
    (let ((output-file #p"sfm.txt"))
      (run "system-file-magic"
           (native (translate-logical-pathname *system-file-index-file*))
           system-name
           (native (translate-logical-pathname output-file)))
      (rest (split-spaces (first-line-of output-file))))))

(defun system-file-magic-cache-file (file)
  (let* ((digest (file-md5 file))
         (pathname (make-pathname :directory '(:relative
                                               ".cache"
                                               "system-file-magic")
                                  :name digest
                                  :type "cache")))
    (merge-pathnames pathname (user-homedir-pathname))))

(defun system-file-magic-system-cache-file (system)
  (let* ((table (ensure-system-file-index))
         (system-file (gethash system table)))
    (unless system-file
      (error "Unknown system -- ~S" system))
    (system-file-magic-cache-file system-file)))

(defun find-system-file-systems (system)
  (let ((cache-file (system-file-magic-system-cache-file system)))
    (when (probe-file cache-file)
      (values (first-form-of cache-file) t))))

(defun cache-system-file-systems (system systems)
  (let ((cache-file (system-file-magic-system-cache-file system)))
    (ensure-directories-exist cache-file)
    (save-form systems cache-file)))

(defun compute-system-file-systems (system)
  (handler-case
      (system-file-magic system)
    (run-error ()
      nil)))

(defun system-file-systems (system)
  (multiple-value-bind (cached foundp)
      (find-system-file-systems system)
    (cond (foundp
           cached)
          (t
           (let ((systems (compute-system-file-systems system)))
             (unless systems
               (warn "No systems in system file for ~S" system))
             (cache-system-file-systems system systems)
             systems)))))
