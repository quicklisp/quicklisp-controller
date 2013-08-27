;;;; update-client-version.lisp

(in-package #:quicklisp-controller)

(defvar *client-directory* "~/src/quicklisp-client/")
(defun client-pathname (pathname)
  (merge-pathnames pathname *client-directory*))

(defun old-client-version ()
  (with-open-file (stream (client-pathname "version.txt"))
    (values (read-line stream))))

(defun new-client-version (&optional (step 0))
  (multiple-value-bind (second minute hour day month year)
      (get-decoded-time)
    (declare (ignore second minute hour))
    (format nil "~4,'0D~2,'0D~2,'0D~2,'0D"
            year month day step)))

(defun replace-version (old-version new-version string)
  (let ((pos (search old-version string)))
    (when pos
      (replace string new-version :start1 pos)
      string)))

(defun update-file-versions (old-version new-version file)
  (let ((temp-file (make-pathname :name (format nil "~A-version"
                                                (pathname-name file))
                                  :defaults file))
        (dirty nil))
    (unwind-protect
         (with-open-file (instream file)
           (with-open-file (outstream temp-file :direction :output
                                      :if-exists :error)
             (loop for line = (read-line instream nil)
                   while line do
                   (let ((replacement (replace-version old-version new-version
                                                       line)))
                     (when replacement
                       (setf dirty t
                             line replacement))
                     (write-line line outstream))))
           (when dirty
             (rename-file temp-file file))
           (values file dirty))
      (when (probe-file temp-file)
        (delete-file temp-file)))))

(defun update-client-versions (&optional (step 0))
  (let ((files (list* (client-pathname "version.txt")
                      (client-pathname "quicklisp.asd")
                      (directory (merge-pathnames "*.lisp"
                                                  *client-directory*))))
        (old-version (old-client-version))
        (new-version (new-client-version step)))
    (dolist (file files)
      (update-file-versions old-version new-version file))))
