;;;; descriptions.lisp

(in-package #:quicklisp-controller)

;;;
;;; Create a description .cdb file for a mock dist
;;; based on the dist build descriptions file
;;;

(defun descriptions-table ()
  (let ((table (make-string-table)))
    (with-open-file (stream #p"qlc:descriptions.txt")
      (loop for (system description) = (read stream nil)
            while system do
            (when (plusp (length description))
              (setf (gethash system table) description))))
    table))

(defun encode-description (string)
  (let ((octets (babel:string-to-octets string :encoding :utf-8)))
    (remove-if (lambda (code)
                 (< code 32))
               octets)))

(defun write-dist-descriptions-cdb (dist-name)
  (let* ((dist (or (ql-dist:dist dist-name)
                  (error "Unknown dist ~S" dist-name)))
         (file (ql-dist:relative-to dist "descriptions.cdb"))
         (tmp (ql-dist:relative-to dist "descriptions.tmp"))
         (descriptions (descriptions-table)))
    (zcdb:with-output-to-cdb (cdb file tmp)
      (dolist (system (ql-dist:provided-systems dist))
        (let* ((name (ql-dist:name system))
               (description (gethash name descriptions)))
          (restart-bind ((skip (lambda (&optional val)
                                 (declare (ignore val))
                                 (go :skip))))
            (when description
            (zcdb:add-record (ascii-encode name)
                             (encode-description description)
                             cdb))))
        :skip))))
