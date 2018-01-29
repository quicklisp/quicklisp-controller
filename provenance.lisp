(in-package #:quicklisp-controller)

(defvar *provenance-context* nil
  "This variable is used to store provenance updates in progress.")

(defun serializable-provenance-object-p (object)
  (or (and (consp object)
	   (every #'serializable-provenance-object-p object))
      (typep object '(or keyword string integer boolean))))

(defun utf8-decode (octets)
  (read-from-string (babel:octets-to-string octets :encoding :utf-8)))

(defun utf8-encode (object)
  (unless (serializable-provenance-object-p object)
    (error "Cannot serialize ~S" object))
  (babel:string-to-octets (prin1-to-string object) :encoding :utf-8))

(defun load-provenance-db (db-file)
  (let ((table (make-hash-table :test 'equal)))
    (when (probe-file db-file)
      (zcdb:map-cdb
       (lambda (raw-key raw-value)
	 (let ((key (utf8-decode raw-key))
	       (value (utf8-decode raw-value)))
	   (setf (gethash key table) value)))
       db-file))
    table))

(defun save-provenance-db (table db-file)
  (zcdb:with-output-to-cdb (cdb db-file (make-pathname :type "cdb-tmp"
						       :defaults db-file))
    (maphash (lambda (key value)
	       (zcdb:add-record (utf8-encode key)
				(utf8-encode value)
				cdb))
	     table)))

(defun call-with-provenance-context (db-file fun)
  (setf db-file (merge-pathnames db-file))
  (if *provenance-context*
      (if (equalp db-file (car *provenance-context*))
	  (funcall fun)
	  (error "Nested provenance context - ~S vs ~S"
		 db-file
		 (car *provenance-context*)))
      (let ((*provenance-context* (cons db-file
					(load-provenance-db db-file))))
	(multiple-value-prog1
	    (funcall fun)
	  (save-provenance-db (cdr *provenance-context*) db-file)))))

(defmacro with-provenance-context ((db-file) &body body)
  `(call-with-provenance-context ,db-file (lambda () ,@body)))

(defun ensure-provenance-context ()
  (unless *provenance-context*
    (error "No provenance context is in effect!"))
  (cdr *provenance-context*))

(defun save-provenance (project key &rest plist &key &allow-other-keys)
  (let ((table (ensure-provenance-context)))
    (push (list key plist) (gethash project table))
    plist))

