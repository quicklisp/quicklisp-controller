;;; depcheck.lisp

(defpackage #:depcheck
  (:use #:cl))

(in-package #:depcheck)

(defvar *direct-dependencies* nil)

(defun load-asdf-system-table (file)
  (let ((table (make-hash-table :test 'equalp)))
    (with-open-file (stream file)
      (loop for line = (read-line stream nil)
            while line do
            (let ((pathname
                   (merge-pathnames line
                                    file)))
              (setf (gethash (pathname-name pathname) table)
                    (truename pathname)))))
    table))

(defvar *systems* nil)

(defun real-system-name (name)
  (setf name (string name))
  (subseq name 0 (position #\/ name)))

(defun system-finder (name)
  (when *systems*
    (gethash (real-system-name name) *systems*)))

(defun sbcl-contrib-p (name)
  (and (<= 3 (length name))
       (string= name "sb-" :end1 3)))

(defun normalize-dependency (name)
  (asdf/find-component:resolve-dependency-spec nil name))

;; Suggestion(fare): back in the days of ASDF 2.014.6, this was necessary,
;; but with ASDF 3, an advice :around function asdf::register-system-definition would be simpler.
;; See also https://bugs.launchpad.net/asdf/+bug/1265700 and some 2013 discussions on asdf-devel.

(defvar *original-register-system-definition* #'asdf::register-system-definition)

(defun register-system-definition-hook (name &rest options
                                        &key depends-on weakly-depends-on defsystem-depends-on
                                        &allow-other-keys)
  (setf *direct-dependencies* (append depends-on weakly-depends-on defsystem-depends-on))
  (apply *original-register-system-definition* name options))

(setf (symbol-function 'asdf::register-system-definition) #'register-system-definition-hook)

(defvar *in-find-system* nil)
(defvar *implied-dependencies* nil)

(defvar *load-op-wrapper*
  '(defmethod asdf:operate :around ((op (eql 'asdf:load-op)) system
                                    &key &allow-other-keys)
    (cond (*in-find-system*
           (push (asdf::coerce-name system) *implied-dependencies*)
           (let ((*in-find-system* nil))
             (call-next-method)))
          (t
           (call-next-method)))))

(defvar *metadata-required-p* nil)

(defun check-system-metadata (system)
  (when *metadata-required-p*
    (flet ((check-attribute (fun description)
             (let ((value (funcall fun system)))
               (cond ((not value)
                      (error "Missing ~A for system ~A"
                             description
                             (asdf:component-name system)))
                     ((and (stringp value) (zerop (length value)))
                      (error "Empty ~A for system ~A"
                             description
                             (asdf:component-name system))))
               (when (and (stringp value) (zerop (length value)))))))
      (check-attribute 'asdf:system-description :description)
      ;; Not yet
      ;;(check-attribute 'asdf:system-license :license)
      (check-attribute 'asdf:system-author :author))))

(defun compute-dependencies (system-file system-name)
  (assert (equal system-name) (primary-system-name system-name)) ;; currently only works on primary-systems
  (let ((asdf:*system-definition-search-functions*
          (list 'system-finder))
        (dependencies nil)
        (*direct-dependencies* nil))
    (let ((*implied-dependencies* nil)
          (*in-find-system* t))
      (check-system-metadata (asdf:find-system system-file))
      (setf dependencies *implied-dependencies*))
    (asdf:load-system system-name)
    (setf dependencies
          (remove system-name
                  (remove-duplicates
                   (mapcar 'asdf::primary-system-name
                           (remove nil
                                   (mapcar 'normalize-dependency
                                           (append *direct-dependencies* dependencies))))
                   :test #'equalp)))
    (sort (remove-if #'sbcl-contrib-p dependencies) #'string<)))

(defun magic (system-file system trace-file)
  (handler-bind ((sb-ext:defconstant-uneql #'continue))
    (with-open-file (stream trace-file :direction :output
                            :if-exists :supersede)
      (format stream "~A~{ ~A~}~%"
              system (compute-dependencies system-file system)))))

(defun main (argv)
  (setf *print-pretty* nil)
  (sb-posix:setenv "SBCL_HOME"
                   (load-time-value
                    (directory-namestring sb-int::*core-string*))
                   1)
  (sb-posix:setenv "CC" "gcc" 1)
  (eval *load-op-wrapper*)
  (destructuring-bind (index project system dependency-file errors-file
                             &optional *metadata-required-p*)
      (rest argv)
    (setf *systems* (load-asdf-system-table index))
    (with-open-file (*error-output* errors-file
                                    :if-exists :supersede
                                    :direction :output)
      (unless (sb-posix:getenv "DEPCHECK_DEBUG")
        (sb-ext:disable-debugger))
      (unwind-protect
           (magic project system dependency-file)
        (ignore-errors (close *error-output*))))
    (when (probe-file dependency-file)
      (delete-file errors-file))))

