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

(defun evaluate-feature-expression (expression)
  (labels ((evaluate-boolean (expression)
	     (ecase (first expression)
	       (:and (evaluate-and expression))
	       (:or (evaluate-or expression))
	       (:not (evaluate-not expression))))
	   (evaluate-and (expression)
	     (every #'evaluate-feature-expression (rest expression)))
	   (evaluate-or (expression)
	     (some #'evaluate-feature-expression (rest expression)))
	   (evaluate-not (expression)
	     (not (evaluate-feature-expression (second expression)))))
    (cond ((symbolp expression)
	   (not (not (position expression *features*))))
	  ((consp expression)
	   (evaluate-boolean expression))
	  (t
	   (error "Badly formed feature expression - ~S" expression)))))

(defun dependency-list-dependency (list)
  (ecase (first list)
    ((:version :require) (second list))
    (:feature
     (when (evaluate-feature-expression (second list))
       (normalize-dependency (third list))))))

(defun normalize-dependency (name)
  (cond ((and (consp name)
              (keywordp (first name)))
	 (let ((dependency (dependency-list-dependency name)))
	   (when dependency
	     (string-downcase dependency))))
        ((or (symbolp name) (stringp name))
         (string-downcase name))
        (t (error "Don't know how to normalize ~S" name))))

(defun make-hook (old-hook system-name)
  (lambda (fun form env)
    (when (and (consp form)
               (eq (first form) 'asdf:defsystem)
               (string-equal (second form) system-name))
      (let ((deps (getf (cddr form) :depends-on))
            (prereqs (getf (cddr form) :defsystem-depends-on))
            (weak (getf (cddr form) :weakly-depends-on)))
        (setf deps (append deps prereqs weak))
        (setf *direct-dependencies* (remove nil
					    (mapcar 'normalize-dependency deps)))))
    (funcall old-hook fun form env)))

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
      (check-attribute 'asdf:system-license :license)
      (check-attribute 'asdf:system-author :author))))

(defun compute-dependencies (system-file system-name)
  (let* ((asdf:*system-definition-search-functions*
          (list #-asdf3 'asdf::sysdef-find-asdf
                #+asdf3.1 'asdf::sysdef-package-inferred-system-search
                'system-finder))
         (dependencies nil)
         (*direct-dependencies* nil)
         (*macroexpand-hook* (make-hook *macroexpand-hook* system-name)))
    (let ((*implied-dependencies* nil)
          (*in-find-system* t))
      (asdf:find-system system-file)
      (check-system-metadata (asdf:find-system system-name))
      (force-output *standard-output*)
      (when (equalp system-file system-name)
        (setf dependencies *implied-dependencies*)))
    (asdf:oos 'asdf:load-op system-name)
    (setf dependencies
          (remove-duplicates (append *direct-dependencies* dependencies)
                             :test #'equalp))
    (setf dependencies
          (sort (remove-if #'sbcl-contrib-p dependencies) #'string<))
    dependencies))

(defun loaded-foreign-libraries ()
  (mapcar 'sb-alien::shared-object-namestring sb-sys:*shared-objects*))

(defun magic (system-file system trace-file)
  (with-open-file (stream trace-file :direction :output
                          :if-exists :supersede)
    (format stream "~A~{ ~A~}~{ FFI:~A~}~%"
            system
	    (compute-dependencies system-file system)
	    (loaded-foreign-libraries))))

(defun setenv (name value)
  (let ((r
         (sb-alien:alien-funcall
          (sb-alien:extern-alien "setenv"
                                 (sb-alien:function
                                  sb-alien:int (sb-alien:c-string :not-null t)
                                  (sb-alien:c-string :not-null t) sb-alien:int))
          name value 1)))
    (if (minusp r)
        (error "setenv")
        r)))

(defun getenv (name)
  (let ((r (sb-alien:alien-funcall
            (sb-alien:extern-alien
             "getenv"
             (sb-alien:function (* sb-alien:char) (sb-alien:c-string :not-null t)))
            name)))
    (declare (type (sb-alien:alien (* sb-alien:char)) r))
    (unless (sb-alien:null-alien r)
      (sb-alien:cast r sb-alien:c-string))))

(defun getpid ()
  (sb-alien:alien-funcall
   (sb-alien:extern-alien
    "getpid"
    (sb-alien:function sb-alien:int))))

(defun set-fasl-output-directory (directory)
  (check-type directory pathname)
  (asdf:clear-output-translations)
  (asdf:initialize-output-translations
   `(:output-translations
     (t ,(merge-pathnames #P "**/*.*" directory))
     :ignore-inherited-configuration
     :disable-cache)))

(defun main (argv)
  (sb-alien:alien-funcall
   (sb-alien:extern-alien "disable_lossage_handler" (function sb-alien:void)))
  (setf *print-pretty* nil)
  (sb-ext:disable-debugger)
  (let (index project system dependency-file errors-file
        (args (rest argv)))
    (macrolet ((check-args (&rest vars)
                 `(progn
                    ,@(loop for var in vars
                            for flag = (format nil "--~A"
                                               (string-downcase var))
                            collect
                            `(unless ,var
                               (error "Missing option ~S" ,flag))))))
      (loop
        (when (endp args)
          (check-args index project system dependency-file errors-file)
          (return))
        (let ((arg (pop args)))
          (cond ((equal arg "--index")
                 (setf index (pop args)))
                ((equal arg "--project")
                 (setf project (pop args)))
                ((equal arg "--system")
                 (setf system (pop args)))
                ((equal arg "--dependency-file")
                 (setf dependency-file (pop args)))
                ((equal arg "--errors-file")
                 (setf errors-file (pop args)))
                ;; Optional args follow
                ((equal arg "--asdf-version")
                 (write-line (asdf:asdf-version))
                 (sb-ext:exit :code 0))
                ((equal arg "--sbcl-version")
                 (write-line (lisp-implementation-version))
                 (sb-ext:exit :code 0))
                ((equal arg "--debug")
                 (sb-ext:enable-debugger))
                ((equal arg "--metadata-required")
                 (setf *metadata-required-p* t))
                ((equal arg "--fasl-directory")
                 (let ((path (pop args)))
                   (ensure-directories-exist path)
                   (set-fasl-output-directory (truename path))))
                (t
                 (error "Unknown argument ~S" arg))))))
    (setf *systems* (load-asdf-system-table index))
    (setenv "SBCL_HOME"
            (load-time-value
             (directory-namestring sb-int::*core-string*)))
    (eval *load-op-wrapper*)
    (with-open-file (*error-output* errors-file
                                    :if-exists :supersede
                                    :direction :output)
      (unwind-protect
           (magic project system dependency-file)
        (ignore-errors (close *error-output*))))
    (when (probe-file dependency-file)
      (delete-file errors-file))))

