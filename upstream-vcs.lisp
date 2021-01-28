;;;; upstream-vcs.lisp

(in-package #:quicklisp-controller)

(defclass vcs-source (upstream-source)
  ((command
    :initarg :command
    :accessor command)
   (command-arguments
    :initarg :command-arguments
    :accessor command-arguments)
   (checkout-subcommand
    :initarg :checkout-subcommand
    :accessor checkout-subcommand)
   (checkout-subcommand-arguments
    :initarg :checkout-subcommand-arguments
    :accessor checkout-subcommand-arguments)
   (update-subcommand
    :initarg :update-subcommand
    :accessor update-subcommand)
   (update-subcommand-arguments
    :initarg :update-subcommand-arguments
    :accessor update-subcommand-arguments))
  (:default-initargs
   :command-arguments nil
   :checkout-subcommand "co"
   :checkout-subcommand-arguments nil
   :update-subcommand "update"
   :update-subcommand-arguments nil))

(defmethod source-description ((source vcs-source))
  (format nil "~A ~A ~A"
          (command source)
          (checkout-subcommand source)
          (location source)))

(defgeneric vcs-checkout-arguments (source checkout-directory)
  (:method ((source vcs-source) checkout-directory)
    (append (list* (command source)
                   (command-arguments source))
            (list* (checkout-subcommand source)
                   (checkout-subcommand-arguments source))
            (list (location source) (native checkout-directory)))))

(defgeneric vcs-checkout (source checkout-directory)
  (:method ((source vcs-source) checkout-directory)
    (apply #'run (vcs-checkout-arguments source checkout-directory))))

(defgeneric vcs-update-arguments (source checkout-directory)
  (:method ((source vcs-source) checkout-directory)
    (append (list* (command source) (command-arguments source))
            (list* (update-subcommand source)
                   (update-subcommand-arguments source)))))

(defgeneric vcs-update (source checkout-directory)
  (:method ((source vcs-source) checkout-directory)
    (with-posix-cwd checkout-directory
      (apply #'run (vcs-update-arguments source checkout-directory)))))

(defgeneric vcs-checkout (vcs-source checkout-directory)
  (:method ((source vcs-source) checkout-directory)
    (let ((args (append (list (command source))
                        (command-arguments source)
                        (list (checkout-subcommand source))
                        (checkout-subcommand-arguments source)
                        (list (location source) (native checkout-directory)))))
      (apply #'run args))))

(defgeneric export-source (vcs-source export-directory))

(defmethod release-tarball-prefix ((source vcs-source))
  (format nil "~A-~A-~A/"
          (project-name source)
          (prefix-timestamp)
          (command source)))

(defgeneric cached-checkout-directory (source)
  (:method ((source vcs-source))
    (merge-logical (format nil "~A/~A/"
                           (project-name source)
                           (string-digest (location source)))
                   "quicklisp-controller:upstream-cache;vcs;")))

(defmethod ensure-source-cache ((source vcs-source))
  (let ((pathname (cached-checkout-directory source)))
    (unless (probe-file pathname)
      (ensure-directories-exist (parent-directory pathname))
      (vcs-checkout source pathname))
    (probe-file pathname)))

(defmethod update-source-cache ((source vcs-source))
  (let ((pathname (cached-checkout-directory source)))
    (if (probe-file pathname)
        (vcs-update source pathname)
        (ensure-source-cache source))
    (probe-file pathname)))




;;; Tags of some sort

(defclass tagged-mixin ()
  ((tag-data
    :initarg :tag-data
    :accessor tag-data)))

(defmethod source-location-initargs :around ((source tagged-mixin))
  (let ((initargs (call-next-method)))
    (append initargs (list :tag-data))))

(defmethod release-tarball-prefix ((source tagged-mixin))
  (format nil "~A-~A-~A/"
          (project-name source)
          (tag-data source)
          (command source)))
