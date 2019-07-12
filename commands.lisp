;;;; commands.lisp

(in-package #:quicklisp-controller)

(defvar *command-output* (make-synonym-stream '*standard-output*))

(define-condition run-error (error)
  ((command
    :initarg :command
    :reader run-error-command)
   (arguments
    :initarg :arguments
    :reader run-error-arguments)
   (exit-code
    :initarg :exit-code
    :reader run-error-exit-code))
  (:report (lambda (condition stream)
             (format stream "Run error (exit code ~D) for:~%  ~S ~{~S~^ ~}"
                     (run-error-exit-code condition)
                     (run-error-command condition)
                     (run-error-arguments condition)))))

(defun prepare-command-argument (argument)
  (typecase argument
    (null nil)
    (list (mapcan #'prepare-command-argument argument))
    (string (list argument))
    (pathname (list (native-namestring argument)))
    (keyword (list (format nil "--~(~A~)" argument)))
    (t (list (princ-to-string argument)))))

(defun run (command &rest arguments)
  (let* ((arguments (mapcan #'prepare-command-argument arguments))
         (process (run-program command arguments
                               :search t
                               :wait t
                               :output *command-output*)))
    (unwind-protect
         (let ((code (process-exit-code process)))
           (if (zerop code)
               t
               (error 'run-error
                      :exit-code code
                      :command command
                      :arguments arguments)))
      (ignore-errors (process-close process)))))

(defmacro with-run-output ((stream (command &rest args)) &body body)
  `(let* ((*command-output* (make-string-output-stream)))
     (run ,command ,@args)
     (with-input-from-string (,stream (get-output-stream-string  *command-output*))
       ,@body)))

(defun native-directory-string (pathname)
  (native-namestring (directory-namestring (probe-file pathname))))

(defmacro with-posix-cwd (new-directory &body body)
  ;; fchdir thing from Linux's getcwd(3)
  (let ((fd (gensym))
        (new (gensym)))
    `(let ((,fd nil)
           (,new (native-directory-string ,new-directory)))
       (unwind-protect
            (let ((*default-pathname-defaults* (probe-file ,new)))
              (setf ,fd (sb-posix:open "." 0))
              (sb-posix:chdir ,new)
              ,@body)
         (when ,fd
           (sb-posix:fchdir ,fd)
           (ignore-errors (sb-posix:close ,fd)))))))

(defmacro with-binary-run-output (pathname &body body)
  `(with-open-file (*command-output* ,pathname :direction :output
                                     :element-type '(unsigned-byte 8)
                                     :if-exists :supersede)
     ,@body))

(defmacro without-run-output (&body body)
  `(let ((*command-output* nil))
     ,@body))

(defun run-output-lines (command &rest args)
  (let ((output (with-output-to-string (*command-output*)
                  (apply #'run command args))))
    (with-input-from-string (stream output)
      (loop for line = (read-line stream nil)
            while line collect line))))

(defun run-output-line (command &rest args)
  (first (apply #'run-output-lines command args )))
