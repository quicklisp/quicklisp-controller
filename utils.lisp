;;;; utils.lisp

(in-package #:quicklisp-controller)

(defvar *random-alphanumeric*
  (concatenate 'string
               "abcdefghijklmnopqrstuvwxyz"
               "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
               "0123456789"))

(defun random-element (vector)
  (aref vector (random (length vector))))

(defun random-char ()
  (random-element *random-alphanumeric*))

(defun random-string (length)
  (map-into (make-string length) 'random-char))

(defun native (pathname)
  (native-namestring (merge-pathnames pathname)))

(defun prefix-timestamp (&optional (time (get-universal-time)))
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time time 0)
    (declare (ignore second minute hour))
    (format nil "~4,'0D~2,'0D~2,'0D" year month day)))

(defvar *in-temporary-directory*)
(setf (documentation '*in-temporary-directory* 'variable)
      "Bound to the current temporary directory of
      CALL-IN-TEMPORARY-DIRECTORY for the duration of the call. Used
      for checking that a pathname is being generated within a
      directory that will be cleaned up.")

(defun temporary-pathname (pathname)
  (unless (boundp '*in-temporary-directory*)
    (error "Not in a temporary directory scope"))
  (values (ensure-directories-exist (merge-pathnames pathname))))

(defun rm-rf (path)
  (run "rm" "-rf" (native path)))

(defun call-in-temporary-directory (template-pathname fun)
  (flet ((random-temporary ()
           (let* ((parts (pathname-directory template-pathname))
                  (last (first (last parts)))
                  (randomized (format nil "~A-~A" last (random-string 8))))
             (make-pathname :directory (nconc (butlast parts) (list randomized))
                            :defaults template-pathname))))
    (block nil
      (tagbody
       retry
         (let* ((path (random-temporary))
                (*in-temporary-directory* path))
           (handler-case
               (progn
                 (sb-posix:mkdir (native path) #o700)
                 (unwind-protect
                      (with-posix-cwd path
                        (return (funcall fun)))
                   (ignore-errors (rm-rf path))))
             (sb-posix:syscall-error (condition)
               (when (= (sb-posix:syscall-errno condition)
                        sb-posix:eexist)
                 (go retry))
               (error condition))))))))

(defmacro in-temporary-directory (template-pathname &body body)
  "Evaluate BODY with the POSIX working directory and
*default-pathname-defaults set to a temporary directory specified by
TEMPLATE-PATHNAME. The TEMPLATE-PATHNAME will be used to form a unique
name in the filesystem."
  `(call-in-temporary-directory ,template-pathname (lambda () ,@body)))

(defmacro in-anonymous-directory (&body body)
  "Like IN-TEMPORARY-DIRECTORY, but does not require specifying a
template pathname."
  (let ((base (gensym)))
    `(let ((,base #p "quicklisp-controller:tmp;anonymous;"))
       (ensure-directories-exist ,base)
       (with-posix-cwd ,base
         (in-temporary-directory "anonymous/"
           ,@body)))))

(defmacro ensure-in-anonymous-directory (&body body)
  `(if (boundp '*in-temporary-directory*)
       (progn ,@body)
       (in-anonymous-directory ,@body)))

(defun copy (from to)
  (run "cp" (native (truename from)) (native to)))

(defun curl (url output)
  (run "curl" "--location" url "--output" (native output)))

(defparameter *days* #("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))
(defvar *months* #("Jan" "Feb" "Mar" "Apr" "May" "Jun"
                   "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))

(defun http-date-string (&optional (time (get-universal-time)))
  "Return a HTTP-style date string."
  (multiple-value-bind (second minute hour day month year day-of-week)
      (decode-universal-time time 0)
    (let ((*print-pretty* nil))
      (format nil "~A, ~2,'0D ~A ~4,'0D ~2,'0D:~2,'0D:~2,'0D GMT"
              (aref *days* day-of-week)
              day
              (aref *months* (1- month))
              year
              hour
              minute
              second))))

(defun string-digest (string)
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence :md5
                             (babel:string-to-octets string
                                                     :encoding :utf-8))))

(defun merge-logical (pathname logical)
  (merge-pathnames pathname
                   (translate-logical-pathname logical)))

(defun parent-directory (pathname)
  (make-pathname :directory (butlast (pathname-directory pathname))
                 :defaults pathname))

(defun split-spaces (line)
  (ppcre:split " " line))

(defmacro destructure-line (lambda-list line &body body)
  `(destructuring-bind ,lambda-list
       (split-spaces ,line)
     ,@body))

(defun call-for-each-line (fun file)
  (with-open-file (stream file)
    (loop for line = (read-line stream nil)
          while line do (funcall fun line))))

(defmacro for-each-line ((line file) &body body)
  `(call-for-each-line (lambda (,line) ,@body) ,file))

(defun file-lines (file)
  (let ((result '()))
    (for-each-line (line file)
      (push line result))
    (nreverse result)))

(defun ignorable-line-p (line)
  (or (zerop (length line))
      (char= (char line 0) #\#)))

(defun config-file-lines (file)
  (remove-if #'ignorable-line-p (file-lines file)))

(defun save-lines (lines file)
  (with-open-file (*standard-output* file
                                     :direction :output
                                     :if-exists :supersede)
    (map nil #'write-line lines))
  (probe-file file))

(defun save-forms (forms file)
  (with-open-file (*standard-output* file
                                     :direction :output
                                     :if-exists :supersede)
    (map nil 'print forms))
  (probe-file file))

(defun save-form (form file)
  (save-forms (list form) file))

(defun tarball-contents (file)
  (with-run-output (stream ("tar" "tzf" (native file)))
    (loop for line = (read-line stream nil)
          while line collect line)))

(defun tarball-prefix (file)
  (let ((contents (tarball-contents file)))
    (let ((prefix (subseq (first contents) 0
                          (1+ (position #\/ (first contents))))))
      (dolist (entry contents prefix)
        (unless (and (<= (length prefix) (length entry))
                     (string= prefix entry :end2 (length prefix)))
          (error "Tarball ~A lacks consistent prefix output directory"
                 file))))))

(defun tarball-canonical-name (file)
  (string-right-trim "/" (tarball-prefix file)))

(defun repack (input-file new-prefix output-file)
  (in-temporary-directory "repack/"
    (let ((old-prefix (tarball-prefix input-file)))
      (run "tar" "xf" input-file)
      (unless (equal old-prefix new-prefix)
        (run "mv" old-prefix new-prefix))
      (run "tar" :owner 0 :group 0 "-cf" "repack.tar" new-prefix)
      (run "gzip" "-n9" "repack.tar")
      (copy "repack.tar.gz" output-file)))
  (probe-file output-file))

(defun package-exported-symbols (package)
  (let ((symbols '()))
    (do-external-symbols (symbol package (sort symbols #'string<))
      (push symbol symbols))))

(defun dump-symbols (prefix)
  "Display a list of all external symbols in the image prefixed by PREFIX."
  (let ((packages (sort (list-all-packages) #'string< :key #'package-name))
        (ignored-packages (mapcar #'find-package '(:keyword :cl))))
    (dolist (package packages)
      (unless (member package ignored-packages)
        (dolist (symbol (package-exported-symbols package))
          (format t "~A ~A ~S~%" prefix (package-name package) symbol))))))

(defun make-string-table (&key case-sensitive)
  (make-hash-table :test (if case-sensitive
                             'equal
                             'equalp)))

(defun first-line-of (file)
  (with-open-file (stream file)
    (read-line stream)))

(defun first-form-of (file)
  (with-open-file (stream file)
    (read stream)))

(defun file-size (file)
  (with-open-file (stream file)
    (file-length stream)))

(defun file-md5 (file)
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-file :md5 file)))

(defun dist-string (&optional (timestamp (get-universal-time)))
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time timestamp 0)
    (declare (ignore second minute hour))
    (format nil "~4,'0D-~2,'0D-~2,'0D" year month day)))


(defgeneric base-directory (object)
  (:method ((object pathname))
    (merge-pathnames object))
  (:method ((object string))
    (base-directory (pathname object))))

(defgeneric relative-to (object pathname)
  (:documentation "Merge PATHNAME with the BASE-DIRECTORY of OBJECT.")
  (:method (object pathname)
    (merge-pathnames pathname (base-directory object))))

(defun relative-to-system (pathname)
  (merge-pathnames pathname
                   quicklisp-controller-config:*base-directory*))

(defun skip (&optional condition)
  (declare (ignore condition))
  (let ((restart (find-restart 'skip)))
    (when restart
      (invoke-restart restart))))

(defmacro with-skipping (&body body)
  `(handler-bind ((error #'skip))
     ,@body))
