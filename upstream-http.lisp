;;;; upstream-http.lisp

(in-package #:quicklisp-controller)

(defclass http-source (upstream-source)
  ((location
    :initarg :location
    :accessor location)))

(defmethod release-tarball-prefix ((source http-source))
  (format nil "~A-~A-~A/"
          (project-name source)
          (prefix-timestamp)
          "http"))

(defmethod source-description ((source http-source))
  (format nil "http-fetch ~A" (location source)))

(defgeneric cache-object-file (source))

(defmethod cache-object-file ((source http-source))
  (merge-logical (format nil "~A/~A.dat"
                         (project-name source)
                         (string-digest (location source)))
                 "quicklisp-controller:upstream-cache;http;"))

(defmethod source-cache-timestamp ((source http-source))
  (and (probe-file (cache-object-file source))
       (file-write-date (cache-object-file source))))

(defmethod make-release-tarball ((source http-source) output-file)
  (let ((prefix (release-tarball-prefix source))
        (package (ensure-source-cache source)))
    (in-temporary-directory prefix
      (if (string= (tarball-canonical-name package)
                   (project-name source))
          (repack (native package) prefix output-file)
          (copy package output-file)))))

(defvar *months* #("Jan" "Feb" "Mar" "Apr" "May" "Jun"
                   "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))

(defun parse-http-timestamp (string)
  "Parse e.g. 'Fri, 30 Jun 2006 09:54:00 GMT' to a universal-time."
  (flet ((number-at (start length)
           (parse-integer string :start start :end (+ start length))))
    (let ((day (number-at 5 2))
          (month-string (subseq string 8 11))
          (year (number-at 12 4))
          (hour (number-at 17 2))
          (minute (number-at 20 2))
          (second (number-at 23 2)))
      (let ((month (1+ (position month-string *months* :test #'string=))))
        (encode-universal-time second minute hour day month year 0)))))

(defun modified-time (url)
  (multiple-value-bind (content code headers)
      (drakma:http-request url :method :head)
    (declare (ignore content code))
    (let ((header
           (cdr (assoc :last-modified headers))))
      (when header
        (parse-http-timestamp header)))))

(defmethod find-source-cache ((source http-source))
  (probe-file (cache-object-file source)))

(defmethod create-source-cache ((source http-source))
  (let ((cached (cache-object-file source)))
    (ensure-directories-exist cached)
    (fetch-http-or-https (location source) cached)
    (probe-file cached)))

(defclass http-bz2-source (http-source) ())

(defmethod create-source-cache ((source http-bz2-source))
  (let ((cached (cache-object-file source)))
    (ensure-directories-exist cached)
    (in-temporary-directory "bz2/"
      (fetch-http-or-https (location source) "temp.bz2")
      (with-binary-run-output "temp.dat"
        (run "bunzip2" "-c" "temp.bz2"))
      (run "gzip" "temp.dat")
      (alexandria:copy-file "temp.dat.gz" cached))
    (probe-file cached)))

(defmethod update-source-cache ((source http-source))
  (let* ((cached (cache-object-file source))
         (url-last-modified (modified-time (location source)))
         (file-last-modified (and (probe-file cached)
                                  (file-write-date cached))))
    (cond ((not file-last-modified)
           (ensure-source-cache source))
          ((or (not url-last-modified)
               (< file-last-modified url-last-modified))
           (create-source-cache source)))
    (probe-file cached)))

(defclass naked-http-source (http-source) ()
  (:documentation
   "A 'naked' http source is from a tarball that unpacks into the
   current directory instead of a proper subdirectory."))

(defmethod create-source-cache ((source naked-http-source))
  (let ((cached (cache-object-file source))
        (name (project-name source)))
    (ensure-directories-exist cached)
    (in-temporary-directory (format nil "~A/" name)
      (fetch-http-or-https (location source) "naked.tgz")
      (sb-posix:mkdir name #o755)
      (with-posix-cwd name
        (run "tar" "xzvf" "../naked.tgz"))
      (run "tar" "czvf" cached name))
    (probe-file cached)))


(defclass https-source (http-source) ())

(defmethod create-source-cache ((source https-source))
  (let ((cached (cache-object-file source)))
    (ensure-directories-exist cached)
    (curl (location source) cached)
    (probe-file cached)))

(defun fetch-http-or-https (loc file)
  (handler-case
      (fetch loc file)
    (ql-http:https-redirect ()
      (curl loc file))))
