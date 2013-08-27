;;;; dist-cache.lisp

(in-package #:quicklisp-controller)

(defgeneric find-cached-release-tarball (source)
  (:method (source)
    (let* ((wild
            (merge-logical
             (make-pathname :name :wild
                            :directory (list :relative
                                             (project-name source)
                                             :wild-inferiors)
                            :type "tgz")
             "quicklisp-controller:dist;tar-cache;"))
           (tarballs (directory wild)))
      (when (cdr tarballs)
        (error "Multiple release tarballs cached for ~A" source))
      (car tarballs))))

(defgeneric cache-release-tarball (source)
  (:method (source)
    (let ((temp (translate-logical-pathname
                 "quicklisp-controller:tmp;cache-release-tarball.tgz")))
      (ensure-directories-exist temp)
      (make-release-tarball source temp)
      (let* ((hash (content-hash temp))
             (prefix (string-right-trim "/" (tarball-prefix temp)))
             (directory (list :relative
                              (project-name source)
                              hash))
             (pathname (merge-logical (make-pathname :name prefix
                                                     :type "tgz"
                                                     :directory directory)
                                      "quicklisp-controller:dist;tar-cache;")))
        (ensure-directories-exist pathname)
        (copy temp pathname)
        (probe-file pathname)))))

(defgeneric ensure-cached-release-tarball (source)
  (:method (source)
    (or (find-cached-release-tarball source)
        (cache-release-tarball source))))


(defun ensure-cached-build-directory (source)
  (setf source (source-designator source))
  (let* ((tarball (ensure-cached-release-tarball source))
         (directory (copy-list (pathname-directory tarball))))
    (setf directory (substitute "build-cache" "tar-cache" directory
                                :test #'string=))
    (let* ((pathname (make-pathname :directory directory)))
      (unless (probe-file pathname)
        (ensure-directories-exist pathname)
        (with-posix-cwd pathname
          (run "tar" "xzvf" tarball)))
      (probe-file pathname))))


(defun build-relative (pathname source)
  (merge-pathnames pathname (ensure-cached-build-directory source)))

;;; System files

(defun blacklist-table (name)
  (let* ((pathname (merge-logical (make-pathname :name name)
                                  #p"quicklisp-controller:projects;qlc-meta;template.txt"))
         (lines (and (probe-file pathname) (config-file-lines pathname)))
         (table (make-hash-table :test 'equalp)))
    (dolist (line lines table)
      (setf (gethash line table) t))))

(defun blacklist-list (name)
  (let ((pathname (merge-logical (make-pathname :name name)
                                  #p"quicklisp-controller:projects;qlc-meta;template.txt")))
    (when (probe-file pathname)
      (config-file-lines pathname))))

(defun make-blacklister (source)
  (lambda (system-file)
    (let ((bad-patterns (blacklist-list "system-pathname-blacklist"))
          (bad-combos (blacklist-table "blacklist"))
          (combo-key (format nil "~A ~A"
                             (project-name source)
                             (pathname-name system-file))))
      (or (gethash combo-key bad-combos)
          (some (lambda (string)
                  (search string (namestring system-file)))
                bad-patterns)))))

(defun blacklistedp (source system-file)
  "Is SYSTEM-FILE for SOURCE somehow forbidden, e.g.  "
  (let ((bad-patterns (blacklist-list "system-pathname-blacklist"))
        (bad-combos (blacklist-table "blacklist"))
        (combo-key (format nil "~A ~A"
                           (project-name source)
                           (pathname-name system-file)))
        (alt-key (format nil "~A ~A"
                         (project-name source)
                         system-file)))
    (or (gethash combo-key bad-combos)
        (gethash alt-key bad-combos)
        (some (lambda (string)
                (search string (namestring system-file)))
              bad-patterns))))

(defun build-system-files-cache-file (source)
  "Return the file in which the system file list is cached."
  (setf source (source-designator source))
  (merge-pathnames "system-files.txt" (ensure-cached-build-directory source)))

(defun build-system-files (source)
  "Return a list of system files in the build directory of SOURCE."
  (setf source (source-designator source))
  (let* ((blacklist-fun (make-blacklister source))
         (base (ensure-cached-build-directory source))
         (wild (merge-pathnames "**/*.asd" base))
         (files (directory wild)))
    (mapcan (lambda (file)
              (unless (funcall blacklist-fun file)
                (list (enough-namestring file base))))
            files)))

(defun cache-build-system-files (source)
  "Save the list of system files for SOURCE."
  (setf source (source-designator source))
  (let ((files (build-system-files source)))
    (save-lines files
                (build-system-files-cache-file source))
    files))

(defun find-build-system-files (source)
  "If there's a cached list of system files for SOURCE available,
return it."
  (setf source (source-designator source))
  (let ((file (build-system-files-cache-file source)))
    (when (probe-file file)
      (file-lines file))))

(defun ensure-build-system-files (source)
  "Return the list of system files for SOURCE, creating & caching it
if needed."
  (setf source (source-designator source))
  (or (find-build-system-files source)
      (cache-build-system-files source)))

(defun system-files (source)
  (setf source (source-designator source))
  (let ((files (ensure-build-system-files source)))
    (remove-if (lambda (file)
                 (blacklistedp source file))
               files)))

(defun system-names (source)
  (setf source (source-designator source))
  (mapcar 'pathname-name (system-files source)))


(defun asdf-systems-table ()
  "Return a hash table that maps system names to system files."
  (let ((table (make-string-table)))
    (map-sources
     (lambda (source)
       (let ((base (ensure-cached-build-directory source))
             (system-files (system-files source)))
         (dolist (file system-files)
           (let ((key (pathname-name file)))
             (when (gethash key table)
               (error "Can't add ~S~%  because of clash with ~S"
                      file (gethash key table)))
             (setf (gethash key table)
                   (merge-pathnames file base)))))))
    table))

(defun save-asdf-system-table (table file)
  (with-open-file (stream file
                          :direction :output
                          :if-exists :supersede)
    (maphash (lambda (system-name system-file)
               (format stream "~A~%"
                       (enough-namestring system-file
                                          (translate-logical-pathname file))))
             table))
  (probe-file file))

(defun load-asdf-system-table (file)
  (let ((table (make-hash-table :test 'equalp)))
    (setf file (translate-logical-pathname file))
    (with-open-file (stream file)
      (loop for line = (read-line stream nil)
            while line do
            (let ((pathname
                   (merge-pathnames line
                                    file)))
              (setf (gethash (pathname-name pathname) table)
                    (truename pathname)))))
    table))


(defvar *system-file-index-file*
  #p"quicklisp-controller:dist;system-file-index")

(defvar *depcheck-fail-file*
  (translate-logical-pathname #p"quicklisp-controller:tmp;depcheck-fail.txt"))

(defun update-system-file-index ()
  (let ((table (asdf-systems-table)))
    (save-asdf-system-table table *system-file-index-file*)
    table))

(defvar *cached-system-file-index*)

(defun ensure-system-file-index ()
  (let ((file *system-file-index-file*))
    (cond ((boundp '*cached-system-file-index*)
           *cached-system-file-index*)
          ((probe-file file)
           (load-asdf-system-table file))
          (t
           (update-system-file-index)))))

(defmacro with-system-index (&body body)
  `(let ((*cached-system-file-index* (ensure-system-file-index)))
     ,@body))

(defvar *system-metadata-required-p* nil
  "If true, a depcheck will fail if :author/:description/:license
  options are missing from a system.")

(defun depcheck (system-name system)
  (ensure-system-file-index)
  (let ((win (translate-logical-pathname #p"quicklisp-controller:tmp;depcheck-win.txt"))
        (fail *depcheck-fail-file*))
    (ignore-errors (delete-file win))
    (ignore-errors (delete-file fail))
    (ignore-errors
      (run "depcheck"
           (native (translate-logical-pathname *system-file-index-file*))
           system-name system win fail *system-metadata-required-p*))
    (let* ((won (probe-file win))
           (first-line (and won (ignore-errors (first-line-of win))))
           (result (and first-line (split-spaces first-line))))
      (unless result
        (when won
          (delete-file win)))
      (values result (probe-file win) (probe-file fail)))))

(defun system-file-magic (system-name)
  (ensure-system-file-index)
  (let ((output-file #p"quicklisp-controller:tmp;sfm.txt"))
    (run "system-file-magic"
         (native (translate-logical-pathname *system-file-index-file*))
         system-name
         (native (translate-logical-pathname output-file)))
    (rest (split-spaces (first-line-of output-file)))))

(defun system-defined-systems (system-name)
  (ensure-system-file-index)
  (ignore-errors (system-file-magic system-name)))

(defun find-fake-winning-systems (source)
  (let ((fake-wins (relative-to source "wins.txt"))
        (*read-eval* nil))
    (when (probe-file fake-wins)
      (with-open-file (stream fake-wins)
        (ignore-errors (read stream))))))

(defun filter-winners (winners)
  (let ((system-file-names (loop for (file-name system-name . deps) in winners
                                 when (string= file-name system-name)
                                 collect file-name)))
    (remove-if-not (lambda (winner)
                     (member (first winner) system-file-names
                             :test 'string=))
                   winners)))

(defun find-winning-systems (source)
  (ensure-system-file-index)
  (or (find-fake-winning-systems source)
      (let* ((winning-files (directory (build-relative "win/*.txt" source)))
             (winners (mapcar #'split-spaces
                              (mapcar #'first-line-of winning-files))))
        (filter-winners winners))))

(defun winning-system-files (source)
  (setf source (source-designator source))
  (ensure-system-file-index)
  (let ((winning-systems (mapcar #'first (find-winning-systems source))))
    (loop for file in (system-files source)
          when  (member (pathname-name file) winning-systems
                        :test #'string=)
          collect file)))

(defun failing-system-files (source)
  (setf source (source-designator source))
  (ensure-system-file-index)
  (directory (build-relative "fail/fail_*.txt" source)))

(defun failing-systems (source)
  (let ((files (mapcar 'pathname-name (failing-system-files source))))
    (mapcar (lambda (name)
              (destructuring-bind (fail project system-file system)
                  (cl-ppcre:split "_" name)
                (declare (ignore fail project))
                (list system-file system)))
            files)))

(defun winfail-file (winfail source system-file system)
  (let ((name (format nil "~A_~A_~A_~A"
                      winfail
                      (project-name source)
                      system-file
                      system)))
    (build-relative (make-pathname :name name
                                   :type "txt"
                                   :directory (list :relative winfail))
                    source)))

(defun map-source-systems (source fun)
  (ensure-system-file-index)
  (setf source (source-designator source))
  (dolist (system-file-name (system-names source))
    (dolist (system (ignore-errors (system-file-magic system-file-name)))
      (unless (blacklistedp source system)
        (funcall fun system-file-name system)))))

(defun acceptable-system-name (name)
  (not (position #\/ name)))

(defun cache-winning-systems (source &key (recheck t))
  "A source may have multiple system files in it. Each system file
might have multiple systems defined in it. Compute the systems which
are loadable for SOURCE and return a list of lists. Each list has the
structure \(SYSTEM-FILE-NAME SYSTEM-NAME &REST DEPENDENCIES). "
  (ensure-system-file-index)
  (setf source (source-designator source))
  (let ((winners '()))
    (map-source-systems
     source
     (lambda (system-name system)
       (when (acceptable-system-name system)
         (let ((cached-winfile (winfail-file "win" source system-name system))
               (cached-failfile (winfail-file "fail" source system-name system)))
           (if (and (not recheck)
                    (probe-file cached-winfile))
               (push (split-spaces (first-line-of cached-winfile)) winners)
               (multiple-value-bind (deps winfile failfile)
                   (depcheck system-name system)
                 (declare (ignore winfile))
                 (cond (deps
                        (ignore-errors (delete-file cached-failfile))
                        (ensure-directories-exist cached-winfile)
                        (save-lines (list (format nil "~A~{ ~A~}"
                                                  system-name deps))
                                    cached-winfile)
                        (push (cons system-name deps) winners))
                       (failfile
                        (ignore-errors (delete-file cached-winfile))
                        (ensure-directories-exist cached-failfile)
                        (copy failfile cached-failfile))
                       (t
                        (error "No deps and no failfile?")))))))))
    winners))

(defun find-more-winning-systems (source)
  (cache-winning-systems source :recheck nil))

(defun ensure-winning-systems (source)
  (or (find-winning-systems source)
      (cache-winning-systems source)))
