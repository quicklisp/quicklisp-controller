;;;; rss-failure-feeds.lisp
;;;;
;;;; Building on the html failure reports, produces a set of RSS feeds
;;;; so authors and other interested people can subscribe to failures.
;;;;
;;;; Each failing source becomes an item in a feed. There are many
;;;; different feeds - one for each source, one for each author (as
;;;; far as that can be determined from e.g. github username
;;;; patterns), and one for everything.
;;;;
;;;; The feeds don't have any state. They don't preserve old
;;;; items. They are created fresh for every run. (This will change if
;;;; it works poorly in feed readers.)
;;;;
;;;; Feeds are written into a directory based on a failure report
;;;; generated from a (failure-data t) call, then uploaded to the
;;;; failure report bucket all at once.
;;;;

(in-package #:quicklisp-controller)

;;; Relevant part of the log file to include

(defun remaining-lines (stream)
  (coerce
   (loop repeat 25
         for line = (read-line stream nil)
         while line
         collect line)
   'vector))

(defun best-logfile-info (logfile)
  "Return a string with the best available logfile info from LOGFILE."
  ;;; Search for "^Backtrace for" and return everything after, and 15
  ;;; lines before. If that line isn't found, just return the last 15
  ;;; lines of the file.
  (let* ((trailing-line-count 25)
         (trailing-lines (make-array trailing-line-count :initial-element nil))
         (i 0))
    (flet ((trail-line (line)
             (setf (aref trailing-lines i) line)
             (setf i (mod (1+ i) trailing-line-count)))
           (trailing-lines ()
             (let ((end (position nil trailing-lines)))
               (if end
                   (subseq trailing-lines 0 end)
                   (concatenate 'vector
                                (subseq trailing-lines i)
                                (subseq trailing-lines 0 i))))))
      (with-open-file (stream logfile)
        (loop
           (let ((line (read-line stream nil)))
             (unless line
               (return (trailing-lines)))
             (unless (boring-log-line-p line)
               (trail-line line))
             (when (ppcre:scan "^Backtrace for" line)
               (return (concatenate 'vector
                                    (trailing-lines)
                                    (remaining-lines stream))))))))))

;;; Grouping sources by author (or other criteria)

(defparameter *location-grouping-patterns*
  '("://github.com/(.*?)/"
    "://bitbucket.org/(.*?)/"
    "(kpe.io)"
    "(xach)"
    "(wcp).sdf-eu.org"
    "(nklein)"
    "gitlab.com/(.*?)/"
    "(hexstreamsoft)"
    "(marijnh)averbeke"
    "(maraist)"
    "(hu\\.dwim)"
    "(lichteblau)"
    "gitlab.common-lisp.net/(.*?)/"))

(defun scan-first-register (pattern target)
  "Scan TARGET with the regex PATTERN. If there is a match, return the
  first register grouping."
  (multiple-value-bind (start end starts ends)
      (ppcre:scan pattern target)
    (declare (ignore end))
    (when (and start (plusp (length starts)))
      (subseq target (aref starts 0) (aref ends 0)))))

(defun group-name (source)
  (let ((location (location source)))
    (dolist (pattern *location-grouping-patterns*)
      (let ((group (scan-first-register pattern location)))
        (when group
          (return group))))))

;;; Producing feeds

(defun item-contents (failing-source)
  (with-output-to-string (stream)
    (let ((failures (failure-data failing-source)))
      (format stream "<p>Project <b>~A</b><p>~%" (name failing-source))
      (format stream "<p>Taken from <code>~A</code></p>~%"
              (first-line-of (source-file (source failing-source))))
      (let ((link (source-link (source failing-source))))
          (when link
            (format stream "<p> site: <a href='~A'>~A</a></p>~%" link link)))
      (format stream "<p>~A</p>~%" (versions-and-such))
      (format stream "<p>~D failing system~:*~P: ~{~A~^, ~}</p>~%"
              (length failures)
              (mapcar 'system-name failures))
      (dolist (system failures)
        (let ((interesting-lines (best-logfile-info
                                  (failure-log-file system))))
          (format stream "<h3>~A</h3>~%~%" (system-name system))
          (format stream "<pre>~%...~%")
          (map nil
               (lambda (line)
                 (if (highlighted-log-line-p line)
                     (format stream "<strong style='color: red'>    ~A</strong>~%"
                             (escape-html line))
                     (format stream "    ~A~%" (escape-html line))))
               interesting-lines)
          (format stream "</pre>~%~%")))
      (format stream "<p><a href='~A'>Full build log</a></p>~%"
              (full-failure-report-url failing-source)))))

(defun item-title (failing-source)
  (format nil "~A" (name (source failing-source))))

(defclass failing-source-item (item)
  ((failing-source
    :reader failing-source
    :initarg :failing-source)))

(defun failing-source-item (failing-source)
  (make-instance 'failing-source-item
                 :failing-source failing-source
                 :title (item-title failing-source)
                 :description (item-contents failing-source)
                 :guid (full-failure-report-url failing-source)
                 :link (full-failure-report-url failing-source)))


(defun feed-link (type title)
  (format nil "http://report.quicklisp.org/feeds/~@[~A/~]~A.rss"
          type title))

(defun feed-key (type title)
  (format nil "~@[~A/~]~A" type title))

(defun make-empty-feeds (&key (sources (all-of-type t)))
  "Return a hash table keyed on feed structure strings with feeds as
  values. There is an \"all\" key, keys starting with \"author/\" for
  each grouping of feeds by author, and keys starting with
  \"project/\" with one feed per project."
  (let ((feeds (make-string-table)))
    (flet ((make-feed (type title)
             (let ((key (feed-key type title)))
               (unless (gethash key feeds)
                 (setf (gethash key feeds)
                       (make-instance 'feed
                                      :pub-date (get-universal-time)
                                      :last-build-date (get-universal-time)
                                      :title title
                                      :link (feed-link type title)
                                      :description
                                      (format nil "Quicklisp build failures for ~S"
                                              title)))))))
      (make-feed nil "all")
      (dolist (source sources feeds)
        (make-feed "project" (name source))
        (make-feed "author" (group-name source))))))

(defun generate-feeds (failure-report)
  "Return a hash-table of feeds. Keys are string that represent feed
 output structure, e.g. 'all' has everything, 'author/xach' has my
 projects, 'project/vecto' has a single project named vecto."
  (let ((feeds (make-empty-feeds)))
    (flet ((ensure-feed (type title)
               (let ((key (feed-key type title)))
                 (or (gethash key feeds)
                     (error "Unknown feed key ~S" key)))))
      (let ((all (ensure-feed nil "all")))
        (dolist (failing-source (failure-data failure-report) feeds)
          (let* ((source (source failing-source))
                 (item (failing-source-item failing-source))
                 (project-name (name source))
                 (group-name (group-name source)))
            (push item (items all))
            (push item (items (ensure-feed "project" project-name)))
            (when group-name
              (push item (items (ensure-feed "author" group-name))))))))))

(defun write-feeds (failure-report output-directory)
  (let ((feeds (generate-feeds failure-report)))
    (let ((*default-pathname-defaults* (truename
                                        (ensure-directories-exist
                                         output-directory))))
      (maphash
       (lambda (key feed)
         (let ((output (merge-pathnames key "feed.rss") ))
           (ensure-directories-exist output)
           (generate-to output feed)))
       feeds)
      *default-pathname-defaults*)))

;;; There can be thousands of feeds, so don't try to publish empty
;;; feed files if they already exist.

(defun existing-published-keys ()
  (let ((keys (zs3:all-keys *failtail-bucket*
                            :prefix "feeds/"
                            :credentials *failtail-credentials*))
        (table (make-string-table)))
    (map nil
         (lambda (s3-key)
           (setf (gethash (zs3:name s3-key) table) t))
         keys)
    table))

(defun empty-feed-file-p (file)
  (with-open-file (stream file)
    (loop for line = (read-line stream nil)
       while line
       never  (ppcre:scan "<item>" line))))

(defun publish-feeds (feed-directory)
  ;; Truename in advance to avoid issues with the truenaming of
  ;; DIRECTORY results and the relative nature of ENOUGH-NAMESTRING.
  (setf feed-directory (truename feed-directory))
  (let ((files (directory (merge-pathnames "**/*.rss" feed-directory)))
        (existing-keys (existing-published-keys)))
    (dolist (file files)
      (let ((key (format nil "feeds/~A"
                         (enough-namestring file feed-directory))))
        (when (or (not (gethash key existing-keys))
                  (not (empty-feed-file-p file)))
          (upload-report-file file key))))))


