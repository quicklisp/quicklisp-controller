;;;; irepl.lisp

(in-package #:quicklisp-controller)

;;; A repl for working through github issues.

(defvar *irepl-command-table* (make-hash-table))
(defvar *irepl-current-command* nil)
(defvar *irepl-state* nil)

(defun irepl-unknown-command ()
  (warn "Unknown command ~A" *irepl-current-command*))

(defmacro define-irepl-command (command &body body)
  (let ((fun-name (copy-symbol command)))
    `(setf (gethash ',command *irepl-command-table*)
           (labels ((,fun-name () ,@body))
             #',fun-name))))

(defun invoke-irepl-command (command)
  (let ((*irepl-current-command* command))
    (funcall (gethash command *irepl-command-table* 'irepl-unknown-command))))

(defgeneric all-issues (state))
(defgeneric current-issue (state))
(defgeneric next-issue (state))
(defgeneric previous-issue (state))
(defgeneric reset (state))

(defgeneric issue-index (state))
(defgeneric (setf issue-index) (new-value state))
(defgeneric issue-count (state))

(defmethod current-issue (state)
  (elt (all-issues state) (issue-index state)))

(defmethod next-issue (state)
  (setf (issue-index state) (mod (1+ (issue-index state))
                                 (issue-count state)))
  (current-issue state))

(defmethod previous-issue (state)
  (setf (issue-index state) (mod (1- (issue-index state))
                                 (issue-count state)))
  (current-issue state))

(defmethod reset (state)
  (let ((issues (githappy:json
                 (githappy:repo-issues :owner "quicklisp"
                                       :repo "quicklisp-projects"))))
    (setf (slot-value state 'issues) issues)
    (setf (issue-index state) 0)))

(defmethod issue-count (state)
  (length (all-issues state)))

(defclass irepl-state ()
  ((issues
    :reader all-issues)
   (issue-index
    :initform 0
    :accessor issue-index)))

(defmethod shared-initialize :after ((state irepl-state) slot-names
                                     &rest args &key &allow-other-keys)
  (declare (ignore args))
  (reset state))

(defun stripm (string)
  (delete #\Return string))

(defun irref (&rest key)
  (let ((value (githappy:jref (current-issue *irepl-state*) key)))
    (if (stringp value)
        (stripm value)
        value)))


(defun irepl ()
  (catch 'irepl-exit
    (setf *irepl-state* (make-instance 'irepl-state))
    (invoke-irepl-command 'show)
    (tagbody
     next
       (with-simple-restart (abort "Return to irepl")
         (fresh-line)
         (princ "> ")
         (let ((command (read)))
           (cond ((and (symbolp command)
                       (not (boundp command)))
                  (shiftf *** ** * (invoke-irepl-command command)))
                 (t
                  (let ((results (multiple-value-list (eval command))))
                    (format t "~{~S~^ ;~%~}" results)
                    (shiftf *** ** * (first results)))))))
       (go next))))


(defparameter *irepl-guess-patterns*
  '(("(github\\.com/[\\w-_/]*)" "https" )))

(define-irepl-command quit
  (throw 'irepl-exit :done))

(define-irepl-command reset
  (reset *irepl-state*))

(define-irepl-command show
  (format t "#~D: ~A~%---~{ [~A]~}~%~A~%"
          (irref "number")
          (irref "title")
          (irref "labels" :* "name")
          (irref "body")))

(define-irepl-command next
  (next-issue *irepl-state*)
  (invoke-irepl-command 'show))

(define-irepl-command previous
  (previous-issue *irepl-state*)
  (invoke-irepl-command 'show))

(defvar *irepl-guess-patterns*
  "https://github.com/")

(define-irepl-command canbuild
  (when *last-source*
    (mark-canbuild *last-source*)
    :canbuild))

(define-irepl-command cantbuild
  (when *last-source*
    (mark-cantbuild *last-source*)
    :cantbuild))

(define-irepl-command commit
  (when *last-source*
    (commit-source *last-source*)
    :committed))
