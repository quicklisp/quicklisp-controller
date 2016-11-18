;;;; config.lisp

(in-package #:quicklisp-controller)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ubiquitous:restore 'quicklisp-controller))

(defvar *report-to-email* (ubiquitous:value 'report-to-email)
  "The email address to which reports are emailed.")

(defparameter githappy:*oauth2-token* (ubiquitous:value 'github-access-token))

