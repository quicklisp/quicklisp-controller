;;;; mail.lisp

(in-package #:quicklisp-controller)

(defun call-with-output-to-mail (fun &key from to subject)
  (unless (and from to subject)
    (error "FROM, TO, and SUBJECT required"))
  (let ((mail (sb-ext:run-program "/usr/sbin/sendmail"
                                  (list "-t")
                                  :search nil
                                  :wait nil
                                  :output nil
                                  :input :stream)))
    (let ((stream (sb-ext:process-input mail)))
      (format stream "From: ~A~%" from)
      (format stream "To: ~A~%" to)
      (format stream "Subject: ~A" subject)
      (terpri stream)
      (funcall fun stream)
      (close stream))))

(defmacro with-output-to-mail ((stream &key from to (subject ""))
                               &body body)
  `(call-with-output-to-mail (lambda (,stream) ,@body)
                             :from ,from
                             :to ,to
                             :subject ,subject))
