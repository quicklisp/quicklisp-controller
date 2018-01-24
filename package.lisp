;;;; package.lisp

(defpackage #:quicklisp-controller
  (:use #:cl
	#:westbrook)
  (:export #:setup-directories)
  (:shadowing-import-from #:sb-ext
                          #:run-program
                          #:process-exit-code
                          #:process-output
                          #:process-close)
  (:shadowing-import-from #:sb-ext
                          #:native-namestring)
  (:shadowing-import-from #:sb-posix
                          #:chdir)
  (:shadowing-import-from #:ql-gunzipper
                          #:gunzip)
  (:shadowing-import-from #:ql-http
                          #:fetch)
  (:shadowing-import-from #:quicklisp-tarhash
                          #:content-hash)
  (:shadowing-import-from #:alexandria
                          #:when-let)
  (:shadowing-import-from #:ql-dist
                          #:provided-systems
                          #:required-systems
                          #:provided-releases
                          #:system-file-name
                          #:name
                          #:dist
                          #:release))

(in-package #:quicklisp-controller)
