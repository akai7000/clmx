;;;; test/package.lisp

(defpackage #:clmx-test
  (:nicknames #:clmxt #:mxt)
  (:use #:cl #:fiveam)
  (:export #:run!
           #:all-tests))