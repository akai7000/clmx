;;;; test/package.lisp

(defpackage #:mxcl-test
  (:nicknames #:mxt)
  (:use #:cl #:fiveam)
  (:export #:run!
           #:all-tests))