;;;; ak-matrix.asd

(asdf:defsystem #:mxcl
  :description "Matrix manipulation library for Common Lisp"
  :author "Andrei Kaplun"
  :license  "MIT"
  :version "0.0.1"
  :homepage "https://andrei12.github.io/mxcl/"
  :bug-tracker "https://github.com/andrei12/mxcl/issues"
  :source-control (:git "https://github.com/andrei12/mxcl.git")
  :serial t
  :components ((:file "package")
               (:file "util")
               (:file "array")
               (:file "matrix")))
