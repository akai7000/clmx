;;;; ak-matrix.asd

(asdf:defsystem #:mxcl
  :description "Matrix manipulation library for Common Lisp"
  :author "Andrei Kaplun"
  :license  "MIT"
  :version "0.0.1"
  :homepage "https://akai7000.github.io/mxcl/"
  :bug-tracker "https://github.com/akai7000/mxcl/issues"
  :source-control (:git "https://github.com/akai7000/mxcl.git")
  :serial t
  :components ((:module "src"
                :serial t
				:components ((:file "package")
                             (:file "util")
                             (:file "array")
                             (:file "matrix")))))

(asdf:defsystem #:mxcl/test
  :description "Tests for MXCL library"
  :author "Andrei Kaplun"
  :license  "MIT"
  :version "0.0.1"
  :depends-on (:mxcl :fiveam)
  :components ((:module "test"
                :serial t
                :components ((:file "package")
                             (:file "main")))))