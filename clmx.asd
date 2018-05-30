;;;; cl-mx.asd

(asdf:defsystem #:clmx
    :description "Matrix manipulation library for Common Lisp"
    :author "Andrei Kaplun"
    :license  "MIT"
    :version "0.0.1"
    :homepage "https://akai7000.github.io/clmx/"
    :bug-tracker "https://github.com/akai7000/clmx/issues"
    :source-control (:git "https://github.com/akai7000/clmx.git")
    :serial t
    :components ((:module "src"
                  :serial t
				  :components ((:file "package")
                               (:file "util")
                               (:file "array")
                               (:file "matrix")))))

(asdf:defsystem #:clmx/test
    :description "Tests for CL-MX library"
    :author "Andrei Kaplun"
    :license  "MIT"
    :version "0.0.1"
    :depends-on (:clmx :fiveam)
    :components ((:module "test"
                  :serial t
                  :components ((:file "package")
                               (:file "main")))))