;;;; ak-matrix.asd

(asdf:defsystem #:ak-matrix
  :description "Matrix manipulation library for Common Lisp"
  :author "Andrei Kaplun"
  :license  "MIT"
  :version "0.0.1"
  :homepage "https://andrei12.github.io/ak-matrix/"
  :bug-tracker "https://github.com/andrei12/ak-matrix/issues"
  :source-control (:git "https://github.com/andrei12/ak-matrix.git")
  :serial t
  :components ((:file "package")
               (:file "matrix"))
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.md")))
