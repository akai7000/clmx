;;;; package.lisp

(defpackage :util
  (:use #:cl)
  (:export :digits
		   :replace-all))

(defpackage :mx-array
  (:use :common-lisp :util)
  (:export :create-default-array
		   :list-to-array
		   :height
		   :width
		   :apply-to-each-cell
		   :reduce-by-column
		   :reduce-by-row
		   :scalar+
		   :scalar*
		   :format-array
		   :add-arrays))
		   
(defpackage :mx-matrix
  (:nicknames #:mx)
  (:use :common-lisp :mx-array)
  (:export :create-matrix))