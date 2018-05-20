;;;; package.lisp

(defpackage :util
  (:use #:cl)
  (:export :digits
           :replace-all))

(defpackage :mx-array
  (:use :common-lisp :util)
  (:export :create-default-array
           :list-to-array
		   :array-to-list
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
  (:export :create-matrix
           :mheight
           :mwidth
           :zeros
           :ones
           :ref
           :identity-matrix
           :add-matrices
           :add-scalar
           :multiply-scalar
           :extract-row-as-list
           :extract-row-as-vector
           :extract-column-as-list
           :is-square
		   :det
		   :multiply-matrices))