;;;; package.lisp

(defpackage :clmx-util
  (:use #:cl)
  (:export :digits
           :replace-all))

(defpackage :clmx-array
  (:use :common-lisp :clmx-util)
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
           
(defpackage :clmx-matrix
  (:nicknames #:clmx #:mx)
  (:use :common-lisp :clmx-array)
  (:export :create-matrix
           :rows
           :cols
           :zero-matrix
           :unit-matrix
           :ref
           :identity-matrix
           :add-matrices
           :add-scalar
           :multiply-scalar
           :extract-row-as-list
           :extract-row-as-vector
           :extract-column-as-list
           :square-matrix-p
		   :det
		   :multiply-matrices))