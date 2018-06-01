;;;; package.lisp

(defpackage :clmx-util
  (:use #:cl)
  (:export :numlength
           :replace-all))

(defpackage :clmx-array
  (:use :common-lisp :clmx-util)
  (:export :create-default-array
           :list-to-array
           :array-to-list
           :height
           :width
           :apply-to-cells
           :reduce-by-column
           :reduce-by-row
           :scalar+
           :scalar*
           :format-array
           :add-arrays))
           
(defpackage :clmx-matrix
  (:nicknames #:clmx #:mx)
  (:use :common-lisp :clmx-array)
  (:export :add-matrices
           :add-scalar
           :apply-to-each-cell
           :create-matrix
           :cols
           :det
           :extract-column-as-list
           :extract-row-as-list
           :extract-row-as-vector
           :flip-horizontally
           :flip-vertically
           :identity-matrix
           :identity-matrix-p
           :multiply-matrices
           :multiply-scalar
           :ref
           :rows
           :square-matrix-p
           :transpose
           :unit-matrix
           :zero-matrix))