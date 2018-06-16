;;;; package.lisp
;;; The package definitions for clmx-util, clmx-array and clmx-matrix

;;   Copyright 2018 Andrei Kaplun
;;
;;   Licensed under the Apache License, Version 2.0 (the "License");
;;   you may not use this file except in compliance with the License.
;;   You may obtain a copy of the License at
;;
;;       http://www.apache.org/licenses/LICENSE-2.0
;;
;;   Unless required by applicable law or agreed to in writing, software
;;   distributed under the License is distributed on an "AS IS" BASIS,
;;   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;   See the License for the specific language governing permissions and
;;   limitations under the License.

(defpackage :clmx-util
  (:use #:cl)
  (:export :numlength
           :replace-all
           :fix-rounding))

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
           :adjugate
           :apply-to-each-cell
           :create-matrix
           :cofactors
           :cols
           :defmx
           :det
           :eigenvalues
           :extract-column-as-list
           :extract-row-as-list
           :extract-row-as-vector
           :flip-horizontally
           :flip-vertically
           :identity-matrix
           :identity-matrix-p
           :inverse
           :multiply-matrices
           :multiply-scalar
           :ref
           :remove-column
           :remove-row
           :rows
           :square-matrix-p
           :transpose
           :unit-matrix
           :zero-matrix))