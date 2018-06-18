;;;; array.lisp
;;; Various array utilities that are used in matrix.lisp

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

(in-package #:clmx-array)

(defun create-default-array (height width &optional default-value)
    (make-array (list height width)
        :initial-element default-value))
        
(defun list-to-array (lst)
    "Convert a nested list to 2-dimensional array."
    (if (atom (car lst))
        (make-array (length lst) :initial-contents lst)
        (make-array (list (length lst) (length (car lst))) :initial-contents lst)))
        
(defun array-to-list (array)
  (loop for i below (array-dimension array 0) collect
    (loop for j below (array-dimension array 1) collect
        (aref array i j))))
                      
(defun height (array)
    (car (array-dimensions array)))
    
(defun width (array)
    (let ((cdr-of-dims (cdr (array-dimensions array))))
         (if (eq cdr-of-dims nil)
             1
            (car cdr-of-dims))))
            
(defun apply-to-cells (function array)
       (let ((new-array (create-default-array (height array) (width array))))
         (loop for h from 0 below (height array) do
          (loop for w from 0 below (width array) do
               (let ((value (aref array h w)))
                    (setf (aref new-array h w)
                    (funcall function value)))))
         new-array))
         
(defun reduce-by-column (function array)
    (list-to-array
        (loop for w from 0 below (width array) collect
            (reduce function
                (loop for h from 0 below (height array) collect
                    (aref array h w))))))

(defun reduce-by-row (function array)
    (list-to-array
        (loop for h from 0 below (height array) collect
            (reduce function
                (loop for w from 0 below (width array) collect
                    (aref array h w))))))
                    
(defun scalar+ (array number)
       (apply-to-cells (lambda (x) (clmx-util:fix-rounding (+ x number))) array))
       
(defun scalar* (array number)
       (apply-to-cells (lambda (x) (clmx-util:fix-rounding (* x number))) array))

(defun format-array (array)
    (let ((max-numlength-vector (reduce-by-column #'max (apply-to-cells #'clmx-util:numlength array))))
         (loop for h from 0 below (height array) do
             (format t "|")
             (loop for w from 0 below (width array) do
                 (let ((padding (write-to-string (1+ (aref max-numlength-vector w))))
                       (val (aref array h w)))
                     (format t
                          (cond ((floatp val) (clmx-util:replace-all "~$f " "$" padding))
                                ((integerp val) (clmx-util:replace-all "~$d " "$" padding))
                                ((complexp val) (clmx-util:replace-all "~$f " "$" padding))
                                (t (clmx-util:replace-all "~$d " "$" padding)))
                          val)))
             (format t "|~%"))))
                  
(defun add-arrays (array-1 array-2)
       (let ((new-array (create-default-array (height array-1) (width array-1))))
         (loop for h from 0 below (height array-1) do
          (loop for w from 0 below (width array-1) do
               (setf (aref new-array h w) (clmx-util:fix-rounding (+ (aref array-1 h w) (aref array-2 h w))))))
         new-array))
        
(defun random-int-array (height width min-num max-num)
    (let ((diff (- (1+ max-num) min-num)))
         (list-to-array
             (loop for i from 0 to (1- width) collect
                 (loop for j from 0 to (1- height) collect
                     (+ (random diff) min-num))))))
                     
;(clmx-util:fix-rounding (+ (random diff) min-num) precision))))))

;   0  500    random 500           random (500 - 0) - 0
;-100  300    (random 400) - 100   random (300 - (-100))
;-200  200    (random 400) - 200   random (200 - (-200))
;-100  500    (random 600) - 100   random (500 - (-100))
; -10  200    (random 210) - 10

