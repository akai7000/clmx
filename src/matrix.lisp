;;;; matrix.lisp
;;;; matrix.lisp
;;; The main source file for CLMX

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

(in-package #:clmx-matrix)

(defclass matrix ()
   ((data-array
     :initarg :data-array
     :initform (error "DATA-ARRAY must be supplied.")
     :reader data-array
     :documentation "Two-dimensional array that holds the data of the matrix")
    (size
     :initarg :size
     :initform '(0 0)
     :reader size
     :documentation "The size of the matrix (rows cols)")))
     
(defgeneric (setf data-array) (data matrix))

(defmethod (setf data-array) (data (mat matrix))
  (setf (slot-value mat 'data-array) data)
  (setf (slot-value mat 'size) (list (height data) (width data)))
)
      
(defmethod print-object ((object matrix) stream)
  "Prints the matrix in a nice readable format"
  (print-unreadable-object (object stream :type t)
    (with-slots (data-array size) object
      (format stream "SIZE: ~a" size)
      (format-array data-array))))
            
(defun create-matrix (&key (contents nil c-supplied-p)
                           (dimensions nil d-supplied-p)
                           (initial-element 0))
     "Create a matrix."
     (let ((data-array
            (cond ((and c-supplied-p (listp contents)) (list-to-array contents))
                  ((and c-supplied-p (arrayp contents)) contents)
                  (d-supplied-p
                    (if (= (length dimensions) 2)
                        (make-array dimensions :initial-element initial-element)
                        (error "DIMENSIONS must be a list of 2 values")))
                  (t (error "CONTENTS or DIMENSIONS must be specified")))))
           (make-instance 'matrix :data-array data-array :size (list (height data-array) (width data-array)))))
      
(defun rows (matrix)
    "Get the number of rows of a matrix."
    (car (slot-value matrix 'size)))
      
(defun cols (matrix)
    "Get the number of columns of a matrix."
    (cadr (slot-value matrix 'size)))
      
(defun zero-matrix (num-rows num-cols)
    "Create a zero-filled 2-dimensional matrix."
    (create-matrix :dimensions (list num-rows num-cols) :initial-element 0))
    
(defun unit-matrix (num-rows num-cols)
    "Create a 2-dimensional matrix filled with 1's."
    (create-matrix :dimensions (list num-rows num-cols) :initial-element 1))
    
(defun ref (matrix row col)
    "Get a value from a matrix at row 'row' and column 'col'."
    (aref (slot-value matrix 'data-array) (1- row) (1- col)))
    
(defun set-value! (matrix row col value)
    "Set a value of a matrix at row 'row' and column 'col'. This function has side effects."
    (setf (aref (slot-value matrix 'data-array) (1- row) (1- col)) value))
    
(defun identity-matrix (num-rows &optional (num-cols num-rows))
    "Create an identity matrix."
    (let ((matrix (zero-matrix num-rows num-cols)))
         (loop for col from 1 to (min num-rows num-cols) do
            (set-value! matrix col col 1))
         matrix))
         
(defun add-matrices (matrix-1 matrix-2)
    "Add two matrices together."
    (create-matrix :contents (clmx-array:add-arrays (slot-value matrix-1 'data-array) (slot-value matrix-2 'data-array))))
    
(defun add-scalar (matrix scalar)
    "Add scalar to a matrix."
    (let ((data (slot-value matrix 'data-array)))
        (create-matrix :contents (clmx-array:scalar+ data scalar))))
    
(defun multiply-scalar (matrix scalar)
    "Multiply matrix by a scalar."
    (let ((data (slot-value matrix 'data-array)))
        (create-matrix :contents (clmx-array:scalar* data scalar))))

(defun apply-to-each-cell (matrix function)
    (let ((data (slot-value matrix 'data-array)))
        (create-matrix :contents (clmx-array:apply-to-cells function data))))

(defun extract-column-as-list (matrix col)
    "Returns the column of the matrix as a list."
    (loop for row from 1 to (rows matrix) collect
        (ref matrix row col)))
        
(defun extract-column-as-vector (matrix col)
    "Returns the column of the matrix as a vector."
    (list-to-array (extract-column-as-list matrix col)))
    
(defun extract-row-as-list (matrix row)
    "Returns the row of the matrix as a list."
    (loop for col from 1 to (cols matrix) collect
        (ref matrix row col)))
            
(defun extract-row-as-vector (matrix row)
    "Returns the row of the matrix as a vector."
    (list-to-array (extract-row-as-list matrix row)))
    
(defun square-matrix-p (matrix)
    "Determine if the matrix is a square matrix."
    (= (cols matrix) (rows matrix)))

(defun identity-matrix-p (matrix)
    "Determine if the matrix is an identity matrix."
    (if (square-matrix-p matrix)
        (= 0 (apply #'+
            (loop for row from 1 to (rows matrix) append
                (loop for col from 1 to (cols matrix) collect
                    (let ((val (ref matrix row col)))
                         (cond ((and (= row col) (= val 1)) 0)
                               ((and (/= row col) (= val 0) 0))
                               (t 1)))))))
        (error "Matrix must be a square matrix.")))
        
(defun transpose (matrix)
    "Find the transpose of the matrix."
    (if (square-matrix-p matrix)
        (create-matrix :contents
            (loop for row from 1 upto (rows matrix) collect
                (loop for col from 1 upto (cols matrix) collect
                    (ref matrix col row))))
        (error "Matrix must be a square matrix.")))
      
(defun flip-horizontally (matrix)
    "Flip the matrix horizontally (reverse column order)."
    (create-matrix :contents
        (loop for row from 1 upto (rows matrix) collect
            (loop for col from (cols matrix) downto 1 collect
                (ref matrix row col)))))

(defun flip-vertically (matrix)
    "Flip the matrix vertically (reverse row order)."
    (create-matrix :contents
        (loop for row from (rows matrix) downto 1 collect
            (loop for col from 1 upto (cols matrix) collect
                (ref matrix row col)))))                 
                     
;; The following 5 functions are used for calculating determinants
(defun remove-elt-from-list (list elt)
    (append (subseq list 0 elt) (nthcdr (1+ elt) list)))

(defun remove-first-row (matrix)
    (let ((data (slot-value matrix 'data-array)))
         (create-matrix :contents (cdr (clmx-array:array-to-list data)))))

(defun remove-column (matrix col-num)
    (let ((data-list (clmx-array:array-to-list (slot-value matrix 'data-array))))
         (create-matrix :contents
            (mapcar #'(lambda (list) (remove-elt-from-list list (1- col-num))) data-list))))
            
(defmacro cell-times-det (col)
  `(* (ref matrix 1 ,col ) (det (remove-column (remove-first-row matrix) ,col))))
  
(defun det (matrix)
    "Calculate the determinant of a square matrix."
    (let ((w (cols matrix)))
        (if (square-matrix-p matrix)
            (cond ((= w 1) (ref matrix 1 1))
                  (t (- (apply #'+
                            (loop for col from 1 to w
                                if (oddp col) collect (cell-times-det col)))
                        (apply #'+
                            (loop for col from 2 to w 
                                if (evenp col) collect (cell-times-det col))))))
            (error "Matrix must be a square matrix."))))

(defun r*c (matrix-1 matrix-2 row col)
    (apply #'+
        (mapcar #'*
            (extract-row-as-list matrix-1 row)
            (extract-column-as-list matrix-2 col))))

(defun multiply-matrices (matrix-1 matrix-2)
    "Multiply matrices."
    (let ((w1 (cols matrix-1))
          (h1 (rows matrix-1))
          (w2 (cols matrix-2))
          (h2 (rows matrix-2)))
         (if (= w1 h2)
             (create-matrix :contents
                (loop for row from 1 to h1 collect
                    (loop for col from 1 to w2 collect
                        (r*c matrix-1 matrix-2 row col))))
             "Cannot multiply: dimensions do not match")))

(defun remove-row (matrix row-num)
    (let ((data (slot-value matrix 'data-array)))
         (create-matrix :contents (remove-elt-from-list (clmx-array:array-to-list data) (1- row-num)))))
         
(defun cofactors (matrix)
	"Calculate matrix of cofactors."
	(if (square-matrix-p matrix)
        (let ((w (cols matrix)))
             (create-matrix :contents
                 (loop for row from 1 to w collect
                     (loop for col from 1 to w collect
                         (let ((d (det (remove-column (remove-row matrix row) col))))
                              (if (oddp (+ row col))
                                  (- d)
                                  d))))))
		(error "Matrix must be a square matrix.")))

(defun adjugate (matrix)
    "Calculate adjugate matrix."
    (transpose (cofactors matrix)))

(defun inverse (matrix)
    "Find the inverse of the matrix. Inefficient algorithm for now."
    (let ((d (det matrix)))
        (if (= d 0)
            (error "Inverse does not exist - determinant of matrix is 0.")
            (multiply-scalar (adjugate matrix) (/ 1 (det matrix))))))
   
(defun eigenvalues (matrix)
    "Find eigenvalues of a matrix."
    (let ((w (cols matrix)))
        (if (square-matrix-p matrix)
            (cond ((= w 1) (ref matrix 1 1))
                  ((= w 2) (let ((a (ref matrix 1 1))
                                 (d (ref matrix 2 2)))
                                (let ((root (sqrt (- (expt (+ a d) 2) (* 4 (det matrix))))))
                                     (values (/ (+ a d root) 2)
                                             (/ (- (+ a d) root) 2)))))
                  (t "This function for now only finds eigenvalues for 1x1 and 2x2 matrices."))
            (error "Matrix must be a square matrix."))))
            
(defmacro defmx (var contents)
    "Shortcut - creates matrix based on contents and assigns to var."
    `(defparameter ,var (create-matrix :contents ,contents)))

(defun random-matrix (rows cols min-num max-num)
    (create-matrix :contents (clmx-array:random-int-array rows cols min-num max-num)))