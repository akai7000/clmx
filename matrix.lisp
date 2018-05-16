;;;; Matrix library
;;; matrix.lisp

(in-package #:mx)

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
	 :documentation "The size of the matrix (height width)")))
	 
(defgeneric (setf data-array) (data matrix))

(defmethod (setf data-array) (data (mat matrix))
  (setf (slot-value mat 'data-array) data)
  (setf (slot-value mat 'size) (list (height data) (width data)))
)
	  
(defmethod print-object ((object matrix) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (data-array size) object
      (format stream "SIZE: ~a" size)
	  (format-array data-array))))
		  	
(defun create-matrix (&key (initial-contents nil ic-supplied-p)
						   (dimensions nil d-supplied-p)
						   (initial-element 0))
	 "Create a matrix."
	 (let ((data-array
			(cond ((and ic-supplied-p (listp initial-contents)) (list-to-array initial-contents))
				  ((and ic-supplied-p (arrayp initial-contents)) initial-contents)
				  (d-supplied-p
					(if (= (length dimensions) 2)
					    (make-array dimensions :initial-element initial-element)
						(error "DIMENSIONS must be a list of 2 values")))
				  (t (error "INITIAL-CONTENTS or DIMENSIONS must be specified")))))
		   (make-instance 'matrix :data-array data-array :size (list (height data-array) (width data-array)))))
	  
(defun mheight (matrix)
	"Get the height of MATRIX (number of rows)"
	(car (slot-value matrix 'size)))
	  
(defun mwidth (matrix)
	"Get the width of MATRIX (number of columns)"
	(cadr (slot-value matrix 'size)))
	  
(defun zeros (height width)
	"Create a zero-filled 2-dimensional matrix."
	(create-matrix :dimensions (list height width) :initial-element 0))
	
(defun ones (height width)
	"Create a 2-dimensional matrix filled with 1's."
	(create-matrix :dimensions (list height width) :initial-element 1))
	
(defun ref (matrix height width)
	"Get a value from MATRIX at row HEIGHT and column WIDTH."
	(aref (slot-value matrix 'data-array) (1- height) (1- width)))
	
(defun set-value (matrix height width value)
	"Set a value from MATRIX at row HEIGHT and column WIDTH."
	(setf (aref (slot-value matrix 'data-array) (1- height) (1- width)) value))
	
(defun eye (height width)
	"Create an identity matrix."
	(let ((matrix (zeros height width)))
		 (loop for col from 1 to (min height width) do
			(set-value matrix col col 1))
		 matrix))
		 
(defun add-matrices (matrix-1 matrix-2)
	"Add two matrices together."
	(create-matrix :initial-contents (add-arrays (slot-value matrix-1 'data-array) (slot-value matrix-2 'data-array))))
	
(defun add-scalar (matrix scalar)
	"Add scalar to a matrix."
	(let ((data (slot-value matrix 'data-array)))
		(create-matrix :initial-contents (scalar+ data scalar))))
	
(defun multiply-scalar (matrix scalar)
	"Multiply matrix by a scalar."
	(let ((data (slot-value matrix 'data-array)))
		(create-matrix :initial-contents (scalar* data scalar))))

(defun extract-row-as-list (matrix row)
	(loop for col from 1 to (mwidth matrix) collect
		(ref matrix row col)))
			
(defun extract-row-as-vector (matrix row)
	(list-to-array (extract-row-as-list matrix row)))
	
(defun extract-column-as-list (matrix col)
	(loop for row from 1 to (mheight matrix) collect
		(ref matrix row col)))
	
	;;  |  1   2   3  |                  |  1   2   3  |
;;  |  4   5   6  |   remove row 2   |  7   9  10  |
;;  |  7   9  10  |       ==>        | 11  12  13  |
;;  | 11  12  13  |


;;(defun remove-column (matrix column)
;;

(defun is-square (matrix)
	"Determine if the matrix is a square matrix."
	(= (mwidth matrix) (mheight matrix)))
