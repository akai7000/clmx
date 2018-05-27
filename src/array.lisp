;;;; Arrays
;;; array.lisp

(in-package #:clmx-array)

(defun create-default-array (height width &optional default-value)
    (make-array (list height width)
        :initial-element default-value))
        
(defun list-to-array (lst)
    "Convert a nested list to 2-dimensional array.
 Examples:
    (list-to-array '((1 2) (3 4)))  ==>  #2A((1 2) (3 4))
    (list-to-array '((1 2 3) (4 5 6)))  ==>  #2A((1 2 3) (4 5 6))"
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
            
(defun apply-to-each-cell (function array)
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
       (apply-to-each-cell (lambda (x) (+ x number)) array))
       
(defun scalar* (array number)
       (apply-to-each-cell (lambda (x) (* x number)) array))
    
(defun format-array (array)
    (let ((max-digits-vector (reduce-by-column #'max (apply-to-each-cell #'digits array))))
         (loop for h from 0 below (height array) do
             (format t "|")
             (loop for w from 0 below (width array) do
                 (let ((padding (aref max-digits-vector w)))
                      (format t
                          (replace-all "~$d "
                          "$"
                          (write-to-string (+ 1 padding))) (aref array h w))))
             (format t "|~%"))))
             
(defun add-arrays (array-1 array-2)
       (let ((new-array (create-default-array (height array-1) (width array-1))))
         (loop for h from 0 below (height array-1) do
          (loop for w from 0 below (width array-1) do
               (setf (aref new-array h w) (+ (aref array-1 h w) (aref array-2 h w)))))
         new-array))
         
             
    