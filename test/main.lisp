;;;; test/main.lisp
;;; This is where all the tests are for CLMX

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

(in-package #:clmx-test)

(def-suite all-tests
    :description "The master suite of all CLMX tests.")

(in-suite all-tests)

(defun test-clmx ()
    (run! 'all-tests))

(test test-add-matrices
    "Testing add-matrices."
    (let ((a (create-matrix :contents '((1 2) (3 4))))
          (b (create-matrix :contents '((2 -2) (7 5))))
          (c (create-matrix :contents '((3.2 2.8 3.7) (-0.4 7.9 2.1))))
          (d (create-matrix :contents '((0.3 -2.0 -0.5) (1.1 3.4 9.9)))))
      (is (= (ref (add-matrices a b) 1 1) 3))
      (is (= (ref (add-matrices a b) 1 2) 0))
      (is (= (ref (add-matrices a b) 2 1) 10))
      (is (= (ref (add-matrices a b) 2 2) 9))
      (is (= (ref (add-matrices c d) 1 2) 0.8))
      (is (= (ref (add-matrices c d) 2 2) 11.3))
      (is (= (ref (add-matrices c d) 2 3) 12.0))))
	  
(test test-add-scalar
	"Testing add-scalar."
	(let ((a (create-matrix :contents '((1 2) (3 4))))
	      (b (create-matrix :contents '((3.2 2.8 3.7) (-0.4 7.9 2.1)))))
	   (is (= (ref (add-scalar a 5) 1 1) 6))
	   (is (= (ref (add-scalar a 5) 2 2) 9))
	   (is (= (ref (add-scalar b 1.1) 1 1) 4.3))
	   (is (= (ref (add-scalar b 1.1) 2 1) 0.7))
	   (is (= (ref (add-scalar b 1.1) 2 3) 3.2))))

(test test-adjugate
    "Testing adjugate."
    (is (= (ref (adjugate (create-matrix :contents '((3 -2 0) (7 3 1) (0 2 4)))) 1 1) 10))
    (is (= (ref (adjugate (create-matrix :contents '((3 -2 0) (7 3 1) (0 2 4)))) 1 2) 8))
    (is (= (ref (adjugate (create-matrix :contents '((3 -2 0) (7 3 1) (0 2 4)))) 1 3) -2))
    (is (= (ref (adjugate (create-matrix :contents '((3 -2 0) (7 3 1) (0 2 4)))) 2 1) -28))
    (is (= (ref (adjugate (create-matrix :contents '((3 -2 0) (7 3 1) (0 2 4)))) 2 2) 12))
    (is (= (ref (adjugate (create-matrix :contents '((3 -2 0) (7 3 1) (0 2 4)))) 2 3) -3))
    (is (= (ref (adjugate (create-matrix :contents '((3 -2 0) (7 3 1) (0 2 4)))) 3 1) 14))
    (is (= (ref (adjugate (create-matrix :contents '((3 -2 0) (7 3 1) (0 2 4)))) 3 2) -6))
    (is (= (ref (adjugate (create-matrix :contents '((3 -2 0) (7 3 1) (0 2 4)))) 3 3) 23)))
    
(test test-apply-to-each-cell
	"Testing apply-to-each-cell."
    (is (= (ref (apply-to-each-cell (create-matrix :contents '((4 9) (16 25))) #'sqrt) 1 1) 2.0))
    (is (= (ref (apply-to-each-cell (create-matrix :contents '((4 9) (16 25))) #'sqrt) 2 2) 5.0))
    (is (= (ref (apply-to-each-cell (create-matrix :contents '((1 2 3) (4 5 6) (7 8 9))) (lambda (x) (expt x 2))) 1 3) 9))
    (is (= (ref (apply-to-each-cell (create-matrix :contents '((1 2 3) (4 5 6) (7 8 9))) (lambda (x) (expt x 2))) 3 1) 49))
    (is (= (ref (apply-to-each-cell (create-matrix :contents '((1 2 3) (4 5 6) (7 8 9))) (lambda (x) (expt x 2))) 3 3) 81)))
    
(test test-create-matrix
    "Testing create-matrix."
    (is (= (ref (create-matrix :dimensions '(4 3) :initial-element 5) 1 1) 5))
    (is (= (ref (create-matrix :dimensions '(4 3) :initial-element 5) 4 3) 5))
    (is (= (ref (create-matrix :contents '((1 2 3) (4 5 6))) 1 1) 1))
    (is (= (ref (create-matrix :contents '((1 2 3) (4 5 6))) 2 3) 6))
    (is (= (ref (create-matrix :contents #2A ((1 2 3) (4 5 6) (7 8 9))) 1 1) 1))
    (is (= (ref (create-matrix :contents #2A ((1 2 3) (4 5 6) (7 8 9))) 3 3) 9))
    (is (= (ref (create-matrix :dimensions '(3 7)) 1 1) 0))
    (is (= (ref (create-matrix :dimensions '(3 7)) 3 7) 0)))
  
(test test-cofactors
    "Testing cofactors."
    (is (= (ref (cofactors (create-matrix :contents '((3 -2 0) (7 3 1) (0 2 4)))) 1 1) 10))
    (is (= (ref (cofactors (create-matrix :contents '((3 -2 0) (7 3 1) (0 2 4)))) 1 2) -28))
    (is (= (ref (cofactors (create-matrix :contents '((3 -2 0) (7 3 1) (0 2 4)))) 1 3) 14))
    (is (= (ref (cofactors (create-matrix :contents '((3 -2 0) (7 3 1) (0 2 4)))) 2 1) 8))
    (is (= (ref (cofactors (create-matrix :contents '((3 -2 0) (7 3 1) (0 2 4)))) 2 2) 12))
    (is (= (ref (cofactors (create-matrix :contents '((3 -2 0) (7 3 1) (0 2 4)))) 2 3) -6))
    (is (= (ref (cofactors (create-matrix :contents '((3 -2 0) (7 3 1) (0 2 4)))) 3 1) -2))
    (is (= (ref (cofactors (create-matrix :contents '((3 -2 0) (7 3 1) (0 2 4)))) 3 2) -3))
    (is (= (ref (cofactors (create-matrix :contents '((3 -2 0) (7 3 1) (0 2 4)))) 3 3) 23)))
    
(test test-cols
    "Testing cols."
    (is (= (cols (create-matrix :contents '((1 2 3) (4 5 6)))) 3))
    (is (= (cols (create-matrix :contents #2A ((1 2 3) (4 5 6)))) 3))
    (is (= (cols (create-matrix :dimensions '(10 20))) 20))
    (is (= (cols (create-matrix :dimensions '(10 20) :initial-element 5)) 20)))
    
(test test-zero-matrix
    "Testing zero-matrix."
    (is (= (ref (zero-matrix 4 3) 1 1) 0))
    (is (= (ref (zero-matrix 4 3) 4 3) 0))
    (is (= (ref (zero-matrix 1 1) 1 1) 0)))
  
(test test-unit-matrix
    "Testing unit-matrix."
    (is (= (ref (unit-matrix 4 3) 1 1) 1))
    (is (= (ref (unit-matrix 4 3) 4 3) 1)))
  
(test test-identity-matrix
    "Testing identity-matrix."
    (is (= (ref (identity-matrix 4) 1 1) 1))
    (is (= (ref (identity-matrix 4) 4 4) 1))
    (is (= (ref (identity-matrix 4) 1 2) 0))
    (is (= (ref (identity-matrix 4) 4 1) 0))
    (is (= (ref (identity-matrix 4) 1 4) 0)))
    