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

(test add-matrices
    "Testing add-matrices."
    (let ((a (mx:create-matrix :contents '((1 2) (3 4))))
          (b (mx:create-matrix :contents '((2 -2) (7 5))))
          (c (mx:create-matrix :contents '((3.2 2.8 3.7) (-0.4 7.9 2.1))))
          (d (mx:create-matrix :contents '((0.3 -2.0 -0.5) (1.1 3.4 9.9)))))
      (is (= (mx:ref (mx:add-matrices a b) 1 1) 3))
      (is (= (mx:ref (mx:add-matrices a b) 1 2) 0))
      (is (= (mx:ref (mx:add-matrices a b) 2 1) 10))
      (is (= (mx:ref (mx:add-matrices a b) 2 2) 9))
      (is (= (mx:ref (mx:add-matrices c d) 1 2) 0.8))
      (is (= (mx:ref (mx:add-matrices c d) 2 2) 11.3))
      (is (= (mx:ref (mx:add-matrices c d) 2 3) 12.0))))
	  
(test add-scalar
	"Testing add-scalar."
	(let ((a (mx:create-matrix :contents '((1 2) (3 4))))
	      (b (mx:create-matrix :contents '((3.2 2.8 3.7) (-0.4 7.9 2.1)))))
	   (is (= (mx:ref (mx:add-scalar a 5) 1 1) 6))
	   (is (= (mx:ref (mx:add-scalar a 5) 2 2) 9))
	   (is (= (mx:ref (mx:add-scalar b 1.1) 1 1) 4.3))
	   (is (= (mx:ref (mx:add-scalar b 1.1) 2 1) 0.7))
	   (is (= (mx:ref (mx:add-scalar b 1.1) 2 3) 3.2))))
	
(test test-create-matrix
    "Testing create-matrix."
    (is (= (mx:ref (mx:create-matrix :dimensions '(4 3) :initial-element 5) 1 1) 5))
    (is (= (mx:ref (mx:create-matrix :dimensions '(4 3) :initial-element 5) 4 3) 5))
    (is (= (mx:ref (mx:create-matrix :contents '((1 2 3) (4 5 6))) 1 1) 1))
    (is (= (mx:ref (mx:create-matrix :contents '((1 2 3) (4 5 6))) 2 3) 6)))
  
(test test-zero-matrix
    "Testing zero-matrix."
    (is (= (mx:ref (mx:zero-matrix 4 3) 1 1) 0))
    (is (= (mx:ref (mx:zero-matrix 4 3) 4 3) 0))
    (is (= (mx:ref (mx:zero-matrix 1 1) 1 1) 0)))
  
(test test-unit-matrix
    "Testing unit-matrix."
    (is (= (mx:ref (mx:unit-matrix 4 3) 1 1) 1))
    (is (= (mx:ref (mx:unit-matrix 4 3) 4 3) 1)))
  
(test test-identity-matrix
    "Testing identity-matrix."
    (is (= (mx:ref (mx:identity-matrix 4) 1 1) 1))
    (is (= (mx:ref (mx:identity-matrix 4) 4 4) 1))
    (is (= (mx:ref (mx:identity-matrix 4) 1 2) 0))
    (is (= (mx:ref (mx:identity-matrix 4) 4 1) 0))
    (is (= (mx:ref (mx:identity-matrix 4) 1 4) 0)))
    