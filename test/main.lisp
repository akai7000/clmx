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

(test test-create-matrix
    "Testing create-matrix."
    (is (= (mx:ref (mx:create-matrix :dimensions '(4 3) :initial-element 5) 1 1) 5))
    (is (= (mx:ref (mx:create-matrix :dimensions '(4 3) :initial-element 5) 4 3) 5))
    (is (= (mx:ref (mx:create-matrix :contents '((1 2 3) (4 5 6))) 1 1) 1))
    (is (= (mx:ref (mx:create-matrix :contents '((1 2 3) (4 5 6))) 2 3) 6)))
  
(test test-zero-matrix
    "Testing zero-matrix."
    (is (= (mx:ref (mx:zero-matrix 4 3) 1 1) 0))
    (is (= (mx:ref (mx:zero-matrix 4 3) 4 3) 0)))
  
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