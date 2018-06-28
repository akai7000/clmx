;;;; util.lisp
;;; Various utilities

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

(in-package #:clmx-util)

(defun numlength (n)
    (length (write-to-string n)))
    
(defun replace-all (string part replacement &key (test #'char=))
    (with-output-to-string (out)
      (loop with part-length = (length part)
            for old-pos = 0 then (+ pos part-length)
            for pos = (search part string
                              :start2 old-pos
                              :test test)
            do (write-string string out
                             :start old-pos
                             :end (or pos (length string)))
            when pos do (write-string replacement out)
            while pos)))
            
(defun intp (number)
    (= number (round number)))
	
(defun round-to (number precision &optional (func #'round))
    (let ((div (expt 10 precision)))
         (/ (funcall func (* number div)) div)))

;(defun fix-rounding (number &optional (precision 6))
;	(* 1.0 (round-to number precision)))
    
(defun fix-rounding (number &optional (precision 6))
	(round-to number precision))
    
(defun timer (function &rest params)
    "Function to test how many seconds a function takes."
    (let ((real-base (get-internal-real-time)))
         (apply function params)
         (* 1.0 (/ (- (get-internal-real-time) real-base) internal-time-units-per-second))))
