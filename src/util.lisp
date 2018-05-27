;;;; Various utilities
;;; util.lisp

(in-package #:mxcl-util)

(defun digits (n)
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