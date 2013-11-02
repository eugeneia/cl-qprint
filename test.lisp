;;;; Test CL-QPRINT.

(defpackage cl-qprint-test
  (:documentation
   "Test routines for CL-QPRINT.")
  (:use :cl
        :cl-qprint)
  (:export :test-cl-qprint))

(in-package :cl-qprint-test)

(defparameter *value-range* 256
  "Range of values in random vectors.")

(defparameter *test-iterations* 1024
  "Number of random strings tested.")

(defun random-vector (length)
  "Return a random vector of LENGTH."
  (let ((vector (make-array length)))
    (loop for i from 0 to (1- length)
       do (setf (aref vector i) (random *value-range*)))
    vector))

(defun equal-vec (x y)
  "Are vectors X, Y equal?"
  (loop for a across x
        for b across y
     unless (= a b)
     return nil
     finally (return t)))

(defun test-cl-qprint ()
  "Test integritiy of CL-QPRINT by asserting the following to be true:

{(DECODE (ENCODE input)) ⇒ input}

for {input} being randomized data."
  (loop for i from 0 to *test-iterations*
     for vector = (random-vector i)
     for digested-vector = (decode (encode vector) :error-p t)
     do (assert (equal-vec vector digested-vector))))
