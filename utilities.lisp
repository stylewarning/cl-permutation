;;;; utilities.lisp
;;;; Copyright (c) 2011-2012 Robert Smith

;;;; Utilities.

(in-package #:cl-permutation)

(defun iota (n)
  "Generate a list of numbers between 0 and N-1."
  (loop :for i :below n :collect i))

(defun iota+1 (n)
  "Generate a list of numbers between 1 and N."
  (loop :for i :from 1 :to n :collect i))

(defun random-between (a b)
  "Generate a random integer between A and B, inclusive."
  (assert (>= b a))
  (if (= a b)
      a
      (+ a (random (- (1+ b) a)))))

(defun nshuffle (vector &optional (parity :any))
  "Shuffle the permutation vector VECTOR with specified parity
  PARITY. PARITY may be

    * :ANY  for any permutation
    * :EVEN for only even permutations
    * :ODD  for only odd permutations"
  
  (assert (member parity '(:any :even :odd)))
  
  (let ((n (length vector))
        (any? (eql parity :any)))
    (loop :for i :below (if any? n (1- n))
          :for r := (random-between i (1- n))
          :when (/= i r)
          :do (progn
                (rotatef (svref vector i)
                         (svref vector r))
                (unless any?
                  (rotatef (svref vector (- n 1))
                           (svref vector (- n 2)))))
          :finally (progn
                     (when (eql parity :odd)
                       (rotatef (svref vector 0)
                                (svref vector 1)))
                     (return vector)))))

(defun maximum (list &key (key 'identity))
  "Compute the maximum of LIST, optionally via the function KEY."
  (loop :for x :in list
        :maximizing (funcall key x)))

(defun product (list &key (key 'identity))
  "Compute the product of the items in LIST, optionally via the
function KEY."
  (reduce '* list :key key :initial-value 1))

(defun hash-table-key-exists-p (hash-table key)
  "Check of KEY exists in HASH-TABLE."
  (multiple-value-bind (val existsp) (gethash hash-table key)
    (declare (ignore val))
    existsp))

(defun hash-table-values (hash-table)
  "Return a list of the hash table values of HASH-TABLE."
  (loop :for v :being :the :hash-values :of hash-table
        :collect v))