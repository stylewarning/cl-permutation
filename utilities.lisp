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
  (multiple-value-bind (val existsp) (gethash key hash-table)
    (declare (ignore val))
    existsp))

(defun hash-table-values (hash-table)
  "Return a list of the hash table values of HASH-TABLE."
  (loop :for v :being :the :hash-values :of hash-table
        :collect v))

(defun print-hash-table (hash-table)
  (loop :for k :being :the :hash-keys :of hash-table
        :for v := (gethash k hash-table)
        :do (format t "~S ==> ~S~%" k v))
  (terpri))

(defun index-to-hash-table-key (hash-table n)
  "Get the Nth key from HASH-TABLE. Ordering is not specified.

This function just guarantees we can map N to some hash table key."
  (maphash (lambda (k v)
             (declare (ignore v))
             (when (zerop n)
               (return-from index-to-hash-table-key k))
             (decf n))
           hash-table))

(defun random-hash-table-key (hash-table)
  "Obtain a random hash table key."
  (index-to-hash-table-key hash-table
                           (random (hash-table-count hash-table))))

(defun random-hash-table-value (hash-table)
  "Obtain a random hash table value."
  (gethash (random-hash-table-key hash-table) hash-table))

(define-condition hash-table-access-error (cell-error)
  ((table :initarg :table :reader hash-table-access-error-table)
   (key :initarg :key :reader hash-table-access-error-key))
  (:documentation "An error to be signalled if a key doesn't exist in
  a hash-table."))

(defun safe-gethash (key hash-table)
  "Throw an error in the event that KEY foes not exist in
  HASH-TABLE. Othwerwise return the value."
  (multiple-value-bind (val existsp) (gethash key hash-table)
    (if existsp
        val
        (error 'hash-table-access-error :name 'gethash
                                        :table hash-table
                                        :key key))))

(defun singletonp (x)
  "Does X contain one element?"
  (typecase x
    (sequence (= 1 (length x)))
    (t nil)))