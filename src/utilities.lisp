;;;; utilities.lisp
;;;; Copyright (c) 2011-2014 Robert Smith

;;;; Various portable utilities used in CL-PERMUTATION.

(in-package #:cl-permutation)

(deftype vector-size (&key (down-by 0))
  "Possible sizes of a vector."
  (check-type down-by (integer 0 #.array-total-size-limit))
  `(integer 0 ,(- array-total-size-limit down-by)))

(deftype vector-index ()
  "Possible indexes to a vector."
  `(integer 0 #.(1- array-total-size-limit)))

(defun iota (n)
  "Generate a list of numbers between 0 and N-1."
  (loop :for i :below n :collect i))

(defun iota-vector (n)
  "Generate the equivalent of (COERCE (IOTA N) 'VECTOR)."
  (loop :with a := (make-array n :element-type 'vector-index
                                 :initial-element 0)
        :for i :below n
        :do (setf (aref a i) i)
        :finally (return a)))

(defun iota+1 (n)
  "Generate a list of numbers between 1 and N."
  (loop :for i :from 1 :to n :collect i))

(defun random-between (a b)
  "Generate a random integer between A and B, inclusive."
  (assert (>= b a))
  (if (= a b)
      a
      (+ a (random (- (1+ b) a)))))

(defun nshuffle (vector &key (parity :any)
                             (start 0))
  "Shuffle the permutation vector VECTOR with specified parity PARITY. PARITY may be

    * :ANY  for any permutation
    * :EVEN for only even permutations
    * :ODD  for only odd permutations

START specifies the starting index where elements should be shuffled."
  
  (assert (member parity '(:any :even :odd)))
  
  (let ((n (length vector))
        (any? (eql parity :any)))
    (loop :for i :from start :below (if any? n (1- n))
          :for r := (random-between i (1- n))
          :when (/= i r)
          :do (progn
                (rotatef (aref vector i)
                         (aref vector r))
                (unless any?
                  (rotatef (aref vector (- n 1))
                           (aref vector (- n 2)))))
          :finally (progn
                     (when (and (eql parity :odd)
                                (< (1+ start) n))
                       (rotatef (aref vector start)
                                (aref vector (1+ start))))
                     (return vector)))))

(defun maximum (list &key (key 'identity))
  "Compute the maximum of LIST, optionally via the function KEY."
  (loop :for x :in list
        :maximizing (funcall key x)))

(defun product (seq &key (key 'identity))
  "Compute the product of the items in SEQ, optionally via the
function KEY."
  (reduce '* seq :key key :initial-value 1))

(defun sign (x)
  "Return the sign of X."
  (cond
    ((plusp x)  1)
    ((zerop x)  0)
    (t         -1)))

(defun hash-table-key-exists-p (hash-table key)
  "Check of KEY exists in HASH-TABLE."
  (multiple-value-bind (val existsp) (gethash key hash-table)
    (declare (ignore val))
    existsp))

(defun hash-table-keys (hash-table)
  "Return a list of the hash table keys of HASH-TABLE."
  (loop :for k :being :the :hash-keys :of hash-table
        :collect k))

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

;; XXX: Not used.
(define-condition hash-table-access-error (cell-error)
  ((table :initarg :table :reader hash-table-access-error-table)
   (key :initarg :key :reader hash-table-access-error-key))
  (:documentation "An error to be signalled if a key doesn't exist in
  a hash-table."))

(defun hash-table-elt (hash-table n)
  "Extract the Nth element from the hash table HASH-TABLE, given some arbitrary ordering of the keys and values. Return the Nth KEY and VALUE as two values."
  (check-type n integer)
  (assert (<= 0 n (1- (hash-table-count hash-table)))
          (n)
          "The index ~S provided is out of bounds.")
  (maphash #'(lambda (k v)
               (if (zerop n)
                   (return-from hash-table-elt (values k v))
                   (decf n)))
           hash-table))

;; XXX: Not used.
(defun safe-gethash (key hash-table)
  "Throw an error in the event that KEY foes not exist in HASH-TABLE. Othwerwise return the value."
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

;;; Queue Implementation
;;; from tarballs_are_good/lisp-random/stack-queue.lisp

(defstruct (queue (:constructor %make-queue)
                  (:predicate queuep))
  (elements nil :type list)
  (last nil :type (or null (cons t null))))

(defun make-queue ()
  "Create a new empty queue."
  (%make-queue))

(defun queue-empty-p (queue)
  "Is the queue QUEUE empty?"
  (null (queue-elements queue)))

(defun list-to-queue (list)
  "Convert the list LIST into a queue. Note: LIST may be modified."
  (%make-queue :elements list
               :last (last list)))

(defun enqueue (queue obj)
  "Add an element OBJ to the end of the queue QUEUE."
  (let ((last (list obj)))
    (if (queue-empty-p queue)
        ;; Set up the queue with the first element. Note that the same
        ;; reference to the singleton list is shared by both
        ;; QUEUE-ELEMENTS and QUEUE-LAST.
        (setf (queue-elements queue) last
              (queue-last queue)     last)
        
        ;; We can now append elements to QUEUE-ELEMENTS simply by
        ;; modifying QUEUE-LAST, whose reference is shared by
        ;; QUEUE-ELEMENTS,
        ;;
        ;; We do this instead of a single SETF for type safety of
        ;; QUEUE-LAST.
        (let ((old (queue-last queue)))
          (setf (queue-last queue) last
                (cdr old)          last))))
  queue)

(defun dequeue (queue)
  "Remove and return an element from the queue QUEUE."
  (pop (queue-elements queue)))

;;; Membership Sets

(defun make-membership-set (size)
  ;; 1+ to account for 1-based indexing
  (make-array (1+ size) :element-type 'bit :initial-element 0))

(defun membership-sets-intersect-p (set-a set-b)
  (some (lambda (a b) (= 1 a b)) set-a set-b))

(defun membership-set-nunion (set-a set-b)
  (map-into set-a #'logior set-a set-b))

(defun membership-set-count (set)
  (count 1 set))

(defun clear-membership-set (set)
  (map-into set (constantly 0)))
