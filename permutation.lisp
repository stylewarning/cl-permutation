;;;; permutation.lisp
;;;; Copyright (c) 2012 Robert Smith

(in-package #:cl-permutation)



;;;;;;;;;;;;;;;;;;;;; PERMUTATION DATA STRUCTURE ;;;;;;;;;;;;;;;;;;;;;

(defstruct (perm (:conc-name perm.)
                 (:print-function print-perm)
                 (:constructor %make-perm))
  (spec #(0) :type (vector (unsigned-byte *))
             :read-only t))

#+#:ignore
(defun print-perm (perm stream depth)
  "Printer for perms."
  (declare (ignore depth))
  (let* ((spec (perm.spec perm))
         (len (length spec)))
    (princ "#[" stream)
    
    (cond
      ((zerop len) (error "Inconsistent permutation; has zero elements."))
      ((= 1 len) nil)
      ((= 2 len) (format stream "~D" (aref spec 1)))
      (t (progn
           (format stream "~D" (aref spec 1))
           (dotimes (i (- len 2))
             (format stream " ~D" (aref spec (+ 2 i)))))))
    
    (princ "]" stream)))

(defun print-perm (perm stream depth)
  "Printer for perms."
  (declare (ignore depth))
  (print-unreadable-object (perm stream :type t :identity nil)
    (let* ((spec (perm.spec perm))
           (len (length spec)))
      (cond
        ((zerop len) (error "Inconsistent permutation; has zero elements."))
        ((= 1 len) nil)
        ((= 2 len) (format stream "~D" (aref spec 1)))
        (t (progn
             (format stream "~D" (aref spec 1))
             (dotimes (i (- len 2))
               (format stream " ~D" (aref spec (+ 2 i))))))))))

(defun contains-1-to-N (elements)
  "Check that ELEMENTS contains the integers between 1 and the length
of the list, inclusive."
  (let ((len (length elements)))
    (loop :for i :in elements
          :sum i :into s
          :finally (return (= s (/ (* len (1+ len)) 2))))))

(defun assert-valid-permutation-elements (elements)
  "Verify (via assertions) that the elements "
  (assert (every 'integerp elements)
            nil
            "Permutation must only have integers.")
  
  (assert (every 'plusp elements)
            nil
            "Permutation must contain positive numbers only.")
  
  (assert (contains-1-to-N elements)
          nil
          "Given permutation must contain the numbers 1 to ~A"
          (length elements)))

(defun perm-reader (stream char n)
  (declare (ignore char n))
  (let ((read-list (read-delimited-list #\] stream t)))
    (assert-valid-permutation-elements read-list)

    (%make-perm :spec (coerce (cons 0 read-list) 'vector))))

(defun enable-perm-reader ()
  "Enable the use of #[...] for perms."
  ;; Set #[
  (set-dispatch-macro-character #\# #\[ #'perm-reader)
  
  ;; Set ]
  (set-macro-character #\] (get-macro-character #\))))

(defun allocate-perm-vector (n)
  "Allocate a vector compatible with a size-N permutation."
  (make-array (1+ n) :element-type '(unsigned-byte *)
                     :initial-element 0))



;;;;;;;;;;;;;;;;;;;;;;; PERMUTATION OPERATIONS ;;;;;;;;;;;;;;;;;;;;;;;

(defun list-to-perm (list)
  "Construct a perm from a list LIST."
  (assert-valid-permutation-elements list)
  (%make-perm :spec (coerce (cons 0 (copy-list list)) 'vector)))

(defun make-perm (&rest elements)
  "Construct a permutation from the numbers ELEMENTS."
  (list-to-perm elements))

(defun perm-identity (n)
  "The identity permutation of size N."
  (%make-perm :spec (iota-vector (1+ n))))

(defun perm-identity-p (perm)
  "Is the permutation PERM an identity permutation?"
  (equalp (perm.spec perm)
          (perm.spec (perm-identity (perm-size perm)))))

(defun random-perm (n &optional (parity :any))
  "Make a random permutation of size N. PARITY specifies the parity of
  the permutation:

    * :ANY  for any permutation
    * :EVEN for only even permutations
    * :ODD  for only odd permutations"
  (let ((spec-0 (coerce (iota+1 n) 'vector)))
    (%make-perm :spec (concatenate 'vector
                                  #(0)
                                  (nshuffle spec-0 parity)))))

(defun perm-ref (perm n)
  "Compute the zero-based index of PERM at N."
  (assert (<= 0 n (1- (perm-size perm)))
          (n)
          "Permutation reference index of ~D must be within the length of the ~
           permutation ~A."
          n perm)
  (aref (perm.spec perm) (1+ n)))

(defun perm-eval (perm n)
  "Evaluate the permutation PERM at index N."
  (assert (<= 1 n (perm-size perm))
          (n)
          "Permutation index of ~D must be within 1 and the length of the ~
           permutation ~A."
          n perm)
  (aref (perm.spec perm) n))

(defun perm-eval* (perm n)
  "Evaluate the permutation PERM at index N."
  (assert (<= 1 n)
          (n)
          "Permutation index of ~D must be greater than 1."
          n)
  (if (> n (perm-size perm))
      n
      (aref (perm.spec perm) n)))

(defun perm-size (perm)
  "The size of a permutation PERM."
  (1- (length (perm.spec perm))))

(defun perm-length (perm)
  "Count the number of inversions in the permutation PERM."
  (let ((n         (perm-size perm))
        (inv-count 0))
    (loop :for i :from 1 :to (1- n)
          :do (loop :for j :from (1+ i) :to n
                    :when (> (perm-eval perm i)
                             (perm-eval perm j))
                    :do (incf inv-count)))
    
    inv-count))

(defun perm-even-p (perm)
  "Is PERM an even permutation?"
  (evenp (perm-length perm)))

(defun perm-odd-p (perm)
  "Is PERM an odd permutation?"
  (oddp (perm-length perm)))

(defun perm-sign (perm)
  "The sign of a permutation PERM."
  (if (perm-even-p perm) 1 -1))

(defun perm-compose (p1 p2)
  "Compose the permutations P1 and P2: x |-> P2(P1(x))."
  #+#:ignore
  (assert (= (perm-size p1)
             (perm-size p2))
          nil
          "Permutations ~A and ~A must have the same size."
          p1 p2)
  
  (let* ((n        (perm-size p1))
         (p12-spec (allocate-perm-vector n)))
    (loop :for i :from 1 :to n
          :do (setf (aref p12-spec i)
                    (perm-eval* p1 (perm-eval* p2 i)))
          :finally (return (%make-perm :spec p12-spec)))))

(defun perm-expt (perm n)
  "Raise a permutation PERM to the Nth power."
  (check-type n integer)
  (assert (not (minusp n))
          (n)
          "Exponent must be non-negative. Given ~S,"
          n)

  (labels ((rec (current-perm n)
             (if (= n 1) 
                 current-perm
                 (rec (perm-compose perm current-perm) (1- n)))))
    (if (zerop n)
        (perm-identity (perm-size perm))
        (rec perm n))))

(defun perm-order (perm)
  "Compute the order of a permutation PERM."
  (labels ((rec (current-perm n)
             (if (perm-identity-p current-perm)
                 n
                 (rec (perm-compose perm current-perm) (1+ n)))))
    (rec perm 1)))

(defun perm-transpose-indexes (perm a b)
  "Transpose the indexes A and B in PERM."
  (assert (<= 1 a (perm-size perm))
          (a)
          "The first transposition index ~A must be in the range of ~
           the permutation."
          a)
  
  (assert (<= 1 b (perm-size perm))
          (b)
          "The second transposition index ~A must be in the range of ~
           the permutation."
          b)
  
  (let ((transposed-spec (copy-seq (perm.spec perm))))
    (rotatef (aref transposed-spec a)
             (aref transposed-spec b))
    (%make-perm :spec transposed-spec)))

(defun perm-transpose-entries (perm a b)
  "Transpose the entries A and B in PERM."
  (assert (<= 1 a (perm-size perm))
          (a)
          "The first transposition index ~A must be in the range of ~
           the permutation."
          a)
  
  (assert (<= 1 b (perm-size perm))
          (b)
          "The second transposition index ~A must be in the range of ~
           the permutation."
          b)
  
  (let* ((transposed-spec (copy-seq (perm.spec perm)))
         (pos-a (position a transposed-spec))
         (pos-b (position b transposed-spec)))
    (rotatef (aref transposed-spec pos-a)
             (aref transposed-spec pos-b))
    (%make-perm :spec transposed-spec)))

(defun perm-inverse (perm)
  "Find the inverse of the permutation PERM."
  (let* ((n          (perm-size perm))
         (perm*-spec (allocate-perm-vector n)))
    (loop :for i :from 1 :to n
          :do (setf (aref perm*-spec (perm-eval perm i)) i)
          :finally (return (%make-perm :spec perm*-spec)))))

(defun permute (perm a)
  "Permute A according to PERM."
  (assert (<= (perm-size perm)
              (length a))
          (perm a)
          "Only able to permute arrays whose size is greater or equal to PERM.")
  
  (let* ((len (length a))
         (result (make-array len)))
    (loop :for i :below len
          :do (setf (aref result i)
                    (aref a (1- (perm-eval* perm (1+ i)))))
          :finally (return result))))

; HI!!!
; HELLO


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; CYCLES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun orbit-of (n perm)
  "Compute the orbit of the element N in the permutation PERM."
  (labels ((orb (k cycle)
             (if (= n k)
                 (cons n (reverse cycle))
                 (orb (perm-eval perm k)
                      (cons k cycle)))))
    (orb (perm-eval perm n) nil)))

;;; We could reduce (mod n (length cycle))
(defun rotate-cycle-clockwise (cycle &optional (n 1))
  "Rotate the elements of a cycle syntactically clockwise, a total of
N times. When N is negative, rotate counterclockwise."
  (cond
    ((null cycle) nil)
    ((zerop n) cycle)
    ((plusp n) (rotate-cycle-clockwise
                (cons (last cycle)
                      (butlast cycle))
                (1- n)))
    ((minusp n) (rotate-cycle-counterclockwise cycle (- n)))))

;;; We could reduce (mod n (length cycle))
(defun rotate-cycle-counterclockwise (cycle &optional (n 1))
  "Rotate the elements of a cycle CYCLE syntactically
counterclockwise, a total of N times. When N is negative, rotate
clockwise."
  (cond
    ((null cycle) nil)
    ((zerop n) cycle)
    ((plusp n) (rotate-cycle-counterclockwise
                (append (cdr cycle)
                        (list (car cycle)))
                (1- n)))
    ((minusp n) (rotate-cycle-clockwise cycle (- n)))))

(defun normalize-cycle-order (cycle)
  "Rotate a cycle CYCLE so its least value is syntactically first."
  (let* ((minimum (reduce #'min cycle))
         (min-pos (position minimum cycle)))
    (rotate-cycle-counterclockwise cycle min-pos)))

(defun normalize-cycles (cycles)
  "Normalize each cycle in CYCLES, then normalize the list of cycles
in descending length (or if the length is the same, ascending first
element)."
  (sort (mapcar #'normalize-cycle-order
                (remove-if #'singletonp cycles))
        (lambda (x y)
          (let ((lenx (length x))
                (leny (length y)))
            (if (= lenx leny)
                (< (first x) (first y))
                (> lenx leny))))))

(defun to-cycles (perm &key (normalizep t))
  "Convert a permutation PERM in its standard representation to its
cycle representation."
  (labels ((next-cycle (todo cycles)
             (if (null todo)
                 cycles
                 (let ((new-cycle (orbit-of (car todo) perm)))
                   (next-cycle (set-difference todo new-cycle)
                               (cons new-cycle cycles))))))
    (let ((cycs (next-cycle (iota+1 (perm-size perm)) nil)))
      (if normalizep
          (normalize-cycles cycs)
          cycs))))

(defun decompose-cycle-to-maps (cycle)
  "Convert a cycle CYCLE to a list of pairs (a_i . b_i) such that a
permutation is the composition of a_i |-> b_i."
  (cond
    ((null cycle) (list nil))
    ((singletonp cycle) (list (cons (car cycle) (car cycle))))
    (t (labels ((get-swaps (the-cycle swaps first-element)
                  (if (null the-cycle)
                      swaps
                      (get-swaps (cdr the-cycle)
                                 (cons (cons (car the-cycle)
                                             (if (null (cdr the-cycle))
                                                 first-element
                                                 (cadr the-cycle)))
                                       swaps)
                                 first-element))))
         (get-swaps cycle nil (car cycle))))))

(defun from-cycles (cycles &optional (size 0))
  "Convert a cycle representation of a permutation CYCLES to the
standard representation."
  (let* ((maximum (max size (reduce #'max
                                    (mapcar #'(lambda (x)
                                                (apply #'max x)) cycles))))
         (perm (coerce (iota (1+ maximum)) 'vector)))
    (dolist (mapping
             (mapcan #'decompose-cycle-to-maps cycles)
             (%make-perm :spec perm))
      (setf (aref perm (car mapping))
            (cdr mapping)))))

(defun cycles-to-one-line (cycles)
  "Convert CYCLES to one-line notation. This is not the same as
  FROM-CYCLES."
  (%make-perm :spec (coerce (cons 0 (mapcan 'identity cycles)) 'vector)))