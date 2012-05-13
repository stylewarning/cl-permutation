;;;; permutation.lisp
;;;; Copyright (c) 2012 Robert Smith

(in-package #:cl-permutation)

(defstruct (perm (:conc-name perm.)
;;                 (:print-function print-perm)
                 )
  (spec #(0) :type (vector (unsigned-byte *))
             :read-only t))

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

(defun verify-perm-elements (elements)
  (let ((len (length elements)))
    (loop :for i :in elements
          :sum i :into s
          :finally (return (= s (/ (* len (1+ len)) 2))))))

(defun perm-reader (stream char n)
  (declare (ignore char n))
  (let ((read-list (read-delimited-list #\] stream t)))
    (assert (every 'integerp read-list)
            nil
            "Permutation syntax must only have integers.")

    (assert (every 'plusp read-list)
            nil
            "Permutation syntax must contain positive numbers only.")
    
    (assert (verify-perm-elements read-list)
            nil
            "Permutation syntax must contain the numbers 1 to ~A ~
             for the permutation given." (length read-list))
    (make-perm :spec (coerce (cons 0 read-list) 'vector))))

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

;;;;;;;;;;;;;;;;;;;;;;; Permutation operations ;;;;;;;;;;;;;;;;;;;;;;;

(defun perm-identity (n)
  "The identity permutation of size N."
  (make-perm :spec (coerce (iota (1+ n)) 'vector)))

(defun random-perm (n &optional (parity :any))
  "Make a random permutation of size N. PARITY specifies the parity of
  the permutation:

    * :ANY  for any permutation
    * :EVEN for only even permutations
    * :ODD  for only odd permutations"
  (let ((spec-0 (coerce (iota+1 n) 'vector)))
    (make-perm :spec (concatenate 'vector
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
  "Compose the permutations P1 and P2."
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
                    (perm-eval p1 (perm-eval p2 i)))
          :finally (return (make-perm :spec p12-spec)))))

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
    (make-perm :spec transposed-spec)))

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
    (make-perm :spec transposed-spec)))

(defun perm-inverse (perm)
  "Find the inverse of the permutation PERM."
  (let* ((n          (perm-size perm))
         (perm*-spec (allocate-perm-vector n)))
    (loop :for i :from 1 :to n
          :do (setf (aref perm*-spec (perm-eval perm i)) i)
          :finally (return (make-perm :spec perm*-spec)))))
          
; HI!!!
