;;;; permutation.lisp
;;;; Copyright (c) 2012 Robert Smith

(in-package #:cl-permutation)

(defstruct (perm (:conc-name perm.)
                        (:print-function print-perm))
  (spec #() :type (vector (unsigned-byte *))
            :read-only t))

(defun print-perm (perm stream depth)
  "Printer for perms."
  (declare (ignore depth))
  (let* ((spec (perm.spec perm))
         (len (length spec)))
    (princ "#[" stream)
    
    (cond
      ((zerop len) nil)
      ((= 1 len) (format stream "~D" (1+ (aref spec 0))))
      (t (progn
          (format stream "~D" (1+ (aref spec 0)))
          (dotimes (i (- len 1))
            (format stream " ~D" (1+ (aref spec (1+ i))))))))
    
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
    (make-perm :spec (coerce (mapcar #'1- read-list) 'vector))))

(defun enable-perm-reader ()
  "Enable the use of #[...] for perms."
  ;; Set #[
  (set-dispatch-macro-character #\# #\[ #'perm-reader)
  
  ;; Set ]
  (set-macro-character #\] (get-macro-character #\))))


;;;;;;;;;;;;;;;;;;;;;;; Permutation operations ;;;;;;;;;;;;;;;;;;;;;;;

(defun perm-ref (perm n)
  "Compute what N maps to in the permutation PERM."
  (assert (<= 0 n (1- (perm-size perm)))
          (n)
          "Permutation index of ~D must be within the length of the ~
           permutation ~A."
          n perm)
  (aref (perm.spec perm) n))

(defun perm-size (perm)
  "The size of a permutation PERM."
  (length (perm.spec perm)))

(defun perm-length (perm)
  "Count the number of inversions in the permutation PERM."
  (let ((spec      (perm.spec perm))
        (n         (perm-size perm))
        (inv-count 0))
    (loop :for i :below (1- n)
          :do (loop :for j :from (1+ i) :to (1- n)
                    :when (> (svref spec i)
                             (svref spec j))
                    :do (incf inv-count)))
    
    inv-count))

(defun perm-even-p (perm)
  "Is PERM an even permutation?"
  (evenp (perm-length perm)))

(defun perm-odd-p (perm)
  "Is PERM an odd permutation?"
  (oddp (perm-length perm)))

(defun perm-compose (p1 p2)
  "Compose the permutations P1 and P2."
  (assert (= (perm-size p1)
             (perm-size p2))
          nil
          "Permutations ~A and ~A must have the same size."
          p1 p2)
  
  (let* ((n        (perm-size p1))
         (p12-spec (make-array n :element-type '(unsigned-byte *)
                                 :initial-element 0)))
    (loop :for i :below n
          :do (setf (aref p12-spec i)
                    (perm-ref p1 (perm-ref p2 i)))
          :finally (return (make-perm :spec p12-spec)))))

(defun perm-inverse (perm)
  "Find the inverse of the permutation PERM."
  (let* ((n          (perm-size perm))
         (perm*-spec (make-array n :element-type '(unsigned-byte *)
                                   :initial-element 0)))
    (loop :for i :below n
          :do (setf (aref perm*-spec (perm-ref perm i)) i)
          :finally (return (make-perm :spec perm*-spec)))))