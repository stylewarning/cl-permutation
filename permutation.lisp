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

