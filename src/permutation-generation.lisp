;;;; permutation-generation.lisp
;;;; Copyright (c) 2012-2014 Robert Smith

(in-package #:cl-permutation)

;;;; This file implements the Steinhaus-Johnson-Trotter algorithm for
;;;; generating permutations.

;;;; XXX FIXME: We cons way too much here. Can we clean it up?

(defun map-into-perm (function perm-spec)
  (let* ((n    (length perm-spec))
         (spec (allocate-perm-vector (1- (length perm-spec)))))
    (dotimes (i n spec)
      (setf (aref spec i)
            (funcall function (aref perm-spec i))))))

(defun abs> (x y)
  (> (abs x)
     (abs y)))

(defun mobilep (idx perm &optional (len (length perm)))
  (let ((val (aref perm idx)))
    (if (plusp val)                     ; Is the value a "left"
                                        ; directed value?

        ;; Left directed.
        (and (> idx 1)                        ; Check that the index
                                              ; is non-zero.
             (abs> val (aref perm (1- idx)))) ; Check the neighbor.
        
        ;; Right directed.
        (and (not (= len idx))                   ; Check that the index
                                                 ; is not maximal.
             (abs> val (aref perm (1+ idx))))))) ; Check the neighbor.

(defun reverse-direction (idx perm)
  (setf (aref perm idx) (- (aref perm idx))))

(defun exists-mobile-p (perm len)
  (loop :for i :from 1 :to len
        :thereis (mobilep i perm len)))

(defun next-perm (perm len)
  (let ((idx -1)
        (max-mob -1))
    (when (exists-mobile-p perm len)
      ;; Find the largest mobile
      (loop :for i :from 1 :to len
            :for x := (aref perm i)
            :if (and (mobilep i perm len)
                     (abs> x max-mob))
              :do (setf idx     i
                        max-mob x)
            :finally (let ((adj-idx (- idx (sign max-mob))))
                       ;; Swap the largest mobile element with its
                       ;; adjacent partner
                       (rotatef (aref perm idx)
                                (aref perm adj-idx))
                       
                       ;; Reverse the direction of all larger
                       ;; elements.
                       (loop :for i :from 1 :to len
                             :for x := (aref perm i)
                             :when (abs> x max-mob)
                               :do (reverse-direction i perm))))
      perm)))

(defun make-perm-generator (n)
  "Create a generator that generates permutations of size N."
  (assert (plusp n)
          (n)
          "Must provide a positive size for permutation generation. Given ~D."
          n)
  (let ((perm t))
    (lambda ()
      ;; Check if PERM is NIL (if the generator was exhausted).
      (when perm
        ;; We do this hackery to be able to emit the initial
        ;; (identity) perm. Initially PERM is just T -- not a vector.
        (if (not (vectorp perm))
            (progn
              (setf perm (make-array (1+ n) :initial-contents (iota (1+ n))))
              (%make-perm :spec (map-into-perm #'abs perm)))
            (let ((next (next-perm perm n)))
              ;; If we are at the end, then set PERM to NIL.
              (if next
                  (%make-perm :spec (map-into-perm #'abs next))
                  (setf perm nil))))))))

(defmacro doperms ((x n &optional result) &body body)
  "Iterate over all permutations of size N, optionally returning
RESULT."
  (let ((perm (gensym "PERM-"))
        (len (gensym "LEN-")))
    `(let ((,len ,n))
       (assert (plusp ,len)
               (,len)
               "Must provide a positive size for permutation generation. Given ~D."
               ,len)
       (loop :for ,perm := (make-array (1+ ,len)
                                       :initial-contents (iota (1+ ,len)))
               :then (next-perm ,perm ,len)
             :while ,perm
             :do (let ((,x (%make-perm :spec (map-into-perm #'abs ,perm))))
                   ,@body)
             :finally (return ,result)))))
