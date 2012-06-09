;;;; permutation-generation.lisp
;;;; Copyright (c) 2012 Robert Smith

(in-package #:cl-permutation)

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
  (let ((perm t))
    (lambda ()
      ;; Check if PERM is NIL (if the generator was exhausted).
      (when perm
        ;; We do this hackery to be able to emit the initial
        ;; (identity) perm. Initially PERM is just T -- not a vector.
        (if (not (vectorp perm))
            (setf perm (iota-vector (1+ n)))
            (let ((next (next-perm perm n)))
              ;; If we are at the end, then set PERM to NIL.
              (if next
                  (%make-perm :spec (map 'vector #'abs next))
                  (setf perm nil))))))))

(defmacro doperms ((x n &optional result) &body body)
  "Iterate over all permutations of size N, optionally returning
RESULT."
  (let ((perm (gensym "PERM-"))
        (len (gensym "LEN-")))
    `(loop :with ,len := ,n
           :for ,perm := (iota-vector (1+,len)) :then (next-perm ,perm ,len)
           :while ,perm
           :do (let ((,x (%make-perm :spec (map 'vector 'abs ,perm))))
                 ,@body)
           :finally (return ,result))))
