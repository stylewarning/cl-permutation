;;;; permutation-special.lisp
;;;; Copyright (c) 2014 Robert Smith

(in-package #:cl-permutation)

;;;; This file contains some special permutation operations and
;;;; computations, that are usually used in specific areas of math.

;;; This function could be computed more efficiently with a better
;;; data structure for merging elements. It would also cons less. It
;;; could be reduced to O(N*log N) where N is the size of the
;;; permutations W and V.
(defun bruhat<= (w v)
  "Does W precede V in the Bruhat sense, or are they equal?"
  (assert (= (perm-size w)
             (perm-size v))
          (w v)
          "The sizes of the perms W and V must be equal.")
  (let ((collected-w nil)
        (collected-v nil))
    (labels ((lex<= (a b)
               (loop :for ai :in a
                     :for bi :in b
                     :do
                        (cond
                          ((< ai bi) (return t))
                          ((> ai bi) (return nil)))
                     :finally (return t))))
      (loop :for i :from 1 :to (perm-size w) :do
        (setf collected-w (merge 'list collected-w
                                 (list (perm-eval w i))
                                 #'<)
              collected-v (merge 'list collected-v
                                 (list (perm-eval v i))
                                 #'<))
        (unless (lex<= collected-w collected-v)
          (return-from bruhat<= nil)))
      
      ;; Every section is satisfied.
      t)))

(defun bruhat< (w v)
    "Does W precede V in the Bruhat sense?

We say that W precedes V in the Bruhat sense if there's a transposition S with V = WS and the length of V is one less the length of W."
  (assert (= (perm-size w)
             (perm-size v))
          (w v)
          "The sizes of the perms W and V must be equal.")
  (and (not (perm= w v))
       (bruhat<= w v)))
