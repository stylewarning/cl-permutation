;;;; examples/symmetric.lisp
;;;;
;;;; Copyright (c) 2014 Robert Smith

(in-package #:cl-permutation-examples)

;;; 120
(defun make-S_5 ()
  (group-from
   '((2 1 3 4 5)
     (1 3 2 4 5)
     (1 2 4 3 5)
     (1 2 3 5 4))))
