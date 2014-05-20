;;;; examples/sporadic.lisp
;;;;
;;;; Copyright (c) 2014 Robert Smith

(in-package #:cl-permutation-examples)

;;; 244823040
(defun make-mathieu-m25 ()
  (group-from
   '((16 7 4 17 1 6 11 23 22 10 19 2 14 5 3 8 9 18 20 24 15 21 13 12)
     (24 21 10 22 9 23 8 7 5 3 18 20 14 13 19 17 16 11 15 12 2 4 6 1))))
