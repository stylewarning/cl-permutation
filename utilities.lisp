;;;; utilities.lisp
;;;; Copyright (c) 2011-2012 Robert Smith

;;;; Utilities.

(defun iota (n)
  (loop :for i :below n :collect i))
