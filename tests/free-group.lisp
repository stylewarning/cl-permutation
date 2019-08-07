;;;; free-group.lisp
;;;;
;;;; Copyright (c) 2019 Robert Smith

(in-package #:cl-permutation-tests)

(deftest test-word-simplification ()
  "Test that words simplify OK."
  (let* ((orders (vector nil 4 2 4 4 4 4))
         (comms  (vector nil '(2) '(1) '(4) '(3) '(6) '(5)))
         (f (cl-permutation::word-simplifier orders comms)))
    (flet ((test (candidate expected)
             (is (equalp expected (funcall f candidate)))))
      (test '0 'NIL)
      (test 'NIL 'NIL)
      (test '(0) 'NIL)
      (test '(1) '(1))
      (test '(1 1) '(1 1))
      (test '(1 1 1) '(-1))
      (test '(1 1 1 1) 'NIL)
      (test '(1 1 1 1 1) '(1))
      (test '(-1) '(-1))
      (test '(-1 -1) '(1 1))
      (test '(-1 -1 -1) '(1))
      (test '(-1 -1 -1 -1) 'NIL)
      (test '(-1 -1 -1 -1 -1) '(-1))
      (test '(1 2) '(1 2))
      (test '(1 2 1 2) '(1 1))
      (test '(1 2 1 2 1 2) '(-1 2))
      (test '(1 2 1 2 1 2 1 2) 'NIL)
      (test '(1 2 1 2 1 2 1 2 1 2) '(1 2)))))
