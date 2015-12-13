;;;; tests/straight-line-program.lisp
;;;;
;;;; Copyright (c) 2015 Robert Smith

(in-package #:cl-permutation-tests)

(in-suite cl-permutation-suite)

(deftest test-slp ()
  "Test some example SLPs using all possible constructors."
  (let ((f (make-free-group 5))
        (ctx (make-instance 'slp-context)))
    (flet ((assign (a b)
             (setf (symbol-assignment ctx a) b)))
      ;; x1 = 4
      (assign :x1 (slp-element 4))

      ;; x2 = 5
      (assign :x2 (slp-element 5))

      ;; x3 = x1 x2
      (assign :x3 (compose-slp (slp-symbol :x1)
                               (slp-symbol :x2)))

      ;; x4 = x3^-1 * 2 * x2
      (assign :x4 (compose-slp (invert-slp (slp-symbol :x3))
                               (compose-slp (slp-element 2)
                                            (slp-symbol :x2))))
      
      (is (equal (make-free-group-element f -5 -4 2 5)
                 (evaluate-slp f ctx (slp-symbol :x4)))))))
