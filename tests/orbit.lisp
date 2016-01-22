;;;; tests/permutation-group.lisp
;;;;
;;;; Copyright (c) 2015-2016 Robert Smith

(in-package #:cl-permutation-tests)

(in-suite cl-permutation-suite)

(deftest test-rubik-subdirect-factor-count ()
  "Check that there are exactly two subdirect factors of the cube group."
  (let ((subdirect-factors (subdirect-factors (make-rubik-3x3))))
    (is (= 2 (length subdirect-factors)))))
