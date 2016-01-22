;;;; tests/permutation.lisp
;;;;
;;;; Copyright (c) 2014 Robert Smith

(in-package #:cl-permutation-tests)

(in-suite cl-permutation-suite)

(deftest test-perm-identity ()
  "Test that PERM-IDENTITY is consistent with PERM-IDENTITY-P."
  (is (perm:perm-identity-p (perm:perm-identity 1)))
  (is (perm:perm-identity-p (perm:perm-identity 2)))
  (is (perm:perm-identity-p (perm:perm-identity 3)))
  (is (perm:perm-identity-p (perm:perm-identity 4)))
  (is (perm:perm-identity-p (perm:perm-identity 5))))

(deftest test-from-cycles ()
  (is (perm= (from-cycles (list (make-cycle 1 2)))
             (make-perm 2 1)))
  (is (perm= (from-cycles (list (make-cycle 1 2 3)))
             (make-perm 2 3 1)))
  (is (perm= (from-cycles (list (make-cycle 1 2 3)
                                (make-cycle 4 5)))
             (make-perm 2 3 1 5 4))))

(defun naive-from-cycles (cycles size)
  (reduce #'perm-compose cycles
          :initial-value (perm-identity size)
          :key (lambda (c) (from-cycles (list c) size))))

(deftest test-from-cycles-overlapping ()
  (let ((cycles (list (make-cycle 6 7)
                      (make-cycle 2 4 5)
                      (make-cycle 1 9 4 8))))
    (is (perm= (from-cycles cycles)
               (naive-from-cycles cycles 9)))))
