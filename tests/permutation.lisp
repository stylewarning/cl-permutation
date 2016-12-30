;;;; tests/permutation.lisp
;;;;
;;;; Copyright (c) 2014 Robert Smith

(in-package #:cl-permutation-tests)

(deftest test-perm-identity ()
  "Test that PERM-IDENTITY is consistent with PERM-IDENTITY-P."
  (dotimes (i 5)
    (is (perm:perm-identity-p (perm:perm-identity i)))))

(deftest test-from-cycles ()
  (is (perm= (from-cycles (list (make-cycle 1 2)))
             (make-perm 2 1)))
  (is (perm= (from-cycles (list (make-cycle 1 2 3)))
             (make-perm 2 3 1)))
  (is (perm= (from-cycles (list (make-cycle 1 2 3)
                                (make-cycle 4 5)))
             (make-perm 2 3 1 5 4))))

(deftest test-perm-compose ()
  (let ((a (make-perm 2 1))
        (b (make-perm 2 1 4 3))
        (c (make-perm 1 3 2)))
    (is (perm= (make-perm 1 2 4 3) (perm-compose a b)))
    (is (perm= (make-perm 2 3 1)   (perm-compose a c)))
    (is (perm= (make-perm 2 4 1 3) (perm-compose b c)))
    (is (perm= (make-perm 1 2 4 3) (perm-compose b a)))
    (is (perm= (make-perm 3 1 2)   (perm-compose c a)))
    (is (perm= (make-perm 3 1 4 2) (perm-compose c b)))))

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
