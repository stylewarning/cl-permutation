;;;; tests/block.lisp
;;;;
;;;; Copyright (c) 2015-2018 Robert Smith

(in-package #:cl-permutation-tests)

(deftest test-simple-block-system ()
  "Compute the block system for a simple group."
  (let ((g (group-from-cycles (list (list (make-cycle 1 2 3 4 5 6))
                                    (list (make-cycle 2 6)
                                          (make-cycle 3 5)))
                              6)))
    (let ((blocks (find-minimal-block-system-containing g '(1 3))))
      (is (find '(1 3 5) blocks :test 'equalp))
      (is (find '(2 4 6) blocks :test 'equalp)))))

(deftest test-bike-blocks ()
  "Test that blocks are computed correctly for the bicycle groups."
  (let* ((bike (perm-examples:make-bicycle 3))
         (bike-blocks (perm::raw-block-subsystems bike))
         (broke-bike (perm-examples:make-bicycle-with-fixed-wheel 3))
         (broke-bike-blocks (perm::raw-block-subsystems broke-bike)))
    (is (= 1 (length bike-blocks)))
    (is (= 1 (length broke-bike-blocks)))
    (is (equal bike-blocks broke-bike-blocks))))

(deftest test-rubik-block-system ()
  "Compute the block system of subdirect factors, and ensure they look right."
  (let ((bss (perm::raw-block-subsystems *3x3*)))
    (destructuring-bind (edges corners) bss
      (values
       (is (every (lambda (b) (= 2 (length b))) edges))
       (is (every (lambda (b) (= 3 (length b))) corners))))))
