;;;; tests/block.lisp
;;;;
;;;; Copyright (c) 2015 Robert Smith

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

(deftest test-rubik-block-system ()
  "Compute the block system of subdirect factors, and ensure they look right."
  (let ((bss (block-systems (perm-examples:make-rubik-3x3))))
    (destructuring-bind (edges corners) bss
      (values
       (is (every (lambda (b) (= 2 (length b))) edges))
       (is (every (lambda (b) (= 3 (length b))) corners))))))
