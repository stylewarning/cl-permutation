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

(deftest test-rubik-intrablock-groups ()
  "Test that the intrablock groups of the Rubik's cube are calculated correctly."
  (let* ((subsystems (perm::group-block-subsystems *3x3*))
         (corners (find 3 subsystems :test #'= :key #'perm::block-subsystem-block-size))
         (edges   (find 2 subsystems :test #'= :key #'perm::block-subsystem-block-size))
         (corner-gens (perm::block-subsystem-intrablock-group-generators corners))
         (edge-gens   (perm::block-subsystem-intrablock-group-generators edges))
         (corner-intra (perm:generate-perm-group corner-gens))
         (edge-intra   (perm:generate-perm-group edge-gens)))
    (is (= 3 (perm:group-degree corner-intra :true t)))
    (is (= 3 (perm:group-order corner-intra)))
    (is (perm:group-element-p (perm:make-perm 2 3 1) corner-intra))

    (is (= 2 (perm:group-degree edge-intra :true t)))
    (is (= 2 (perm:group-order edge-intra)))
    (is (perm:group-element-p (perm:make-perm 2 1) edge-intra))))

(deftest test-orientation-of-identity-and-superflip ()
  (let* ((edge-subsys (first (perm::group-block-subsystems *3x3*)))
         (coord (perm::intrablock-coordinate-function edge-subsys))
         (identity (group-identity *3x3*))
         (superflip (from-cycles (mapcar (lambda (x) (apply #'make-cycle x))
                                         (perm::block-subsystem-orbit edge-subsys))
                                 48)))
    (let ((c1 (funcall coord identity))
          (c2 (funcall coord superflip)))
      (is (every #'zerop c1))
      (is (every (lambda (x) (= 1 x)) c2)))))
