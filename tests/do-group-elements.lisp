;;;; tests/permutation-group.lisp
;;;;
;;;; Copyright (c) 2016 Robert Smith

(in-package #:cl-permutation-tests)

(in-suite cl-permutation-suite)

(deftest test-rank-unrank ()
  (let ((g (perm-examples:make-S5))
        (i 0)
        (failures nil))
    (multiple-value-bind (rank-el unrank-el)
        (group-element-rank-functions g)
      (do-group-elements (el g)
        (let ((ranked (funcall rank-el el))
              (unranked (funcall unrank-el i)))
          (unless (= i ranked)
            (push (list ':RANK i el ranked) failures))
          (unless (perm= el unranked)
            (push (list ':UNRANK i el unranked) failures))
          ;; Check that identity is 0.
          (when (perm-identity-p el)
            (is (zerop i))))
        (incf i))

      ;; Make sure we have no failures.
      (is (null failures)))))
