;;;; tests/permutation-group.lisp
;;;;
;;;; Copyright (c) 2016 Robert Smith

(in-package #:cl-permutation-tests)

(defun test-rank-unrank (group &optional (limit (group-order group)))
  (let ((i 0)
        (failures nil))
    (multiple-value-bind (rank-el unrank-el)
        (group-element-rank-functions group)
      (do-group-elements (el group failures)
        (when (= i limit)
          (return-from test-rank-unrank failures))
        (let ((ranked (funcall rank-el el))
              (unranked (funcall unrank-el i)))
          (unless (= i ranked)
            (push (list ':RANK i el ranked) failures))
          (unless (perm= el unranked)
            (push (list ':UNRANK i el unranked) failures))
          ;; Check that identity is 0.
          (when (perm-identity-p el)
            (is (zerop i))))
        (incf i)))))

(deftest test-rank-unrank-s5 ()
  (is (null (test-rank-unrank (make-s5)))))

(deftest test-rank-unrank-2x2 ()
  (is (null (test-rank-unrank *2x2* 10000))))

(deftest test-rank-unrank-3x3 ()
  (is (null (test-rank-unrank *3x3* 1000))))
