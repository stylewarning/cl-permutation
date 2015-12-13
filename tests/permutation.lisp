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
