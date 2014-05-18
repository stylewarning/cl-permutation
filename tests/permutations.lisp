;;;; tests/permutations.lisp
;;;;
;;;; Copyright (c) 2014 Robert Smith

(in-package #:cl-permutation-tests)

(fiveam:in-suite cl-permutation-suite)

(fiveam:test perm-identity
  (fiveam:is (perm:perm-identity-p (perm:perm-identity 1)))
  (fiveam:is (perm:perm-identity-p (perm:perm-identity 2)))
  (fiveam:is (perm:perm-identity-p (perm:perm-identity 3)))
  (fiveam:is (perm:perm-identity-p (perm:perm-identity 4)))
  (fiveam:is (perm:perm-identity-p (perm:perm-identity 5))))
