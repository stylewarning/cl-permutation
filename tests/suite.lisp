;;;; tests/suite.lisp
;;;;
;;;; Copyright (c) 2014 Robert Smith

(in-package #:cl-permutation-tests)

(fiveam:def-suite cl-permutation-suite
  :description "Tests for the CL-PERMUTATION package.")

(defun run-tests ()
  (fiveam:run! 'cl-permutation-suite))
