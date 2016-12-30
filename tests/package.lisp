;;;; tests/package.lisp
;;;;
;;;; Copyright (c) 2014-2016 Robert Smith

(fiasco:define-test-package #:cl-permutation-tests
  (:use #:cl)
  (:nicknames #:perm-tests)
  (:use #:fiasco
        #:cl-permutation
        #:cl-permutation-examples)

  ;; suite.lisp
  (:export
   #:run-tests))
