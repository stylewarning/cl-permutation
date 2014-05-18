;;;; tests/package.lisp
;;;;
;;;; Copyright (c) 2014 Robert Smith

(defpackage #:cl-permutation-tests
  (:use #:cl)
  (:nicknames #:perm-tests)
  
  ;; suite.lisp
  (:export
   #:run-tests))
