;;;; tests/package.lisp
;;;;
;;;; Copyright (c) 2014 Robert Smith

(defpackage #:cl-permutation-tests
  (:use #:cl)
  (:nicknames #:perm-tests)
  (:use #:hu.dwim.stefil
        #:cl-permutation
        #:cl-permutation-examples)
  
  ;; suite.lisp
  (:export
   #:run-tests))
