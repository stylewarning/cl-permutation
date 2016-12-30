;;;; tests/suite.lisp
;;;;
;;;; Copyright (c) 2014 Robert Smith

(in-package #:cl-permutation-tests)

(defun run-tests ()
  (fiasco:run-package-tests :package '#:cl-permutation-tests))
