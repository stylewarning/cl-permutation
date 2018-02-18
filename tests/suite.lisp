;;;; tests/suite.lisp
;;;;
;;;; Copyright (c) 2014-2018 Robert Smith

(in-package #:cl-permutation-tests)

(defun run-the-tests ()
  (fiasco:run-package-tests :package '#:cl-permutation-tests))
