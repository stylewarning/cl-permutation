;;;; tests/suite.lisp
;;;;
;;;; Copyright (c) 2014-2018 Robert Smith

(in-package #:cl-permutation-tests)

(defun run-the-tests ()
  (fiasco:run-package-tests :package '#:cl-permutation-tests))

;; Create some groups that will be used immutably in the tests.

(defvar *2x2* (perm-examples:make-rubik-2x2))
(defvar *3x3* (perm-examples:make-rubik-3x3))
(defvar *mm*  (progn
                (when *compile-verbose*
                  (format t "~&; Computing Megaminx group. This takes a few seconds...~%"))
                (perm-examples:make-megaminx)))
