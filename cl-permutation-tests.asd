;;;; cl-permutation-tests.asd
;;;;
;;;; Copyright (c) 2014-2019 Robert Smith

(asdf:defsystem #:cl-permutation-tests
  :description "Regression tests for the library CL-PERMUTATION."
  :author "Robert Smith <robert@stylewarning.com>"
  :license "BSD 3-clause (See LICENSE)"
  :depends-on (#:cl-permutation
               #:cl-permutation-examples
               #:fiasco)
  :perform (asdf:test-op (o s)
                         (uiop:symbol-call :cl-permutation-tests
                                           '#:run-the-tests))
  :pathname "tests/"
  :serial t
  :components ((:file "package")
               (:file "suite")
               (:file "permutation")
               (:file "straight-line-program")
               (:file "permutation-group")
               (:file "do-group-elements")
               (:file "homomorphism")
               (:file "orbit")
               (:file "block")
               (:file "free-group")))
