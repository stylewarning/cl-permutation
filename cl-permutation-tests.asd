;;;; cl-permutation-tests.asd
;;;;
;;;; Copyright (c) 2014 Robert Smith

(asdf:defsystem #:cl-permutation-tests
  :description "Regression tests for the library CL-PERMUTATION."
  :author "Robert Smith <quad@symbo1ics.com>"
  :license "BSD 3-clause (See LICENSE)"
  :depends-on ("cl-permutation" "fiveam")
  :serial t
  :components ((:module tests
                :serial t
                :components
                ((:file "package")
                 (:file "suite")
                 (:file "permutations")))))
