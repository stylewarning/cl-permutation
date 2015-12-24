;;;; cl-permutation.asd
;;;;
;;;; Copyright (c) 2012-2015 Robert Smith

(asdf:defsystem #:cl-permutation
  :description "A library for operating on permutations and permutation groups."
  :author "Robert Smith <quad@symbo1ics.com>"
  :license "BSD 3-clause (See LICENSE)"
  :depends-on (#:alexandria
               #:cl-algebraic-data-type)
  :in-order-to ((asdf:test-op (asdf:test-op #:cl-permutation-tests)))
  :serial t
  :components ((:module src
                :serial t
                :components
                ((:file "package")
                 (:file "utilities")
                 (:file "permutation")
                 (:file "bruhat")
                 (:file "permutation-generation")
                 (:file "group")
                 (:file "free-group")
                 (:file "straight-line-program")
                 (:file "permutation-group")
                 (:file "block")
                 (:file "do-group-elements")
                 (:file "combinatorial-ranking")
                 (:file "find-subgroups")
                 (:file "extra-functions")))))

