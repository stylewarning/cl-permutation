;;;; cl-permutation.asd
;;;;
;;;; Copyright (c) 2012-2019 Robert Smith

(asdf:defsystem #:cl-permutation
  :description "A library for operating on permutations and permutation groups."
  :author "Robert Smith <robert@stylewarning.com>"
  :license "BSD 3-clause (See LICENSE)"
  :depends-on (#:alexandria
               #:iterate
               #:cl-algebraic-data-type
               #:closer-mop
               #:uiop
               #:bordeaux-fft
               #:priority-queue
               #:cl-cont
               )
  :in-order-to ((asdf:test-op (asdf:test-op #:cl-permutation-tests)))
  :pathname "src/"
  :serial t
  :components ((:file "package")
               (:file "utilities")
               (:file "coroutines")
               (:file "permutation")
               (:file "bruhat")
               (:file "permutation-generation")
               (:file "group")
               (:file "free-group")
               (:file "straight-line-program")
               (:file "permutation-group")
               (:file "minkwitz")
               (:file "4-list-algorithm")
               (:file "homomorphism")
               (:file "orbit")
               (:file "do-group-elements")
               (:file "block")
               (:file "combinatorial-ranking")
               (:file "find-subgroups")
               (:file "god")
               (:file "extra-functions")))
