;;;; cl-permutation.asd
;;;; Copyright (c) 2012-2014 Robert Smith

(asdf:defsystem #:cl-permutation
  :serial t
  :description "A library for operating on permutations and
  permutation groups."
  :author "Robert Smith <quad@symbo1ics.com>"
  :license "BSD 3-clause (See LICENSE)"
  :serial t
  :components ((:file "package")
               (:file "cl-permutation")
               (:file "utilities")
               (:file "permutation")
               (:file "bruhat")
               (:file "permutation-generation")
               (:file "permutation-group")
               (:file "do-group-elements")
               (:file "extra-functions")))

