;;;; cl-permutation.asd
;;;; Copyright (c) 2012 Robert Smith

(asdf:defsystem #:cl-permutation
  :serial t
  :description "A library for operating on permutations and
  permutation groups."
  :author "Robert Smith <quad@symbo1ics.com>"
  ;; :license "Specify license here"
  :serial t
  :components ((:file "package")
               (:file "cl-permutation")
               (:file "utilities")
               (:file "permutation")
               (:file "permutation-group")))

