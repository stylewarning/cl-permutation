;;;; cl-permutation-examples.asd
;;;;
;;;; Copyright (c) 2014-2018 Robert Smith

(asdf:defsystem #:cl-permutation-examples
  :description "Examples of permutation-groups."
  :author "Robert Smith <robert@stylewarning.com>"
  :license "BSD 3-clause (See LICENSE)"
  :depends-on (#:cl-permutation #:alexandria)
  :serial t
  :components ((:module examples
                :serial t
                :components
                ((:file "package")
                 (:file "symmetric")
                 (:file "sporadic")
                 (:file "rubik-like")
                 (:file "misc")))))
