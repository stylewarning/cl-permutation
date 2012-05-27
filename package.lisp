;;;; package.lisp
;;;; Copyright (c) 2012 Robert Smith

(defpackage #:cl-permutation
  (:use #:cl)
  (:export
   ;; permutation.lisp
   #:perm
   #:enable-perm-reader
   #:list-to-perm
   #:perm-identity
   #:perm-identity-p
   #:random-perm
   #:perm-ref
   #:perm-eval
   #:perm-eval*
   #:perm-size
   #:perm-length
   #:perm-even-p
   #:perm-odd-p
   #:perm-compose
   #:perm-expt
   #:perm-order
   #:perm-transpose-indexes
   #:perm-transpose-entries
   #:perm-inverse
   
   ;; permutation-group.lisp
   #:perm-group
   #:generate-perm-group
   #:group-from
   #:group-order
   #:group-element-p
   #:random-group-element))

