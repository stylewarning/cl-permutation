;;;; package.lisp
;;;; Copyright (c) 2012 Robert Smith

(defpackage #:cl-permutation
  (:use #:cl)
  (:nicknames #:perm)

  ;; permutation.lisp
  (:export
   #:perm                               ; Type, Structure
   #:enable-perm-reader
   #:list-to-perm
   #:perm-to-list
   #:word-to-perm
   #:perm-to-word
   #:make-perm
   #:perm-identity
   #:perm-identity-p
   #:random-perm
   #:perm-ref
   #:perm-eval
   #:perm-eval*
   #:perm-inverse-eval
   #:perm-inverse-eval*
   #:perm=
   #:perm=*
   #:perm-size
   #:perm-length
   #:perm-even-p
   #:perm-odd-p
   #:perm-sign
   #:perm-compose
   #:perm-expt
   #:perm-order
   #:perm-transpose-indexes
   #:perm-transpose-entries
   #:perm-inverse
   #:perm-fixpoints
   #:permute
   
   #:cycle
   #:make-cycle
   #:*canonicalize-cycle-on-creation*
   #:cycle-length
   #:cycle-identity-p
   #:cycle-ref
   #:orbit-length
   #:orbit-of
   #:rotate-cycle
   #:canonicalize-cycle
   #:canonicalize-cycles
   #:to-cycles
   #:from-cycles
   #:cycles-to-one-line)                ; Possibly will be removed.
  
  ;; permutation-special.lisp
  (:export
   #:bruhat<=
   #:bruhat<)
  
  ;; permutation-generation.lisp
  (:export
   #:make-perm-generator
   #:doperms)
  
  ;; permutation-group.lisp
  (:export
   #:perm-group
   #:generate-perm-group
   #:group-from
   #:group-from-cycles
   #:group-order
   #:group-element-p
   #:random-group-element
   #:transversal-decomposition))

