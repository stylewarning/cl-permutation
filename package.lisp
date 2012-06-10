;;;; package.lisp
;;;; Copyright (c) 2012 Robert Smith

(defpackage #:cl-permutation
  (:use #:cl)
  (:nicknames #:perm)
  (:export
   ;; permutation.lisp
   #:perm                               ; Type, Structure
   #:enable-perm-reader
   #:list-to-perm
   #:make-perm
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
   #:perm-sign
   #:perm-compose
   #:perm-expt
   #:perm-order
   #:perm-transpose-indexes
   #:perm-transpose-entries
   #:perm-inverse
   #:perm-fixpoints
   #:permute
   
   #:orbit-of
   #:rotate-cycle-clockwise             ; Possibly will be removed.
   #:rotate-cycle-counterclockwise      ; Possibly will be removed.
   #:normalize-cycle-order              ; Possibly will be removed.
   #:normalize-cycles
   #:to-cycles
   #:from-cycles
   #:cycles-to-one-line                 ; Possibly will be removed.
   
   ;; permutation-generation.lisp
   #:make-perm-generator
   #:doperms
   
   ;; permutation-group.lisp
   #:perm-group
   #:generate-perm-group
   #:group-from
   #:group-from-cycles
   #:group-order
   #:group-element-p
   #:random-group-element))

