;;;; src/package.lisp
;;;;
;;;; Copyright (c) 2012-2015 Robert Smith

(defpackage #:cl-permutation
  (:use #:cl)
  (:nicknames #:perm)

  ;; permutation.lisp
  (:export
   #:perm-element
   #:perm                               ; Type, Structure
   #:enable-perm-reader
   #:list-to-perm
   #:perm-to-list
   #:vector-to-perm
   #:perm-to-vector
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
   #:perm-point-fixed-p
   #:perm-last-non-fixpoint
   #:perm-fixpoints
   #:permute
   
   #:cycle-element
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
   #:cycle-type
   #:cycles-to-one-line)                ; Possibly will be removed.
  
  ;; bruhat.lisp
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
   #:group-degree
   #:group-identity
   #:generate-perm-group
   #:group-from
   #:group-from-cycles
   #:group-order
   #:group-element-p
   #:random-group-element
   #:transversal-decomposition
   #:group-orbits)

  ;; combinatorial-ranking.lisp
  (:export
   #:combinatorial-spec                 ; CLASS
   #:size                               ; READER
   #:radix-spec                         ; CLASS
   #:radix                              ; READER
   #:mixed-radix-spec                   ; CLASS
   #:perm-spec                          ; CLASS
   #:combination-spec                   ; ClASS
   #:word-spec                          ; CLASS
   
   #:cardinality                        ; GENERIC FUNCTION, METHOD

   #:make-perm-spec                     ; FUNCTION
   #:make-combination-spec              ; FUNCTION
   #:make-radix-spec                    ; FUNCTION
   #:vector-to-mixed-radix-spec         ; FUNCTION
   #:vector-to-word-spec                ; FUNCTION

   #:rank                               ; GENERIC FUNCTION, METHOD
   #:unrank                             ; GENERIC FUNCTION, METHOD

   #:map-spec                           ; FUNCTION
   #:print-objects-of-spec              ; FUNCTION
   )
  )

