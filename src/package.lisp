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
   #:perm-evaluator
   #:perm-eval*
   #:perm-evaluator*
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
   #:perm-compose-flipped
   #:perm-conjugate
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
   #:cycles-to-one-line
   #:find-conjugator
   )

  ;; bruhat.lisp
  (:export
   #:bruhat<=
   #:bruhat<
   )

  ;; permutation-generation.lisp
  (:export
   #:make-perm-generator
   #:doperms
   )

  ;; group.lisp
  (:export
   #:identity-element                   ; GENERIC
   #:compose                            ; GENERIC
   #:inverse                            ; GENERIC
   #:generators                         ; GENERIC
   #:num-generators                     ; GENERIC
   )

  ;; free-group.lisp
  (:export
   #:free-group                         ; CLASS
   #:make-free-group                    ; FUNCTION
   #:make-free-group-element            ; FUNCTION
   #:free-group-identity-p              ; FUNCTION
   )

  ;; straight-line-program.lisp
  (:export
   #:slp                                ; TYPE
   #:slp-element                        ; TYPE, CONSTRUCTOR
   #:slp-symbol                         ; TYPE, CONSTRUCTOR
   #:slp-composition                    ; TYPE, CONSTRUCTOR
   #:slp-inversion                      ; TYPE, CONSTRUCTOR
   #:slp-context                        ; CLASS
   #:symbol-assignment                  ; ACCESSOR
   #:compose-slp                        ; FUNCTION
   #:invert-slp                         ; FUNCTION
   #:evaluate-slp                       ; FUNCTION
   )

  ;; permutation-group.lisp
  (:export
   #:perm-group                         ; CLASS
   #:group-degree                       ; FUNCTION
   #:group-identity                     ; FUNCTION
   #:generate-perm-group                ; FUNCTION
   #:group-from                         ; FUNCTION
   #:group-from-cycles                  ; FUNCTION
   #:group-order                        ; FUNCTION
   #:group-element-p                    ; FUNCTION
   #:subgroup-p                         ; FUNCTION
   #:random-group-element               ; FUNCTION
   #:transversal-decomposition          ; FUNCTION
   #:group-orbits                       ; FUNCTION
   #:orbit-group-homomorphism           ; FUNCTION
   #:group-from-orbit                   ; FUNCTION
   #:subdirect-factors                  ; FUNCTION
   #:naive-generator-decomposition      ; FUNCTION
   #:generator-decomposition            ; FUNCTION
   )

  ;; homomorphism.lisp
  (:export
   #:image                              ; GENERIC, METHOD
   #:homomorphism-preimage              ; GENERIC, METHOD
   #:homomorphism-image                 ; GENERIC, METHOD
   #:generator-homomorphism             ; CLASS
   #:function-homomorphism              ; CLASS
   #:homomorphism-induced-perm-group    ; FUNCTION
   )

  ;; block.lisp
  (:export
   #:find-minimal-block-system-containing
                                        ; FUNCTION
   #:find-non-trivial-block-system      ; FUNCTION
   #:block-systems                      ; FUNCTION
   #:primitive-group-p                  ; FUNCTION
   )

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

   #:cardinality                        ; GENERIC, METHOD

   #:make-perm-spec                     ; FUNCTION
   #:make-combination-spec              ; FUNCTION
   #:make-radix-spec                    ; FUNCTION
   #:vector-to-mixed-radix-spec         ; FUNCTION
   #:vector-to-word-spec                ; FUNCTION

   #:rank                               ; GENERIC, METHOD
   #:unrank                             ; GENERIC, METHOD

   #:map-spec                           ; FUNCTION
   #:print-objects-of-spec              ; FUNCTION
   )

  ;; do-group-elements.lisp
  (:export
   #:group-element-rank-functions       ; FUNCTION
   #:do-group-elements                  ; FUNCTION
   )
  )
