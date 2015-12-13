;;;; examples/package.lisp
;;;;
;;;; Copyright (c) 2014 Robert Smith

(defpackage #:cl-permutation-examples
  (:use #:cl #:perm)
  (:nicknames #:perm-examples)

  ;; symmetric.lisp
  (:export
   #:make-S5                            ; FUNCTION
   )

  ;; sporadic.lisp
  (:export
   #:make-mathieu-m25                   ; FUNCTION
   )

  ;; rubik-like.lisp
  (:export
   #:make-rubik-2x2                     ; FUNCTION
   #:make-rubik-3x3                     ; FUNCTION
   #:make-rubik-4x4                     ; FUNCTION
   #:make-skewb                         ; FUNCTION
   #:make-megaminx                      ; FUNCTION
   )
  )
