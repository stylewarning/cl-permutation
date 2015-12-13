;;;; group.lisp
;;;;
;;;; Copyright (c) 2015 Robert Smith

(in-package #:cl-permutation)

;;; Definition of the group protocol.

;;; Refresher:
;;;
;;; A group G is a set along with a binary operation @ such that:
;;;
;;;    * G is closed under @
;;;
;;;    * There's an identity element e in G such that for an element g
;;;      in G, e@g = g@e = g
;;;
;;;    * All elements in g in G have an inverse g' such that g'@g =
;;;      g@g' = e
;;;
;;;    * The binary operation is associative: (a@b)@c = a@(b@c)

(defgeneric identity-element (G)
  (:documentation "Return the identity element of the group G."))

(defgeneric compose (G a b)
  (:documentation "Compose two elements A and B within the group G."))

(defgeneric inverse (G a)
  (:documentation "Compute the inverse of A within the group G."))
