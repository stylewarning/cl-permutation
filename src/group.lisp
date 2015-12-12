;;;; group.lisp
;;;;
;;;; Copyright (c) 2015 Robert Smith

(in-package #:cl-permutation)

;;; Definition of the group protocol.

;;; A group G is a set along with a binary operation * such that:
;;;
;;;    - G is closed under *
;;;    - identity element e in G, s.t. e*g = g = g*e
;;;    - elements have an inverse: a' inv of a
;;;            a'*a = e = a*a'
;;;    - (a*b)*c = a*(b*c)

(defgeneric identity-element (G)
  (:documentation "Return the identity element of the group G."))

(defgeneric compose (G a b)
  (:documentation "Compose two elements A and B within the group G."))

(defgeneric inverse (G a)
  (:documentation "Compute the inverse of A within the group G."))
