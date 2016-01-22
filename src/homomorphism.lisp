;;;; homomorphism.lisp
;;;;
;;;; Copyright (c) 2016 Robert Smith

(in-package #:cl-permutation)

(defgeneric homomorphism-preimage (hom)
  (:documentation "Preimage group of the homomorphism HOM."))

(defgeneric homomorphism-image (hom)
  (:documentation "Image group of the homomorphism HOM."))

(defgeneric image (homomorphism object)
  (:documentation "Compute the image of object OBJECT with respect to the homomorphism HOMOMORPHISM.")
  ;; By default, we allow functions to look like homomorphisms. It's
  ;; up to the user to determine whether the given function is
  ;; actually a homomorphism.
  (:method ((hom function) object)
    (funcall hom object)))

;; We provide an abstract class HOMOMORPHISM so that subclasses get
;; default FUNCALLABLE-INSTANCE-FUNCTIONs.
(defclass homomorphism ()
  ()
  (:metaclass c2mop:funcallable-standard-class))

(defmethod initialize-instance :after ((hom homomorphism) &key)
  (c2mop:set-funcallable-instance-function
   hom
   (lambda (elt) (image hom elt))))

(defclass function-homomorphism (homomorphism)
  ((from-group
    :initarg :from-group
    :reader homomorphism-preimage
    :documentation "Preimage of the homomorphism.")
   (to-group
    :initarg :to-group
    :reader homomorphism-image
    :documentation "Image of the homomorphism.")
   (function :initarg :function
             :reader homomorphism-function
             :documentation "Homomorphism function."))
  (:metaclass c2mop:funcallable-standard-class)
  (:documentation "Simple class which wraps homomorphic functions, associating them with the preimage and image of the function."))

(defmethod image ((hom function-homomorphism) obj)
  (funcall (homomorphism-function hom) obj))

(defclass generator-homomorphism (homomorphism)
  ((from-group
    :initarg :from-group
    :reader homomorphism-preimage
    :documentation "Preimage of the homomorphism.")
   (to-group
    :initarg :to-group
    :reader homomorphism-image
    :documentation "Image of the homomorphism.")
   (generator-map
    :initarg :generator-map
    :reader homomorphism-generator-map
    :documentation "A unary function mapping generators of FROM-GROUP to objects of the resulting group TO-GROUP."))
  (:metaclass c2mop:funcallable-standard-class)
  (:documentation "A perm group homomorphism constructed from images of its genertators.

This class is FUNCALLable."))

;;; XXX FIXME: This is a pretty hairy function. Lots of stuff can also
;;; be precomputed. This should be cleaned up and made a bit more
;;; efficient.
(defmethod image ((hom generator-homomorphism) (elt perm))
  "Given a homomorphism HOM, compute the image of ELT."
  (let ((gen-map (homomorphism-generator-map hom))
        (preimage (homomorphism-preimage hom))
        (image (homomorphism-image hom)))
    (alexandria:if-let
        ((x (funcall gen-map elt)))
        x
        (labels ((core (x)
                   ;; Perm -> Image
                   (funcall gen-map
                            ;; Free -> Perm
                            (free-group-generator-to-perm-group-generator
                             preimage
                             x)))
                 (ev (slp)
                   (evaluate-slp
                    image
                    (perm-group.slp-context preimage)
                    slp
                    :homomorphism #'core)))
          (let* ((d (transversal-decomposition elt preimage :remove-identities t))
                 (ctx (perm-group.slp-context preimage)))
    (labels ((to-sigma-symbol (tt)
               (sigma-symbol (car tt) (cdr tt)))
             (find-slp (tt)
               (symbol-assignment ctx (to-sigma-symbol tt))))
      (reduce (lambda (acc x)
                (compose image acc x))
              d
              :initial-value (identity-element image)
              :key (lambda (sigma)
                     (ev (find-slp sigma))))))))))

(defun homomorphism-induced-perm-group (group hom)
  "Given a group GROUP, and a homomorphism HOM mapping elements of that group to permutations,compute the homomorphism-induced group."
  (generate-perm-group
   (remove-if #'perm-identity-p (mapcar hom (generators group)))))
