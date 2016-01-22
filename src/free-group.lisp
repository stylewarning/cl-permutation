;;;; free-group.lisp
;;;;
;;;; Copyright (c) 2015 Robert Smith

(in-package #:cl-permutation)

(defclass free-group ()
  ((num-generators :initarg :num-generators
                   :reader  free-group-num-generators))
  (:documentation "Representation of a free group whose symbols are:

    * identity element: 0
    * generators: 1 .. N
    * inverse generators: -N .. -1

Elements of the group are either individual integers or lists thereof. The lists represent compositions of generators. The BNF grammar looks something like:

    <free group generator> := 1 | 2 | ... | N
    <free group atom>      := <free group generator>
                            | 0
                            | -<free group generator>
    <free group element>   := <free group atom>
                            | ( <free group atom>* )

An empty list corresponds to an empty composition, which is identity (0)."))

(defun make-free-group (num-generators)
  "Make a free group that contains NUM-GENERATORS generators."
  (check-type num-generators (integer 0))
  (make-instance 'free-group :num-generators num-generators))

(defun free-group-element-valid-p (g element)
  "Given the free group G and some purported element ELEMENT, return a boolean indicating whether it is a valid element of G."
  (check-type g free-group)
  (let ((num-generators (free-group-num-generators g)))
    (typecase element
      (integer (<= (- num-generators) element num-generators))
      (list (every (lambda (e)
                     (free-group-element-valid-p g e))
                   element))
      (t nil))))

(defun canonicalize-free-group-element (g e)
  (declare (ignore g))
  (alexandria:flatten (list e)))

(defun make-free-group-element (g &rest elements)
  "Make an element of the free group G where ELEMENTS are either integer generators of G, or other elements created by this function."
  (check-type g free-group)
  (flet ((process (e)
           (if (listp e)
               (canonicalize-free-group-element g e)
               (list e))))
    (assert (every (lambda (e)
                     (free-group-element-valid-p g e))
                   elements)
            (elements)
            "The provided free group contains invalid elements.")
    (mapcan #'process elements)))

(defmethod identity-element ((g free-group))
  0)

(defmethod compose ((g free-group) a b)
  (assert (free-group-element-valid-p g a)
          (a)
          "The value A is not a valid element of the provided free group.")
  (assert (free-group-element-valid-p g b)
          (b)
          "The value A is not a valid element of the provided free group.")
  (make-free-group-element g a b))

(defmethod inverse ((g free-group) a)
  (etypecase a
    ;; a^-1 = -a
    (integer (- a))
    ;; (a b c)^-1 = (c^-1 b^-1 a^-1) 
    (list
     (canonicalize-free-group-element
      g
      (mapcar (lambda (e) (inverse g e))
              (reverse a))))))

(defmethod generators ((G free-group))
  (loop :for i :from 1 :to (num-generators G)
        :collect i))

(defmethod num-generators ((G free-group))
  (free-group-num-generators G))
