;;;; src/straight-line-program.lisp
;;;;
;;;; Copyright (c) 2015 Robert Smith

(in-package #:cl-permutation)

;;; "Straight Line Programs" (SLPs) are used to construct or record
;;; group elements and operations. They can, in a sense, be seen as
;;; "lazy group computations" with symbolic naming.
;;;
;;; SLPs are discriminated by four constructors, as shown in the
;;; following DEFDATA.

(adt:defdata slp
  ;; A concrete group element.
  (slp-element     t)
  ;; A symbol representing an assigned variable.
  (slp-symbol      symbol)
  ;; The composition between two SLPs.
  (slp-composition slp slp)
  ;; The inverse of an SLP.
  (slp-inversion   slp))

(defclass slp-context ()
  ((symbol-table :initform (make-hash-table :test 'eq)
                 :accessor symbol-table
                 :documentation "A mapping between symbols and their representation as SLPs."))
  (:documentation "Represents a context (e.g., symbol assignments) in which an SLP can be evaluated."))

(defun symbol-assignment (ctx symbol)
  "Within the context CTX and the symbol SYMBOL, look up its representation. Return NIL if it does not exist."
  (values (gethash symbol (symbol-table ctx))))

(defun (setf symbol-assignment) (representation ctx symbol)
  "Assign to the symbol SYMBOL the representation REPRESENTATION within the context CTX."
  (check-type representation slp)
  (let ((current-assignment (symbol-assignment ctx symbol)))
    ;; Check that the symbol is not already assigned to.
    (unless (null current-assignment)
      (cerror "Assign to it anyway."
              "The symbol ~S is already assigned to."
              symbol))

    (values (setf (gethash symbol (symbol-table ctx))
                  representation))))

(defun compose-slp (slp1 slp2)
  "Compose two SLPs SLP1 and SLP2."
  (slp-composition slp1 slp2))

(defun invert-slp (slp)
  "Invert the SLP SLP."
  (slp-inversion slp))

;;; "Evaluation" is a map from an SLP to an element of a group.

(defun evaluate-slp (group ctx slp &key homomorphism)
  "Within a group GROUP, and given the context CTX and the straight line program SLP, compute its evaluation (the value of the SLP in the target group).

If HOMOMORPHISM is provided, then the image of each SLP-ELEMENT will be computed. The image of the homomorphism should be GROUP."
  (let ((phi (if (null homomorphism)
                 (lambda (x) x)
                 homomorphism)))
    (labels ((ev (slp)
               (adt:match slp slp
                 ((slp-element x) (funcall phi x))
                 ((slp-symbol s)
                  (let ((assignment (symbol-assignment ctx s)))
                    (assert (not (null assignment))
                            ()
                            "Encountered a symbol ~S which has no assignment ~
                           when evaluating an SLP."
                            s)
                    (ev assignment)))
                 ((slp-composition a b)
                  (compose group (ev a) (ev b)))
                 ((slp-inversion x)
                  (inverse group (ev x))))))
      (ev slp))))
