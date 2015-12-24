;;;; block.lisp
;;;;
;;;; Copyright (c) 2015 Robert Smith

(in-package #:cl-permutation)

;;; Disjoint-Set (DJS) Data Structure
;;;
;;; Below we implement the well known union-find algorithms.
;;;
;;; In a DJS node, we don't actually hold any reference to any value
;;; that the node might be referring to. Instead, we rely on some
;;; mapping between values and DJS objects. This allows us to use a
;;; cons structure.

;;; Probably a good idea to turn this on with all of the circular
;;; structures to appear below.
(setf *print-circle* t)

(deftype djs ()
  "Type representing a valid disjoint-set (DJS) node."
  '(cons djs (and fixnum unsigned-byte)))

(defun %make-djs (parent rank)
  (cons parent rank))

(defun djs-parent (d)
  (car d))

(defun (setf djs-parent) (new-value d)
  (rplaca d new-value))

(defun djs-rank (d)
  (cdr d))

(defun (setf djs-rank) (new-value d)
  (rplacd d new-value))

(defun djs ()
  "Construct a fresh DJS node."
  (let ((d (%make-djs nil 0)))
    (setf (djs-parent d) d)))

(defun djs-find (d)
  "Find the canonical DJS node of which the DJS node D is a part."
  (let ((parent (djs-parent d)))
    (unless (eq d parent)
      ;; path compression
      (setf parent         (djs-parent d)
            (djs-parent d) parent))
    parent))

(defun djs-union (a b)
  "Link together the DJS nodes A and B."
  (let ((a-root (djs-find a))
        (b-root (djs-find b)))
    (unless (eq a-root b-root)
      (let ((a-rank (djs-rank a))
            (b-rank (djs-rank b)))
        ;; union by rank
        (cond
          ((< a-rank b-rank)
           (setf (djs-parent a) b-root))
          ((> a-rank b-rank)
           (setf (djs-parent b) a-root))
          (t
           (setf (djs-parent b) a-root)
           (incf (djs-rank a)))))))
  nil)
