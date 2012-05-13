;;;; permutation-group.lisp
;;;; Copyright (c) 2012 Robert Smith

(in-package #:cl-permutation)

(defun group-element-p (perm group)
  (labels ((is-in (perm group k)
             ;; ...
             ))
    (is-in perm group 0)))

(defun add-generator (perm trans group &optional (k 0))
  ;; ...
  )

(defun update-transversal (perm trans group &optional (k 0))
  ;; ...
  )

(defun strong-generating-set (generators)
  ;; ...
  )

(defun group-order (generators)
  (let ((transversals (strong-generating-set generators)))
    (product (hash-table-values transversals) :key 'hash-table-size)))
