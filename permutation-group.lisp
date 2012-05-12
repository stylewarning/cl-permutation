;;;; permutation-group.lisp
;;;; Copyright (c) 2012 Robert Smith

(in-package #:cl-permutation)

(defun group-element-p (perm group)
  (labels ((is-in (perm group k)
             ;; ...
             ))
    (is-in perm group 0)))

(defun add-generator (perm trans group)
  (labels ((rec (perm trans group k)
             ;; ...
             ))
    (rec perm trans group 0)))

(defun update-transversal (perm trans group)
  (labels ((rec (perm trans group k)
             ;; ...
             ))
    (rec perm trans group 0)))

(defun sgs (generators)
  ;; ...
  )

(defun group-order (generators)
  ;; ...
  )
