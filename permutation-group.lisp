;;;; permutation-group.lisp
;;;; Copyright (c) 2012 Robert Smith

(in-package #:cl-permutation)

(defun group-element-p (perm group)
  (labels ((is-in (perm k)
             (when (zerop k)
               (setf k (perm-size p)))
             
             (or (= 1 k)
                 (let ((j (perm-eval p k)))
                   (multiple-value-bind (k-val k-exists-p) (gethash k group)
                     (when k-exists-p
                       (multiple-value-bind (j-val j-exists-p) (gethash j k-val)
                         (when j-exists-p
                           (is-in (perm-compose j-val p) (1- k))))))))))
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
