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
  (labels ((identity-table (n)
             "Make a hash table mapping I to the identity permutation
of size I for 1 <= I <= N."
             (let ((ht (make-hash-table)))
               (dotimes (i n ht)
                 (setf (gethash (1+ i) ht)
                       (perm-identity (1+ i)))))))

    (let ((n (maximum generators :key 'perm-size))
          (trans (make-hash-table))
          (group (make-hash-table)))
      
      ;; Initialize GROUP to map I -> (I -> Identity(I)).
      (dotimes (i n)
        (setf (gethash (1+ i) group) (identity-table (1+ i))))
      
      ;; Add the generators.
      (loop :for generator :in generators
            :do (multiple-value-setq (trans group)
                  (add-generator generator trans group))
            :finally (return (values group trans))))))

(defun group-order (generators)
  (let ((transversals (strong-generating-set generators)))
    (product (hash-table-values transversals) :key 'hash-table-size)))
