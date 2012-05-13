;;;; permutation-group.lisp
;;;; Copyright (c) 2012 Robert Smith

(in-package #:cl-permutation)

(defvar *prods*)

(defun sigma (group k j)
  (safe-gethash j (safe-gethash k group)))

(defun group-element-p (perm group &optional (k (perm-size perm)))
  (or (= 1 k)
      (let ((j (perm-eval perm k)))
        (multiple-value-bind (k-val k-exists-p) (gethash k group)
          (when k-exists-p
            (multiple-value-bind (j-val j-exists-p) (gethash j k-val)
              (when j-exists-p
                (group-element-p (perm-compose (perm-inverse j-val) perm) 
                                 group 
                                 (1- k)))))))))

(defun add-generator (perm trans group &optional (k (perm-size perm)))
  (setf (gethash k trans)
        (union (gethash k trans) (list perm)))
  
  (block :outer
    (let ((redo nil))
      (loop
        (setf redo nil)
        
        (loop :for s :in (hash-table-values (gethash k group))
              :do (loop :for tt :in (gethash k trans)
                        :for prod := (perm-compose tt s)
                        :do (progn
                              (setf (gethash prod *prods*) t)
                              
                              (when (and (hash-table-key-exists-p *prods* prod)
                                         (not (group-element-p prod group)))
                                (multiple-value-setq (trans group)
                                  (update-transversal prod trans group k))
                                
                                (setf redo t)))))
        
        (unless redo
          (return-from :outer)))))

  (values trans group))

(defun update-transversal (perm trans group &optional (k (perm-size perm)))
  (let ((j (perm-eval perm k)))
    (handler-case
        (let ((new-perm (perm-compose (perm-inverse (sigma group k j))
                                      perm)))
          (if (group-element-p new-perm group (1- k))
              (values trans group)
              (add-generator new-perm trans group (1- k))))
      (hash-table-access-error (c) 
        (declare (ignore c))
        (progn
          (setf (gethash j (gethash k group)) perm)
          (values trans group))))))

(defun strong-generating-set (generators)
  (labels ((identity-table (n)
             "Make a hash table mapping N to the identity permutation
of size N"
             (let ((ht (make-hash-table)))
               (setf (gethash n ht)
                     (perm-identity n))
               
               ht)))

    (let ((n (maximum generators :key 'perm-size))
          (trans (make-hash-table))
          (group (make-hash-table)))
      
      ;; Initialize GROUP to map I -> (I -> Identity(I)).
      (dotimes (i n)
        (setf (gethash (1+ i) group) (identity-table (1+ i))))
      
      ;; Initialize the product table
      (setf *prods* (make-hash-table))
      
      ;; Add the generators.
      (loop :for generator :in generators
            :do (multiple-value-setq (trans group)
                  (add-generator generator trans group))
            :finally (return (values group trans))))))

(defun group-order (generators)
  (let ((transversals (strong-generating-set generators)))
    (product (hash-table-values transversals) :key 'hash-table-count)))

