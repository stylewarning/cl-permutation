;;;; permutation-group.lisp
;;;; Copyright (c) 2012 Robert Smith

(in-package #:cl-permutation)

(defvar *prods*)

(defun sigma (trans k j)
  (safe-gethash j (safe-gethash k trans)))

(defun group-element-p (perm trans &optional (k (perm-size perm)))
  (or (= 1 k)
      (let ((j (perm-eval perm k)))
        (multiple-value-bind (k-val k-exists-p) (gethash k trans)
          (when k-exists-p
            (multiple-value-bind (j-val j-exists-p) (gethash j k-val)
              (when j-exists-p
                (group-element-p (perm-compose (perm-inverse j-val) perm) 
                                 trans 
                                 (1- k)))))))))

(defun add-generator (perm sgs trans &optional (k (perm-size perm)))
  (setf (gethash k sgs)
        (union (gethash k sgs) (list perm)))
  
  (block :outer
    (let ((redo nil))
      (loop
        (setf redo nil)
        
        (loop :for s :in (hash-table-values (gethash k trans))
              :do (loop :for tt :in (gethash k sgs)
                        :for prod := (perm-compose tt s)
                        :do (progn
                              (setf (gethash prod *prods*) t)
                              
                              (when (and (hash-table-key-exists-p *prods* prod)
                                         (not (group-element-p prod trans)))
                                (multiple-value-setq (sgs trans)
                                  (update-sgsversal prod sgs trans k))
                                
                                (setf redo t)))))
        
        (unless redo
          (return-from :outer)))))

  (values sgs trans))

(defun update-sgsversal (perm sgs trans &optional (k (perm-size perm)))
  (let ((j (perm-eval perm k)))
    (handler-case
        (let ((new-perm (perm-compose (perm-inverse (sigma trans k j))
                                      perm)))
          (if (group-element-p new-perm trans (1- k))
              (values sgs trans)
              (add-generator new-perm sgs trans (1- k))))
      (hash-table-access-error (c) 
        (declare (ignore c))
        (progn
          (setf (gethash j (gethash k trans)) perm)
          (values sgs trans))))))

(defun strong-generating-set (generators)
  (labels ((identity-table (n)
             "Make a hash table mapping N to the identity permutation
of size N"
             (let ((ht (make-hash-table)))
               (setf (gethash n ht)
                     (perm-identity n))
               
               ht)))

    (let ((n (maximum generators :key 'perm-size))
          (sgs (make-hash-table))
          (trans (make-hash-table)))
      
      ;; Initialize TRANS to map I -> (I -> Identity(I)).
      (dotimes (i n)
        (setf (gethash (1+ i) trans) (identity-table (1+ i))))
      
      ;; Initialize the product table
      (setf *prods* (make-hash-table))
      
      ;; Add the generators.
      (loop :for generator :in generators
            :do (multiple-value-setq (sgs trans)
                  (add-generator generator sgs trans))
            :finally (return (values trans sgs))))))

(defun group-order (generators)
  (let ((transversals (strong-generating-set generators)))
    (product (hash-table-values transversals) :key 'hash-table-count)))

