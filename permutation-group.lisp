;;;; permutation-group.lisp
;;;; Copyright (c) 2012 Robert Smith, Brendan Pawlowski

;;;; Reference: Efficient Representation of Perm Groups. Donald Knuth. 1990.

(in-package #:cl-permutation)

;;; A TRANSVERSAL SYSTEM (trans) is represented as a hash table, which
;;; takes a K and returns a table which takes a J and returns
;;; sigma_kj. That is
;;;
;;;    K -> (J -> sigma_kj)

(defstruct (perm-group (:conc-name perm-group.)
                       (:print-function perm-group-printer))
  generators
  strong-generators
  transversal-system)

(defun perm-group-printer (group stream depth)
  (declare (ignore depth))
  (print-unreadable-object (group stream :type t :identity nil)
    (format stream "of ~D generator~:p" (length (perm-group.generators group)))))

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
                              (setf (gethash prod *product-membership*) t)
                              
                              (when (and (hash-table-key-exists-p *product-membership* prod)
                                         (not (group-element-p prod trans)))
                                (multiple-value-setq (sgs trans)
                                  (update-transversal prod sgs trans k))
                                
                                (setf redo t)))))
        
        (unless redo
          (return-from :outer)))))

  (values sgs trans))

(defun update-transversal (perm sgs trans &optional (k (perm-size perm)))
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

(defun generate-perm-group (generators)
  (labels ((identity-table (n)
             "Make a hash table mapping N to the identity permutation
of size N"
             (let ((ht (make-hash-table)))
               (setf (gethash n ht)
                     (perm-identity n))
               
               ht)))

    (let ((n (maximum generators :key 'perm-size))
          (sgs (make-hash-table))
          (trans (make-hash-table))
          (*product-membership* (make-hash-table)))
      (declare (special *product-membership*))
      
      ;; Initialize TRANS to map I -> (I -> Identity(I)).
      (dotimes (i n)
        (setf (gethash (1+ i) trans) (identity-table (1+ i))))
      
      ;; Add the generators.
      (loop :for generator :in generators
            :do (multiple-value-setq (sgs trans)
                  (add-generator generator sgs trans))
            :finally (return (make-perm-group :generators generators
                                              :strong-generators sgs
                                              :transversal-system trans))))))

(defun group-order (group)
  "Compute the order of the permutation group GROUP."
  (let ((transversals (perm-group.transversal-system group)))
    (product (hash-table-values transversals) :key 'hash-table-count)))

(defun random-group-element (group)
  "Generate a random element of the group GROUP."
  (loop :for v :being :the :hash-values :of (perm-group.transversal-system group)
        :collect (random-hash-table-value v) :into random-sigmas
        :finally (return (let ((maxlen (maximum random-sigmas :key 'perm-size)))
                           (reduce 'perm-compose (mapcar (lambda (s)
                                                           (perm-compose (perm-identity maxlen) s))
                                                         random-sigmas))))))