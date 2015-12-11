;;;; permutation-group.lisp
;;;;
;;;; Copyright (c) 2012-2014 Robert Smith

(in-package #:cl-permutation)

;;; A TRANSVERSAL SYSTEM (trans) is represented as a hash table, which
;;; takes a K and returns a table which takes a J and returns
;;; sigma_kj. That is
;;;
;;;    K -> (J -> sigma_kj)

(defstruct (perm-group (:conc-name perm-group.)
                       (:print-function perm-group-printer))
  element-size                          ; Non-negative integer
  generators
  strong-generators
  transversal-system)

(defun group-degree (group &key true)
  "What is the degree of the group GROUP?

If TRUE is a true-value, then the true degree will be returned (i.e., the maximum non-fixed point index). For example, consider

    G = <(1 3 2 4 5)>

then

    (group-degree G :true nil) ==> 5  [default]
    (group-degree G :true t)   ==> 3."
  (if true
      (maximum (perm-group.generators group) :key #'perm-last-non-fixpoint)
      (perm-group.element-size group)))

(defun group-identity (group)
  "Return the identity element of the group GROUP."
  (perm-identity (group-degree group)))

(deftype transversal ()
  ;; It is actually (simple-array (or null hash-table) (*)), but we
  ;; will want to use SVREF. While such a type would collapse into
  ;; SIMPLE-VECTOR in most implementations, we don't want to assume
  ;; such.
  `simple-vector)

(declaim (inline make-transversal))
(defun make-transversal (n)
  "Make a transversal of size N."
  (make-array n :initial-element nil))

(declaim (inline transversal-ref))
(defun transversal-ref (trans k)
  "Get the Nth element of the transversal TRANS."
  (declare (type transversal trans))
  (svref trans (1- k)))

(defun (setf transversal-ref) (new-value trans k)
  (declare (type transversal trans))
  (setf (svref trans (1- k)) new-value))

(defun perm-group-printer (group stream depth)
  (declare (ignore depth))
  (print-unreadable-object (group stream :type t :identity nil)
    (format stream "of ~D generator~:p" (length (perm-group.generators group)))))

(defun sigma (trans k j)
  (gethash j (transversal-ref trans k)))

(defun reduce-over-trans-decomposition (f initial-value perm trans &optional (k (perm-size perm)))
  "Reduce F over the transversal decomposition of PERM within the transversal system TRANS. Return two values:

    1. The result of folding over, or NIL if no decomposition exists.
    2. NIL iff no decomposition exists.

F is a function of three arguments:

    ACCUM: The \"accumulator\" argument. INITIAL-VALUE is the initial value of this argument.
    K, J : Two arguments representing the sigma.

If all K and J are accumulated into a list, then the list would represent the transversal decomposition of PERM."
  (labels ((next (perm k acc)
             (if (= 1 k)
                 (values acc t)
                 (let* ((j (perm-eval perm k))
                        (k-val (transversal-ref trans k)))
                   (if (null k-val)
                       (values nil nil)
                       (multiple-value-bind (j-val j-exists-p) (gethash j k-val)
                         (if (null j-exists-p)
                             (values nil nil)
                             (next (perm-compose (perm-inverse j-val) perm) 
                                   (1- k)
                                   (funcall f acc k j)))))))))
    (declare (dynamic-extent #'next))
    (next perm k initial-value)))

(defun trans-decomposition (perm trans &optional (k (perm-size perm)))
  (flet ((collector (decomp k j)
           (acons k j decomp)))
    (declare (dynamic-extent #'collector))
    (values (reduce-over-trans-decomposition #'collector nil perm trans k))))

(defun trans-element-p (perm trans &optional (k (perm-size perm)))
  #+#:equivalent (not (null (trans-decomposition perm trans k)))
  (values
   (reduce-over-trans-decomposition
    (load-time-value (constantly t))
    t
    perm
    trans
    k)))

(defun add-generator (perm sgs trans &optional (k (perm-size perm)))
  ;; Add the permutation to the generating set.
  (pushnew perm (gethash k sgs))
  
  (let ((redo nil))
    (loop
      (loop :for s :being :the :hash-values :of (transversal-ref trans k) :do
        (dolist (tau (gethash k sgs))
          (let ((prod (perm-compose tau s)))
            (unless (trans-element-p prod trans)
              (multiple-value-setq (sgs trans)
                (update-transversal prod sgs trans k))
              
              (setf redo t)))))
      
      ;; Break out?
      (unless redo
        (return-from add-generator (values sgs trans)))
      
      ;; Reset the REDO flag.
      (setf redo nil))))

(defun update-transversal (perm sgs trans &optional (k (perm-size perm)))
  (let ((j (perm-eval perm k)))
    (multiple-value-bind (sigma exists?) (sigma trans k j)
      (cond
        (exists?
         (let ((new-perm (perm-compose (perm-inverse sigma)
                                       perm)))
           (if (trans-element-p new-perm trans (1- k))
               (values sgs trans)
               (add-generator new-perm sgs trans (1- k)))))
        (t
         (setf (gethash j (transversal-ref trans k)) perm)
         (values sgs trans))))))

(defun generate-perm-group (generators)
  "Generate a permutation group generated by the list of permutations GENERATORS."
  (labels ((identity-table (n)
             "Make a hash table mapping N to the identity permutation of size N."
             (let ((ht (make-hash-table)))
               (setf (gethash n ht)
                     (perm-identity n))
               
               ht)))
    (let* ((n (maximum generators :key 'perm-size))
           (sgs (make-hash-table))
           (trans (make-transversal n)))
      ;; Initialize TRANS to map I -> (I -> Identity(I)).
      (loop :for i :from 1 :to n :do
        (setf (transversal-ref trans i) (identity-table i)))
      
      ;; Add the generators.
      (dolist (generator generators)
        (multiple-value-setq (sgs trans)
          (add-generator generator sgs trans)))
      
      ;; Return the group.
      (make-perm-group :element-size (maximum generators :key #'perm-size)
                       :generators (copy-list generators)
                       :strong-generators sgs
                       :transversal-system trans))))

(defun group-from (generators-as-lists)
  "Generate a permutation group from a list of generators, which are represented as lists."
  (generate-perm-group (mapcar #'list-to-perm generators-as-lists)))

;;; TODO: Automatically try calculating size.
(defun group-from-cycles (generators-as-cycles size)
  "Generate a permutation group from a list of generators, which are represented as cycles."
  (generate-perm-group (mapcar (lambda (c)
                                 (from-cycles c size))
                               generators-as-cycles)))

(defun group-order (group)
  "Compute the order of the permutation group GROUP."
  (let ((transversals (perm-group.transversal-system group)))
    (product transversals :key #'hash-table-count)))

(defun group-element-p (perm group)
  "Decide if the permutation PERM is an element of the group GROUP."
  (trans-element-p perm (perm-group.transversal-system group)))

(defun random-group-element (group)
  "Generate a random element of the group GROUP."
  (loop :for v :across (perm-group.transversal-system group)
        :collect (random-hash-table-value v) :into random-sigmas
        :finally (return (let ((maxlen (maximum random-sigmas :key #'perm-size)))
                           (reduce #'perm-compose
                                   random-sigmas
                                   :key (lambda (s) (perm-compose (perm-identity maxlen) s)))))))

;;; XXX: This can be made more efficient by directly reducing over,
;;; removing identities within the fold function.
(defun transversal-decomposition (perm group &key remove-identities)
  "Decompose the permutation PERM into transversal sigmas of the group GROUP."
  (let ((decomp
          (trans-decomposition perm (perm-group.transversal-system group))))
    (if remove-identities
        (delete-if (lambda (sigma)
                     (= (car sigma)
                        (cdr sigma)))
                   decomp)
        decomp)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;; Group Orbits ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-membership-set (size)
  ;; 1+ to account for 1-based indexing
  (make-array (1+ size) :element-type 'bit :initial-element 0))

(defun membership-sets-intersect-p (set-a set-b)
  (some (lambda (a b) (= 1 a b)) set-a set-b))

(defun membership-set-nunion (set-a set-b)
  (map-into set-a #'logior set-a set-b))

(defun membership-set-count (set)
  (count 1 set))

(defun clear-membership-set (set)
  (map-into set (constantly 0)))

(defun group-orbits (group)
  "Compute the orbits of the group GROUP. This will be a list of arrays of points."
  (let* ((d (group-degree group))
         (orbit-membership (make-membership-set d))
         (orbit-memberships nil))
    (flet ((orbit-completed-for-element (x)
             (some (lambda (orbit)
                     (= 1 (sbit orbit x)))
                   orbit-memberships))
           (membership-set-to-orbit (set)
             (let ((orbit (make-array (membership-set-count set)))
                   (j 0))
               (loop :for i :from 1 :to d
                     :when (= 1 (sbit set i))
                       :do (setf (aref orbit j) i)
                           (incf j)
                     :finally (return orbit)))))
      ;; We compute the orbit of each point for each generator,
      ;; intersecting each time.
      (loop :for i :from 1 :to d :do
        (unless (orbit-completed-for-element i)
          (clear-membership-set orbit-membership)
          ;; Compute the orbit of the element across all generators.
          (dolist (g (perm-group.generators group))
            (map-orbit (lambda (k)
                         (setf (sbit orbit-membership k) 1))
                       i
                       g))
          ;; Incorporate that orbit anywhere it has intersected.
          (let ((intersecting-orbit (find-if (lambda (set)
                                               (membership-sets-intersect-p
                                                orbit-membership
                                                set))
                                             orbit-memberships)))
            (if (null intersecting-orbit)
                (push (copy-seq orbit-membership) orbit-memberships)
                (membership-set-nunion intersecting-orbit orbit-membership)))))
      ;; Return the orbits.
      (mapcar #'membership-set-to-orbit orbit-memberships))))

;;; XXX: We may want to store the group was derived from and the
;;; function from the factor group to the original group.
(defun group-from-orbit (original-group orbit)
  "Produce a group by having the group ORIGINAL-GROUP act on the orbit ORBIT of that group."
  (let ((len (length orbit)))
    (labels ((relevant-cycle-p (cycle)
               "Is the cycle CYCLE at all relevant? This equates to finding an element in the cycle that exists within the orbit."
               (some (lambda (cycle-element)
                       (find cycle-element orbit))
                     (cycle-rep cycle)))
             (remap-element (element)
               (1+ (position element orbit)))
             (remap-cycle (cycle)
               (let ((c (copy-seq (cycle-rep cycle))))
                 (%make-cycle :canonicalized nil
                              :rep (map-into c #'remap-element c))))
             (process-generator (g)
               (from-cycles
                (mapcar #'remap-cycle
                        (remove-if-not #'relevant-cycle-p
                                       (to-cycles g)))
                len)))
      (generate-perm-group
       (remove-if #'perm-identity-p (mapcar #'process-generator (perm-group.generators original-group)))))))

(defun subdirect-factors (group)
  "Compute \"subdirect factors\" of the group GROUP.

 These are groups whose direct product has GROUP as a subgroup."
  (mapcar (lambda (o) (group-from-orbit group o))
          (group-orbits group)))

;;;;;;;;;;;;;;;;;;;;;;;;;;; Debug Routines ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun print-trans (group)
  (loop
    :for k :from 1
    :for vk :across (perm-group.transversal-system group)
    :do (progn
          (format t "~D:~%" k)
          (loop :for j :being :the :hash-keys :in vk
                :for vj := (gethash j vk)
                :do (format t "    ~D: ~A~%" j vj)))))
