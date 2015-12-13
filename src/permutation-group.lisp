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
  transversal-system
  free-group
  slp-context)

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
  (perm-identity (group-degree group :true nil)))

;;; Implementation of the group protocol.

(defmethod identity-element ((g perm-group))
  (group-identity g))

(defmethod compose ((g perm-group) a b)
  (perm-compose a b))

(defmethod inverse ((g perm-group) a)
  (perm-inverse a))

(defmethod generators ((g perm-group))
  (copy-list (perm-group.generators g)))

(defmethod num-generators ((g perm-group))
  (length (perm-group.generators g)))


;;;;;;;;;;;;;;; Transversal Systems and Schreier-Sims ;;;;;;;;;;;;;;;;

(deftype transversal ()
  ;; It could actually be (simple-array (or null hash-table) (*)), but
  ;; we will want to use SVREF. While such a type would collapse into
  ;; SIMPLE-VECTOR in most implementations, we don't want to assume
  ;; such.
  ;;
  ;; Elements of the transversal are constructed by MAKE-SIGMA-TABLE.
  `simple-vector)

(declaim (inline make-transversal))
(defun make-transversal (n)
  "Make a transversal of size N."
  (make-array n :initial-element nil))

(declaim (inline transversal-ref))
(defun transversal-ref (trans k)
  "Get the Kth element of the transversal TRANS. This is representative of all sigma_k."
  (declare (type transversal trans))
  (svref trans (1- k)))

(defun (setf transversal-ref) (new-value trans k)
  (declare (type transversal trans))
  (setf (svref trans (1- k)) new-value))

(defun perm-group-printer (group stream depth)
  (declare (ignore depth))
  (print-unreadable-object (group stream :type t :identity nil)
    (format stream "of ~D generator~:p" (num-generators group))))

(defun make-sigma-table (k)
  "Make a representation of sigma_K, initialized witk sigma_KK = identity.

This is represented as a hash table mapping J to permutations sigma_KJ."
  (let ((ht (make-hash-table :test 'eql)))
    (setf (gethash k ht)
          (perm-identity k))
    ht))

;;; SIGMAs are elements of the transversal system. A SIGMA is either
;;; NIL or some permutation that maps K to J.
(defun sigma (trans k j)
  "Retrieve sigma_kj for the transversal system TRANS."
  (let ((sigma_k (transversal-ref trans k)))
    (values (gethash j sigma_k))))

(defun (setf sigma) (new-value trans k j)
  (setf (gethash j (transversal-ref trans k)) new-value))

(defun sigma-symbol (k j)
  "Return a symbol representing sigma_kj. This is used for perms that are added to the transversal system during group construction."
  (alexandria:format-symbol ':keyword "SIGMA_(~D,~D)" k j))

(defun tau-symbol ()
  "Return a freshly made symbol for tau."
  (gensym "TAU-"))

;;; XXX: Can we simplify this by just looking for SIGMA?
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
  "Decompose PERM into a list of sigmas within the transversal system TRANS. The composition of the sigmas equals the original perm up to K.

The sigma (SIGMA K J) is represented by the cons cell (K . J)."
  ;; XXX: Could avoid NREVERSE by collecting correctly.
  (flet ((collector (decomp k j)
           (acons k j decomp)))
    (declare (dynamic-extent #'collector))
    (nreverse (reduce-over-trans-decomposition #'collector nil perm trans k))))

(defun trans-element-p (perm trans &optional (k (perm-size perm)))
  #+#:equivalent (not (null (trans-decomposition perm trans k)))
  (values
   (reduce-over-trans-decomposition
    (load-time-value (constantly t))
    t
    perm
    trans
    k)))

(defvar *context*)
(setf (documentation '*context* 'variable)
      "Special variable used to hold the context being built up for a group.")

(defvar *taus*)
(setf (documentation '*taus* 'variable)
      "Special variable used to hold a mapping between permutation objects (by EQ) to a symbol (one generated by #'TAU-SYMBOL) which is referred to by the SLP context.")

;;; For #'ADD-GENERATOR and #'UPDATE-TRANSVERSAL:
;;;
;;;   * PERM: the perm to introduce to the SGS and transversal system.
;;;
;;;   * SGS: the strong generating set
;;;
;;;   * TRANS: the transversal system
;;;
;;;   * K: All points above K should be fixpoints. K is what is
;;;        recursed on.
;;;
;;;   * SLP: An SLP to construct PERM. Used for recording purposes.
;;;
;;; Both return an updated SGS and transversal system as two values.
;;;
;;; Both also use *CONTEXT* to record SLPs, which is expected to be
;;; bound to. This is indeed the case by the main entry point
;;; #'GENERATE-PERM-GROUP.

;;; Algorithm B from Knuth, with my own modifications.
(defun add-generator (perm sgs trans k slp)  
  ;; Add the perm to the SGS.
  (pushnew perm (gethash k sgs))

  ;; Generate a new tau for the perm, and remember it.
  (let ((t-sym (tau-symbol)))
    (setf (gethash perm *taus*) t-sym)
    (setf (symbol-assignment *context* t-sym) slp))
 
  ;; Process the perm, adding it to the group structure.
  (let ((redo nil))
    (loop
      (loop :for s :being :the :hash-values :of (transversal-ref trans k)
              :using (hash-key j)
            :for s-sym := (sigma-symbol k j) :do
              (dolist (tau (gethash k sgs))
                (let* ((prod (perm-compose tau s))
                       (t-sym (gethash tau *taus*))
                       (prod-slp (compose-slp (slp-symbol t-sym)
                                              (slp-symbol s-sym))))
                  (unless (trans-element-p prod trans)
                    (multiple-value-setq (sgs trans)
                      (update-transversal prod sgs trans k prod-slp))
                    
                    (setf redo t)))))
      
      ;; Break out?
      (unless redo
        (return-from add-generator (values sgs trans)))
      
      ;; Reset the REDO flag.
      (setf redo nil))))

;;; Algorithm B from Knuth, with my own modifications.
(defun update-transversal (perm sgs trans k slp)
  (let* ((j (perm-eval perm k))
         (sigma (sigma trans k j)))
    (cond
      ((not (null sigma))
       (let ((new-perm (perm-compose (perm-inverse sigma)
                                     perm))
             (new-perm-slp (compose-slp
                            (invert-slp 
                             (slp-symbol (sigma-symbol k j)))
                            slp)))
         (if (trans-element-p new-perm trans (1- k))
             (values sgs trans)
             (add-generator new-perm sgs trans (1- k) new-perm-slp))))
      (t
       (setf (sigma trans k j) perm)
       (setf (symbol-assignment *context* (sigma-symbol k j))
             slp)
       (values sgs trans)))))

(defun generate-perm-group (generators)
  "Generate a permutation group generated by the list of permutations GENERATORS."
  (let* ((n (maximum generators :key 'perm-size))
         (sgs (make-hash-table))
         (trans (make-transversal n))
         (fg (make-free-group (length generators)))
         (*context* (make-instance 'slp-context))
         (*taus* (make-hash-table :test 'eq)))
    ;; Initialize TRANS to map sigma_KK: K -> (K -> Identity(K)).
    ;;
    ;; Also record their SLPs as (SLP-ELEMENT <fg identity>).
    (loop :for k :from 1 :to n :do
      (setf (transversal-ref trans k) (make-sigma-table k))
      (setf (symbol-assignment *context* (sigma-symbol k k))
            (slp-element (identity-element fg))))
    
    ;; Add the generators.
    ;;
    ;; We iterate through the generators of the induced free group
    ;; as well purely to record SLPs. We can then construct a
    ;; homomorphism from the free group to the perm group to
    ;; construct elements from their generators.
    (loop :for generator :in generators
          :for fg-generator :in (generators fg)
          :do (multiple-value-setq (sgs trans)
                (add-generator generator
                               sgs
                               trans
                               n
                               (slp-element fg-generator))))
    
    ;; Return the group.
    (make-perm-group :element-size (maximum generators :key #'perm-size)
                     :generators (copy-list generators)
                     :strong-generators sgs
                     :transversal-system trans
                     :slp-context *context*
                     :free-group fg)))

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
        :finally (return
                   (reduce #'perm-compose
                           random-sigmas
                           :initial-value (group-identity group)))))

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
          (dolist (g (generators group))
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
       (remove-if #'perm-identity-p (mapcar #'process-generator (generators original-group)))))))

(defun subdirect-factors (group)
  "Compute \"subdirect factors\" of the group GROUP.

 These are groups whose direct product has GROUP as a subgroup."
  (mapcar (lambda (o) (group-from-orbit group o))
          (group-orbits group)))


;;;;;;;;;;;;;;;;;;;;;; Generator Decomposition ;;;;;;;;;;;;;;;;;;;;;;;

(defun free-group-generator-to-perm-group-generator (perm-group free-group-generator)
  "Convert the free group generator FREE-GROUP-GENERATOR to a generator within the perm group PERM-GROUP."
  (let ((i free-group-generator))
    (cond
      ((zerop i) (group-identity perm-group))
      ((plusp i) (elt (generators perm-group) (1- i)))
      ((minusp i) (perm-inverse
                   (free-group-generator-to-perm-group-generator
                    perm-group
                    (- i)))))))

(defun free-group->perm-group-homomorphism (free-group perm-group)
  "Construct a homomorphism from the perm group PERM-GROUP's free group to elements of the perm group itself."
  (assert (= (num-generators free-group)
             (num-generators perm-group))
          (free-group perm-group)
          "The free group and the perm group must have the ~
           same number of generators.")
  ;; The perm group contains the free group.
  (lambda (elts)
    (typecase elts
      (integer (free-group-generator-to-perm-group-generator
                perm-group
                elts))
      (list (loop :with result := (group-identity perm-group)
                  :for i :in elts
                  :do (setf result
                            (perm-compose result
                                          (free-group-generator-to-perm-group-generator perm-group i)))
                  :finally (return result))))))

(defun generator-decomposition (perm group)
  "Compute the generator decomposition of the permutation PERM of the group GROUP.

Note: The result is likely very long and inefficient."
  (let* ((d (transversal-decomposition perm group :remove-identities t))
         (ctx (perm-group.slp-context group))
         (fg (perm-group.free-group group))
         (hom (free-group->perm-group-homomorphism fg group)))
    (labels ((to-sigma-symbol (tt)
               (sigma-symbol (car tt) (cdr tt)))
             (find-slp (tt)
               (symbol-assignment ctx (to-sigma-symbol tt)))
             (eval-slp (slp)
               (evaluate-slp fg ctx slp)))
      (mapcar hom                           ; Free -> Perm
              (delete (identity-element fg) ; Remove identities.
                      (mapcan #'eval-slp    ; Eval SLPs
                              (mapcar #'find-slp d)))))))
