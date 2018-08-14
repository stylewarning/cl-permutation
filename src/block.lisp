;;;; block.lisp
;;;;
;;;; Copyright (c) 2015-2018 Robert Smith

(in-package #:cl-permutation)

;;; This file has to do with the computation of "block systems". A
;;; "block" is roughly a set of points that always "move together". I
;;; am not sure where its name comes from, but blocks on a Rubik's
;;; cube-like puzzle would be the actual physical cubelets. (Sometimes
;;; cubelets don't make *minimal* blocks, however. An example of where
;;; this is the case is the <M, U> subgroup of the cube.)

;;; TODO: Do group theory within the blocks themselves.

;;; Disjoint-Set (DJS) Data Structure
;;;
;;; Below we implement the well known union-find algorithms. We don't
;;; implement the common optimization of balancing-by-rank, because we
;;; have an additional constraint: We need to arbitrarily be able to
;;; change the canonical element of each DJS.
;;;
;;; We implement this by endowing DJS's with a "DJS-REP" data
;;; structure, which adds a level of indirection. This data structure
;;; just contains a single pointer to DJS node. There is an invariant
;;; that if one DJS-REP points to a node N, then no other DJS-REP node
;;; points to N as well.
;;;
;;; DJS nodes point to their representative DJS-REP, which changes
;;; when a DJS-UNION operation occurs. When DJS-UNION occurs, we may
;;; mess up the invariant above. This invariant is later repaired
;;; "on-demand" within DJS-FIND.

;;; We turn *PRINT-CIRCLE* on because each DJS/DJS-REP creates a new
;;; circular structure.
(setf *print-circle* t)

(defstruct (djs (:constructor %make-djs))
  "Representation of a disjoint-set data structure. Each node has a representative element denoted by REPRESENTATIVE, which points to a DJS-REP node representing the canonical element of that disjoint-set."
  ;; FIXME: This type produces a warning because the compiler
  ;; presumably doesn't know about DJS-REP yet.
  (representative nil :type (or null djs-rep))
  value)

;;; TODO: I think we can eliminate DJS-REP. We can just use a cons
;;; cell.
(defstruct (djs-rep (:constructor %make-djs-rep))
  "Pointer to the representative element of a DJS."
  (djs nil :type djs))

(defun djs (value)
  "Construct a fresh DJS node."
  (let* ((node (%make-djs :value value))
         (rep (%make-djs-rep :djs node)))
    (setf (djs-representative node) rep)
    ;; Return the DJS node.
    node))

(defun djs-change-representative (djs)
  "Change the representative of DJS to point to itself."
  (setf (djs-rep-djs (djs-representative djs)) djs))

(defun djs-find (d)
  "Find the canonical DJS node of which the DJS node D is a part."
  (loop :with rep := (djs-representative d)
        :with rep-node := (djs-rep-djs rep)
        :with rep-node-rep := (djs-representative rep-node)
        ;; Fix-up out-of-date rep nodes. Once updated, this loop will
        ;; not execute in future invocations.
        :until (eq rep rep-node-rep) :do
          (setf (djs-representative d) rep-node-rep
                rep                    rep-node-rep
                rep-node               (djs-rep-djs rep)
                rep-node-rep           (djs-representative rep-node))
        :finally (return (djs-rep-djs rep-node-rep))))

(defun djs-union (a b)
  "Link together the DJS nodes A and B.

The representative of the union will be that of B."
  (let ((a-rep (djs-representative a))
        (b-rep (djs-representative b)))
    (unless (eq a-rep b-rep)
      ;; Set representative of A to be B-REP. Now A won't point to
      ;; it's old representative.
      (setf (djs-representative a) b-rep)
      ;; Change the old A representative to point to B.
      (setf (djs-rep-djs a-rep)
            (djs-rep-djs b-rep))))
  nil)


;;; Internal driver algorithms for block system computation.

(defun find-minimal-block-system-containing (perm-group alphas)
  "Find the minimal blocks of the permutation group PERM-GROUP which contain the list of points ALPHAS.

Returns a list of lists. Each sub-list represents a block. Each block is an image of one another under the generators of the group."
  ;; This is an implementation of Atkinson's algorithm, along with
  ;; additional features I've added.
  (check-type perm-group perm-group)
  (assert (not (null alphas)) (alphas) "ALPHAS must contain at least 1 point.")
  (assert (listp alphas) (alphas) "ALPHAS must be a list.")
  (let* ((degree (group-degree perm-group))
         ;; CLASSES is a map from point to DJS. We can reverse this
         ;; map by inspecting the value of DJS via DJS-VALUE. See the
         ;; functions CLASS and REP below.
         (classes (make-array (1+ degree) :initial-element nil))
         ;; PROCESSED-ELEMENTS tells us which elements we've
         ;; processed, so we can return only the elements that are a
         ;; part of the block system.
         (processed-elements (make-membership-set degree)))
    (assert (every (lambda (alpha) (<= 1 alpha degree)) alphas)
            ()
            "ALPHAS contains invalid points. They must be between 1 and ~
             the degree of the group, which is ~D."
            degree)
    ;; Set up our equivalence classes as DJS's.
    ;;
    ;; First, initialize all classes..
    (loop :for i :from 1 :to degree :do
      (setf (aref classes i) (djs i)))

    ;; Next, put all ALPHA_I in the same equivalence class.
    (let ((alpha_1-class (aref classes (first alphas))))
      (loop :for i :in (rest alphas) :do
        (let ((alpha_i-class (aref classes i)))
          ;; Ensure that alpha_1-class is the representative of this
          ;; union. Currently, this is implicit in the DJS-UNION call.
          (djs-union alpha_i-class alpha_1-class))))
    ;; Note that we have processed all ALPHA elements.
    (dolist (alpha alphas)
      (setf (sbit processed-elements alpha) 1))
    ;; Now for the main algorithm...
    (labels ((class (point)
               "Find the class of the point POINT."
               (aref classes point))
             (rep (point)
               "Find the class representative of the point POINT."
               (djs-value (djs-find (class point)))))
      (let ((q (make-queue)))
        ;; Initialize the queue with ALPHA_I for I > 1.
        (dolist (alpha (rest alphas))
          (enqueue q alpha))
        ;; Iterate until queue is empty.
        (loop :until (queue-empty-p q) :do
          (let ((gamma (dequeue q)))
            (dolist (g (generators perm-group))
              (let* ((delta (rep gamma))
                     (kappa (rep (perm-eval g gamma)))
                     (lam   (rep (perm-eval g delta))))
                (unless (= kappa lam)
                  (let ((kappa-class (class kappa))
                        (lam-class (class lam)))
                    (setf (sbit processed-elements lam)   1)
                    (setf (sbit processed-elements kappa) 1)
                    (setf (sbit processed-elements delta) 1)
                    ;; Merge the kappa and lambda classes.
                    (djs-union lam-class kappa-class)
                    ;; Make kappa the representative of merged class.
                    (djs-change-representative kappa-class)
                    ;; Add LAM for processing.
                    (enqueue q lam))))))))
      ;; Return equivalence classes.
      (let ((table (make-hash-table :test 'eql)))
        (loop :for j :from 1 :to degree
              :when (= 1 (sbit processed-elements j))
                :do (push j (gethash (rep j) table nil))
              :finally (let ((equiv-classes nil))
                         (maphash (lambda (k v)
                                    (declare (ignore k))
                                    (push (nreverse v) equiv-classes))
                                  table)
                         (return (values (nreverse equiv-classes)
                                         processed-elements))))))))

(defun atkinson (ω gs)
  "M. D. Atkinson's original algorithm as specified in his original paper \"An Algorithm for Finding Blocks of a Permutation Group\", with very light modifications.

Given a point ω and a list of generators GS, return an array F whose size is max deg(gs), and whose elements are specified as follows:

If a point p appears in F, then the minimal block containing p is the list of all positions of p in F."
  ;; Step 1: Initialize
  (let* ((C nil)
         (n (loop :for g :in gs :maximize (perm-size g)))
         (f (coerce (iota+1 n) 'vector)))
    (prog (α β γ δ gs-left g)
     STEP-2
       (push ω C)
       (setf (aref f (1- ω)) 1)

     STEP-3
       (setf β (pop C))
       (setf α (aref f (1- β)))

     STEP-4
       ;; Atkinson instead sets an index j = 0 here to iterate through
       ;; GS. We just iterate directly.
       (setf gs-left gs)

     STEP-5
       ;; Atkinson would have done j += 1 here and accessed GS
       ;; directly.
       (setf g (pop gs-left))
       (setf γ (perm-eval g α))
       (setf δ (perm-eval g β))

     STEP-6
       (when (= (aref f (1- γ))
                (aref f (1- δ)))
         (go STEP-9))

     STEP-7
       (unless (< (aref f (1- δ))
                  (aref f (1- γ)))
         (rotatef δ γ))

     STEP-8
       (let ((fγ (aref f (1- γ)))
             (fδ (aref f (1- δ))))
         (setf f (nsubstitute fδ fγ f))
         (push fγ C))

     STEP-9
       (unless (null gs-left)
         (go STEP-5))

     STEP-10
       (unless (null C)
         (go STEP-3))

     STEP-11
       (return f))))

(defun trivial-block-system-p (bs)
  "Is the block system BS a trivial block system?"
  (or (= 1 (length bs))
      (every (lambda (b) (= 1 (length b))) bs)))

(defun find-non-trivial-block-system (group)
  "Find a non-trivial block system of the permutation group GROUP.

GROUP must be transitive in order for this to produce truly non-trivial block systems."
  (loop :for i :from 2 :to (group-degree group)
        :for bs := (find-minimal-block-system-containing group (list 1 i))
        :unless (trivial-block-system-p bs)
          :do (return bs)))

(defun canonicalize-raw-block-subsystems (bss)
  "Take a raw list of block systems BSS and canonicalize them."
  (labels ((canonicalize-block (blk)
             (sort (copy-list blk) #'<))
           (canonicalize-system (bs)
             (sort (mapcar #'canonicalize-block bs) #'< :key #'first)))
    (mapcar #'canonicalize-system bss)))

(defun raw-block-subsystems (group &key (canonicalize t))
  "Compute all minimal, disjoint block subsystems of the group GROUP.

Returns a list of block systems."
  (labels ((find-block-system (orbit)
             (loop :with first := (aref orbit 0)
                   :for i :from 1 :below (length orbit)
                   :for p := (aref orbit i)
                   :for bs := (find-minimal-block-system-containing
                               group
                               (list first p))
                   :unless (trivial-block-system-p bs)
                     :do (return bs)
                   :finally (return (list (coerce orbit 'list))))))
    (let* ((orbits (group-orbits group))
           (bss (mapcar #'find-block-system orbits)))
      (if canonicalize
          (canonicalize-raw-block-subsystems bss)
          bss))))

(defun primitive-group-p (group)
  "Is the perm group GROUP primitive?"
  (trivial-block-system-p (raw-block-subsystems group)))


;;; Now we start wrapping this up into better packaging.

(defstruct block-subsystem
  "Representation of a block subsystem of a group. A \"block subsystem\" is a G-orbit of a block."
  ;; The group it came from.
  group
  ;; The block in the lowest slot.
  base-block
  ;; Number of blocks in the subsystem.
  size
  ;; Block size (number of points per block).
  block-size
  ;; The block subsystem itself.
  orbit
  ;; The slot in which each block rests in identity, indexed by the
  ;; block's minimum element. This is an a-list of (min-elt . slot)
  ;; pairs.
  ;;
  ;; TODO: We don't actually need to store this. It's readily
  ;; available from a canonicalized ORBIT.
  block-slots)

(defun block-slot (block-subsystem blk)
  "In which slot is BLK found in the block subsystem BLOCK-SUBSYSTEM?"
  (let ((found (assoc (list-minimum blk) (block-subsystem-block-slots block-subsystem))))
    (assert found (block-subsystem blk)
            "The block ~A was not found in the block subsystem ~A." blk block-subsystem)
    (cdr found)))

(defun group-block-subsystems (group)
  "Return a list of block subsystems of the group GROUP."
  (flet ((process-raw-block-subsystem (bs)
           (assert (not (null bs)))
           (let* ((sorted-blocks (sort (copy-list bs) #'< :key #'list-minimum))
                  (num-blocks (length sorted-blocks))
                  (base-block (first sorted-blocks))
                  (block-size (length base-block)))
             (make-block-subsystem
              :group group
              :base-block base-block
              :size num-blocks
              :block-size block-size
              :orbit sorted-blocks
              :block-slots (loop :for i :from 1
                                 :for blk :in sorted-blocks
                                 :for m := (list-minimum blk)
                                 :collect (cons m i))))))
    (mapcar #'process-raw-block-subsystem (raw-block-subsystems group :canonicalize t))))


;;; Interblock Group & Permutations

(defun block-subsystem-interblock-group-homomorphism (subsys)
  "Given a block subsystem SUBSYS, compute a homomorphism to its interblock group."
  (lambda (gen)
    (list-to-perm
     (loop :with gen. := (perm-evaluator gen)
           :for blk :in (block-subsystem-orbit subsys)
           :collect (block-slot subsys (mapcar gen. blk))))))

(defun block-subsystem-interblock-group (subsys)
  "Compute the interblock group of a block subsystem SUBSYS."
  (let ((hom (block-subsystem-interblock-group-homomorphism subsys)))
    (generate-perm-group (mapcar hom (generators (block-subsystem-group subsys))))))


;;; Intrablock Group & Orientations

(defun block-subsystem-intra-generators (subsys)
  "Compute the generators of the intrablock group, along with the
computed reference frames."
  (check-type subsys block-subsystem)
  (let* ((num-slots (block-subsystem-size subsys))
         (G-generators (perm-group.generators (block-subsystem-group subsys)))
         (blocks (block-subsystem-orbit subsys))
         (reference-frames (make-array num-slots :initial-element nil))
         (intra-gens '()))
    (flet ((block-slot (blk)
             "What's the slot of this block?"
             (position (list-minimum blk) blocks :key #'first)))
      ;; Arbitrarily choose a reference frame for an arbitrary
      ;; block. We choose ρ = identity for the first (i.e., base) block.
      (setf (aref reference-frames 0) (block-subsystem-base-block subsys))
      ;; Now we seek to choose reference frames for each of the other
      ;; slots. To do this, we start with a reference frame we know ρᵢ
      ;; for slot i, and compute γ.ρ and check if we landed in a new
      ;; slot j. If we did, then ρⱼ := γ.ρ is the reference frame for
      ;; that slot j. We queue up ρⱼ as a slot to explore later.
      ;;
      ;; We keep going until we've established reference frames for
      ;; all of the slots.
      (loop :with unexplored-frames := (list-to-queue (list (block-subsystem-base-block subsys)))
            :until (zerop (count nil reference-frames))
            :for ρ := (dequeue unexplored-frames)
            :do
               (dolist (γ G-generators)
                 (let* ((γ.ρ (mapcar (perm-evaluator γ) ρ))
                        (j (block-slot γ.ρ))
                        (ρⱼ (aref reference-frames j)))
                   (unless ρⱼ
                     (setf (aref reference-frames j) γ.ρ)
                     (enqueue unexplored-frames γ.ρ)))))
      ;; Now that we have all of the frames, we want to find
      ;; generators for the intrablock group. To do this, we look at
      ;; the action of each γ on each frame ρ. If the action isn't
      ;; identity, then we've found a generator of the intrablock
      ;; group.
      (loop :for ρ :across reference-frames :do
        (dolist (γ G-generators)
          (let* ((γ.ρ (mapcar (perm-evaluator γ) ρ))
                 (j (block-slot γ.ρ))
                 (ρⱼ (aref reference-frames j)))
            (assert (not (null ρⱼ)) () "Inconsistent state. Somehow we missed a reference frame.")
            ;; Check whether this action isn't identity, and do it
            ;; cheaply.
            (unless (every #'= ρⱼ γ.ρ)
              (pushnew (permuter ρⱼ γ.ρ) intra-gens :test #'perm=)))))
      ;; All done. Return the values.
      (values intra-gens reference-frames))))


;;; TODO: Coordinates

#+#:ignore
(progn
  (defun make-block-coord (permutation orientation)
    (cons permutation orientation))

  (defun block-coord-permutation (blk-coord)
    (car blk-coord))

  (defun block-coord-orientation (blk-coord)
    (cdr blk-coord))

  (defstruct (coord-transformations (:conc-name nil)
                                    (:constructor %make-coord-trans))
    block-subsystem
    perm-cardinality
    to-perm
    from-perm
    orient-cardinality
    to-orient
    from-orient
    )



  (defun make-coord-transformation (block-subsystem)
    (let ((perm-group (block-subsystem-permutation-group block-subsystem))
          (orientation-vector-spec
            (make-radix-spec (block-subsystem-block-size block-subsystem)
                             (block-subsystem-size block-subsystem)))
          (orientation-spec
            (make-perm-spec (block-subsystem-block-size block-subsystem))))
      (flet ((to-orient (index)
               (map 'vector
                    (lambda (o) (let ((v (unrank orientation-spec o)))
                                  (vector-to-perm (map-into v #'1+ v))))
                    (unrank orientation-vector-spec index)))
             (from-orient (o-vec)
               (rank orientation-vector-spec
                     (map 'vector (lambda (x) (rank orientation-spec (perm-to-vector x)))
                          o-vec))))
        (multiple-value-bind (from-perm to-perm)
            (group-element-rank-functions perm-group)
          (%make-coord-trans
           :block-subsystem block-subsystem
           :perm-cardinality (group-order perm-group)
           :orient-cardinality (cardinality orientation-vector-spec)
           :from-perm from-perm
           :to-perm to-perm
           :from-orient #'from-orient
           :to-orient #'to-orient)))))

  (defun identity-block-coordinate (subsys)
    (loop :with ℓ := (block-subsystem-size subsys)
          :with si := (perm-identity (block-subsystem-block-size subsys))
          :for bs :in (block-subsystem-orbit subsys)
          :collect (make-block-coordinate
                    (perm-identity ℓ)
                    (make-array ℓ :initial-element si))))


  (defun perm-to-coords (perm block-systems)
    (labels ((permute-bss (bss)
               (loop :with ev := (alexandria:curry #'perm-eval perm)
                     :for bs :in bss
                     :collect (loop :for blk :in bs
                                    :collect (mapcar ev blk))))
             (rank-block (blk)
               (rank (make-perm-spec (length blk)) (coerce blk 'vector))))
      (let ((p-bss (permute-bss (group-bss-raw block-systems)))
            ;; Allocate some fresh coords.
            (coords (identity-block-coordinate block-systems)))
        ;; First, we compute the permutation vectors.
        (loop :for bs :in p-bss
              :for (p . o) :in coords
              :collect (make-block-coordinate
                        (apply #'make-perm (mapcar (lambda (blk)
                                                     (aref (group-bss-ordering block-systems)
                                                           (list-minimum blk)))
                                                   bs))
                        (map 'vector #'rank-block bs))))))
)
