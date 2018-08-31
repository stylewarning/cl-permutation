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
  ;; The block subsystem itself, in sorted order. This is the G-orbit
  ;; of BASE-BLOCK.
  orbit)

(defun block-slot (subsys blk)
  "In which slot is BLK found in the block subsystem BLOCK-SUBSYSTEM?"
  ;; Ergh, we assume BLK is of the right size and is a valid block of
  ;; SUBSYS.
  (1+ (position (list-minimum blk) (block-subsystem-orbit subsys) :test #'= :key #'first)))

(defun group-block-subsystems (group)
  "Return a list of block subsystems of the group GROUP."
  (flet ((process-raw-block-subsystem (bs)
           (assert (not (null bs)))
           (let* ((sorted-blocks (sort (copy-list bs) #'< :key #'list-minimum))
                  (base-block (first sorted-blocks)))
             (make-block-subsystem
              :group group
              :base-block base-block
              :size (length sorted-blocks)
              :block-size (length base-block)
              :orbit sorted-blocks))))
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

(defun block-subsystem-intrablock-group-generators (subsys)
  "Compute (possibly redundant) generators of the intrablock group, along with the computed reference frames."
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
      ;; slots in such a way that we don't depend upon our of
      ;; coordinates (i.e., how we map from G to Aut n, or how we
      ;; decide to order blocks themselves). To do this, we start with
      ;; a reference frame we know ρᵢ for slot i, and compute γ.ρᵢ and
      ;; check if we landed in a new slot j. If we did, then ρⱼ :=
      ;; γ.ρᵢ is the reference frame for that slot j. We queue up ρⱼ
      ;; as a slot to explore later.
      ;;
      ;; We keep going until we've established reference frames for
      ;; all of the slots.
      (loop :with unexplored-frames := (list-to-queue (list (aref reference-frames 0)))
            :until (zerop (count nil reference-frames))
            :for ρ := (dequeue unexplored-frames)
            :do (dolist (γ G-generators)
                  (let* ((γ.ρ (mapcar (perm-evaluator γ) ρ))
                         (j (block-slot γ.ρ))
                         (ρⱼ (aref reference-frames j)))
                    (when (null ρⱼ)
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

      ;; TODO: Verify this ensures the identity element in the group
      ;; corresponds to the identity elements in the intrablock
      ;; groups. It seems to be the case for some tests.
      ;;
      ;; All done. Return the values.
      (values intra-gens reference-frames))))

(defun block-subsystem-intrablock-group (subsys)
  (generate-perm-group (block-subsystem-intrablock-group-generators subsys)))

(defun intrablock-coordinate-function (subsys)
  ;; Interpretation of g → #(X Y Z ...)
  ;;
  ;;    The block in slot 0 will undergo a change in orientation by X.
  ;;    The block in slot 1 will undergo a change in orientation by Y.
  ;;    etc.
  (multiple-value-bind (gens frame) (block-subsystem-intrablock-group-generators subsys)
    (let ((group (generate-perm-group gens)))
      (multiple-value-bind (rank unrank) (group-element-rank-functions group)
        (declare (ignore unrank))
        (lambda (g)
          (let ((γ (perm-evaluator g)))
            ;; N.B.: We can't iterate through the orbit. We must
            ;; iterate through the frame! Otherwise we will have
            ;; incorrect computations of π.
            (loop :with coord := (make-array (block-subsystem-size subsys) :initial-element nil)
                  :for i :from 0
                  :for b :across frame
                  :for γ.b := (mapcar γ b)
                  :for slot := (position-if (lambda (r) (find (list-minimum γ.b) r)) frame)
                  :for ρ := (elt frame slot)
                  :for π := (permuter ρ γ.b)
                  :do (assert (group-element-p π group))
                      (setf (aref coord i) (funcall rank π))
                  :finally (return coord))))))))

;;; Coordinates

(defstruct (block-coordinate-transformation (:conc-name coord.)
                                            (:constructor %make-coord))
  order
  rank                                  ; GROUP -> INDEX
  )

(defun make-subsystem-transformations (subsys)
  (let* ((size             (block-subsystem-size subsys))
         (interblock-group (block-subsystem-interblock-group subsys))
         (interblock-hom   (block-subsystem-interblock-group-homomorphism subsys))
         (intrablock-group (block-subsystem-intrablock-group subsys))
         (intrablock-base  (group-order intrablock-group))
         (intrablock-spec  (make-radix-spec intrablock-base size))
         (intrablock-coord (intrablock-coordinate-function subsys)))
    (multiple-value-bind (inter-rank inter-unrank)
        (group-element-rank-functions interblock-group)
      (declare (ignore inter-unrank))
      (values
       ;; Interblock xform
       (%make-coord
        :order (group-order interblock-group)
        :rank (lambda (g) (funcall inter-rank (funcall interblock-hom g))))
       ;; Intrablock xform
       (%make-coord
        :order (cardinality intrablock-spec)
        :rank (lambda (g)
                ;; We want to interpret this coordinate not as "the
                ;; block in position X undergoes a change in
                ;; orientation by Y", but rather "the block X
                ;; undergoes a change in orientation by Y". We do this
                ;; by putting all of the block orientation changes
                ;; back into place, using knowledge of the interblock
                ;; group we computed above.
                (let ((coord (funcall intrablock-coord g))
                      (block-perm (funcall interblock-hom g)))
                  (rank intrablock-spec (permute (perm-inverse block-perm) coord))))
        ;; :unrank (lambda (r) (unrank intrablock-spec r))
        )))))

;;;; EXAMPLES?

(defun verbose ()
  (let ((last-time nil))
    (lambda (control &rest args)
      (let* ((current-time (get-internal-real-time))
             (elapsed-time-ms (if (null last-time)
                                  0
                                  (round (* 1000 (- current-time last-time))
                                         internal-time-units-per-second))))
        (format t "[~6D ms] " elapsed-time-ms)
        (apply #'format t control args)
        (terpri)
        (finish-output)
        (setf last-time (get-internal-real-time))
        nil))))

(defun subsystem-solver (group &key (verbose (verbose)))
  (let (subsystems xforms tables (num-gens (length (generators group))))
    (when verbose (funcall verbose "Computing subsystems"))
    (setf subsystems (group-block-subsystems group))

    (when verbose (funcall verbose "Computing coordinate transforms"))
    (setf xforms (loop :for subsys :in subsystems
                       :append (multiple-value-list (make-subsystem-transformations subsys))))
    (setf xforms (remove-if (lambda (x) (= 1 (coord.order x))) xforms))

    (when verbose (funcall verbose "Computing ~D God tables" (length xforms)))
    (loop :for i :from 1
          :for xform :in xforms
          :for order := (coord.order xform)
          :do
             (when (and verbose (> order 10000000))
               (unless (y-or-n-p "Attempting to compute table over 10M. Continue?")
                 (return-from subsystem-solver nil)))
             (when verbose (funcall verbose "Computing ~:R God table of size ~D" i order))
             (push (compute-god-table group :rank-cardinality order :rank-element (coord.rank xform)) tables)
             (when verbose (funcall verbose "Done!")))
    (setf tables (reverse tables))

    (when verbose (funcall verbose "Computing IDDFS closure"))

    (labels ((coord (g)
               (loop :with c := (make-array (length tables) :element-type 'fixnum :initial-element 0)
                     :for i :from 0
                     :for xform :in xforms
                     :do (setf (aref c i) (funcall (coord.rank xform) g))
                     :finally (return c)))
             (transition-coord (coord gen-idx)
               (map 'simple-vector
                    (lambda (c tbl)
                      (aref (god-table-entry-transition
                             (aref (god-table-vector tbl) c))
                            gen-idx))
                    coord
                    tables))
             (dfs (coord depth moves)
               (cond
                 ;; Solved.
                 ((every #'zerop coord)
                  (values t moves))
                 ;; The solution must be of at least the depths specified by each
                 ;; pruning table.
                 ((or (zerop depth)
                      (loop :for tbl :in tables
                            :for idx :from 0
                            :for c :across coord
                            :for entry := (aref (god-table-vector tbl) c)
                            :do (when (null entry)
                                  (error "Couldn't compute depth for coord[~d]=~d. ~
                                          Coord xform is ~S"
                                         idx
                                         c
                                         (nth idx xforms)))
                            :thereis (< depth (god-table-entry-depth entry))))
                  (values nil nil))
                 ;; Descend. We use transition tables here to actually
                 ;; compute the map.
                 (t
                  (dotimes (i num-gens nil)
                    (let ((next-coord (transition-coord coord i)))
                      ;; coordᵢ ← (aref transᵢ coordᵢ mv)
                      (multiple-value-bind (found? solution)
                          (dfs next-coord (1- depth) (cons i moves))
                        (when found?
                          (return-from dfs (values t solution))))))))))
      (lambda (g)
        (block nil
          (loop :with coord := (coord g)
                :for depth :from 0
                :do (multiple-value-bind (found? solution)
                        (dfs coord depth nil)
                      (cond
                        (found? (return (reverse solution)))
                        (t      (write-string "."))))))))))

(defun solve-and-verify (group solver element &key (move-printer #'princ))
  (format t "~&Position: ~A~%" element)
  (let* ((start-time (get-internal-real-time))
         (solution (funcall solver element)))
    (format t "~&Solution found in ~D ms!~%"
            (round (* 1000 (- (get-internal-real-time) start-time))
                   internal-time-units-per-second))
    (let ((*print-pretty* nil))
      (format t "Solution (apply left-to-right): ")
      (dolist (mv solution)
        (funcall move-printer mv)
        (write-char #\Space))
      (terpri))
    (let ((solution*element (reduce #'perm-compose (reverse solution)
                                    :initial-value element
                                    :key (lambda (n) (nth n (generators group))))))


      (format t "Verification: ~:[Failed! solution*element=~A~;Success!~]"
              (perm-identity-p solution*element)
              solution*element)
      solution)))

;;; Solving second phase of Kociemba:
;;;
;;; (defparameter *g2 (perm-examples::make-kociemba-g2))
;;;
;;; (defparameter *solver (subsystem-solver *g2))
;;;
;;; (let ((r (random-group-element *g2))
;;;             (printer (lambda (i) (format t "~[U~;U2~;U'~;D~;D2~;D'~;R2~;L2~;F2~;B2~]" i))))
;;;         (solve-and-verify *g2 *solver r :move-printer printer))
