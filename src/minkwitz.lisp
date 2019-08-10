;;;; minkwitz.lisp
;;;;
;;;; Copyright (c) 2019 Robert Smith

(in-package #:cl-permutation)

;;; This file contains an implementation of Minkwitz's algorithm.

(defstruct (entry (:constructor entry (seen word))
                  (:copier nil)
                  (:predicate nil))
  "Representation of an entry in a Minkwitz table."
  ;; Has the algorithm seen this entry?
  (seen t   :type boolean)
  ;; The value of an entry, which is a free group word.
  (word nil :read-only t))
#+sbcl (declaim (sb-ext:freeze-type entry))

(declaim (inline new-entry mark-entry-as-seen))
(defun new-entry (x)
  (entry nil x))
(defun mark-entry-as-seen (x)
  (setf (entry-seen x) t))

(defclass minkwitz-table ()
  ((group :initarg :group
          :reader minkwitz-table-group
          :documentation "The group for which this table is valid.")
   (system :initarg :system
           :reader minkwitz-table-system
           :documentation "The transversal system storing strong generators as short words.

Specifically, this is a SIMPLE-VECTOR of HASH-TABLEs. The Nth element of this vector corresponds to the Nth stabilizer group of GROUP, ordered according to the group's base. The table is a partial map ω ↦ π such that π(ω) = b, where b is the base point.")
   (num-rounds :initarg :num-rounds
               :reader minkwitz-table-num-rounds
               :documentation "The total number of words searched to get this table.")
   (simplifier :initarg :simplifier
               :reader minkwitz-table-simplifier
               :documentation "The simplification function that was used during the generation of the table. (See GENERATE-MINKWITZ-TABLE.)")
   (word-generator :initarg :word-generator
                   :reader minkwitz-table-word-generator
                   :documentation "The word-generating function that was used during the creation of this table. (See GENERATE-MINKWITZ-TABLE.)"))
  (:documentation "Data used to decompose elements of a group into generators (and inverses)."))

(defmethod print-object ((obj minkwitz-table) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~D gens; max=~D"
            (num-generators (minkwitz-table-group obj))
            (upper-bound-decomposition-length obj))))

;;; This roughly what Minkwitz calls the "table quality". "Roughly"
;;; because he counts the identity element as having length 1.
(defun upper-bound-decomposition-length (m)
  "What is the maximum length of a decomposition using the Minkwitz table M?"
  (loop :for νᵢ :across (minkwitz-table-system m)
        :sum (loop :for img :being :the :hash-values :of νᵢ
                   :if (free-group-identity-p (entry-word img))
                     :maximize 0
                   :else
                     :maximize (word-length (entry-word img)))))

(defun minkwitz-table-distribution (m)
  "what is the distribution of word lengths for a given Minkwitz table M? Return a vector V such that (AREF V L) gives the proportion of decompositions that are length L."
  (let* ((max (upper-bound-decomposition-length m))
         (ν (minkwitz-table-system m))
         (size (expt 2 (ceiling (log (1+ max) 2.0)))))
    (flet ((make-distribution (τ)
             (loop :with d := (make-array size :initial-element 0.0d0)
                   :for w :being :the :hash-values :of τ
                   :do (incf (aref d (length (entry-word w))))
                   :finally (return (map-into d (lambda (x)
                                                  (/ x (hash-table-count τ)))
                                              d))))
           (combine-freqs (a b)
             (map 'vector (lambda (x y) (* x y)) a b)))
      (let* ((dx (loop :for τ :across ν
                       :when (< 1 (hash-table-count τ))
                         :collect (make-distribution τ)))
             (δ (bordeaux-fft:sifft (reduce #'combine-freqs dx :key #'bordeaux-fft:sfft))))
        (map 'simple-vector #'realpart δ)))))

(defun minkwitz-table-num-words (m)
  "How many words are represented by the Minkwitz table M?"
  (%system-size (minkwitz-table-system m)))

(defun %system-size (ν)
  (loop :with p := 1
        :for νᵢ :across ν
        :do (setf p (* p (hash-table-count νᵢ)))
        :finally (return p)))

(defun stabilizer-orbits (group)
  "Compute a list of orbits corresponding to the BSGS of GROUP. In particular, calculate { i ↦ G⁽ⁱ⁾bᵢ₊₁ }."
  (multiple-value-bind (base sgs) (group-bsgs group)
    (loop :for b :in base
          :for s :in sgs
          :collect (loop :with orbit := nil
                         :for g :in s
                         :collect (map-orbit (lambda (p) (pushnew p orbit)) b g)
                         :finally (return (sort orbit #'<))))))

(declaim (ftype (function (t t) simple-vector) %make-system))
(defun %make-system (base orbit-lengths)
  (let* ((k (length base))
         (ν (make-array k)))
    (loop :for i :below k
          :for bᵢ :across base
          :for o :across orbit-lengths
          ;; The table maps points to words.
          :for table := (make-hash-table :test 'eql :size o)
          :do (setf (aref ν i) table)
              ;; We do *NOT* fill the table with sentinel values
              ;; because we use the *existence* of the entries as
              ;; indicators of completeness of filling the table.
              (setf (gethash bᵢ table) (entry t nil)))
    ν))

;;; TODO: Allow re-entrant calling to improve lengths.
(defun generate-minkwitz-table (group &key (min-rounds 0)
                                           improve-every
                                           (improve-max-tries most-positive-fixnum)
                                           (initial-length-limit 10.0d0)
                                           (growth-factor 5/4)
                                           (simplifier (word-simplifier-for-perm-group group) simplifier-provided-p)
                                           (word-generator nil word-generator-provided-p))
  "Generate a MINKWITZ-TABLE suitable for decomposing elements of GROUP into its generators (+ inverses).

The arguments are:

    GROUP: A permutation group.

    MIN-ROUNDS: The *minimum* number of words to search. (Setting this number larger implies more time will be spent searching for short generators.)

    IMPROVE-EVERY: A positive fixnum indicating the number of words that should be searched for before an improvement step occurs. (\"Improvement\" can be quite time-consuming for groups whose stabilizers have a large number of generators—roughly quadratic. On the other hand, improving too infrequently may cause the table to take a long time to fill.)

    IMPROVE-MAX-TRIES (optional): The maximum number of new element pairs should be used to improve the table during improvement. (Lowering this number to say 10000 might make table generation faster at the expense of shorter words.)

    INITIAL-LENGTH-LIMIT: The algorithm prunes sequences which are too long (especially after composition). What should be the initial limit of the length be? (Setting this too low means that the table will take longer to fill. Setting it too high means that longer words will be stored earlier. It is a little counterintuitive, but sometimes storing longer words earlier can lead to shorter lengths overall.)

    GROWTH-FACTOR: After every IMPROVE-EVERY rounds, by what factor should the length limit be grown? (If this number is too small, then the table will take longer to fill up, but it will more aggressively search for shorter words. If this number is too large, the length limit is meaningless.)

    SIMPLIFIER: A function mapping words to words. Should simplify the words. (IDENTITY is a valid simplifier.) (Default is a group-aware word simplifier taking into account generator orders and commutation relations.)

    WORD-GENERATOR: A function taking a count and a length and produces a word. (These arguments need not be respected in any way.) By default, this will be set to a word generator that scans through all words in a breadth-first order. (Note that the simplifier will *not* be used automatically, unless the default generator is used.)
"
  (declare (optimize (speed 0) safety debug))
  ;;(declare (optimize speed (safety 0) (debug 0)))
  (declare (type (function (*) *) simplifier))

  ;; Deviations from Minkwitz's paper, and other implementation notes:
  ;;
  ;;     * We rename 'n' to MIN-ROUNDS, which dictates the *minimum*
  ;;       number of rounds to do. (A value of 0 indicates the
  ;;       algorithm should terminate immediately upon filling the
  ;;       table.)
  ;;
  ;;     * We rename 's' to IMPROVE-EVERY.
  ;;
  ;;     * We rename 'l' to LENGTH-LIMIT. Minkwitz gives no suggestion
  ;;       on what LENGTH-LIMIT ought to be, except that L should
  ;;       start small.
  ;;
  ;;     * We factor out GROWTH-FACTOR. 5/4 is no silver
  ;;       bullet. Minkwitz just says 'l' should grow slowly, and he
  ;;       uses 5/4 in his pseudocode.
  ;;
  ;;     * We add SIMPLIFIER, a function that allows words to be
  ;;       simplified. IDENTITY is perfectly reasonable as a simplifier.
  ;;
  ;;     * We relax the LENGTH-LIMIT condition so that the current
  ;;       word (as produced by NEXT) is always valid as a
  ;;       length. This is because we produce words in order of
  ;;       increasing length.
  ;;
  ;;     * We simplify the algorithm to not spend time in empty
  ;;       transversals.
  ;;
  ;;     * WORD-GENERATOR is in charge of generating words. It takes
  ;;       as arguments the count (starting from 0) and the current
  ;;       length limit as arguments. (They may be freely ignored.) If
  ;;       you supply a word generator, the simplifier will *not* be
  ;;       used automatically.
  ;;
  ;;     * IMPROVE-MAX-TRIES puts a bound on the number of element
  ;;       pairs which are put through for improvement.

  (check-type group                perm-group)
  (check-type improve-every        (or null (and fixnum (integer 1))))
  (check-type min-rounds           (and fixnum unsigned-byte))
  (check-type initial-length-limit (real (0) *))
  (check-type growth-factor        (real 1 *))
  (check-type word-generator       (or null symbol function))
  (check-type improve-max-tries    (and fixnum unsigned-byte))

  (uiop:nest
   ;; Ensure LENGTH-LIMIT is a double float so it doesn't grow out of
   ;; control as a rational.
   (let* ((length-limit (coerce initial-length-limit 'double-float))
          (order (group-order group))
          (base (coerce (group-bsgs group) 'simple-vector))
          (k (length base))
          (free-group (perm-group.free-group group))
          (ϕ (free-group->perm-group-homomorphism free-group group))
          (next (or word-generator
                    (let ((gen (word-generator free-group)))
                      (lambda (c l)
                        (declare (ignore l))
                        (funcall simplifier (funcall gen c))))))
          (orbit-lengths (nreverse
                          (map 'simple-vector #'length (perm-group.transversal-system group))))
          ;; If an orbit only has 1 element in it, then it's not worth
          ;; exploring. This vector should be accessed with
          ;; EXPLORE-INDEX?.
          (explore-vec (map 'simple-bit-vector
                            (lambda (x) (if (= 1 x) 0 1))
                            orbit-lengths))
          ;; Set all νᵢ to be undefined, except νᵢ(bᵢ) = identity
          (ν (%make-system base orbit-lengths))
          ;; Minkwitz suggests that IMPROVE-EVERY is somewhat difficult to
          ;; choose, but k² works well in practice.
          ;;
          ;; I find k² to be awful if the size of νᵢ is too
          ;; large. It's better to just select a lower k in those
          ;; cases.
          (improve-every (or improve-every (* k k)))
          ;; The total number of rounds that were run. This will be
          ;; set after the table is built.
          (num-rounds nil))
     (declare (type double-float length-limit)
              (type (and fixnum unsigned-byte) k improve-every)
              (type (function (*) *) ϕ)
              (type simple-vector ν)
              (type simple-vector base)))

   ;;; Utility functions.
   (flet ((explore-index? (i)
            ;; Should we explore elements in νᵢ?
            (= 1 (sbit explore-vec i)))
          (transversal-filled? (i)
            ;; Is the i'th transversal filled?
            (= (svref orbit-lengths i) (hash-table-count (aref ν i))))
          (check-table (where)
            ;; This can be used for debugging. If the table is
            ;; inconsistent, this may trigger it.
            (declare (optimize (speed 0) safety debug))
            (loop :for i :from 1 :to k
                  :for bᵢ :across base
                  :for νᵢ :across ν
                  :append (loop :for ω :being :the :hash-keys :of νᵢ
                                  :using (hash-value νᵢω)
                                :when (/= bᵢ (unsafe/perm-eval (funcall ϕ (entry-word νᵢω)) ω))
                                  :do (error "Table inconsistency at ~S: ν~D[ω=~D] /= ~D" where i ω bᵢ)))))
     (declare (inline explore-index?)))

   ;;; Main logic.
   (labels ((%step (i tt)
              ;; i is a *point*
              ;; tt is a word
              (declare (type perm-element i))
              ;; In the original paper, 'r' is a pass-by-reference. We
              ;; simply return it here.
              (setf tt (funcall simplifier tt))
              (let* ((νᵢ (aref ν (1- i)))
                     (bᵢ (svref base (1- i)))
                     (ω  (unsafe/perm-eval (funcall ϕ tt) bᵢ))
                     (tt⁻¹ (inverse free-group tt)))
                (declare (type hash-table νᵢ)
                         (type perm-element bᵢ ω))
                (symbol-macrolet ((b′ (gethash ω νᵢ)))
                  (cond
                    ((not (null b′))
                     (prog1 (values (compose free-group (entry-word b′) tt)) ;; original paper sez t*b′
                       (when (< (word-length tt) (word-length (entry-word b′)))
                         (setf b′ (new-entry tt⁻¹))
                         (%step i tt⁻¹))))
                    (t
                     (setf b′ (new-entry tt⁻¹))
                     (%step i tt⁻¹)
                     (values (identity-element free-group)))))))

            (%round (length-limit c tt)
              (declare (type double-float length-limit)
                       (type perm-element c))
              ;; This function is supposed to receive tt by reference,
              ;; in the original paper. Not sure why...
              (setf tt (funcall simplifier tt))
              (loop :for i :from c :to k
                    :while (and (not (free-group-identity-p tt))
                                (< (word-length tt) length-limit))
                    :do (when (explore-index? (1- i))
                          (setf tt (%step i tt)))))

            (%improve (length-limit)
              (declare (type double-float length-limit))
              (let ((rounds-left improve-max-tries))
                (dotimes (j k)
                  (when (> 1 (hash-table-count (aref ν j)))
                    ;; XXX: Do we *need* to explore *every* pair in the
                    ;; cartesian product?
                    (loop :named outer :for x :of-type entry :being :the :hash-values :of (aref ν j) :do
                      (unless (entry-seen x)
                        (loop :for y :of-type entry :being :the :hash-values :of (aref ν j) :do
                          ;; 1+j because it refers to the j'th stabilizer G⁽ʲ⁾
                          (%round length-limit (1+ j) (compose free-group (entry-word y) (entry-word x)))
                          (%round length-limit (1+ j) (compose free-group (entry-word x) (entry-word y)))
                          (unless (plusp (decf rounds-left))
                            (return-from outer)))))
                    ;; Mark all of vⱼ as seen.
                    (loop :for x :of-type entry :being :the :hash-values :of (aref ν j)
                          :do (mark-entry-as-seen x))))))

            (%fill-orbits (length-limit)
              (declare (type double-float length-limit))
              (loop :for i :below k
                    :for νᵢ :of-type hash-table :across ν
                    :for bᵢ :across base
                    :do (unless (transversal-filled? i)
                          (let ((orbit nil))
                            (loop :for y :being :the :hash-values :of νᵢ
                                  :do (pushnew (unsafe/perm-eval (funcall ϕ (entry-word y)) bᵢ) orbit))
                            ;; At this point, every point in the orbit
                            ;; should be a key in the hash table.
                            (loop :for j :from (1+ i) :below k :do
                              (loop :for x :of-type entry :being :the :hash-values :of (aref ν j)
                                    :do (let* ((x (entry-word x))
                                               (x⁻¹ (inverse free-group x))
                                               (ϕx (funcall ϕ x))
                                               (orb-x (mapcar (perm-evaluator ϕx) orbit)))
                                          (loop :for p :of-type perm-element :in (set-difference orb-x orbit)
                                                :for orb-pt :of-type perm-element := (perm-inverse-eval ϕx p)
                                                :for tt := (funcall
                                                            simplifier
                                                            (compose free-group
                                                                     (entry-word (gethash orb-pt νᵢ))
                                                                     x⁻¹))
                                                :when (< (word-length tt) length-limit)
                                                  :do (setf (gethash p νᵢ) (new-entry tt))))))))))

            (%table-fullp (ν)
              (let ((size (%system-size ν)))
                (assert (<= size order))
                (= size order)))))
   (progn
     ;; ...
     (when *perm-group-verbose*
       (let ((*print-pretty* nil))
         (fresh-line)
         (format t ";; Minkwitz Parameters:~%")
         (format t ";;     MIN-ROUNDS          : ~S~%" min-rounds)
         (format t ";;     IMPROVE-EVERY       : ~S~%" improve-every)
         (format t ";;     IMPROVE-MAX-TRIES   : ~S~%" improve-max-tries)
         (format t ";;     GROWTH-FACTOR       : ~S~%" growth-factor)
         (format t ";;     INITIAL-LENGTH-LIMIT: ~S~%" length-limit)
         (format t ";; A simplifier was ~:[not~;~] provided.~%" simplifier-provided-p)
         (format t ";; A word generator was ~:[not~;~] provided.~%" word-generator-provided-p)
         (format t ";; The group has ~D generator~:P and is of order ~D.~%" (num-generators group) order)
         (format t ";; The base is ~S.~%" base)
         (format t ";; The orbit lengths are ~S.~%" orbit-lengths)))
     ;; The code starting here reflects Minkwitz's SGSWordQuick
     ;; procedure, except for the construction of ν, which is above.
     (loop :with start-time := (get-internal-real-time)
           :for count :of-type fixnum :from 1 :below most-positive-fixnum
           :until (and (%table-fullp ν) (<= min-rounds count))
           :do (let* ((tt (funcall next (1- count) (floor length-limit)))
                      (current-length-limit
                        (max length-limit (coerce (1+ (word-length tt)) 'double-float))))
                 (%round current-length-limit 1 tt)
                 (when (zerop (mod count improve-every))
                   (when *perm-group-verbose*
                     (format t ";; ~D: Improving lengths...~%" count))
                   (%improve current-length-limit)
                   (unless (%table-fullp ν)
                     (when *perm-group-verbose*
                       (format t ";; ~D: Filling orbits...~%" count))
                     (%fill-orbits current-length-limit)
                     ;; Grow the length limit itself, not simply the
                     ;; current one for this round.
                     (setf length-limit (* growth-factor length-limit))))

                 ;; Some logging.
                 (when (and *perm-group-verbose*
                            (zerop (mod count (min 10000 improve-every))))
                   (format t ";; ~D: [~3,1F%] @ l=~A: ~A [~D ms]~%;;     "
                           count
                           (* 100 (/ (%system-size ν) (group-order group)))
                           (floor current-length-limit)
                           tt
                           (round (* 1000 (- (get-internal-real-time) start-time))
                                  internal-time-units-per-second))
                   (loop :with flag := nil
                         :for νᵢ :across ν
                         :for o :across orbit-lengths
                         :if (= 1 o)
                           :do (unless flag
                                 (format t " -")
                                 (setf flag t))
                         :else
                           :do (setf flag nil)
                               (format t " ~D" (hash-table-count νᵢ)))
                   (terpri)
                   (finish-output)
                   (setf start-time (get-internal-real-time))))
           :finally (setf num-rounds (1- count)))
     ;; Do a sanity check on the consistency of the table.
     (check-table "end")
     (make-instance 'minkwitz-table
                    :group group
                    :system ν
                    :num-rounds num-rounds
                    :simplifier simplifier
                    :word-generator next))))

(defun compute-factorization-generators (group)
  "Modify the permutation group PERM-GROUP so that it is capable of factorizing elements into generators."
  (unless (perm-group.factorization-generators group)
    (setf (perm-group.factorization-generators group)
          (generate-minkwitz-table group)))
  t)

(defun generator-decomposition (g group &key return-original-generators)
  "Given an element g ∈ GROUP, factorize it into a sequence of generators, represented as a list of elements in the homomorphic FREE-GROUP.

If RETURN-ORIGINAL-GENERATORS is true, return the group's original generators as permutations.

This is also called \"factorization\"."
  (check-type g perm)
  (check-type group perm-group)
  (compute-factorization-generators group)
  (let* ((fg (perm-group.free-group group))
         (ϕ (free-group->perm-group-homomorphism fg group))
         (ν (minkwitz-table-system
             (perm-group.factorization-generators group))))
    (labels ((walk-stabilizer (i π base-left factorization)
               (cond
                 ((null base-left)
                  (assert (perm-identity-p π)
                          ()
                          "Inconsistency in GENERATOR-DECOMPOSITION. Possibly an issue with the table?")
                  (inverse fg (remove-if #'free-group-identity-p
                                         (canonicalize-free-group-element
                                          fg
                                          factorization))))
                 (t
                  (let* ((νᵢ (aref ν i))
                         (bᵢ (first base-left))
                         (ωᵢ (unsafe/perm-eval π bᵢ))
                         (νᵢωᵢ (entry-word (gethash ωᵢ νᵢ)))
                         (νᵢπ (perm-compose (funcall ϕ νᵢωᵢ) π)))
                    (walk-stabilizer (1+ i) νᵢπ (rest base-left) (cons νᵢωᵢ factorization)))))))
      (let ((decomp (funcall
                     (minkwitz-table-simplifier
                      (perm-group.factorization-generators group))
                     (walk-stabilizer 0
                                      g
                                      (group-bsgs group)
                                      nil))))
        (if (not return-original-generators)
            decomp
            (mapcar ϕ decomp))))))
