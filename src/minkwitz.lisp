;;;; minkwitz.lisp
;;;;
;;;; Copyright (c) 2019 Robert Smith

(in-package #:cl-permutation)

;;; This file contains an implementation of Minkwitz's algorithm.

;;;;;;;;;;;;;;;;;; Improved Generator Decomposition ;;;;;;;;;;;;;;;;;;

(defun minkwitz-table-quality (ν)
  (loop :for νᵢ :across ν
        :sum (loop :for img :across νᵢ
                   :maximize (word-length (car img)))))

(defun car* (x)
  (check-type x cons)
  (car x))

(defun stabilizer-orbits (group)
  "Compute a list of orbits corresponding to the BSGS of GROUP. In particular, calculate { i ↦ G⁽ⁱ⁾bᵢ₊₁ }."
  (multiple-value-bind (base sgs) (group-bsgs group)
    (loop :for b :in base
          :for s :in sgs
          :collect (loop :with orbit := nil
                         :for g :in s
                         :collect (map-orbit (lambda (p) (pushnew p orbit)) b g)
                         :finally (return (sort orbit #'<))))))

;; TODO FIXME: This isn't fully working or implemented.
(defun %compute-factorization-generators (group &key improve-every
                                                     (length-limit 2)
                                                     (min-rounds 0)
                                                     (growth-factor 5/4))
  (declare (optimize (speed 0) safety debug))

  ;; Deviations from Minkwitz's paper:
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
  (check-type group         perm-group)
  (check-type improve-every (or null integer))
  (check-type min-rounds    unsigned-byte)
  (check-type length-limit  (real (0) *))
  (check-type growth-factor (real 1 *))
  (uiop:nest
   (let* ((deg (group-degree group))
          (base (group-bsgs group))
          (k (length base))
          (free-group (perm-group.free-group group))
          (ϕ (free-group->perm-group-homomorphism free-group group))
          (next (word-generator free-group))
          (ν nil)))
   (labels ((verify-table (where)
              (loop :for i :from 1 :to k
                    :for bᵢ :in base
                    :for νᵢ :across ν
                    :append (loop :for ω :from 1 :to deg
                                  :for νᵢω :across νᵢ
                                  :when (and (not (null νᵢω))
                                             (/= bᵢ (perm-eval (funcall ϕ (car* νᵢω)) ω)))
                                    :do (break "at ~S: ν~D[ω=~D] /= ~D" where i ω bᵢ))))
            (%step (i tt)
              ;;
              ;; In the original paper, 'r' is a pass-by-reference. We
              ;; simply return it here.
              (check-type i unsigned-byte)
              (assert (free-group-element-valid-p free-group tt))
              (assert (<= 1 i k))
              ;; tt is a free group word
              (let* ((νᵢ (aref ν (1- i)))
                     (bᵢ (elt base (1- i)))
                     (ω (1- (perm-eval (funcall ϕ tt) bᵢ)))
                     (tt⁻¹ (inverse free-group tt)))
                (symbol-macrolet ((b′ (aref νᵢ ω)))
                  (cond
                    ((not (null b′))
                     (prog1 (compose free-group (car* b′) tt) ;; original paper sez t*b′
                       (when (< (word-length tt) (word-length (car* b′)))
                         (setf b′ (cons tt⁻¹ t))
                         (%step i tt⁻¹))))
                    (t
                     (setf b′ (cons tt⁻¹ t))
                     (%step i tt⁻¹)
                     (identity-element free-group))))))

            (%round (length-limit c tt)
              ;;
              ;; This function is supposed to receive tt by
              ;; reference. Not sure why...
              (check-type c unsigned-byte)
              ;; TT is a free-group word.
              (assert (<= 1 c k))
              (loop :for i :from c :to k
                    :while (and (not (free-group-identity-p tt))
                                (< (word-length tt) length-limit))
                    :do (setf tt (%step i tt))))

            (%improve (length-limit)
              (dotimes (j k)
                (loop :for x :across (aref ν j) :do
                  (when x
                    (loop :for y :across (aref ν j) :do
                      (when y
                        (when (or (cdr x) (cdr y))
                          ;; 1+j because it refers to the j'th stabilizer G⁽ʲ⁾
                          (%round length-limit (1+ j) (compose free-group (car* y) (car* x))))))))
                ;; Mark all of vⱼ as old.
                (loop :for x :across (aref ν j)
                      :when x
                        :do (rplacd x nil))))

            (%fill-orbits (length-limit)
              (loop :for i :below k
                    :for νᵢ :across ν
                    :for bᵢ :in base
                    :do (let ((orb nil))
                          (loop :for y :across νᵢ
                                :when y
                                  :do (pushnew (perm-eval (funcall ϕ (car* y)) bᵢ) orb))
                          (assert (every (lambda (x)
                                           (not (null (aref νᵢ (1- x)))))
                                         orb)
                                  ()
                                  "Inconsistency in table.")
                          (loop :for j :from (1+ i) :below k :do
                            (loop :for x :across (aref ν j)
                                  :when x
                                    :do (let* ((x (car* x))
                                               (x⁻¹ (inverse free-group x))
                                               ;; (ϕx⁻¹ (funcall ϕ x⁻¹))
                                               (ϕx (funcall ϕ x))
                                               (ϕx⁻¹ (perm-inverse ϕx))
                                               (orb-x (mapcar (perm-evaluator ϕx) orb)))
                                          #+ignore
                                          (progn
                                            (format t "Orbit = ~A~%" orb)
                                            (format t "X = ~A~%" x)
                                            (format t "ϕX = ~A~%" ϕx)
                                            (dolist (o orb)
                                              (let* ((to (perm-eval ϕx o))
                                                     (from (perm-eval ϕx⁻¹ to)))
                                                (format t "    ϕX(~D) ↦ ~D -> ~D~%"
                                                        o
                                                        to
                                                        from)
                                                (finish-output)
                                                (when (/= o from)
                                                  (break "FOund a BAD PERM")))))
                                          (loop :for p :in (set-difference orb-x orb)
                                                :for orig-orb-pt := (perm-eval ϕx⁻¹ p)
                                                :for tt := (compose free-group
                                                                    (car* (aref νᵢ (1- orig-orb-pt)))
                                                                    x⁻¹
                                                                    )
                                                :when (< (word-length tt) length-limit)
                                                  :do (setf (aref νᵢ (1- p)) (cons tt t)))))))))

            (%table-fullp (ν)
              (let ((size (%table-size ν))
                    (order (group-order group)))
                (assert (<= size order))
                (= size order)))

            (%table-size (ν)
              ;; How many group elements does ν represent?
              (loop :with p := 1
                    :for νᵢ :across ν
                    :do (setf p (* p (count-if-not #'null νᵢ)))
                    :finally (return p)))

            (%make-table ()
              ;; Make a fresh table.
              (let ((ν (make-array k)))
                (dotimes (i k)
                  (setf (aref ν i) (make-array deg :initial-element nil)))
                (loop :with id := (identity-element free-group)
                      :for b :in base
                      :for νᵢ :across ν :do
                        ;; (CONS 0 NIL) ==> CAR is the item being
                        ;; stored, CDR is whether it was newly
                        ;; generated by the algorithm.
                        (setf (aref νᵢ (1- b)) (cons id nil)))
                ν))))
   (progn
     ;; Minkwitz suggests that IMPROVE-EVERY is somewhat difficult to
     ;; choose, but K² works well in practice.
     (when (null improve-every) (setf improve-every (* k k)))

     ;; The code starting here reflects Minkwitz's SGSWordQuick
     ;; procedure.
     ;;
     ;; Set all νᵢ to be undefined, except νᵢ(bᵢ) = identity
     (setf ν (%make-table))
     (loop :for count :from 0
           :until (and (%table-fullp ν) (<= min-rounds count))
           :do (let ((tt (funcall next count)))
                 (%round length-limit 1 tt)
                 (verify-table "round")
                 (when (zerop (mod count improve-every))
                   (when *perm-group-verbose*
                     (format t "~D: p[~3,1F] @ l=~A: ~A~%   ~{~D~^ ~}~%"
                             count
                             (* 100 (/ (%table-size ν) (group-order group)))
                             (round length-limit)
                             tt
                             (map 'list (lambda (νᵢ)
                                          (count-if-not #'null νᵢ))
                                  ν)))
                   (%improve length-limit)
                   (verify-table "improve")
                   (unless (%table-fullp ν)
                     (%fill-orbits length-limit)
                     (verify-table "fill")
                     (setf length-limit (* growth-factor length-limit))
                     ))))
     (when *perm-group-verbose*
       (format t "Table quality: ~A~%" (minkwitz-table-quality ν)))
     (verify-table "end")
     ;; Return the table, cleaning the usage flags out.
     (loop :for νᵢ :across ν
           :do (map-into νᵢ #'car νᵢ))
     ν)))

(defun compute-factorization-generators (group)
  "Modify the permutation group PERM-GROUP so that it is capable of factorizing elements into generators."
  (unless (perm-group.factorization-generators group)
    (setf (perm-group.factorization-generators group)
          (%compute-factorization-generators group)))
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
         (ν (perm-group.factorization-generators group)))
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
                         (ωᵢ (perm-eval π bᵢ))
                         (νᵢωᵢ (aref νᵢ (1- ωᵢ)))
                         (νᵢπ (perm-compose (funcall ϕ νᵢωᵢ) π)))
                    (walk-stabilizer (1+ i) νᵢπ (rest base-left) (cons νᵢωᵢ factorization)))))))
      (let ((decomp (walk-stabilizer 0
                                     g
                                     (group-bsgs group)
                                     nil)))
        (if (not return-original-generators)
            decomp
            (mapcar ϕ decomp))))))
