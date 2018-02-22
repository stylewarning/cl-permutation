;;;; orbit.lisp
;;;;
;;;; Copyright (c) 2015-2016 Robert Smith

(in-package #:cl-permutation)

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
        ;; Even if we have seen I in an orbit before, we still have to
        ;; see how *all* the generators act on I. So we can't skip it.
        (clear-membership-set orbit-membership)
        ;; Compute the orbit of the element across all generators.
        (dolist (g (generators group))
          (map-orbit (lambda (k) (setf (sbit orbit-membership k) 1))
                     i
                     g))
        ;; Incorporate that orbit anywhere it has intersected.
        (multiple-value-bind (intersecting non-intersecting)
            (partition-if (lambda (set)
                            (membership-sets-intersect-p
                             orbit-membership
                             set))
                          orbit-memberships)
          (cond
            ((endp intersecting)
             ;; Add the newly found orbit.
             (push (copy-seq orbit-membership) orbit-memberships))
            ((endp (rest intersecting)) ; 1 element
             ;; Modify the single orbit this intersects with.
             (membership-set-nunion (first intersecting) orbit-membership))
            (t
             ;; Multiple orbits intersect, so we need to coalesce
             ;; them all. First, we nunion into our fresh orbit. (I
             ;; promise I am not a shill for Orbit® Gum.)
             (dolist (orb intersecting)
               (membership-set-nunion orbit-membership orb))
             ;; Now tack this on to our set of non-intersecting
             ;; orbits, and save them. We are wasting space by
             ;; copying, but oh well. Who's counting anyway?
             (setf orbit-memberships (cons
                                      (copy-seq orbit-membership)
                                      non-intersecting))))))
      ;; Return the orbits.
      (mapcar #'membership-set-to-orbit orbit-memberships))))

(defun orbit-group-homomorphism (original-group orbit)
  "Compute a homomorphism between elements of the permutation group ORIGINAL-GROUP to the naturally induced group of an orbit ORBIT of ORIGINAL-GROUP."
  (let* ((len (length orbit))
         (element-map (make-array (1+ (group-degree original-group)))))
    ;; Compute a map between points in the original group and points
    ;; in the resulting orbit group. This is used to construct the
    ;; homomorphism.
    (loop :for i :from 1
          :for x :across orbit
          :do (setf (aref element-map x) i))
    ;; Create an actual homomorphism function.
    (labels ((homomorphism (g)
               (let ((result (allocate-perm-vector len)))
                 (loop :for i :from 1 :to len
                       :for x :across orbit
                       :do (setf (aref result i)
                                 (aref element-map (perm-eval g x)))
                       :finally (return (%make-perm :rep result))))))
      #'homomorphism)))

;;; XXX: We may want to store the group was derived from and the
;;; function from the factor group to the original group.
(defun group-from-orbit (original-group orbit)
  "Produce a group by having the group ORIGINAL-GROUP act on the orbit ORBIT of that group.

As a second value, the homomorphism will be returned."
  (let* ((hom (orbit-group-homomorphism original-group orbit))
         (induced-group (homomorphism-induced-perm-group original-group hom)))
    (values
     induced-group
     (make-instance 'function-homomorphism :from-group original-group
                                           :to-group induced-group
                                           :function hom))))

(defun subdirect-factors (group)
  "Compute \"subdirect factors\" of the group GROUP.

 These are groups whose direct product has GROUP as a subgroup.

As a second value, return the corresponding list of homomorphisms between GROUP and the subdirect factors."
  (iter:iter
    (iter:for o :in (group-orbits group))
    (iter:for (values g hom) := (group-from-orbit group o))
    (iter:collect g :into groups)
    (iter:collect hom :into homs)
    (iter:finally (return (values groups homs)))))
