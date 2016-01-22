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
