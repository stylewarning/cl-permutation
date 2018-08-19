;;;; do-group-elements.lisp
;;;;
;;;; Copyright (c) 2014 Robert Smith

(in-package #:cl-permutation)

;;;; Here we define a macro to iterate over all elements of a group in
;;;; some order. See reference [2] for details.

(defun group-radix (group)
  "Compute the radix of the group GROUP."
  (map 'simple-vector #'length (perm-group.transversal-system group)))

;;; Note, in the below ranking/unranking functions, everything is
;;; based on the transversal decomposition, and the *position* of the
;;; sigma_kj in the sigma_k list. This is almost surely not at
;;; position j!

(defun group-element-from-signature (group signature)
  ;; SIGNATURE is the output of ranking a MIXED-RADIX-SPEC, and has
  ;; elements between 0 and the position's radix. Since zero
  ;; corresponds to identity, we can skip them. When it is non-zero,
  ;; we subtract one, since the identity element is guaranteed to be
  ;; sigma_kk, which we don't want.
  (loop :with result := (group-identity group)
        :for sigma_k :across (perm-group.transversal-system group)
        :for n :across signature
        :unless (zerop n)
          :do (setf result (perm-compose (cdr (nth (1- n) sigma_k))
                                         result))
        :finally (return result)))

(defun group-element-rank-functions (group)
  "Generate two functions as values:

    1. A function to map elements of the permutation group GROUP to integers [0, 2^|GROUP| - 1].

    2. The inverse of the above function."
  ;; FIXME: We need to make sure this fails when we attempt to rank
  ;; something not in the group.
  (let ((spec (vector-to-mixed-radix-spec (group-radix group))))
    (flet ((rank-element (el)
             (let* ((trans (perm-group.transversal-system group))
                    (set (make-array (size spec))))
               ;; Get the SET data structure filled.
               (reduce-over-trans-decomposition
                (lambda (decomp k j)
                  (declare (ignore decomp))
                  ;; Identity is 0, everything else is 1 + position as
                  ;; found in the built-up transversal system.
                  (setf (svref set (1- k))
                        (if (= j k)
                            0
                            (1+ (position j (svref trans (1- k)) :key #'car))))
                  nil)
                nil
                el
                trans)

               ;; Finally, rank it.
               (rank spec set)))
           (unrank-element (idx)
             (group-element-from-signature group (unrank spec idx))))
      (values #'rank-element #'unrank-element))))

(defmacro do-group-elements ((var group &optional return) &body body)
  "Iterate through all of the elements of the group GROUP, binding each element to VAR and executing BODY. Optionally return a value specified by RETURN."
  (let ((ggroup    (gensym "GROUP-"))
        (spec      (gensym "SPEC-"))
        (signature (gensym "SIGNATURE-"))
        (rank      (gensym "RANK-")))
    `(let* ((,ggroup ,group)
            (,spec (vector-to-mixed-radix-spec (group-radix ,ggroup))))
       ;; Map across all elements of the group.
       (map-spec
        (lambda (,rank ,signature)
          (declare (ignore ,rank))
          (let ((,var (group-element-from-signature ,ggroup ,signature)))
            (tagbody
               ,@body)))
        ,spec)
       ;; Return the return value.
       ,return)))
