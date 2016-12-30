;;;; do-group-elements.lisp
;;;;
;;;; Copyright (c) 2014 Robert Smith

(in-package #:cl-permutation)

;;;; Here we define a macro to iterate over all elements of a group in
;;;; some order. See reference [2] for details.

(defun group-radix (group)
  "Compute the radix of the group GROUP."
  (map 'simple-vector #'length (perm-group.transversal-system group)))

;;; TODO: Make this cons less.
(defun group-element-from-signature (group signature)
  ;; SIGNATURE is the output of ranking a MIXED-RADIX-SPEC, and has
  ;; elements between 0 and the position's radix. Since zero
  ;; corresponds to identity, we can skip them. When it is non-zero,
  ;; we subtract one, since the identity element is guaranteed to be
  ;; sigma_kk, which we don't want.
  (let ((perms (loop :for sigma_k :across (perm-group.transversal-system group)
                     :for n :across signature
                     :collect (nth-value 1 (hash-table-elt trans n)))))
    (reduce #'perm-compose
            perms
            :key (let ((id (perm-identity (maximum perms :key #'perm-size))))
                   (lambda (s)
                     (perm-compose id s))))))

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
