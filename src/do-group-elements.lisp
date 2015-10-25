;;;; do-group-elements.lisp
;;;;
;;;; Copyright (c) 2014 Robert Smith

(in-package #:cl-permutation)

;;;; Here we define a macro to iterate over all elements of a group in
;;;; some order.
;;;;
;;;; XXX FIXME: Everything in this file is extremely inefficient and
;;;; could be nearly trivially sped up.

;;; Taken from https://bitbucket.org/tarballs_are_good/lisp-random/src/37a571c75755ae8ccad79d46bd0c0e9ef77e72da/miscellaneous_exercises/password-variants.lisp?at=default#cl-35
(defun incrementer (signature)
  (labels ((propagate (radix sig carry accum)
             (cond
               ((or (null radix)
                    (null sig))
                (if (plusp carry)
                    (error "Radix overflow.")
                    (nreverse accum)))
               
               ;; No overflow...
               ((< (+ carry (car radix)) (car sig))
                (propagate (cdr radix)
                           (cdr sig)
                           0
                           (cons (+ carry (car radix))
                                 accum)))
               
               ;; Overflow... Propagate the carries forward.
               (t
                (propagate (cdr radix)
                           (cdr sig)
                           1
                           (cons 0 accum))))))
    (let ((num (make-list (length signature) :initial-element 0)))
      (lambda ()
        (prog1 num
          (setf num (ignore-errors (propagate num signature 1 nil))))))))

;;; Defined in reference [2].
(defun group-radix (group)
  (map 'list #'hash-table-count (perm-group.transversal-system group)))

(defun group-element-from-signature (group signature)
  (let* ((perms (mapcar (lambda (trans n)
                          (nth-value 1 (hash-table-elt trans n)))
                        (coerce (perm-group.transversal-system group) 'list)
                        signature))
         (id (perm-identity (maximum perms :key #'perm-size))))
    (reduce #'perm-compose
            (mapcar (lambda (s)
                      (perm-compose id s))
                    perms))))

(defmacro do-group-elements ((var group &optional return) &body body)
  "Iterate through all of the elements of the group GROUP, binding each element to VAR and executing BODY. Optionally return a value specified by RETURN."
  (let ((ggroup      (gensym "GROUP-"))
        (incrementer (gensym "INCREMENTER-"))
        (signature   (gensym "SIGNATURE-")))
    `(let* ((,ggroup ,group)
            (,incrementer (incrementer (group-radix ,ggroup))))
       (loop :for ,signature := (funcall ,incrementer) :then (funcall ,incrementer)
             :while ,signature
             :do (let ((,var (group-element-from-signature ,ggroup ,signature)))
                   ,@body)
             :finally (return ,return)))))
