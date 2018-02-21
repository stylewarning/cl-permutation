;;;; god.lisp
;;;;
;;;; Copyright (c) 2016 Robert Smith

(in-package #:cl-permutation)

(defclass god-table ()
  ((group :initarg :group
          :reader god-table-group)
   (table :initarg :table
          :reader god-table-vector)
   (target :initarg :target
           :reader god-table-target)
   (generators :initarg :generators
               :reader god-table-generators)))

;; TODO: Change to IDDFS?
(defun compute-god-table (group &key (target (group-identity group))
                                     (verbose t))
  (let ((generators (loop :for i :from 0
                          :for g :in (generators group)
                          :collect (cons i g)))
        ;; Table of (CONS MOVE CAME-FROM)
        (table (make-array (group-order group) :initial-element nil))
        (positions-left (make-queue)))
    (multiple-value-bind (rank-element unrank-element)
        (group-element-rank-functions group)
      (declare (ignore unrank-element))

      ;; Record TARGET as starting position.
      (enqueue positions-left target)
      (let ((target-rank (funcall rank-element target)))
        (setf (svref table target-rank) (cons -1 target-rank)))

      ;; Start iterating.
      (loop :for num-elements-explored :from 1
            :for next := (dequeue positions-left)
            :do
               (when (and verbose (zerop (mod num-elements-explored 50000)))
                 (format t "~D~%" (length (queue-elements positions-left))))
               (loop :for (i . g) :in generators
                     :for p := (perm-compose g next)
                     :for r := (funcall rank-element p)
                     :when (null (svref table r))
                       :do (enqueue positions-left p)
                           (setf (svref table r)
                                 (cons i (funcall rank-element next))))
            :until (queue-empty-p positions-left))

      ;; Return a GOD-TABLE object.
      (make-instance 'god-table :group group
                                :table table
                                :target target
                                :generators generators))))

(defgeneric reconstruct-perm (god-table perm)
  (:method ((table god-table) perm)
    (declare (optimize speed))
    (multiple-value-bind (rank-element unrank-element)
        (group-element-rank-functions (god-table-group table))
      (declare (ignore unrank-element)
               (type function rank-element))
      (labels ((chase (current collected)
                 (destructuring-bind (move . next)
                     (svref (god-table-vector table) current)
                   (declare (type integer move))
                   (if (= -1 move)
                       (nreverse collected)
                       (chase next (cons move collected))))))
        (chase (funcall rank-element perm) nil)))))

;;; Example:
;;; (defvar *god (compute-god-table (perm-examples:make-rubik-2x2)))
;;; (reconstruct-perm *god (random-group-element (god-table-group *god)))
