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

(defstruct god-table-entry
  move
  came-from
  depth
  transition)

(defun compute-god-table (group &key (target (group-identity group))
                                     (generators (generators group))
                                     (order (group-order group))
                                     (rank-element (group-element-rank-functions group))
                                     (verbose t))
  (let ((generators (loop :for i :from 0
                          :for g :in generators
                          :collect (cons i g)))
        ;; Table of (list MOVE CAME-FROM DEPTH)
        (table (make-array order :initial-element nil))
        (positions-left (make-queue))
        (make-trans (lambda () (make-array (length generators)))))
    ;; Record TARGET as starting position.
    (enqueue positions-left target)
    (let ((target-rank (funcall rank-element target)))
      (setf (svref table target-rank) (make-god-table-entry :move -1
                                                            :came-from target-rank
                                                            :depth 0
                                                            :transition (funcall make-trans))))

    ;; Start iterating.
    (loop :for num-elements-explored :from 1
          :for next := (dequeue positions-left)
          :for next-rank := (funcall rank-element next)
          :for next-entry := (svref table next-rank)
          :do
             (when (and verbose (zerop (mod num-elements-explored 50000)))
               (format t "~D~%" (length (queue-elements positions-left))))
             (loop :for (i . g) :in generators
                   :for p := (perm-compose g next)
                   :for r := (funcall rank-element p)
                   ;; Record the coordinate transition.
                   :do (setf (svref (god-table-entry-transition next-entry) i) r)
                   ;; Traverse BFS style.
                   :when (null (svref table r))
                     :do (let ((came-from (funcall rank-element next)))
                           (enqueue positions-left p)
                           (setf (svref table r)
                                 (make-god-table-entry
                                  :move i
                                  :came-from came-from
                                  :depth (1+ (god-table-entry-depth (svref table came-from)))
                                  :transition (funcall make-trans)))))
          :until (queue-empty-p positions-left))

    ;; Return a GOD-TABLE object.
    (make-instance 'god-table :group group
                              :table table
                              :target target
                              :generators generators)))

(defgeneric reconstruct-perm (god-table perm)
  (:method ((table god-table) perm)
    (declare (optimize speed))
    (multiple-value-bind (rank-element unrank-element)
        (group-element-rank-functions (god-table-group table))
      (declare (ignore unrank-element)
               (type function rank-element))
      (labels ((chase (current collected)
                 (with-slots (move came-from) (svref (god-table-vector table) current)
                   (declare (type integer move))
                   (if (= -1 move)
                       (nreverse collected)
                       (chase came-from (cons move collected))))))
        (chase (funcall rank-element perm) nil)))))

;;; Example:
;;; (defvar *god (compute-god-table (perm-examples:make-rubik-2x2)))
;;; (reconstruct-perm *god (random-group-element (god-table-group *god)))
