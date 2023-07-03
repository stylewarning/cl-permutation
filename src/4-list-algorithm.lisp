;;;; 4-list-algorithm.lisp
;;;;
;;;; Copyright (c) 2023 Robert Smith

(in-package #:cl-permutation)

;;;; This file implements the planning algorithm from the paper
;;;; "Planning and Learning in Permutation Groups" by Fiat, Moses,
;;;; Shamir, Shimshoni, and Tardos. In the paper, they call it a
;;;; "t-list algorithm", and their main example specialized to t=4.
;;;;
;;;; This file requires "generators.lisp" and priority-queue

(defstruct (perm-tree (:constructor %make-perm-tree))
  "A trie-like data structure to store permutations."
  num-elements
  perm-size
  root)

(defun make-perm-tree (perm-size)
  (%make-perm-tree :num-elements 0
                   :perm-size perm-size
                   :root (make-array (1+ perm-size) :initial-element nil)))

(defun perm-tree-member-p (tree perm)
  (let ((size (perm-tree-perm-size tree)))
    (labels ((check (i node)
               (let ((point (perm-eval perm i)))
                 (cond
                   ((= i size)
                    (not (null (aref node point))))
                   (t
                    (let ((next-node (aref node point)))
                      (and (not (null next-node))
                           (check (1+ i) next-node))))))))
      (check 1 (perm-tree-root tree)))))

(defun ingest-perm (tree perm &key (value t) force ignore)
  (let ((size (perm-tree-perm-size tree)))
    (labels ((record (point node perm value)
               (incf (perm-tree-num-elements tree))
               (setf (aref node point) (cons perm value)))
             (ingest (i node)
               (let ((point (perm-eval perm i)))
                 (cond
                   ((= i size)
                    (let ((old-value (aref node point)))
                      (cond
                        ((null old-value)
                         (record point node perm value)
                         value)
                        (force
                         (record point node perm value)
                         value)
                        (ignore
                         nil)
                        (t
                         (error "Trying to overwrite existing value.")))))
                   (t
                    (let ((next-node (or (aref node point)
                                         (setf (aref node point)
                                               (make-array (1+ size) :initial-element nil)))))
                      (ingest (1+ i) next-node)))))))
      (ingest 1 (perm-tree-root tree)))))

(defun map-perm-tree (f tree &key re-order)
  ;; TODO: modify so that we can visit all elements *after* a particular one?
  "Visit the permutations of TREE in lexicographic order. F should be a function taking two arguments:

    1. The perm

    2. The value associated with the perm.

RE-ORDER is an optional argument, either a function or a perm that re-orders the children.
  
For example, if SIZE = 10, then #[10 9 8 7 6 5 4 3 2 1] would traverse in reverse lexicographic order."

  (declare (optimize speed))
  (let* ((size      (perm-tree-perm-size tree))
         (max-depth size)
         (relabel (etypecase re-order
                    (null #'identity)
                    (perm (lambda (i)
                            (unsafe/perm-eval re-order i)))
                    (function re-order))))
    (declare (type fixnum size))
    (labels ((dfs (depth node)
               (declare (type fixnum depth)
                        (type simple-vector node))
               (cond
                 ((= depth max-depth)
                  (loop :for i :from 1 :to size
                        :for x := (aref node (funcall relabel i))
                        :unless (null x)
                          :do (funcall f (car x) (cdr x))))
                 (t
                  (loop :for i :from 1 :to size
                        :for x := (aref node (funcall relabel i))
                        :unless (null x)
                          :do (dfs (1+ depth) x))))))
      (dfs 1 (perm-tree-root tree)))))

(defmacro do-perm-tree ((p v tree) &body body)
  (let ((iter-tree (gensym "ITER-TREE"))
        (tree-once (gensym "TREE-ONCE")))
    `(let ((,tree-once ,tree))
       (flet ((,iter-tree (,p ,v)
                ,@body))
         (declare (dynamic-extent #',iter-tree))
         (map-perm-tree #',iter-tree ,tree-once)
         nil))))

(defun collect-perm-tree (tree)
  (let ((elements nil))
    (do-perm-tree (p v tree)
      (declare (ignore v))
      (push p elements))
    (nreverse elements)))

(defun perm-tree-least (tree)
  (do-perm-tree (p v tree)
    (return-from perm-tree-least (values p v))))

(defun perm-tree-next-perm (tree perm)
  ;; This could be made more efficient with coroutines in the map
  ;; function.
  (flet ((f (p v)
           (declare (ignore v))
           (when (perm< perm p)
             (return-from perm-tree-next-perm p))))
    (map-perm-tree #'f tree)
    ;; otherwise return nil
    nil))

;;;

(defun find-smallest-above (tree x y &optional (yx (perm-compose y x)))
  ;; Find the P in TREE such that YP is the smallest element after
  ;; YX.
  (assert (perm-tree-member-p tree x))
  (flet ((seek (p wp)
           (let ((yp (perm-compose y p)))
             (when (perm< yx yp)
               (return-from find-smallest-above (values p yp wp))))))
    (map-perm-tree #'seek tree :re-order (perm-inverse y))
    ;; if we get this far, we didn't find anything...
    (values nil nil nil)))

(defun perm-priority (perm)
  ;; same as RANK
  (loop :with size := (perm-size perm)
        :with rank := 0
        :for i :from 0 :below (1- size)
        :for elt := (perm-ref perm i)
        :do (let ((inversions (loop :for j :from (1+ i) :below size
                                    :for elt-after := (perm-ref perm j)
                                    :count (> elt elt-after))))
              (setf rank (+ inversions (* rank (- size i)))))
        :finally (return rank)))

(defun schroeppel-shamir (l1-tree l2-tree)
  (coroutine
    (let ((q (pq:make-pqueue #'<)))
      (multiple-value-bind (x1 wx1) (perm-tree-least l1-tree)
        (do-perm-tree (y wy l2-tree)
          (let ((yx1 (perm-compose y x1)))
            (pq:pqueue-push (list y wy x1 wx1 yx1)
                            (perm-priority yx1)
                            q))))
      (loop :until (pq:pqueue-empty-p q)
            :do (destructuring-bind (y wy x wx yx) (pq:pqueue-pop q)
                  (yield (cons yx (append wy wx)))
                  (multiple-value-bind (next-x next-yx next-wx)
                      (find-smallest-above l1-tree x y yx)
                    (unless (null next-x)
                      (pq:pqueue-push
                       (list y wy next-x next-wx next-yx)
                       (perm-priority next-yx)
                       q))))))))

(defun in-common? (a b &key (test '=)
                            (compare '<)
                            (key 'identity)
                            (join 'cons)
                            (return-immediately nil))
  (let ((common nil)
        (ai (next-or a nil))
        (bi (next-or b nil))
        (lap (get-internal-real-time))
        (count 0))
    (loop
      (when (zerop (mod count 5000))
        (format t "~&~D: ~D~%" count (round (- (get-internal-real-time) lap) internal-time-units-per-second))
        (setf lap (get-internal-real-time)))
      (incf count)
      (when (or (null ai) (null bi))
        (return-from in-common? common))
      (let ((ax (funcall key ai))
            (bx (funcall key bi)))
        (cond
          ((funcall test ax bx)
           (let ((result (funcall join ai bi)))
             (when return-immediately
               (return-from in-common? result))
             (push result common))
           (setf ai (next-or a nil)
                 bi (next-or b nil)))
          ((funcall compare ax bx)
           (setf ai (next-or a nil)))
          ((funcall compare bx ax)
           (setf bi (next-or b nil)))
          (t
           (error "unreachable")))))))

(defun transform-tree (f tree)
  (let ((new-tree (make-perm-tree (perm-tree-perm-size tree))))
    (do-perm-tree (p v tree)
      (multiple-value-bind (new-p new-v) (funcall f p v)
        (ingest-perm new-tree new-p :value new-v)))
    new-tree))

;;;; 2x2

(defun generate-words-of-bounded-length (gens word-length)
  (let* ((size (loop :for g :in gens :maximize (perm-size g)))
         (tree (make-perm-tree size))
         (id   (perm-identity size)))
    (ingest-perm tree id :value nil)
    (labels ((f (max cur perm word)
               (loop :for i :from 1
                     :for g :in gens
                     :for p := (perm-compose g perm)
                     :for w := (cons i word)
                     :do (ingest-perm tree p :value w :ignore t)
                         (when (< cur max)
                           (f max (1+ cur) p w)))))
      (loop :for max :from 1 :to word-length :do (f max 1 id nil))
      tree)))

(defun test-ss (g &key (group (cl-permutation-examples:make-rubik-2x2)))
  (let* ((gens (perm-group.generators group))
         (free (perm-group.free-group group))
         (free->2x2 (free-group->perm-group-homomorphism free group))
         
         (L1 (generate-words-of-bounded-length gens 5))
         (L2 L1)
         (L3 (transform-tree (lambda (p v)
                               (values (perm-compose (perm-inverse p) g)
                                       (reverse (mapcar #'- v))))
                             L1))
         (L4 (transform-tree (lambda (p v)
                               (values (perm-inverse p)
                                       (reverse (mapcar #'- v))))
                             L1))
         (L1L2 (schroeppel-shamir L1 L2))
         (L3L4 (schroeppel-shamir L3 L4)))
    (format t "|L_i| = ~D~%" (perm-tree-num-elements L1))
    (let ((solution (in-common? L1L2 L3L4 :key 'car
                                          :test 'perm=
                                          :compare 'perm<
                                          :join (lambda (a b)
                                                  (cons (car a)
                                                        (append
                                                         (reverse (mapcar #'- (cdr b)))
                                                         (cdr a))))
                                          :return-immediately t)))
      (print solution)
      (values (car solution)
              (perm-compose (perm-inverse (funcall free->2x2 (cdr solution)))
                            g)))))
