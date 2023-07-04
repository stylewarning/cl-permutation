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

;;; Sparse Array
;;;
;;; We use sparse arrays to implement the permutation trie
;;; (PERM-TREE), since occupancy is generally very, very small. We
;;; save a lot of memory this way.

(defstruct (sparse-array (:constructor make-sparse-array ()))
  (bitmap   0   :type integer)
  (elements nil :type list))

(defun assoc* (i list)
  (declare (optimize speed (safety 0))
           (type perm-element i)
           (type list list))
  (loop :until (null list) :do
    (let ((x (pop list)))
      (declare (type (cons perm-element t) x))
      (when (= i (car x))
        (return-from assoc* x))))
  nil)

(declaim (inline raw-saref))
(defun raw-saref (sa n)
  (assoc* n (sparse-array-elements sa)))

(defun saref (sa n)
  (if (logbitp n (sparse-array-bitmap sa))
      (cdr (raw-saref sa n))
      nil))

(defun (setf saref) (new-value sa n)
  (let ((existing (raw-saref sa n)))
    (cond
      ((consp existing)
       (rplacd existing new-value))
      (t
       (push (cons n new-value) (sparse-array-elements sa))
       (setf (sparse-array-bitmap sa)
             (dpb 1 (byte 1 n) (sparse-array-bitmap sa)))
       new-value))))

;;; Permutation Tree
;;;
;;; This is a data structure that associates a permutation with
;;; arbitrary data. It has the benefit that permutations can be
;;; visited in lexicographic order, and permutations overlap in
;;; memory.

(defstruct (perm-tree (:constructor %make-perm-tree))
  "A trie-like data structure to store permutations."
  num-elements
  perm-size
  root)

(defmethod print-object ((object perm-tree) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~D perm~:P" (perm-tree-num-elements object))))

(defun occupancy (tree)
  (let ((total   0)
        (non-nil 0))
    (labels ((tally (x)
               (incf total)
               (typecase x
                 (sparse-array
                  (incf non-nil)
                  (mapcar (lambda (c) (tally (cdr c))) (sparse-array-elements x)))
                 (vector
                  (incf non-nil)
                  (map nil #'tally x))
                 (null
                  nil)
                 (otherwise
                  (incf non-nil)))))
      (tally (perm-tree-root tree))
      (values non-nil total))))

(defun make-perm-tree (perm-size)
  (%make-perm-tree :num-elements 0
                   :perm-size perm-size
                   :root (make-sparse-array)))

(defun perm-tree-member-p (tree perm)
  (let ((size (perm-tree-perm-size tree)))
    (labels ((check (i node)
               (let ((point (perm-eval perm i)))
                 (cond
                   ((= i size)
                    (not (null (saref node point))))
                   (t
                    (let ((next-node (saref node point)))
                      (and (not (null next-node))
                           (check (1+ i) next-node))))))))
      (check 1 (perm-tree-root tree)))))

(defun ingest-perm (tree perm &key (value t) force ignore)
  (let ((size (perm-tree-perm-size tree)))
    (labels ((record (point node perm value)
               (incf (perm-tree-num-elements tree))
               (setf (saref node point) (cons perm value)))
             (ingest (i node)
               (let ((point (perm-eval perm i)))
                 (cond
                   ((= i size)
                    (let ((old-value (saref node point)))
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
                    (let ((next-node (or (saref node point)
                                         (setf (saref node point)
                                               (make-sparse-array)))))
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
                        (type sparse-array node))
               (cond
                 ((= depth max-depth)
                  (loop :for i :from 1 :to size
                        :for x := (saref node (funcall relabel i))
                        :unless (null x)
                          :do (funcall f (car x) (cdr x))))
                 (t
                  (loop :for i :from 1 :to size
                        :for x := (saref node (funcall relabel i))
                        :unless (null x)
                          :do (dfs (1+ depth) x))))))
      (dfs 1 (perm-tree-root tree)))))

(defun perm-tree-next-perm (tree perm &key (re-order
                                            (perm-identity (perm-tree-perm-size tree))))
  "Find the lexicographic successor to a perm."
  (declare (optimize speed))
  (assert (= (perm-size perm) (perm-tree-perm-size tree)))

  (let* ((size (perm-size perm))
         (max-depth size))
    (flet ((relabel (i)
             (unsafe/perm-eval re-order i)))
      (declare (inline relabel))
      (labels ((search-at-depth (current-depth to-depth node)
                 (declare (type fixnum current-depth to-depth)
                          (type sparse-array node))
                 (cond
                   ((< current-depth to-depth)
                    (let* ((next-index (unsafe/perm-eval perm current-depth))
                           (next (saref node next-index)))
                      (when next
                        (search-at-depth (1+ current-depth) to-depth next))))
                   (t
                    ;; TODO make this better
                    (loop :with skip := t
                          :with point := (unsafe/perm-eval perm current-depth)
                          :for i :from 1 :to size
                          :for i* := (relabel i)
                          :do (cond
                                (skip
                                 (when (= i* point)
                                   (setf skip nil)))
                                (t
                                 (let ((next (saref node i*)))
                                   (when next
                                     (dfs (1+ current-depth) next)))))))))

               (dfs (depth node)
                 (declare (type fixnum depth)
                          (type sparse-array node))
                 (cond
                   ((= depth max-depth)
                    (loop :for i :from 1 :to size
                          :for x := (saref node (relabel i))
                          :unless (null x)
                            :do (return-from perm-tree-next-perm (values (car x) (cdr x)))))
                   (t
                    (loop :for i :from 1 :to size
                          :for x := (saref node (relabel i))
                          :unless (null x)
                            :do (dfs (1+ depth) x))))))
        (loop :for depth :from (1- size) :downto 1 :do
          (search-at-depth 1 depth (perm-tree-root tree)))
        ;; If we reached here, we found nothing...
        (values nil nil)))))

(defmacro do-perm-tree ((p v tree &key re-order) &body body)
  (let ((iter-tree (gensym "ITER-TREE"))
        (tree-once (gensym "TREE-ONCE")))
    `(let ((,tree-once ,tree))
       (flet ((,iter-tree (,p ,v)
                ,@body))
         (declare (dynamic-extent #',iter-tree))
         (map-perm-tree #',iter-tree ,tree-once :re-order ,re-order)
         nil))))

(defun collect-perm-tree (tree)
  (let ((elements nil))
    (do-perm-tree (p v tree)
      (declare (ignore v))
      (push p elements))
    (nreverse elements)))

;;; should be equal to perm tree
;;; TODO make a test
(defun collect-perm-tree2 (tree &key re-order)
  (loop :with node := (perm-tree-least tree :re-order re-order)
        :until (null node)
        :collect (prog1 node
                   (setf node (perm-tree-next-perm tree node :re-order re-order)))))

(defun perm-tree-least (tree &key re-order)
  (do-perm-tree (p v tree :re-order re-order)
    (return-from perm-tree-least (values p v))))

(defun schroeppel-shamir2 (l1-tree l2-tree)
  (let ((q (pq:make-pqueue #'perm<)))
    ;; Initialize the queue with minimums.
    (do-perm-tree (y wy l2-tree)
      (multiple-value-bind (x wx)
          (perm-tree-least l1-tree :re-order (perm-inverse y))
        (pq:pqueue-push
         (list y wy x wx)
         (perm-compose y x)
         q)))

    ;; Create an iterator over products of L1 and L2
    (labels ((iterator ()
               (when (pq:pqueue-empty-p q)
                 (return-from iterator nil))
               (multiple-value-bind (components yx)
                   (pq:pqueue-pop q)
                 (destructuring-bind (y wy x wx) components
                     ;; update queue
                     (multiple-value-bind (next-x next-wx)
                         (perm-tree-next-perm l1-tree x :re-order (perm-inverse y))
                       (when next-x
                         (pq:pqueue-push
                          (list y wy next-x next-wx)
                          (perm-compose y next-x)
                          q)))
                   ;; return our element
                   (cons yx (append wy wx))))))
      #'iterator)))

(defun in-common?* (&rest args)
  (write-line "; profiling")
  (time ;sb-sprof:with-profiling (:reset t :mode :cpu :report :graph)
    (apply #'in-common? args)))

(defun in-common? (a b &key (test '=)
                            (compare '<)
                            (key 'identity)
                            (join 'cons)
                            (return-immediately nil)
                            (report-interval 100000)
                            (limit nil ;500000
                             ))

  (let ((common nil)
        (ai (funcall a))
        (bi (funcall b))
        (lap (get-internal-real-time))
        (count 0))
    (loop
      (when (and (plusp count) (zerop (mod count report-interval)))
        (let ((elapsed-time (/ (- (get-internal-real-time) lap) internal-time-units-per-second)))
          (format t "~&~D: ~D (~A perms/sec)~%"
                  count
                  (round elapsed-time)
                  (float (/ report-interval elapsed-time)))
          (setf lap (get-internal-real-time))))
      (when (and limit (>= count limit))
        (return-from in-common? common))
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
           (setf ai (funcall a)
                 bi (funcall b)))
          ((funcall compare ax bx)
           (setf ai (funcall a)))
          (t                            ; equiv. (funcall compare bx ax)
           (setf bi (funcall b))))))))

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

(defun 3x3-htm ()
  (let* ((original (perm-group.generators (cl-permutation-examples:make-rubik-3x3)))
         (new (loop :for g :in original
                    :collect (perm-expt g 1)
                    :collect (perm-expt g 2)
                    :collect (perm-expt g 3))))
    (generate-perm-group new)))

(defvar *3x3 (3x3-htm))

(defun logout (string)
  (write-line string)
  (finish-output))

(defun test-ss (g &key group)
  (check-type group perm-group)
  (let* ((gens (perm-group.generators group))
         (free (perm-group.free-group group))
         (free->2x2 (free-group->perm-group-homomorphism free group))

         (L1 (progn
               (logout "; generating words")
               (generate-words-of-bounded-length gens 5)))
         (L2 L1)
         (L4 (progn
               (logout "; xform trees")
               (transform-tree (lambda (p v)
                                 (values (perm-inverse p)
                                         (reverse (mapcar #'- v))))
                               L1)))
         (L3 (transform-tree (lambda (p v)
                               (values (perm-compose p g) v))
                             L4))

         (L1L2 (progn
                 (logout "; SS 1")
                 (schroeppel-shamir2 L1 L2)))
         (L3L4 (progn
                 (logout "; SS 2")
                 (schroeppel-shamir2 L3 L4))))
    (format t "~&|L_i| = ~D~%" (perm-tree-num-elements L1))
    (let ((solution (in-common?* L1L2 L3L4 :key 'car
                                           :test 'perm=
                                           :compare 'perm<
                                           :join (lambda (a b)
                                                   (cons (car a)
                                                         (append
                                                          (reverse (mapcar #'- (cdr b)))
                                                          (cdr a))))
                                           :return-immediately t)))
      (when solution
        (fresh-line)
        (format t "input : ~A~%" g)
        (format t "common: ~A~%" (car solution))
        (let ((*print-pretty* nil))
          (format t "* * * reconstruction: ~A~%" (cdr solution)))
        (values (car solution)
                (perm-compose (perm-inverse (funcall free->2x2 (cdr solution)))
                              g))))))
