;;;; 4-list-algorithm.lisp
;;;;
;;;; Copyright (c) 2023 Robert Smith

(in-package #:cl-permutation)

(declaim (optimize speed))

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

(declaim (inline sparse-array-bitmap sparse-array-elements %make-sparse-array))
(defstruct (sparse-array (:constructor %make-sparse-array ()))
  ;;(bitmap   0   :type (and unsigned-byte fixnum))
  (elements nil :type list))
#+sbcl (declaim (sb-ext:freeze-type sparse-array))

(declaim (inline sparse-array-count))
(defun sparse-array-count (sa)
  (length (sparse-array-elements sa))
  ;;(logcount (sparse-array-bitmap sa))
  )

(defun make-sparse-array (size)
  (declare (ignore size))
  (%make-sparse-array)
  #+ig
  (%make-sparse-array (make-array (1+ size) :element-type 'bit :initial-element 0)))

(declaim (inline sparse-array-singleton-p))
(defun sparse-array-singleton-p (sa)
  (null (cdr (sparse-array-elements sa))))

(declaim (inline assoc*))
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

(declaim (inline saref))
(defun saref (sa n)
  (declare (type sparse-array sa)
           (type perm-element n)
           (optimize speed (safety 0)))
  (cdr (raw-saref sa n))
  #+ig
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
       ;; (incf (sparse-array-count sa))
       #+ig
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
  (num-elements 0 :type (and fixnum unsigned-byte))
  (perm-size 0 :type vector-size)
  root)

(defmethod print-object ((object perm-tree) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~D perm~:P" (perm-tree-num-elements object))))

(defun occupancy (tree)
  (let ((histogram (make-array (1+ (perm-tree-perm-size tree)) :initial-element 0)))
    (labels ((tally (x)
               (typecase x
                 (sparse-array
                  (let ((present (sparse-array-count x)))
                    (incf (aref histogram present))
                    (loop :for (i . y) :in (sparse-array-elements x)
                          :do (tally y)
                     )))
                 (otherwise
                  nil))))
      (tally (perm-tree-root tree))
      (values histogram))))

(defun make-perm-tree (perm-size)
  (%make-perm-tree :num-elements 0
                   :perm-size perm-size
                   :root (make-sparse-array perm-size)))

(defun perm-tree-ref (tree perm)
  (let ((size (perm-tree-perm-size tree)))
    (labels ((check (i node)
               (let ((point (perm-eval perm i)))
                 (cond
                   ((= i size)
                    (let ((result (saref node point)))
                      (and (not (null result))
                           (cdr result))))
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
                                               (make-sparse-array size)))))
                      (ingest (1+ i) next-node)))))))
      (ingest 1 (perm-tree-root tree)))))

(defun map-perm-tree (f tree &key re-order)
  ;; TODO: modify so that we can visit all elements *after* a particular one?
  "Visit the permutations of TREE in lexicographic order. F should be a function taking two arguments:

    1. The perm

    2. The value associated with the perm.

RE-ORDER is an optional argument, either a function or a perm that re-orders the children.

For example, if SIZE = 10, then #[10 9 8 7 6 5 4 3 2 1] would traverse in reverse lexicographic order."

  (declare (optimize speed)
           (type function f))
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

(defun saref1 (&rest args)
  (apply #'saref args))

(defun saref2 (&rest args)
  (apply #'saref args))

(defun saref3 (&rest args)
  (apply #'saref args))

(defun saref4 (&rest args)
  (apply #'saref args))

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
      (labels ((search-at-depth (current-depth to-depth node path)
                 (declare (type fixnum current-depth to-depth)
                          (type sparse-array node))
                 (cond
                   ((< current-depth to-depth)
                    (let* ((next-index (unsafe/perm-eval perm current-depth))
                           (next (saref3 node next-index)))
                      (cond
                        ;; We didn't actually find our target
                        ;; perm. That's OK, we can still find the
                        ;; successor to it.
                        ((null next)
                         (bottom-up-dfs perm current-depth path))
                        ;; We found the next node, keep descending.
                        (t
                         (search-at-depth
                          (1+ current-depth)
                          to-depth
                          next
                          (cons next path))))))
                   ;; We reached the node just above the leaf, start
                   ;; searching here.
                   (t
                    (bottom-up-dfs perm current-depth path))))
               (bottom-up-dfs (perm current-depth path)
                 (loop :for node :in path
                       ;;:for count :of-type fixnum := (sparse-array-count node)
                       :for depth :of-type fixnum := current-depth :then (1- depth)
                       :unless (sparse-array-singleton-p node)
                         :do
                            (loop :with skip := t
                                  :with point :of-type perm-element := (unsafe/perm-eval perm depth)
                                  :for i :of-type perm-element :from 1 :to size
                                  :for i* :of-type perm-element := (relabel i)
                                  :do (cond
                                        (skip
                                         ;; When we've reached our
                                         ;; point---which is
                                         ;; skipped---we can now stop
                                         ;; skipping.
                                         (when (= i* point)
                                           (setf skip nil)))
                                        (t
                                         (let ((next (saref4 node i*)))
                                           (when next
                                             (dfs (1+ depth) next))))))))
               (dfs (depth node)
                 (declare (type fixnum depth)
                          (type sparse-array node))
                 (cond
                   ((= depth max-depth)
                    (if (sparse-array-singleton-p node) ;(= 1 (sparse-array-count node))
                        (return-from perm-tree-next-perm
                          (cadar (sparse-array-elements node)))
                        (loop :for i :of-type perm-element :from 1 :to size
                              :for x := (saref1 node (relabel i))
                              :unless (null x)
                                :do (return-from perm-tree-next-perm (car x)))))
                   (t
                    ;; TODO count optimization?
                    (if (sparse-array-singleton-p node) ; (= 1 count)
                        (dfs (1+ depth) (cdar (sparse-array-elements node)))
                        (loop :for i :of-type perm-element :from 1 :to size
                              :for x := (saref2 node (relabel i))
                              :unless (null x)
                                :do (dfs (1+ depth) x)))))))
        (search-at-depth 1 (1- size) (perm-tree-root tree) (list (perm-tree-root tree)))
        ;; If we reached here, we found nothing...
        nil))))

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

(defun schroeppel-shamir2 (l2-tree l1-tree)
  (let ((q (pq:make-pqueue #'perm<)))
    ;; Initialize the queue with minimums.
    (do-perm-tree (y wy l2-tree)
      (declare (ignore wy))
      (let ((y-inv (perm-inverse y)))
        (let ((x (perm-tree-least l1-tree :re-order y-inv)))
          (pq:pqueue-push
           (list x y y-inv)
           (perm-compose y x)
           q))))

    ;; Create an iterator over products of L1 and L2
    (labels ((iterator ()
               (declare (optimize speed))
               (when (pq:pqueue-empty-p q)
                 (return-from iterator (values nil nil nil)))
               (multiple-value-bind (components yx)
                   (pq:pqueue-pop q)
                 (destructuring-bind (x y y-inv) components
                     ;; update queue
                   (let ((next-x (perm-tree-next-perm l1-tree x :re-order y-inv)))
                     (when next-x
                       (rplaca components next-x) ; save memory
                       (pq:pqueue-push
                        components
                        (perm-compose y next-x)
                        q)))
                   ;; return our element
                   (values yx y x)))))
      #'iterator)))

(defun in-common?* (&rest args)
  (write-line "; profiling")
  (sb-sprof:with-profiling (:reset t :mode :cpu :report :graph)
    (apply #'in-common? args)))

(defun in-common? (a b &key (test '=)
                            (compare '<)
                            (join 'list)
                            (return-immediately nil)
                            (report-interval 100000)
                            num-elements-1
                            num-elements-2
                            (limit nil ;500000
                             ))
  (let ((common nil)
        (lap (get-internal-real-time))
        (count 2))
    (multiple-value-bind (ax f2 f1) (funcall a)
      (multiple-value-bind (bx f4 f3) (funcall b)
        (loop
          (when (or (null ax) (null bx))
            (return-from in-common? common))
          (cond
            ((funcall test ax bx)
             (let ((result (funcall join f4 f3 f2 f1)))
               (when return-immediately
                 (return-from in-common? result))
               (push result common))
             (setf (values ax f2 f1) (funcall a)
                   (values bx f4 f3) (funcall b)))
            ((funcall compare ax bx)
             (setf (values ax f2 f1) (funcall a)))
            (t                            ; equiv. (funcall compare bx ax)
             (setf (values bx f4 f3) (funcall b))))
          ;; Report some info.
          (incf count)
          (when (and (plusp count) (zerop (mod count report-interval)))
            (let* ((elapsed-time (/ (- (get-internal-real-time) lap) internal-time-units-per-second))
                   (perms-per-sec (/ report-interval elapsed-time))
                   (total (+ num-elements-1 num-elements-2))
                   (remaining (- total count))
                   (hours-left (/ remaining perms-per-sec 60 60)))
              (multiple-value-bind (hours hour-fraction) (truncate hours-left 1)
                (format t "~&~:D: ~D sec @ ~D perms/sec; ~3,4F% complete, eta ~D hour~:P ~D minute~:P~%"
                        count
                        (round elapsed-time)
                        (round perms-per-sec)
                        (* 100.0 (/ count total))
                        hours
                        (round (* 60 hour-fraction))))
              (setf lap (get-internal-real-time))))
          (when (and limit (>= count limit))
            (return-from in-common? common)))))))

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

#-d
(progn
  (defun 3x3-htm ()
    (let* ((original (perm-group.generators (cl-permutation-examples:make-rubik-3x3)))
           (new (loop :for g :in original
                      :collect (perm-expt g 1)
                      :collect (perm-expt g 2)
                      :collect (perm-expt g 3))))
      (generate-perm-group new)))
  (defvar *3x3 (3x3-htm)))

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
                 (schroeppel-shamir2 L2 L1)))
         (L3L4 (progn
                 (logout "; SS 2")
                 (schroeppel-shamir2 L4 L3))))
    (format t "~&|L_i| = ~:D~%" (perm-tree-num-elements L1))
    (let ((solution (in-common?* L1L2 L3L4 :test #'perm=
                                           :compare #'perm<
                                           :join (lambda (f4 f3 f2 f1)
                                                   ;; we can save
                                                   ;; memory here if
                                                   ;; L{1,2,3,4} are
                                                   ;; essentially
                                                   ;; equal up to
                                                   ;; transformation.
                                                   (revappend
                                                    (mapcar #'-
                                                            (append
                                                             (perm-tree-ref L4 f4)
                                                             (perm-tree-ref L3 f3)))
                                                    (append
                                                     (perm-tree-ref L2 f2)
                                                     (perm-tree-ref L1 f1))))
                                           
                                           :limit 1000000
                                           :report-interval 500000
                                           :return-immediately t
                                           :num-elements-1 (* (perm-tree-num-elements L1)
                                                              (perm-tree-num-elements L2))
                                           :num-elements-2 (* (perm-tree-num-elements L3)
                                                              (perm-tree-num-elements L4))
                                           )))
      (when solution
        (fresh-line)
        (format t "input : ~A~%" g)
        (format t "common: ~A~%" (car solution))
        (let ((*print-pretty* nil))
          (format t "* * * reconstruction: ~A~%" (cdr solution)))
        (values solution
                (perm-compose (perm-inverse (funcall free->2x2 solution))
                              g))))))
