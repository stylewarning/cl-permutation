;;;; 4-list-algorithm.lisp
;;;;
;;;; Copyright (c) 2023 Robert Smith

(in-package #:cl-permutation)

(declaim (optimize speed (safety 1)))

;;;; This file implements the planning algorithm from the paper
;;;; "Planning and Learning in Permutation Groups" by Fiat, Moses,
;;;; Shamir, Shimshoni, and Tardos. In the paper, they call it a
;;;; "t-list algorithm", and their main example specialized to t=4.

;;; Sparse Array
;;;
;;; We use sparse arrays to implement the permutation trie
;;; (PERM-TRIE), since occupancy is generally very, very small. We
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

(defun saref (sa n)
  (declare (type sparse-array sa)
           (type perm-element n)
           (optimize speed (safety 0))
           )
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

;;; Permutation Trie
;;;
;;; This is a data structure that associates a permutation with
;;; arbitrary data. It has the benefit that permutations can be
;;; visited in lexicographic order, and permutations overlap in
;;; memory.

(defstruct (perm-trie (:constructor %make-perm-trie))
  "A trie-like data structure to store permutations."
  (num-elements 0 :type (and fixnum unsigned-byte))
  (perm-size 0 :type vector-size)
  root)

(defmethod print-object ((object perm-trie) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~D perm~:P" (perm-trie-num-elements object))))

(defun occupancy (trie)
  (let ((histogram (make-array (1+ (perm-trie-perm-size trie)) :initial-element 0)))
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
      (tally (perm-trie-root trie))
      (values histogram))))

(defun make-perm-trie (perm-size)
  (%make-perm-trie :num-elements 0
                   :perm-size perm-size
                   :root (make-sparse-array perm-size)))

(defun perm-trie-ref (trie perm)
  (let ((size (perm-trie-perm-size trie)))
    (labels ((check (i node)
               (let ((point (perm-eval perm i)))
                 (cond
                   ((= i size)
                    (let ((result (saref node point)))
                      (and (not (null result))
                           (cdr result))))
                   (t
                    (let ((next-node (saref node point)))
                      (etypecase next-node
                        (null nil)
                        (sparse-array (check (1+ i) next-node))
                        (cons (if (perm= perm (car next-node))
                                  (cdr next-node)
                                  nil)))))))))
      (check 1 (perm-trie-root trie)))))

(defun ingest-perm (trie perm &key (value t) force ignore)
  (let ((size (perm-trie-perm-size trie)))
    (labels ((record (point node perm value)
               (incf (perm-trie-num-elements trie))
               (setf (saref node point) (cons perm value)))
             (ingest (i node perm value)
               (let ((point (perm-eval perm i)))
                 (cond
                   ;; special case at the leaf
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
                    (let ((object-at-location (saref node point)))
                      (etypecase object-at-location
                        (null
                         (record point node perm value)
                         value)
                        (sparse-array
                         (ingest (1+ i) object-at-location perm value))
                        (cons
                         (let ((old-perm (car object-at-location))
                               (old-value (cdr object-at-location)))
                           (cond
                             ((perm= perm old-perm)
                              (cond
                                (ignore
                                 nil)
                                (force
                                 (rplacd object-at-location value))
                                (t
                                 (error "Trying to overwrite existing value."))))
                             (t
                              (let ((next-node (make-sparse-array size)))
                                (setf (saref node point) next-node)
                                ;; don't double count
                                (decf (perm-trie-num-elements trie))
                                (ingest (1+ i) next-node old-perm old-value)
                                (ingest (1+ i) next-node perm value)))))))))))))
      (ingest 1 (perm-trie-root trie) perm value))))

(defun map-perm-trie (f trie &key re-order)
  ;; TODO: modify so that we can visit all elements *after* a particular one?
  "Visit the permutations of TRIE in lexicographic order. F should be a function taking two arguments:

    1. The perm

    2. The value associated with the perm.

RE-ORDER is an optional argument, either a function or a perm that re-orders the children.

For example, if SIZE = 10, then #[10 9 8 7 6 5 4 3 2 1] would traverse in reverse lexicographic order."

  (declare (optimize speed)
           (type function f))
  (let* ((size      (perm-trie-perm-size trie))
         (max-depth size)
         (relabel (etypecase re-order
                    (null #'identity)
                    (perm (lambda (i)
                            (unsafe/perm-eval re-order i)))
                    (function re-order))))
    (declare (type fixnum size))
    (labels ((dfs (depth node)
               (declare (type fixnum depth))
               (etypecase node
                 (cons
                  (funcall f (car node) (cdr node)))
                 (sparse-array
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
                             :do (dfs (1+ depth) x))))))))
      (dfs 1 (perm-trie-root trie)))))

(defun perm-trie-next-perm (trie perm &key (re-order
                                            (perm-identity (perm-trie-perm-size trie))))
  "Find the lexicographic successor to a perm."
  (declare (optimize speed))
  (assert (= (perm-size perm) (perm-trie-perm-size trie)))

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
                           (next (saref node next-index)))
                      (etypecase next
                        ;; We didn't actually find our target
                        ;; perm. That's OK, we can still find the
                        ;; successor to it.
                        (null
                         (bottom-up-dfs perm current-depth path))
                        ;; We found something... search above it
                        ;;
                        ;; XXX: check??????????????????????????????/
                        (cons
                         (bottom-up-dfs perm current-depth path))
                        ;; We found the next node, keep descending.
                        (sparse-array
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
                       :for depth :of-type fixnum := current-depth :then (1- depth)
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
                                       (let ((next (saref node i*)))
                                         (dfs (1+ depth) next)))))))
               (dfs (depth node)
                 (declare (type fixnum depth))
                 (etypecase node
                   (null nil)
                   (cons
                    (return-from perm-trie-next-perm (car node)))
                   (sparse-array
                    (cond
                      ((= depth max-depth)
                       (if (sparse-array-singleton-p node) ;(= 1 (sparse-array-count node))
                           (return-from perm-trie-next-perm
                             (cadar (sparse-array-elements node)))
                           (loop :for i :of-type perm-element :from 1 :to size
                                 :for x := (saref node (relabel i))
                                 :unless (null x)
                                   :do (return-from perm-trie-next-perm (car x)))))
                      (t
                       ;; TODO count optimization?
                       (if nil ;(sparse-array-singleton-p node) ; (= 1 count)
                           (dfs (1+ depth) (cdar (sparse-array-elements node)))
                           (loop :for i :of-type perm-element :from 1 :to size
                                 :for x := (saref node (relabel i))
                                 :unless (null x)
                                   :do (dfs (1+ depth) x)))))))))
        (search-at-depth 1 (1- size) (perm-trie-root trie) (list (perm-trie-root trie)))
        ;; If we reached here, we found nothing...
        nil))))

(defmacro do-perm-trie ((p v trie &key re-order) &body body)
  (let ((iter-trie (gensym "ITER-TRIE"))
        (trie-once (gensym "TRIE-ONCE")))
    `(let ((,trie-once ,trie))
       (flet ((,iter-trie (,p ,v)
                ,@body))
         (declare (dynamic-extent #',iter-trie))
         (map-perm-trie #',iter-trie ,trie-once :re-order ,re-order)
         nil))))

(defun collect-perm-trie (trie)
  (let ((elements nil))
    (do-perm-trie (p v trie)
      (declare (ignore v))
      (push p elements))
    (nreverse elements)))

;;; should be equal to perm trie
;;; TODO make a test
(defun collect-perm-trie2 (trie &key re-order)
  (loop :with node := (perm-trie-least trie :re-order re-order)
        :until (null node)
        :collect (prog1 node
                   (setf node (perm-trie-next-perm trie node :re-order re-order)))))

(defun perm-trie-least (trie &key re-order)
  (do-perm-trie (p v trie :re-order re-order)
    (return-from perm-trie-least (values p v))))

(defun schroeppel-shamir2 (l2-trie l1-trie)
  (let ((q (pq:make-pqueue #'perm<)))
    ;; Initialize the queue with minimums.
    (do-perm-trie (y wy l2-trie)
      (declare (ignore wy))
      (let ((y-inv (perm-inverse y)))
        (let ((x (perm-trie-least l1-trie :re-order y-inv)))
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
                   (let ((next-x (perm-trie-next-perm l1-trie x :re-order y-inv)))
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
  (time ;sb-sprof:with-profiling (:reset t :mode :cpu :report :graph)
    (apply #'in-common? args)))

(defun in-common? (a b &key (test '=)
                            (compare '<)
                            (join 'list)
                            (return-immediately nil)
                            (report-interval 100000)
                            num-elements-1
                            num-elements-2
                            verbose
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
          (when (and verbose
                     (plusp count)
                     (zerop (mod count report-interval)))
            (let* ((elapsed-time (/ (- (get-internal-real-time) lap) internal-time-units-per-second))
                   (perms-per-sec (/ report-interval elapsed-time))
                   (total (+ num-elements-1 num-elements-2))
                   (remaining (- total count))
                   (hours-left (/ remaining perms-per-sec 60 60)))
              (multiple-value-bind (hours hour-fraction) (truncate hours-left 1)
                (format t "~&~:D: ~D sec @ ~:D perms/sec; ~3,4F% complete, eta ~D hour~:P ~D minute~:P~%"
                        count
                        (round elapsed-time)
                        (round perms-per-sec)
                        (* 100.0 (/ count total))
                        hours
                        (round (* 60 hour-fraction))))
              (setf lap (get-internal-real-time))))
          (when (and limit (>= count limit))
            (return-from in-common? common)))))))

(defun transform-trie (f trie)
  (let ((new-trie (make-perm-trie (perm-trie-perm-size trie))))
    (do-perm-trie (p v trie)
      (multiple-value-bind (new-p new-v) (funcall f p v)
        (ingest-perm new-trie new-p :value new-v)))
    new-trie))

;;;; 2x2

(defun generate-words-of-bounded-length (gens word-length)
  (let* ((size (loop :for g :in gens :maximize (perm-size g)))
         (trie (make-perm-trie size))
         (id   (perm-identity size)))
    (ingest-perm trie id :value nil)
    (labels ((f (max cur perm word)
               (loop :for i :from 1
                     :for g :in gens
                     :for p := (perm-compose g perm)
                     :for w := (cons i word)
                     :do (ingest-perm trie p :value w :ignore t)
                         (when (< cur max)
                           (f max (1+ cur) p w)))))
      (loop :for max :from 1 :to word-length :do (f max 1 id nil))
      trie)))

#+d
(progn
  (defun 3x3-htm ()
    (let* ((original (perm-group.generators (cl-permutation-examples:make-rubik-3x3)))
           (new (loop :for g :in original
                      :collect (perm-expt g 1)
                      :collect (perm-expt g 2)
                      :collect (perm-expt g 3))))
      (generate-perm-group new)))
  (defun scramble (group num-moves)
    (let ((s (group-identity group)))
      (loop :repeat num-moves :do
        (setf s (perm-compose (alexandria:random-elt (perm-group.generators group))
                              s)))
      s))
  (defvar *3x3 (3x3-htm)))

(defun test-ss (g &key group (wordlen 5))
  (check-type group perm-group)
  (labels ((logout (string)
             (write-line string)
             (finish-output)))
    (let* ((gens (perm-group.generators group))
           (free (perm-group.free-group group))
           (free->2x2 (free-group->perm-group-homomorphism free group))

           (L1 (progn
                 (logout "; generating words")
                 (generate-words-of-bounded-length gens wordlen)))
           (L2 L1)
           (L4 (progn
                 (logout "; xform tries")
                 (transform-trie (lambda (p v)
                                   (values (perm-inverse p)
                                           (reverse (mapcar #'- v))))
                                 L1)))
           (L3 (transform-trie (lambda (p v)
                                 (values (perm-compose p g) v))
                               L4))
           (L1L2 (progn
                   (logout "; SS 1")
                   (schroeppel-shamir2 L2 L1)))
           (L3L4 (progn
                   (logout "; SS 2")
                   (schroeppel-shamir2 L4 L3))))
      (format t "~&|L_i|    = ~:D~%" (perm-trie-num-elements L1))
      (format t "~&2|L_i|^2 = ~:D~%" (* 2 (expt (perm-trie-num-elements L1) 2)))
      (format t "tally: ~A~%" (let* ((o (occupancy L1))
                                     (s (reduce #'+ o)))
                                (map 'vector (lambda (x)
                                               (* 100.0 (/ x s)))
                                     o)))

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
                                                               (perm-trie-ref L4 f4)
                                                               '(0)
                                                               (perm-trie-ref L3 f3)))
                                                      (append
                                                       '(0)
                                                       (perm-trie-ref L2 f2)
                                                       '(0)
                                                       (perm-trie-ref L1 f1))))
                                             
                                        ;:limit 5000000
                                             :report-interval 10000000
                                             :return-immediately t
                                             :num-elements-1 (* (perm-trie-num-elements L1)
                                                                (perm-trie-num-elements L2))
                                             :num-elements-2 (* (perm-trie-num-elements L3)
                                                                (perm-trie-num-elements L4))
                                             )))
        (when solution
          (fresh-line)
          (format t "input : ~A~%" g)
          (let ((*print-pretty* nil))
            (format t "* * * reconstruction: ~A~%" solution))
          (values solution
                  (perm-compose (perm-inverse (funcall free->2x2 solution))
                                g)))))))
