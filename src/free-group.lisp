;;;; free-group.lisp
;;;;
;;;; Copyright (c) 2015 Robert Smith

(in-package #:cl-permutation)

(defclass free-group ()
  ((num-generators :initarg :num-generators
                   :reader  free-group-num-generators))
  (:documentation "Representation of a free group whose symbols are:

    * identity element: 0
    * generators: 1 .. N
    * inverse generators: -N .. -1

Elements of the group are either individual integers or lists thereof. The lists represent compositions of generators. The BNF grammar looks something like:

    <free group generator> := 1 | 2 | ... | N
    <free group atom>      := <free group generator>
                            | 0
                            | -<free group generator>
    <free group element>   := <free group atom>
                            | ( <free group atom>* )

An empty list corresponds to an empty composition, which is identity (0)."))

(defun make-free-group (num-generators)
  "Make a free group that contains NUM-GENERATORS generators."
  (check-type num-generators (integer 0))
  (make-instance 'free-group :num-generators num-generators))

(defun free-group-element-valid-p (g element)
  "Given the free group G and some purported element ELEMENT, return a boolean indicating whether it is a valid element of G."
  (check-type g free-group)
  (let ((num-generators (free-group-num-generators g)))
    (typecase element
      (integer (<= (- num-generators) element num-generators))
      (list (every (lambda (e)
                     (free-group-element-valid-p g e))
                   element))
      (t nil))))

(defun canonicalize-free-group-element (g e)
  (declare (ignore g))
  (alexandria:flatten (list e)))

(defun make-free-group-element (g &rest elements)
  "Make an element of the free group G where ELEMENTS are either integer generators of G, or other elements created by this function."
  (check-type g free-group)
  (flet ((process (e)
           (if (listp e)
               (canonicalize-free-group-element g e)
               (list e))))
    (assert (every (lambda (e)
                     (free-group-element-valid-p g e))
                   elements)
            (elements)
            "The provided free group contains invalid elements.")
    (mapcan #'process elements)))

(defun free-group-identity-p (x)
  "Is X an identity element of a free group?"
  (cond
    ((integerp x) (zerop x))
    ((listp x) (every #'zerop x))
    (t nil)))

(defmethod identity-element ((g free-group))
  0)

(defmethod compose ((g free-group) (a null)    b)           b)
(defmethod compose ((g free-group) (a (eql 0)) b)           b)
(defmethod compose ((g free-group) a           (b null))    a)
(defmethod compose ((g free-group) a           (b (eql 0))) a)

(defmethod compose ((g free-group) a b)
  (assert (free-group-element-valid-p g a)
          (a)
          "The value A is not a valid element of the provided free group.")
  (assert (free-group-element-valid-p g b)
          (b)
          "The value A is not a valid element of the provided free group.")
  (make-free-group-element g a b))

(defmethod inverse ((g free-group) a)
  (etypecase a
    ;; a^-1 = -a
    (integer (- a))
    ;; (a b c)^-1 = (c^-1 b^-1 a^-1) 
    (list
     (canonicalize-free-group-element
      g
      (mapcar (lambda (e) (inverse g e))
              (reverse a))))))

(defmethod generators ((G free-group))
  (loop :for i :from 1 :to (num-generators G)
        :collect i))

(defmethod num-generators ((G free-group))
  (free-group-num-generators G))

;;; Word generation & simplification

(deftype word ()
  '(or fixnum list))

(declaim (ftype (function (t) (and fixnum unsigned-byte)) word-length))
(defun word-length (w)
  "What is the length of the word W?"
  (etypecase w
    (fixnum 1)
    (list (max 1 (length w)))))

(defun word-generator (group)
  "Return a lambda function taking a non-negative integer N and returning the Nth word in a sequence which enumerates all words of the free group GROUP."
  (check-type group free-group)
  (%word-generator (free-group-num-generators group)))

(defun %word-generator (num-generators)
  "Return a lambda function taking a non-negative integer N and returning the Nth word in a sequence which enumerates all words of NUM-GENERATORS generators."
  (let* ((b/2 num-generators)
         (b (* 2 b/2)))
    (labels ((process (x)
               (if (<= x b/2)
                   x
                   (- b/2 x)))
             (words-in-level (l)
               (expt b l))
             (words-below-level (l)
               (loop :for i :below l :sum (words-in-level i)))
             (find-level (n)
               (if (zerop n)
                   0
                   (loop :for l :from 0
                         :when (<= (words-below-level l)
                                   n
                                   (1- (words-below-level (1+ l))))
                           :do (return l))))
             (generate (n l)
               (let ((n (- n (words-below-level l))))
                 (loop :repeat l
                       :collect (multiple-value-bind (quo rem) (floor n b)
                                  (setf n quo)
                                  (process (1+ rem)))))))
      (lambda (n)
        (generate n (find-level n))))))

(defun word-simplifier (orders commuting)
  "Let:

    ORDERS: A vector of orders of each generator, or NIL if unknown/infinite.

    COMMUTING: a vector of length N+1 where the Jth element is a list of all commuting generators of J

Then return a unary function which takes elements of G and returns simplified versions of them."
  (check-type orders vector)
  (check-type commuting vector)
  (assert (= (length orders) (length commuting)))
  (assert (plusp (length orders)))
  (assert (null (aref orders 0)))
  (assert (null (aref commuting 0)))
  (lambda (w)
    (etypecase w
      (integer (if (zerop w) '() w))
      (list
       ;; Remove identities from W and make a copy.
       (let ((w (loop :for x :in w :unless (zerop x) :collect x)))
         (cond
           ;; Identity or singleton: return it.
           ((or (endp w) (endp (rest w))) w)
           ;; Arbitrary word.
           (t
            ;; Get rid of identities.
            (let ((simplified-word nil))
              (labels ((simplify-power (pt count)
                         ;; Simplify PT^COUNT for PT > 0.
                         ;;
                         ;; Return either:
                         ;;
                         ;;     - NIL; it simplifies to identity.
                         ;;
                         ;;     - N; the simplest representation is PT^N for 1 <= N < ORDER.
                         (let ((order (aref orders pt)))
                           (if (null order)
                               (values pt count)
                               (let ((count (mod count order)))
                                 (if (zerop count)
                                     nil
                                     count)))))
                       (push-point (point count)
                         ;; Push the POINT onto SIMPLIFIED-WORD a
                         ;; total of COUNT times. Take into account
                         ;; the known order to push inverses if
                         ;; necessary.
                         (let ((final (simplify-power point count))
                               (order (aref orders point)))
                              (when final
                                ;; Having a FINAL implies an ORDER.
                                (cond
                                  ((<= final (/ order 2))
                                   (loop :repeat final :do (push point simplified-word)))
                                  (t
                                   (loop :repeat (- order final) :do (push (- point) simplified-word)))))))
                       (sift-commuting (point w)
                         ;; Sift through the commuting elements at the
                         ;; head of W to POINT. Return two values:
                         ;;
                         ;;     1. The number of POINT's found (i.e.,
                         ;;     POINT^N) through the commuting
                         ;;     elements. Note that N can be negative.
                         ;;
                         ;;     2. The remaining W with the commuting
                         ;;     elements at the head.
                         (let* ((count (if (plusp point) 1 -1))
                                (point (abs point))
                                (comms (aref commuting point))
                                (commuters-found nil))
                           (loop :for wi :on w
                                 :for r := (abs (car wi))
                                 :while (or (= r point)
                                            (find r comms))
                                 :do (cond
                                       ;; We ran into an identical-ish point.
                                       ((= r point)
                                        (cond
                                          ((plusp (car wi))  (incf count))
                                          ((minusp (car wi)) (decf count))))
                                       ;; We ran into a commuter.
                                       (t
                                        (push (car wi) commuters-found)))
                                 :finally (return (values point count (nreconc commuters-found wi))))))
                       (simp (w)
                         ;; W is guaranteed to have no identities.
                         (cond
                           ;; Reached the end of the word. Return our answer.
                           ((endp w)
                            (nreverse simplified-word))
                           ;; Sift through commuting elements and do a
                           ;; run-length-encoding sort of thing.
                           (t
                            (multiple-value-bind (point count w-remaining)
                                (sift-commuting (first w) (rest w))
                              (push-point point count)
                              (simp w-remaining))))))
                ;; Remove identities, and simplify.
                (simp w))))))))))
