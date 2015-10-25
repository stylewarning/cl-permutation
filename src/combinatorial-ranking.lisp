;;;; combinatorial-ranking.lisp
;;;;
;;;; Copyright (c) 2011-2015 Robert Smith

(in-package #:cl-permutation)

;;; This code was originally written in Fortran 95 (in 2008), and was
;;; subsequently converted into Lisp as a part of the QSolve project
;;; (https://bitbucket.org/tarballs_are_good/qsolve). It has been
;;; merged into CL-PERMUTATION due to its mathematical generality. Its
;;; main structure remains the same, except CLOS is used instead of
;;; structures.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Utilities ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun zero-array (length)
  "Make an array of zeroes of length LENGTH."
  (make-array length :element-type 'unsigned-byte
                     :initial-element 0))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Structures ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass combinatorial-spec ()
  ((cardinality-cache :initform nil
                      :accessor cardinality-cache)
   (size :initarg :size
         :reader size))
  (:documentation "Abstract class representing linear sequences of objects of size SIZE."))

(defclass radix-spec (combinatorial-spec)
  ((radix :initarg :radix
          :accessor radix.radix))
  (:documentation "Representation of a sequence of numbers of length SIZE whose elements are between 0 and RADIX - 1."))

(defclass permutation-spec (combinatorial-spec)
  ()
  (:documentation "Representation of a permutation of size SIZE."))

(defclass combination-spec (combinatorial-spec)
  ((zero-count :initarg :zero-count
               :accessor comb.zero-count))
  (:documentation "Representation of a sequence "))

(defclass multi-spec (combinatorial-spec)
  ((types :initarg :types
          :accessor multi.types
          :documentation "Non-negative integer representing the number of distinct types within the permutation.")
   (type-counts :initarg :type-counts
                :accessor multi.type-counts
                :documentation "Vector of integers representing the count of each individual type. (The sum of this array should equal TYPES.)"))
  (:documentation "Representation of a permutation with repeated elements."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Cardinality ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric cardinality (spec)
  (:documentation "Compute the cardinality of SPEC."))

(defmethod cardinality :around ((spec combinatorial-spec))
  (or (cardinality-cache spec)
      (setf (cardinality-cache spec)
            (call-next-method))))

(defmethod cardinality ((spec radix-spec))
  (expt (radix.radix spec) (size spec)))

(defmethod cardinality ((spec permutation-spec))
  (alexandria:factorial (size spec)))

(defmethod cardinality ((spec combination-spec))
  (alexandria:binomial-coefficient (size spec) (comb.zero-count spec)))

(defmethod cardinality ((spec multi-spec))
  ;; NOTE: We could use a MAP/REDUCE here.
  (let ((p (alexandria:factorial (size spec))))
    (loop :with type-counts := (multi.type-counts spec)
          :for i :below (multi.types spec)
          :do (setf p (floor p (alexandria:factorial (aref type-counts i))))
          :finally (return p))))


;;;;;;;;;;;;;;;;;;;;;;;;;;; Initialization ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun array-for-spec (spec)
  (zero-array (size spec)))

(defun array-to-permutation-spec (perm)
  "Given an array PERM, convert it to a permutation spec."
  ;; TODO: Ensure correctness of perm.
  (make-instance 'permutation-spec :size (length perm)))

(defun array-to-radix-spec (radixset radix)
  "Given a radix RADIX, construct a RADIX-SPEC from the array RADIXSET."
  (make-instance 'radix-spec :size (length radixset)
                             :radix radix))

(defun array-to-multi-spec (multiset)
  ;;; TODO: optimize this whole thing.
  (let ((spec (make-instance 'multi-spec :size (length multiset)))
        (last 0)
        (types 1))

    ;; Count the types
    ;;
    ;; NOTE: Could use (LENGTH (REMOVE-DUPLICATES ...))
    ;;
    ;; XXX : Do type counting here.
    (loop :for x :across (sort (copy-seq multiset) #'<)
          :when (/= last x)
            :do (progn
                  (setf last x)
                  (incf types)))

    ;; Set appropriate slots
    (setf (multi.types spec) types
          (multi.type-counts spec) (zero-array types))

    ;; Initialize the type-counts array
    ;;
    ;; XXX: This is O(n^2)
    (dotimes (i types)
      (setf (aref (multi.type-counts spec) i)
            (count i multiset)))

    ;; Return the set.
    spec))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Ranking ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric rank (spec set)
  (:documentation "Rank the set SET to an integer according to the spec SPEC."))

(defmethod rank ((spec radix-spec) set)
  (let ((radix (radix.radix spec)))
    (reduce #'(lambda (next sum)
                (+ (* sum radix)
                   next))
            set
            :initial-value 0
            :from-end t)))

(defmethod rank ((spec permutation-spec) set)
  (let ((rank 0))
    (loop :for i :from 0 :below (1- (size spec))
          :do (progn
                (setf rank (* rank (- (size spec) i)))
                ;; XXX: Use COUNT.
                (loop :for j :from i :below (size spec)
                      :when (> (aref set i)
                               (aref set j))
                        :do (incf rank))))
    ;; Return the rank
    rank))

(defmethod rank ((spec combination-spec) set)
  (let ((z    (comb.zero-count spec))
        (rank 0))
    (loop :for i :from (1- (size spec)) :downto 0
          :when (zerop (aref set i))
            :do (progn
                  (incf rank (binomial-coefficient-or-zero i z))
                  (decf z))
          :finally (return rank))))

(defmethod rank ((spec multi-spec) set)
  (let ((size                    (size spec))
        (current-cardinality     (cardinality spec))
        (unprocessed-type-counts (copy-seq (multi.type-counts spec)))
        (rank                    0))
    (loop :for current-position :below (1- size)
          :while (< 1 current-cardinality)
          :do (let ((current-offset 0)
                    (current-type (aref set current-position))
                    (length-remaining (- size current-position)))

                ;; Compute the offset
                ;;
                ;; XXX: This can be maintained in an auxiliary data
                ;;      structure and updated incrementally.
                (dotimes (i current-type)
                  (incf current-offset (aref unprocessed-type-counts i)))

                ;; Update the rank
                (incf rank (floor (* current-cardinality current-offset)
                                  length-remaining))

                ;; This is guaranteeed to decrease in size, because
                ;; the count of the current type <= LENGTH-REMAINING/
                (setf current-cardinality
                      (floor (* current-cardinality
                                (aref unprocessed-type-counts current-type))
                             length-remaining))

                ;; Account for the type which we've processed.
                (decf (aref unprocessed-type-counts current-type))))

    ;; Return the rank
    rank))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Unranking ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric unrank (spec idx)
  (:documentation "Unrank the integer rank IDX according to SPEC."))

;;; TODO: Clean this up.
;;;
;;; XXX: This can be made more efficient by precomputing the size of
;;; the unranked index with logs.
(defmethod unrank ((spec radix-spec) (idx integer))
  (let ((radix (radix.radix spec))
        (set (array-for-spec spec)))
    (dotimes (i (size spec) set)
      (multiple-value-bind (quo rem) (floor idx radix)
        (setf (aref set i) rem
              idx quo)))))

(defmethod unrank ((spec permutation-spec) (idx integer))
  (let ((size (size spec))
        (set (array-for-spec spec)))
    ;; (setf (aref set (1- size)) 0)

    (loop
      :for i :from (- size 2) :downto 0
      :do (progn
            (setf (aref set i) (mod idx (- size i)))
            (setf idx (floor idx (- size i)))
            (loop :for j :from (1+ i) :to (1- (size spec))
                  :when (>= (aref set j)
                            (aref set  i))
                    :do (incf (aref set j))))
      :finally (return set))))

(defmethod unrank ((spec combination-spec) (idx integer))
  (let ((z (comb.zero-count spec))
        (set (array-for-spec spec)))
    ;; Inefficient to create the array then update all of its values.
    (map-into set (constantly 1) set)
    (loop :for i :from (1- (size spec)) :downto 0
          :do (let ((tmp (binomial-coefficient-or-zero i z)))
                (when (>= idx tmp)
                  (decf idx tmp)
                  (setf (aref set i) 0)
                  (decf z)))
          :finally (return set))))

(defmethod unrank ((spec multi-spec) (idx integer))
  (let* ((set                     (array-for-spec spec))
         (size                    (size spec))
         (unprocessed-type-counts (copy-seq (multi.type-counts spec)))
         (current-cardinality     (cardinality spec)))
    (dotimes (current-position size set)
      (let ((length-remaining (- size current-position))
            (current-offset 0)
            (current-type 0))
        ;; Compute the next type, as well as the offset to adjust the
        ;; index.
        (loop
          ;; SELECTOR could be a standard division, resulting in a
          ;; rational number. However, since we are using it to
          ;; check an inequality (namely >=), we can floor it to
          ;; keep in the domain of integers.
          :with selector := (floor (* idx length-remaining) current-cardinality)
          :while (>= selector (+ current-offset
                                 (aref unprocessed-type-counts current-type)))
          :do (incf current-offset (aref unprocessed-type-counts current-type))
              (incf current-type))

        ;; This will divide evenly.
        (decf idx (/ (* current-cardinality current-offset) length-remaining))
        
        (assert (integerp idx))

        ;; This will divide evenly.
        (setf current-cardinality
              (/ (* current-cardinality (aref unprocessed-type-counts current-type))
                 length-remaining))
        
        (assert (integerp current-cardinality))

        (decf (aref unprocessed-type-counts current-type))

        (setf (aref set current-position) current-type)))))


;;; Enumeration of all sets
;;;
;;; This function is mostly for testing purposes.

(defun map-spec (f spec)
  "Call the function F across all elements described by SPEC.

F should be a binary function whose first argument represents the rank of object passed as the second argument."
  (dotimes (i (cardinality spec))
    (funcall f i (unrank spec i))))

(defun enumerate-all (spec)
  "Given the set (used as a model), enumerate all possible sets."
  (map-spec (lambda (rank obj)
              (let ((calculated-rank (rank spec obj)))
                (assert (= rank calculated-rank) nil "Mismatch in ranking/unranking ~A" rank)
                (format t "~D ==> ~A ==> ~D~%"
                        rank
                        obj
                        calculated-rank)))
            spec))
