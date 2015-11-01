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

(defun binomial-coefficient-or-zero (n k)
  "If N < K, return 0, otherwise return the binomial coefficient."
  (if (< n k)
      0
      (alexandria:binomial-coefficient n k)))

(defun zero-array (length)
  "Make an array of zeroes of length LENGTH."
  (make-array length :element-type 'unsigned-byte
                     :initial-element 0))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Structures ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass combinatorial-spec ()
  ((cardinality-cache :initform nil
                      :accessor cardinality-cache
                      :type (or null unsigned-byte))
   (size :initarg :size
         :reader size))
  (:documentation "Abstract class representing linear sequences of objects of size SIZE."))

(defclass radix-spec (combinatorial-spec)
  ((radix :initarg :radix
          :reader radix))
  (:documentation "Representation of a sequence of numbers of length SIZE whose elements are between 0 and RADIX - 1."))

(defclass mixed-radix-spec (combinatorial-spec)
  ((radix :initarg :radix
          :reader radix))
  (:documentation "Representation of a mixed-radix number of size SIZE with mixed radix RADIX."))

(defclass perm-spec (combinatorial-spec)
  ()
  (:documentation "Representation of a perm of size SIZE."))

(defclass combination-spec (combinatorial-spec)
  ((zero-count :initarg :zero-count
               :reader comb.zero-count))
  (:documentation "Representation of a sequence "))

(defclass word-spec (combinatorial-spec)
  ((types :initarg :types
          :reader word.types
          :documentation "Non-negative integer representing the number of distinct elements within the word. Note that this will include the additional zero type, even though there are never any zero elements.")
   (type-counts :initarg :type-counts
                :reader word.type-counts
                :documentation "Vector of non-negative integers representing the count of each individual element type. (The sum of this vector should equal TYPES.)"))
  (:documentation "Representation of a word of elements 1 to TYPES."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Cardinality ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric cardinality (spec)
  (:documentation "Compute the cardinality of SPEC. This represents the total number of elements described by the spec."))

;;; Cache the computed cardinality. These objects are intended to be
;;; immutable at the API boundary.
(defmethod cardinality :around ((spec combinatorial-spec))
  (or (cardinality-cache spec)
      (setf (cardinality-cache spec)
            (call-next-method))))

(defmethod cardinality ((spec radix-spec))
  ;; RADIX^SIZE
  (expt (radix spec) (size spec)))

(defmethod cardinality ((spec mixed-radix-spec))
  ;; RADIX1 * RADIX2 * ... * RADIXn
  (reduce (lambda (a b) (* a b))
          (radix spec)
          :initial-value 1))

(defmethod cardinality ((spec perm-spec))
  ;; (SIZE)!
  (alexandria:factorial (size spec)))

(defmethod cardinality ((spec combination-spec))
  ;; C(SIZE, ZEROES)
  (alexandria:binomial-coefficient (size spec) (comb.zero-count spec)))

(defmethod cardinality ((spec word-spec))
  ;;          (SIZE)!
  ;; --------------------------
  ;;  (C_1)! (C_2)! ... (C_N)!
  (reduce (lambda (p type-count)
            ;; This will always divide evenly.
            (floor p (alexandria:factorial type-count)))
          (word.type-counts spec)
          :initial-value (alexandria:factorial (size spec))))


;;;;;;;;;;;;;;;;;;;;;;;;;;; Initialization ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun array-for-spec (spec &key (initial-element 0))
  (make-array (size spec) :initial-element initial-element))

(defun make-perm-spec (n)
  "Make a PERM-SPEC representing the set of permutations S_n."
  (check-type n unsigned-byte)
  (make-instance 'perm-spec :size n))

(defun make-radix-spec (radix size)
  "Make a RADIX-SPEC representing all numbers between 0 and RADIX^SIZE - 1."
  (check-type radix (integer 2))
  (check-type size unsigned-byte)
  (make-instance 'radix-spec :size size :radix radix))

(defun make-mixed-radix-spec (radix)
  "Make a MIXED-RADIX-SPEC representing all mixed-radix numbers specified by the vector RADIX."
  (check-type radix vector)
  (assert (every (lambda (x) (<= 2 x)) radix) (radix) "The radix must be a vector of numbers larger than 2.")
  (make-instance 'mixed-radix-spec :radix radix
                                   :size (length radix)))

(defun vector-to-word-spec (word)
  "WORD should be a vector containing 1, 2, ..., N, possibly with repeated elements."
  (let* ((size (length word))
         (sorted (sort (copy-seq word) #'<))
         ;; We have a type for '0', even though its count should be 0,
         ;; hence the "1+".
         (types (1+ (aref sorted (1- size))))
         (type-counts (zero-array types)))

    (loop :for x :across sorted
          :do (incf (aref type-counts x)))

    (make-instance 'word-spec :size size
                              :types types
                              :type-counts type-counts)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Ranking ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric rank (spec set)
  (:documentation "Rank the set SET to an integer according to the spec SPEC."))

(defmethod rank ((spec radix-spec) set)
  (let ((radix (radix spec)))
    ;; Horner's method.
    (reduce (lambda (next sum)
              (+ next (* sum radix)))
            set
            :initial-value 0
            :from-end t)))

(defmethod rank ((spec mixed-radix-spec) set)
  (let ((radix (radix spec))
        (i (size spec)))
    ;; Horner's method, generalized for mixed radix numerals.
    (reduce (lambda (next sum)
              (+ next (* sum (aref radix (decf i)))))
            set
            :initial-value 0
            :from-end t)))

(defmethod rank ((spec perm-spec) set)
  (loop :with size := (size spec)
        :with rank := 0
        :for i :from 0 :below (1- size)
        :for elt :across set
        :do (let ((inversions (count-if (lambda (elt-after)
                                          (> elt elt-after))
                                        set
                                        :start (1+ i))))
              (setf rank (+ inversions (* rank (- size i)))))
        :finally (return rank)))

(defmethod rank ((spec combination-spec) set)
  (loop :with z := (comb.zero-count spec)
        :with rank := 0
        :for i :from (1- (size spec)) :downto 0
        :when (zerop (aref set i))
          :do (progn
                (incf rank (binomial-coefficient-or-zero i z))
                (decf z))
        :finally (return rank)))

(defmethod rank ((spec word-spec) set)
  (let ((size                    (size spec))
        (current-cardinality     (cardinality spec))
        (unprocessed-type-counts (copy-seq (word.type-counts spec)))
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
                ;; the count of the current type <= LENGTH-REMAINING.
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

(defmethod unrank ((spec radix-spec) (idx integer))
  (let ((radix (radix spec))
        (set (array-for-spec spec)))
    (dotimes (i (size spec) set)
      (multiple-value-bind (quo rem) (floor idx radix)
        (setf (aref set i) rem
              idx quo)))))

(defmethod unrank ((spec mixed-radix-spec) (idx integer))
  (let ((radix (radix spec))
        (set (array-for-spec spec)))
    (dotimes (i (size spec) set)
      (multiple-value-bind (quo rem) (floor idx (svref radix i))
        (setf (aref set i) rem
              idx quo)))))

(defmethod unrank ((spec perm-spec) (idx integer))
  (let ((size (size spec))
        (set (array-for-spec spec)))
    (loop
      :for i :from (- size 2) :downto 0
      :do (progn
            (setf (aref set i) (mod idx (- size i)))
            (setf idx (floor idx (- size i)))
            (loop :for j :from (1+ i) :to (1- (size spec))
                  :when (>= (aref set j)
                            (aref set i))
                    :do (incf (aref set j))))
      :finally (return set))))

(defmethod unrank ((spec combination-spec) (idx integer))
  (let ((z (comb.zero-count spec))
        (set (array-for-spec spec :initial-element 1)))
    (loop :for i :from (1- (size spec)) :downto 0
          :do (let ((tmp (binomial-coefficient-or-zero i z)))
                (when (>= idx tmp)
                  (decf idx tmp)
                  (setf (aref set i) 0)
                  (decf z)))
          :finally (return set))))

(defmethod unrank ((spec word-spec) (idx integer))
  (let* ((set                     (array-for-spec spec))
         (size                    (size spec))
         (unprocessed-type-counts (copy-seq (word.type-counts spec)))
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
