;;;; permutation.lisp
;;;; Copyright (c) 2012-2015 Robert Smith

(in-package #:cl-permutation)

;;;;;;;;;;;;;;;;;;;;; PERMUTATION DATA STRUCTURE ;;;;;;;;;;;;;;;;;;;;;

(defvar *print-with-perm-syntax* nil
  "Print permutations with special permutation syntax?")

(deftype perm-element ()
  "An element of a perm."
  ;; Since a perm can contain 1 to the maximum of VECTOR-INDEX, the
  ;; type is also that of a VECTOR-INDEX.
  'vector-index)

(deftype raw-perm ()
  "Type defining the internal representation of a perm."
  `(simple-array perm-element (*)))

(defstruct (perm (:conc-name perm.)
                 (:print-function print-perm)
                 (:constructor %make-perm))
  (rep (iota-vector 1) :type raw-perm))

;;; XXX: fix the duplication.
(defun print-perm (perm stream depth)
  "Printer for perms."
  (declare (ignore depth))
  (let* ((spec (perm.rep perm))
         (len (length spec)))
    (if *print-with-perm-syntax*
        (progn
          (format stream "#[")
          (cond
            ((zerop len) (error "Inconsistent permutation; has zero elements."))
            ((= 1 len) nil)
            ((= 2 len) (format stream "~D" (aref spec 1)))
            (t (progn
                 (format stream "~D" (aref spec 1))
                 (dotimes (i (- len 2))
                   (format stream " ~D" (aref spec (+ 2 i)))))))
          (format stream "]"))
        (print-unreadable-object (perm stream :type t :identity nil)
          (cond
            ((zerop len) (error "Inconsistent permutation; has zero elements."))
            ((= 1 len) nil)
            ((= 2 len) (format stream "~D" (aref spec 1)))
            (t (progn
                 (format stream "~D" (aref spec 1))
                 (dotimes (i (- len 2))
                   (format stream " ~D" (aref spec (+ 2 i)))))))))))

(defun contains-1-to-N (elements)
  "Check that ELEMENTS contains the integers between 1 and the length of the sequence, inclusive."
  (let ((len (length elements))
        (sum 0))
    (map nil (lambda (x) (incf sum x)) elements)
    (= sum (/ (* len (1+ len)) 2))))

(defun assert-valid-permutation-elements (elements)
  "Verify (via assertions) that the elements "
  (assert (every 'integerp elements)
          nil
          "Permutation must only have integers.")

  (assert (every 'plusp elements)
          nil
          "Permutation must contain positive numbers only.")

  (assert (contains-1-to-N elements)
          nil
          "Given permutation must contain the numbers 1 to ~A"
          (length elements)))

(defun perm-reader (stream char n)
  (declare (ignore char n))
  (let ((read-list (read-delimited-list #\] stream t)))
    (assert-valid-permutation-elements read-list)

    (%make-perm :rep (make-array (1+ (length read-list))
                                 :element-type 'perm-element
                                 :initial-contents (cons 0 read-list)))))

(defun enable-perm-reader ()
  "Enable the use of #[...] for perms."

  ;; Enable special printing
  (setf *print-with-perm-syntax* t)

  ;; Set #[
  (set-dispatch-macro-character #\# #\[ #'perm-reader)

  ;; Set ]
  (set-macro-character #\] (get-macro-character #\))))

(defun allocate-perm-vector (n)
  "Allocate a vector compatible with a size-N permutation."
  (check-type n (vector-size :down-by 1))
  (make-array (1+ n) :element-type 'perm-element
                     :initial-element 0))



;;;;;;;;;;;;;;;;;;;;;;; PERMUTATION OPERATIONS ;;;;;;;;;;;;;;;;;;;;;;;

(declaim (inline list-to-perm))
(defun list-to-perm (list)
  "Construct a perm from a list LIST."
  (assert-valid-permutation-elements list)
  (loop :with rep := (allocate-perm-vector (length list))
        :for i :from 1
        :for x :in list
        :do (setf (aref rep i) x)
        :finally (return (%make-perm :rep rep))))
(declaim (notinline list-to-perm))

(defun perm-to-list (perm)
  "Convert a permutation PERM to a list representation."
  (loop :for i :below (perm-size perm)
        :collect (perm-ref perm i)))

(defun vector-to-perm (word)
  "Convert a vector VECTOR to a permutation. VECTOR must represent a valid elements of a permutation."
  (assert-valid-permutation-elements word)
  (loop :with rep := (allocate-perm-vector (length word))
        :for i :from 1
        :for x :across word
        :do (setf (aref rep i) x)
        :finally (return (%make-perm :rep rep))))

(defun perm-to-vector (perm)
  "Convert a permutation PERM to a vector."
  (subseq (perm.rep perm) 1))

(defun make-perm (&rest elements)
  "Construct a permutation from the numbers ELEMENTS."
  (declare (dynamic-extent elements)
           (inline list-to-perm))
  (list-to-perm elements))

(defun perm-identity (n)
  "The identity permutation of size N."
  (%make-perm :rep (iota-vector (1+ n))))

(defun perm-identity-p (perm)
  "Is the permutation PERM an identity permutation?"
  (loop :with rep := (perm.rep perm)
        :for i :from 1 :below (length rep)
        :always (= i (aref rep i))))

(defun random-perm (n &optional (parity :any))
  "Make a random permutation of size N. PARITY specifies the parity of the permutation:

    * :ANY  for any permutation
    * :EVEN for only even permutations
    * :ODD  for only odd permutations"
  (%make-perm :rep (nshuffle (iota-vector (1+ n))
                             :parity parity
                             :start 1)))

(defun perm-ref (perm n)
  "Compute the zero-based index of PERM at N."
  (assert (<= 0 n (1- (perm-size perm)))
          (n)
          "Permutation reference index of ~D must be within the length of the ~
           permutation ~A."
          n perm)
  (aref (perm.rep perm) (1+ n)))

(defun perm-eval (perm n)
  "Evaluate the permutation PERM at index N."
  (assert (<= 1 n (perm-size perm))
          (n)
          "Permutation index of ~D must be within 1 and the length of the ~
           permutation ~A."
          n perm)
  (aref (perm.rep perm) n))

(defun perm-evaluator (perm)
  "Return an evaluation function for the permutation PERM (a la PERM-EVAL)."
  (lambda (n) (perm-eval perm n)))

(defun perm-eval* (perm n)
  "Evaluate the permutation PERM at index N. If N is larger than the size of the permutation, return the fixed point."
  (assert (<= 1 n)
          (n)
          "Permutation index of ~D must be greater than 1."
          n)
  (if (> n (perm-size perm))
      n
      (aref (perm.rep perm) n)))

(defun perm-evaluator* (perm)
  "Return an evaluation function for the permutation PERM (a la PERM-EVAL*)."
  (lambda (n) (perm-eval* perm n)))

(defun perm-inverse-eval (perm n)
  "Evaluate the inverse of the permutation PERM at index N."
  (assert (<= 1 n (perm-size perm))
          (n)
          "Permutation index of ~D must be within 1 and the length of the ~
           permutation ~A."
          n perm)
  (position n (perm.rep perm) :start 1))

(defun perm-inverse-eval* (perm n)
  "Evaluate the inverse of the permutation PERM at index N. If N is larger than the size of the permutation, return the fixed point."
  (assert (<= 1 n)
          (n)
          "Permutation index of ~D must be greater than 1."
          n)
  (if (> n (perm-size perm))
      n
      (position n (perm.rep perm) :start 1)))

(defun perm= (perm other-perm)
  "Are PERM and OTHER-PERM mathematically equal? (Note: Different sized perms are considered unequal. See PERM=* for extended equality.)"
  (let ((size (perm-size perm)))
    (and (= size (perm-size other-perm))
         (loop :for i :from 1 :to size
               :always (= (perm-eval perm i)
                          (perm-eval other-perm i))))))

(defun perm=* (perm other-perm)
  "Are PERM and OTHER-PERM mathematically equal when viewed as functions on naturals? (Note: For inequality on different sized perms, see PERM=.)"
  (loop :for i :from 1 :to (max (perm-size perm)
                                (perm-size other-perm))
        :always (= (perm-eval* perm i)
                   (perm-eval* other-perm i))))

(defun perm-size (perm)
  "The size of a permutation PERM."
  (1- (length (perm.rep perm))))

(defun perm-length (perm)
  "Count the number of inversions in the permutation PERM."
  (let ((n         (perm-size perm))
        (inv-count 0))
    (loop :for i :from 1 :to (1- n)
          :do (loop :for j :from (1+ i) :to n
                    :when (> (perm-eval perm i)
                             (perm-eval perm j))
                    :do (incf inv-count)))

    inv-count))

(defun perm-even-p (perm)
  "Is PERM an even permutation?"
  (evenp (perm-length perm)))

(defun perm-odd-p (perm)
  "Is PERM an odd permutation?"
  (oddp (perm-length perm)))

(defun perm-sign (perm)
  "The sign of a permutation PERM."
  (if (perm-even-p perm) 1 -1))

(declaim (inline %perm-compose-upto))
(defun %perm-compose-upto (p1 p2 n)
  ;; N limits the resulting perm size.
  (let* ((p12-spec (allocate-perm-vector n)))
    (loop :for i :from 1 :to n
          :do (setf (aref p12-spec i)
                    (perm-eval* p1 (perm-eval* p2 i)))
          :finally (return (%make-perm :rep p12-spec)))))
(declaim (notinline %perm-compose-upto))

(declaim (inline perm-compose))
(defun perm-compose (p1 p2)
  "Compose the permutations P1 and P2: x |-> P1(P2(x)).

Example: If P1 = 2 |-> 3 and P2 = 1 |-> 2 then (perm-compose P1 P2) = 1 |-> 3."
  (declare (inline %perm-compose-upto))
  (%perm-compose-upto p1 p2 (max (perm-size p1) (perm-size p2))))
(declaim (notinline perm-compose))

(defun perm-compose-flipped (p1 p2)
  "Compose the permutatons P1 and P2: x |-> P2(P1(x)). This is equivalent to (PERM-COMPOSE P2 P1)."
  (declare (inline perm-compose))
  (perm-compose p2 p1))

(defun perm-conjugate (p c)
  "Conjugate the permutation P by C. This is C P C^-1."
  (perm-compose c (perm-compose p (perm-inverse c))))

(defun perm-expt (perm n)
  "Raise a permutation PERM to the Nth power. If N is negative, then the inverse will be raised to the -Nth power."
  (check-type n integer)
  (labels ((rec (current-perm n)
             (if (= n 1)
                 current-perm
                 (rec (perm-compose perm current-perm) (1- n)))))
    (cond
      ((minusp n) (rec (perm-inverse perm) (- n)))
      ((zerop n)  (perm-identity (perm-size perm)))
      (t (rec perm n)))))

(defun perm-order (perm)
  "Compute the order of a permutation PERM. This is the non-negative power required to exponentiate it to identity."
  (labels ((rec (current-perm n)
             (if (perm-identity-p current-perm)
                 n
                 (rec (perm-compose perm current-perm) (1+ n)))))
    (rec perm 1)))

(defun perm-transpose-indexes (perm a b)
  "Transpose the indexes A and B in PERM."
  (assert (<= 1 a (perm-size perm))
          (a)
          "The first transposition index ~A must be in the range of ~
           the permutation."
          a)

  (assert (<= 1 b (perm-size perm))
          (b)
          "The second transposition index ~A must be in the range of ~
           the permutation."
          b)

  (let ((transposed-spec (copy-seq (perm.rep perm))))
    (rotatef (aref transposed-spec a)
             (aref transposed-spec b))
    (%make-perm :rep transposed-spec)))

(defun perm-transpose-entries (perm a b)
  "Transpose the entries A and B in PERM."
  (assert (<= 1 a (perm-size perm))
          (a)
          "The first transposition index ~A must be in the range of ~
           the permutation."
          a)

  (assert (<= 1 b (perm-size perm))
          (b)
          "The second transposition index ~A must be in the range of ~
           the permutation."
          b)

  (let* ((transposed-spec (copy-seq (perm.rep perm)))
         (pos-a (position a transposed-spec))
         (pos-b (position b transposed-spec)))
    (rotatef (aref transposed-spec pos-a)
             (aref transposed-spec pos-b))
    (%make-perm :rep transposed-spec)))

(defun perm-inverse (perm)
  "Find the inverse of the permutation PERM."
  (let* ((n          (perm-size perm))
         (perm*-spec (allocate-perm-vector n)))
    (loop :for i :from 1 :to n
          :do (setf (aref perm*-spec (perm-eval perm i)) i)
          :finally (return (%make-perm :rep perm*-spec)))))

(defun perm-point-fixed-p (perm k)
  "Is K fixed in the perm PERM?"
  (= k (perm-eval* perm k)))

(defun perm-last-non-fixpoint (perm)
  "Find the last non-fixed point of the perm PERM. If it exists, return the index A and the point B as two values. These satisfy

    (PERM-EVAL PERM A) = B

If a fixed point doesn't exist, return NIL."
  (let ((size (perm-size perm)))
    (loop :for i :from size :downto 1
          :for x := (perm-eval perm i)
          :when (/= i x)
            :do (return (values i x))
          :finally (return nil))))

;;; This can be a bit more optimized. We can just look at the internal
;;; representation.
(defun perm-fixpoints (perm &optional (n (perm-size perm)))
  "Return a list of the fixed points in PERM less than or equal to N, which is the perm's size by default."
  (check-type n (integer 1))
  (loop :for i :from 1 :to n
        :when (= i (perm-eval* perm i))
          :collect i))

(defun permute (perm a &key type)
  "Permute the sequence A according to PERM. The return an array by default unless TYPE is specified."
  (assert (<= (perm-size perm)
              (length a))
          (perm a)
          "Only able to permute arrays whose size is greater or equal to PERM.")

  (let* ((len (length a))
         (result (make-array len)))
    (loop :for i :below len
          :do (setf (aref result i)
                    (elt a (1- (perm-eval* perm (1+ i)))))
          :finally (return (if type
                               (coerce result type)
                               (coerce result (type-of a)))))))

(defun permuter (from to &key (test 'eql))
  "Compute a permutation P such that (PERMUTE P FROM) == TO."
  (loop :for x :in from
        :collect (1+ (position x to :test test)) :into m
        :finally (return (apply #'make-perm m))))

(defun commutesp (a b)
  "Do the permutations A and B commute?"
  (perm= (perm-compose a b)
         (perm-compose b a)))

; HI!!!
; HELLO


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; CYCLES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *canonicalize-cycle-on-creation* t)

(deftype cycle-element ()
  "An element of a cycle."
  `perm-element)

(defstruct (cycle (:constructor %make-cycle)
                  (:print-function print-cycle))
  (canonicalized nil :type boolean)
  (rep #() :type (vector cycle-element)))

(defun print-cycle (cycle stream depth)
  "Printer for cycles.

An asterisk in printed syntax denotes that the cycle has not been canonicalized (though it may be already be canonical)."
  (declare (ignore depth))
  (let* ((spec (cycle-rep cycle))
         (len (length spec)))
    (print-unreadable-object (cycle stream :type t :identity nil)
      (write-string "(" stream)
      (cond
        ((zerop len) nil)
        ((= 1 len) (format stream "~D" (aref spec 0)))
        (t (progn
             (format stream "~D" (aref spec 0))
             (dotimes (i (1- len))
               (format stream " ~D" (aref spec (1+ i)))))))
      (write-string ")" stream)
      (unless (cycle-canonicalized cycle)
        (write-string "*" stream)))))

(defun check-cycle-elements (elements)
  "Ensure that the elements ELEMENTS are those of a valid cycle."
  (assert (or (null elements)
              (every (lambda (x)
                       (typep x 'cycle-element))
                     elements))
          ()
          "Elements of a cycle must be positive integers acceptable to MAKE-PERM.")

  ;;; XXX: This can be done much more efficiently.
  (assert (= (length elements)
             (length (remove-duplicates elements)))
          ()
          "There must be no duplicate elements in a cycle."))

(defun make-cycle (&rest elements)
  "Create a new cycle with the elements ELEMENTS."
  (check-cycle-elements elements)

  (let ((cycle (%make-cycle :rep (make-array (length elements)
                                             :element-type 'cycle-element
                                             :initial-contents elements))))
    (if *canonicalize-cycle-on-creation*
        (canonicalize-cycle cycle)
        cycle)))

(defun cycle-length (cycle)
  "Compute the length of the cycle CYCLE."
  (length (cycle-rep cycle)))

(defun cycle-identity-p (cycle)
  "Is the cycle CYCLE representative of an identity permutation?"
  (let ((len (cycle-length cycle)))
    (or (zerop len)
        (= 1 len))))

(defun cycle-ref (cycle n)
  "Compute the Nth element of the cycle CYCLE. Treat the cycle as if it is circular (so indexes greater than the cycle length or less than zero will wrap around)."
  (aref (cycle-rep cycle)
        (mod n (cycle-length cycle))))

(defun orbit-length (n perm)
  "Compute the length of the orbit of the element N in the permutation PERM."
  (loop :for i :from 1
        :for k := (perm-eval perm n) :then (perm-eval perm k)
        :until (= n k)
        :finally (return i)))

(defun map-orbit (f n perm)
  "Given a unary function F, apply it to each element of the orbit of N within the perm PERM."
  ;; N will be the first element of the orbit.
  (funcall f n)
  ;; Call F on the rest of the orbit.
  (loop :with len := (orbit-length n perm)
        :for i :from 1 :below len
        :for k := (perm-eval perm n) :then (perm-eval perm k)
        :until (= n k)
        :do (funcall f k)))

(defun orbit-of (n perm)
  "Compute the orbit of the element N in the permutation PERM. Return a cycle representing the orbit of N."
  (let ((spec (make-array (orbit-length n perm) :element-type 'cycle-element
                                                :initial-element n))
        (i 0))
    (flet ((assign (k)
             (setf (aref spec i) k)
             (incf i)))
      ;; Assign all elements of the orbit.
      (map-orbit #'assign n perm)
      ;; Return the representative cycle.
      (%make-cycle :rep spec))))

(defun rotate-vector! (vec n)
  "Rotate the vector VEC a total of N elements left/counterclockwise in-place. If N is negative, rotate in the opposite direction."
  ;; This is written in such a way that it takes O(n) time and O(1)
  ;; space. This uses a technique where the array is divided into
  ;; blocks equal to the length of the GCD of N and the length of the
  ;; vector VEC. Rotated elements will move between these blocks
  ;; accordingly.
  (let* ((len (length vec))
         (n   (mod n len)))
    (dotimes (i (gcd n len) vec)
      (let ((tmp (aref vec i))
            (j   i))
        (loop :named juggle :do
          (let ((k (+ j n)))
            (when (>= k len)
              (decf k len))
            (when (= k i)
              (return-from juggle nil))
            (setf (aref vec j) (aref vec k)
                  j            k)))
        (setf (aref vec j) tmp)))))

(defun rotate-cycle (cycle &optional (n 1))
  "Rotate the elements of a cycle CYCLE syntactically counterclockwise/left, a total of N times. When N is negative, rotate in the opposite direction. Return a fresh cycle."
  (%make-cycle :rep (rotate-vector! (copy-seq (cycle-rep cycle))
                                    n)))

(defun canonicalize-cycle (cycle)
  "Rotate a cycle CYCLE so its least value is syntactically first."
  (cond
    ((cycle-canonicalized cycle) cycle)
    ((cycle-identity-p cycle) (setf (cycle-canonicalized cycle) t)
                              cycle)
    (t (let* ((minimum (reduce #'min (cycle-rep cycle)))
              (canonicalized-cycle (rotate-cycle cycle
                                                 (position minimum
                                                           (cycle-rep cycle)))))
         (setf (cycle-canonicalized canonicalized-cycle) t)
         canonicalized-cycle))))

#+#:ignore
(defun old-canonicalize-cycles (cycles)
  "Canonicalize each cycle in the list of cycles CYCLES, then canonicalize the list of cycles in descending length (or if the length is the same, ascending first element)."
  (sort (mapcar #'canonicalize-cycle
                (remove-if #'cycle-identity-p cycles))
        (lambda (x y)
          (let ((lenx (cycle-length x))
                (leny (cycle-length y)))
            (if (= lenx leny)
                (< (cycle-ref x 0)
                   (cycle-ref y 0))
                (> lenx leny))))))

(defun canonicalize-cycles (cycles)
  "Canonicalize each cycle in the list of cycles CYCLES, then canonicalize the list of cycles in descending value of the first position of the cycle."
  (sort (mapcar #'canonicalize-cycle
                (remove-if (lambda (cycle)
                             (zerop (cycle-length cycle)))
                           cycles))
        #'>
        :key (lambda (cycle) (cycle-ref cycle 0))))

;;; TODO: Make this efficient.
(defun to-cycles (perm &key (canonicalizep t))
  "Convert a permutation PERM in its standard representation to its cycle representation."
  (labels ((next-cycle (todo cycles)
             (if (null todo)
                 cycles
                 (let ((new-cycle (orbit-of (car todo) perm)))
                   (next-cycle (set-difference todo
                                               (coerce (cycle-rep new-cycle)
                                                       'list))
                               (cons new-cycle cycles))))))
    (let ((cycles (next-cycle (iota+1 (perm-size perm)) nil)))
      (if canonicalizep
          (canonicalize-cycles cycles)
          cycles))))

(defun map-cycle-mappings (f cycle &key omit-last)
  "Apply a binary function F to all pairs (a_i, b_i) such that the cycle is the composition of a_i |-> b_i.

If OMIT-LAST is T, then the last mapping will be omitted. For example, for the cycle (P1 P2 ... Pn), the mapping Pn |-> P1 will be excluded."
  (loop :for i :below  (if omit-last
                           (1- (cycle-length (canonicalize-cycle cycle)))
                           (cycle-length (canonicalize-cycle cycle)))
        :do (funcall f
                     (cycle-ref cycle i)
                     (cycle-ref cycle (1+ i)))))

(defun from-cycles (cycles &optional (size 0))
  "Convert a cycle representation of a permutation CYCLES to the standard representation.

SIZE is ignored if it is less than the maximum point within the cycles."
  (let* ((maximum (max size (loop :for cycle :in cycles
                                  :maximize (loop :for i :across (cycle-rep cycle)
                                                  :maximize i))))
         (perm (iota-vector (1+ maximum))))
    (dolist (cycle cycles (%make-perm :rep perm))
      (map-cycle-mappings (lambda (from to)
                            (rotatef (aref perm from)
                                     (aref perm to)))
                          cycle
                          :omit-last t))))

(defun cycle-type (perm)
  "Compute the cycle type of a perm PERM.

The cycle type is a partition of the perm's size, and is equal to the lengths of the cycles in descending order of the perm's cycle decomposition."
  (sort (mapcar #'cycle-length (to-cycles perm :canonicalizep nil))
        #'>))

(defun cycles-to-one-line (cycles)
  "Convert CYCLES to one-line notation. This is the same as flattening the cycles.

Note: This is not the same as FROM-CYCLES."
  (let ((elts (loop :for cycle :in cycles
                    :append (loop :for element :across (cycle-rep cycle)
                                  :collect element))))
    (assert-valid-permutation-elements elts)
    elts))

(defun find-conjugator (x y)
  "Find an element that conjugates X to Y. In other words, find the permutation C such that

    Y = C X C^-1."
  (let ((x-cycles (sort (to-cycles x :canonicalizep nil) #'> :key #'cycle-length))
        (y-cycles (sort (to-cycles y :canonicalizep nil) #'> :key #'cycle-length)))
    (cond
      ((or (not (= (length x-cycles) (length y-cycles)))
           (not (every (lambda (cx cy) (= (cycle-length cx) (cycle-length cy)))
                       x-cycles
                       y-cycles)))
       nil)
      (t
       (let ((x1 (cycles-to-one-line x-cycles))
             (y1 (cycles-to-one-line y-cycles)))
         (list-to-perm (mapcar #'cdr (sort (mapcar #'cons x1 y1) #'< :key #'car))))))))
