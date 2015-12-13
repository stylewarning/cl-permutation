;;;; tests/permutation-group.lisp
;;;;
;;;; Copyright (c) 2015 Robert Smith

(in-package #:cl-permutation-tests)

(in-suite cl-permutation-suite)

(deftest test-group-orders ()
  "Test that the group order is being computed correctly for a few known examples."
  (is (= 3674160 (group-order (make-rubik-2x2))))
  (is (= 43252003274489856000 (group-order (make-rubik-3x3))))
  #+#:skip-test
  (is (= 100669616553523347122516032313645505168688116411019768627200000000000
         (group-order (make-megaminx)))))

(deftest test-transversal-decomposition (group p)
  "Test that the transversal decomposition of the perm P can reconstruct the perm."
  (is (perm=*
       p
       (reduce #'perm-compose
               (transversal-decomposition p group :remove-identities t)
               :initial-value (group-identity group)
               :key (lambda (kj)
                      (perm::sigma
                       (perm::perm-group.transversal-system group)
                       (car kj)
                       (cdr kj)))))))

(deftest test-transversal-decomposition-randomly ()
  "Randomly test transversal decompositions."
  (let ((rubik2 (make-rubik-2x2))
        (rubik3 (make-rubik-3x3)))
    (loop :repeat 10 :do
      (test-transversal-decomposition rubik2 (random-group-element rubik2))
      (test-transversal-decomposition rubik3 (random-group-element rubik3)))))

(defun parse-sigma-symbol (s)
  "Given a sigma symbol like |SIGMA_(k,j)|, return two values, K and J. Error it's invalid."
  (let ((name (symbol-name s)))
    (when (null (search "SIGMA" name :test 'char=))
      (error "Invalid sigma symbol ~S" s))
    (values (parse-integer name
                           :start (1+ (position #\( name))
                           :end (position #\, name))
            (parse-integer name
                           :start (1+ (position #\, name))
                           :end (position #\) name)))))

(deftest test-sigma-slps (group)
  "Test that the sigma SLPs match the transversal system"
  (loop :with free-group := (perm::perm-group.free-group group)
        :with hom := (perm::free-group->perm-group-homomorphism
                       free-group
                       group)
        :with wrong := 0
        :with ctx := (perm::perm-group.slp-context group)
        :for count :from 0
        :for slp :being :the :hash-values :of (perm::symbol-table ctx)
          :using (hash-key sym)
        :when (keywordp sym) :do
          (multiple-value-bind (k j)
              (parse-sigma-symbol sym)
            (let ((found (perm::sigma (perm::perm-group.transversal-system group) k j))
                  (val (funcall hom (evaluate-slp free-group ctx slp))))
              (unless (perm=* val found)
                (incf wrong))))
        :finally (is (zerop wrong) "There were ~D wrong sigma SLPs." wrong)))

(deftest test-sigma-slps-for-rubik ()
  "Test that the sigma SLPs are sensible for the 2x2x2 and 3x3x3 Rubik's cubes."
  (test-sigma-slps (make-S5))
  (test-sigma-slps (make-rubik-2x2))
  #+#:skip-test
  (test-sigma-slps (make-rubik-3x3)))

(deftest test-generator-decomposition (group p)
  "Check that the perm P decomposes into generators within the perm group GROUP which reconstruct the perm."
  (let ((gens (generator-decomposition p group)))
    (is (perm= p
               (reduce #'perm-compose
                       gens
                       :initial-value (group-identity group))))))

(deftest test-generator-decomposition-randomly ()
  (let ((rubik (make-rubik-2x2)))
    (loop :repeat 10 :do
      (test-generator-decomposition rubik (random-group-element rubik)))))
