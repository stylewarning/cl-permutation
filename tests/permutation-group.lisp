;;;; tests/permutation-group.lisp
;;;;
;;;; Copyright (c) 2015-2018 Robert Smith

(in-package #:cl-permutation-tests)

(deftest test-random-group-element-randomly ()
  "Test the generation of random group elements actually produces elements of the group."
  (loop :repeat 10
        :do (is (group-element-p (random-group-element *3x3*) *3x3*))))

(deftest test-group-orders ()
  "Test that the group order is being computed correctly for a few known examples."
  (is (= 120 (group-order (make-s5))))
  (is (= 3674160 (group-order *2x2*)))
  (is (= 43252003274489856000 (group-order *3x3*)))
  (is (= 100669616553523347122516032313645505168688116411019768627200000000000
         (group-order *mm*))))

(deftest test-subgroup-test ()
  "Test that SUBGROUP-P works."
  (let ((G (group-from-cycles (list
                               (list (make-cycle 1 2))
                               (list (make-cycle 2 3)))
                              4))
        (H (group-from-cycles (list
                               (list (make-cycle 1 3)))
                              4))
        (J (group-from-cycles (list
                               (list (make-cycle 1 4)))
                              4)))
    (is (subgroup-p G H))
    (is (not (subgroup-p G J)))))

(deftest test-normal-subgroup-test ()
  "Test that NORMAL-SUBGROUP-P works."
  (destructuring-bind (F R U B L D) (generators *3x3*)
    ;; trivial normal subgroups
    (is (normal-subgroup-p *3x3* *3x3*))
    (is (normal-subgroup-p *3x3* (generate-perm-group (list (group-identity *3x3*)))))
    ;; other tests
    ;;
    ;; TODO: FIXME
    #+fails
    (is (normal-subgroup-p *3x3* (generate-perm-group (list (perm-expt F 2) R U (perm-expt B 2) L D))))
    (is (not (normal-subgroup-p *3x3* (generate-perm-group (list F)))))))

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
  (loop :repeat 10 :do
    (test-transversal-decomposition *2x2* (random-group-element *2x2*))
    (test-transversal-decomposition *3x3* (random-group-element *3x3*))))

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
  (test-sigma-slps *2x2*)
  #+#:skip-test
  (test-sigma-slps *3x3*))

(deftest test-naive-generator-decomposition (group p)
  "Check that the perm P decomposes into generators within the perm group GROUP which reconstruct the perm. (Naive method.)"
  (let ((gens (naive-generator-decomposition p group :return-original-generators t)))
    (is (perm= p
               (reduce #'perm-compose
                       gens
                       :initial-value (group-identity group))))))

(deftest test-naive-generator-decomposition-randomly ()
  "Check veracity of generator decomposition of random elements of the 2x2 cube group. (Naive method.)"
  (loop :repeat 100 :do
    (test-naive-generator-decomposition *2x2* (random-group-element *2x2*))))

(deftest test-factorization-using-free-group (group p)
  "Test that P correctly factorizes as free group generators."
  (let* ((f (generator-decomposition p group :return-original-generators nil))
         (ϕ (perm::free-group->perm-group-homomorphism
             (perm::perm-group.free-group group) group)))
    (is (perm= p (funcall ϕ f)))))

(deftest test-generator-decomposition (group p)
  "Check that the perm P decomposes into generators within the perm group GROUP which reconstruct the perm."
  (let ((gens (generator-decomposition p group :return-original-generators t)))
    (is (perm= p
               (reduce #'perm-compose
                       gens
                       :initial-value (group-identity group))))))

(deftest test-generator-decomposition-randomly ()
  "Check veracity of generator decomposition of random elements of the 2x2 cube group."
  (loop :repeat 10
        :for r := (random-group-element *2x2*)
        :do (test-generator-decomposition *2x2* r)
            (test-factorization-using-free-group *2x2* r)))


