;;;; tests/orbit.lisp
;;;;
;;;; Copyright (c) 2015-2016 Robert Smith

(in-package #:cl-permutation-tests)

(deftest test-group-orbits ()
  "Test that GROUP-ORBITS counts the right number of orbits."
  (is (= 1 (length (perm:group-orbits *2x2*))))
  (is (= 2 (length (perm:group-orbits *3x3*))))
  (is (= 2 (length (perm:group-orbits *mm*))))
  (is (= 3 (length (perm:group-orbits (perm:generate-perm-group
                                       (list
                                        (perm:make-perm 2 1 3 4 5 6)
                                        (perm:make-perm 1 2 4 3 5 6)
                                        (perm:make-perm 1 2 3 4 6 5)))))))
  (is (= 1 (length (perm:group-orbits (perm-examples:make-bicycle 3)))))
  (is (= 1 (length (perm:group-orbits (perm-examples:make-bicycle-with-fixed-wheel 3))))))

(deftest test-rubik-subdirect-factor-count ()
  "Check that there are exactly two subdirect factors of the cube group."
  (let ((subdirect-factors (subdirect-factors *3x3*)))
    (is (= 2 (length subdirect-factors)))))
