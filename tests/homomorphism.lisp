;;;; tests/homomorphism.lisp
;;;;
;;;; Copyright (c) 2016 Robert Smith

(in-package #:cl-permutation-tests)

(defun convert-homomorphism (func-hom)
  (let ((preimage (homomorphism-preimage func-hom))
        (image    (homomorphism-image    func-hom)))
    (make-instance 'generator-homomorphism
                   :from-group preimage
                   :to-group image
                   :generator-map func-hom)))

(deftest randomly-test-generator-homomorphism-correctness-for-group (group count)
  (multiple-value-bind (groups homs)
      (subdirect-factors group)
    (declare (ignore groups))
    (let* ((func-hom (first homs))
           (gen-hom (convert-homomorphism func-hom)))
      (loop :repeat count
            :for r := (random-group-element group)
            :do (is (perm= (funcall func-hom r)
                           (funcall gen-hom r)))))))

(deftest test-generator-homomorphism-correctness ()
  (randomly-test-generator-homomorphism-correctness-for-group (make-rubik-3x3) 25)
  ;; too slow
  ;;(randomly-test-generator-homomorphism-correctness-for-group (make-megaminx) 1)
  )
