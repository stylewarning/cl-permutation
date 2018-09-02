;;;; src/right-transversal.lisp
;;;;
;;;; Copyright (c) 2018 Robert Smith

(in-package #:cl-permutation)

(defclass right-cosets ()
  ((subgroup :initarg :subgroup
             :reader coset-subgroup)
   (group :initarg :group
          :reader coset-group)))

(defun make-right-cosets (H G)
  (assert (subgroup-p G H))
  (make-instance 'right-cosets :subgroup H :group G))

(defun same-coset-p (x y cosets)
  (group-element-p (perm-compose (perm-inverse x) y)
                   (coset-subgroup cosets)))

(defun num-cosets (cosets)
  (/ (group-order (coset-group cosets))
     (group-order (coset-subgroup cosets))))

(defun right-transversal (cosets)
  (let* ((n (num-cosets cosets))
         (G (coset-group cosets))
         (H (coset-subgroup cosets))
         (representatives (list (group-identity G))))
    (format t "Computing ~D transversals of ~A~%" n cosets)
    (labels ((random-G ()
               (let ((e (random-group-element G)))
                 (if (group-element-p e H)
                     (random-G)
                     e)))
             (already-seen? (x)
               (dolist (rep representatives nil)
                 (when (group-element-p (perm-compose rep x) H)
                   (return t)))
               #+ig
               (member x representatives :test (lambda (x y)
                                                 (same-coset-p x y cosets)))))
      (loop :with last-time := (get-internal-real-time)
            :with found := 1
            :for tried :from 0
            :while (< found n)
            :for r := (random-G)
            :do (when (zerop (mod tried 1000))
                  (format t "Tried ~D, found ~D (~2,2F%), dt=~D ms~%"
                          tried
                          found
                          (* 100.0 (/ found n))
                          (round (* 1000 (- (get-internal-real-time) last-time))
                                 internal-time-units-per-second))
                  (setf last-time (get-internal-real-time))
                  (finish-output))
            :unless (already-seen? r)
              :do (progn
                    (push (perm-inverse r) representatives)
                    (incf found))
            :finally (return (mapcar #'perm-inverse representatives))))))

(defun thistlethwaite ()
  (let ((3x3 (perm-examples:make-rubik-3x3)))
    (destructuring-bind (F R U B L D)
        (generators 3x3)
      (destructuring-bind (F2 R2 U2 B2 L2 D2)
          (mapcar (lambda (g) (perm-expt g 2)) (generators 3x3))
        (let ((thistle-0 (generate-perm-group (list F R U B L D)))
              (thistle-1 (generate-perm-group (list F R U2 B L D2)))
              (thistle-2 (generate-perm-group (list F2 R U2 B2 L D2)))
              (thistle-3 (generate-perm-group (list F2 R2 U2 B2 L2 D2)))
              (thistle-4 (generate-perm-group (list (group-identity 3x3)))))
          (let ((coset1/0 (make-right-cosets thistle-1 thistle-0))
                (coset2/1 (make-right-cosets thistle-2 thistle-1))
                (coset3/2 (make-right-cosets thistle-3 thistle-2))
                (coset4/3 (make-right-cosets thistle-4 thistle-3)))
            (list :G1/G0 (length (right-transversal coset1/0))
                  :G2/G1 (length (right-transversal coset2/1))
                  :G3/G2 (length (right-transversal coset3/2))
                  :G4/G3 (length (right-transversal coset4/3)))))))))
