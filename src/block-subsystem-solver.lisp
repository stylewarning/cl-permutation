;;;; block-subsystem-solver.lisp
;;;;
;;;; Copyright (c) 2015-2023 Robert Smith

(in-package #:cl-permutation)

;;;; This file implements a "solver" using block subsystems as
;;;; heuristics.

(defun verbose ()
  (let ((last-time nil))
    (lambda (control &rest args)
      (let* ((current-time (get-internal-real-time))
             (elapsed-time-ms (if (null last-time)
                                  0
                                  (round (* 1000 (- current-time last-time))
                                         internal-time-units-per-second))))
        (format t "[~6D ms] " elapsed-time-ms)
        (apply #'format t control args)
        (terpri)
        (finish-output)
        (setf last-time (get-internal-real-time))
        nil))))

(defun subsystem-solver (group &key (verbose (verbose)))
  (let (subsystems xforms tables (num-gens (length (generators group))))
    (when verbose (funcall verbose "Computing subsystems"))
    (setf subsystems (group-block-subsystems group))

    (when verbose (funcall verbose "Computing coordinate transforms"))
    (setf xforms (loop :for subsys :in subsystems
                       :append (multiple-value-list (make-subsystem-transformations subsys))))
    (setf xforms (remove-if (lambda (x) (= 1 (coord.order x))) xforms))

    (when verbose (funcall verbose "Computing ~D God tables" (length xforms)))
    (loop :for i :from 1
          :for xform :in xforms
          :for order := (coord.order xform)
          :do
             (when (and verbose (> order 10000000))
               (unless (y-or-n-p "Attempting to compute table over ~
                                  10M (~D exactly). Continue?"
                                 order)
                 (return-from subsystem-solver nil)))
             (when verbose (funcall verbose "Computing ~:R God table of size ~D" i order))
             (push (compute-god-table group :rank-cardinality order :rank-element (coord.rank xform)) tables)
             (when verbose (funcall verbose "Done!")))
    (setf tables (reverse tables))

    (when verbose (funcall verbose "Computing IDDFS closure"))

    (labels ((coord (g)
               (loop :with c := (make-array (length tables) :element-type 'fixnum :initial-element 0)
                     :for i :from 0
                     :for xform :in xforms
                     :do (setf (aref c i) (funcall (coord.rank xform) g))
                     :finally (return c)))
             (transition-coord (coord gen-idx)
               (map 'simple-vector
                    (lambda (c tbl)
                      (aref (god-table-entry-transition
                             (aref (god-table-vector tbl) c))
                            gen-idx))
                    coord
                    tables))
             (dfs (coord depth moves)
               (cond
                 ;; Solved.
                 ((every #'zerop coord)
                  (values t moves))
                 ;; The solution must be of at least the depths specified by each
                 ;; pruning table.
                 ((or (zerop depth)
                      (loop :for tbl :in tables
                            :for idx :from 0
                            :for c :across coord
                            :for entry := (aref (god-table-vector tbl) c)
                            :do (when (null entry)
                                  (error "Couldn't compute depth for coord[~d]=~d. ~
                                          Coord xform is ~S"
                                         idx
                                         c
                                         (nth idx xforms)))
                            :thereis (< depth (god-table-entry-depth entry))))
                  (values nil nil))
                 ;; Descend. We use transition tables here to actually
                 ;; compute the map.
                 (t
                  (dotimes (i num-gens nil)
                    (let ((next-coord (transition-coord coord i)))
                      ;; coordᵢ ← (aref transᵢ coordᵢ mv)
                      (multiple-value-bind (found? solution)
                          (dfs next-coord (1- depth) (cons i moves))
                        (when found?
                          (return-from dfs (values t solution))))))))))
      (lambda (g)
        (block nil
          (format t "Searching: ")
          (loop :with coord := (coord g)
                :for depth :from 0
                :do (multiple-value-bind (found? solution)
                        (dfs coord depth nil)
                      (cond
                        (found? (format t "*") (return (reverse solution)))
                        (t      (format t "~D-" depth) (finish-output))))))))))

(defun solve-and-verify (group solver element &key (move-printer #'princ))
  (format t "~&Position: ~A~%" element)
  (let* ((start-time (get-internal-real-time))
         (solution (funcall solver element)))
    (format t "~&Solution found in ~D ms!~%"
            (round (* 1000 (- (get-internal-real-time) start-time))
                   internal-time-units-per-second))
    (let ((*print-pretty* nil))
      (format t "Solution (apply left-to-right): ")
      (dolist (mv solution)
        (funcall move-printer mv)
        (write-char #\Space))
      (terpri))
    (let ((solution*element (reduce #'perm-compose (reverse solution)
                                    :initial-value element
                                    :key (lambda (n) (nth n (generators group))))))


      (format t "Verification: ~:[Failed! solution*element=~A~;Success!~]"
              (perm-identity-p solution*element)
              solution*element)
      solution)))

;;; Solving second phase of Kociemba:
;;;
;;; (defparameter *g2 (perm-examples::make-kociemba-g2))
;;;
;;; (defparameter *solver (subsystem-solver *g2))
;;;
;;; (let ((r (random-group-element *g2))
;;;             (printer (lambda (i) (format t "~[U~;U2~;U'~;D~;D2~;D'~;R2~;L2~;F2~;B2~]" i))))
;;;         (solve-and-verify *g2 *solver r :move-printer printer))
