;;;; extra-functions.lisp
;;;; Copyright (c) 2012 Robert Smith

;;; This file contains extra or experimental functions which are not
;;; officially a part of the API.

(in-package #:cl-permutation)

(defun perm-extend (perm &optional (n 1))
  "Extend a permutation PERM"
  (assert (plusp n))
  (perm-compose (perm-identity (+ (perm-size perm) n))
                perm))

(defun last-to-position (size new-pos)
  "Create a permutation that will permute the last element of a
permutation of size SIZE to the position NEW-POS."
  (loop :for i :from size :downto new-pos
        :collect i :into cycle
        :finally (return (from-cycles (list cycle) size))))

(defun perm-inject (perm inject-to)
  "For a permutation PERM of size N, inject N+1 to the position
INJECT-TO."
  (perm-compose (perm-extend perm)
                (last-to-position (1+ (perm-size perm))
                                  inject-to)))

(defun perm-eject (perm)
  "Remove the largest element of a permutation."
  ;; Here, we are going to do it the ugly way.
  ;;
  ;; TODO FIXME: make better
  (let* ((size (perm-size perm))
         (new-spec (remove size (perm.rep perm))))
    (%make-perm new-spec)))
