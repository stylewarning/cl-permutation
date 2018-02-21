;;;; examples/rubik-like.lisp
;;;;
;;;; Copyright (c) 2014 Robert Smith

(in-package #:cl-permutation-examples)

;;; Order: 3674160
;;       +--+--+
;;       |5 |6 |
;;       +--+--+
;;       |7 |8 |
;; +--+--+--+--+--+--+--+--+
;; |19|20|1 |2 |9 |10|13|14|
;; +--+--+--+--+--+--+--+--+
;; |xx|21|3 |4 |11|12|15|xx|
;; +--+--+--+--+--+--+--+--+
;;       |16|17|
;;       +--+--+
;;       |xx|18|
;;       +--+--+

(defun make-rubik-2x2 ()
  (group-from-cycles
   (list (list (make-cycle 1 2 4 3)     ; F
               (make-cycle 7 9 17 21)
               (make-cycle 8 11 16 20))

         (list (make-cycle 9 10 12 11)  ; R
               (make-cycle 2 6 15 17)
               (make-cycle 4 8 13 18))

         (list (make-cycle 5 6 8 7)     ; U
               (make-cycle 2 20 14 10)
               (make-cycle 1 19 13 9)))
   21))

;;                +--------------+
;;                |              |
;;                |  1    2    3 |
;;                |              |
;;                |  4   up    5 |
;;                |              |
;;                |  6    7    8 |
;;                |              |
;; +--------------+--------------+--------------+--------------+
;; |              |              |              |              |
;; |  9   10   11 | 17   18   19 | 25   26   27 | 33   34   35 |
;; |              |              |              |              |
;; | 12  left  13 | 20 front  21 | 28 right  29 | 36  back  37 |
;; |              |              |              |              |
;; | 14   15   16 | 22   23   24 | 30   31   32 | 38   39   40 |
;; |              |              |              |              |
;; +--------------+--------------+--------------+--------------+
;;                |              |
;;                | 41   42   43 |
;;                |              |
;;                | 44  down  45 |
;;                |              |
;;                | 46   47   48 |
;;                |              |
;;                +--------------+

;; 43252003274489856000
(defun make-rubik-3x3 ()
  (group-from-cycles
   (list (list (make-cycle 18 21 23 20) ; F
               (make-cycle 17 19 24 22)
               (make-cycle 8 30 41 11)
               (make-cycle 7 28 42 13)
               (make-cycle 6 25 43 16))

         (list (make-cycle 26 29 31 28) ; R
               (make-cycle 25 27 32 30)
               (make-cycle 8 33 48 24)
               (make-cycle 5 36 45 21)
               (make-cycle 3 38 43 19))

         (list (make-cycle 11 35 27 19) ; U
               (make-cycle 10 34 26 18)
               (make-cycle 9 33 25 17)
               (make-cycle 2 5 7 4)
               (make-cycle 1 3 8 6))

         (list (make-cycle 34 37 39 36) ; B
               (make-cycle 33 35 40 38)
               (make-cycle 3 9 46 32)
               (make-cycle 2 12 47 29)
               (make-cycle 1 14 48 27))

         (list (make-cycle 10 13 15 12) ; L
               (make-cycle 9 11 16 14)
               (make-cycle 6 22 46 35)
               (make-cycle 4 20 44 37)
               (make-cycle 1 17 41 40))

         (list (make-cycle 42 45 47 44) ; D
               (make-cycle 41 43 48 46)
               (make-cycle 16 24 32 40)
               (make-cycle 15 23 31 39)
               (make-cycle 14 22 30 38)))
   48))                                 ; size

;;                +--------------+
;;                |              |
;;                |  1    2    3 |
;;                |              |
;;                |  4    5    6 |
;;                |              |
;;                |  7    8    9 |
;;                |              |
;; +--------------+--------------+--------------+--------------+
;; |              |              |              |              |
;; | 19   20   21 | 10   11   12 | 13   14   15 | 16   17   18 |
;; |              |              |              |              |
;; | **  left  ** | **   22   ** | ** right  ** | **   28   ** |
;; |              |              |              |              |
;; | **   **   ** | **   23   ** | **   **   ** | **   27   ** |
;; |              |              |              |              |
;; +--------------+--------------+--------------+--------------+
;;                |              |
;;                | **   24   ** |
;;                |              |
;;                | **   25   ** |
;;                |              |
;;                | **   26   ** |
;;                |              |
;;                +--------------+


(defun make-rubik-MU-group ()
  "Create the <M, U> group of the 3x3 cube."
  (group-from-cycles
   (list
    ;; U
    (list (make-cycle 1 3 9 7)
          (make-cycle 2 6 8 4)
          (make-cycle 11 20 17 14)
          (make-cycle 12 21 18 15)
          (make-cycle 10 19 16 13))
    ;; M'
    (list (make-cycle 24 11 2 27)
          (make-cycle 25 22 5 28)
          (make-cycle 26 23 8 17)))
   28))

;;; Doesn't work *as intended*
(defun make-rubik-4x4 ()
  (group-from-cycles
   (list
    (list (make-cycle 20 68 52 36)
          (make-cycle 19 67 51 35)
          (make-cycle 18 66 50 34)
          (make-cycle 17 65 49 33)
          (make-cycle 6 7 11 10)
          (make-cycle 3 12 14 5)
          (make-cycle 2 8 15 9)
          (make-cycle 1 4 16 13))
    (list (make-cycle 38 39 43 42)
          (make-cycle 35 44 46 37)
          (make-cycle 34 40 47 41)
          (make-cycle 33 36 48 45)
          (make-cycle 16 61 81 20)
          (make-cycle 15 57 82 24)
          (make-cycle 14 53 83 28)
          (make-cycle 13 49 84 32))
    (list (make-cycle 54 55 59 58)
          (make-cycle 51 60 62 53)
          (make-cycle 50 56 63 57)
          (make-cycle 49 52 64 61)
          (make-cycle 16 65 96 48)
          (make-cycle 12 69 92 44)
          (make-cycle 8 73 88 40)
          (make-cycle 4 77 84 36))
    (list (make-cycle 22 23 27 26)
          (make-cycle 19 28 30 21)
          (make-cycle 18 24 31 25)
          (make-cycle 17 20 32 29)
          (make-cycle 13 45 93 68)
          (make-cycle 9 41 89 72)
          (make-cycle 5 37 85 76)
          (make-cycle 1 33 81 80))
    (list (make-cycle 70 71 75 74)
          (make-cycle 67 76 78 69)
          (make-cycle 66 72 79 73)
          (make-cycle 65 68 80 77)
          (make-cycle 16 29 84 64)
          (make-cycle 12 25 88 60)
          (make-cycle 8 21 92 56)
          (make-cycle 4 17 96 52))
    (list (make-cycle 86 87 91 90)
          (make-cycle 83 92 94 85)
          (make-cycle 82 88 95 89)
          (make-cycle 81 84 96 93)
          (make-cycle 32 48 64 80)
          (make-cycle 31 47 63 79)
          (make-cycle 30 46 62 78)
          (make-cycle 29 45 61 77))
    (list (make-cycle 24 72 56 40)
          (make-cycle 23 71 55 39)
          (make-cycle 22 70 54 38)
          (make-cycle 21 69 53 37))
    (list (make-cycle 12 62 85 19)
          (make-cycle 11 58 86 23)
          (make-cycle 10 54 87 27)
          (make-cycle 9 50 88 31))
    (list (make-cycle 15 66 95 47)
          (make-cycle 11 70 91 43)
          (make-cycle 7 74 87 39)
          (make-cycle 3 78 83 35))
    (list (make-cycle 14 46 94 67)
          (make-cycle 10 44 90 71)
          (make-cycle 6 38 86 75)
          (make-cycle 2 34 82 79))
    (list (make-cycle 8 18 89 63)
          (make-cycle 7 22 90 58)
          (make-cycle 6 26 91 55)
          (make-cycle 5 30 92 51))
    (list (make-cycle 28 44 60 76)
          (make-cycle 27 43 59 75)
          (make-cycle 26 42 58 74)
          (make-cycle 25 41 57 73)))
   (* 6 4 4)))

(defun make-skewb ()
  (group-from-cycles
   (list
    (list (make-cycle  1 11 17)
          (make-cycle  2 12 20)
          (make-cycle  4 10 18)
          (make-cycle 22  6 14)
          (make-cycle 25 27 29))
    (list (make-cycle  2 10 22)
          (make-cycle  1  9 23)
          (make-cycle  3 11 21)
          (make-cycle 17  5 15)
          (make-cycle 25 27 30))
    (list (make-cycle  4 14 20)
          (make-cycle  1 15 19)
          (make-cycle  3 13 17)
          (make-cycle  7 11 23)
          (make-cycle 25 28 29))
    (list (make-cycle  6 12 18)
          (make-cycle  5 11 19)
          (make-cycle  7  9 17)
          (make-cycle 21  1 13)
          (make-cycle 26 27 29)))
   30))

;;; 100669616553523347122516032313645505168688116411019768627200000000000
(defun make-megaminx ()
  (group-from-cycles
   (list
    (list (make-cycle 1 9 7 5 3)
          (make-cycle 50 40 30 120 11)
          (make-cycle 52 42 32 22 13)
          (make-cycle 2 10 8 6 4)
          (make-cycle 51 41 31 21 12))

    (list (make-cycle 11 13 15 17 19)
          (make-cycle 3 120 72 62 54)
          (make-cycle 5 28 70 60 52)
          (make-cycle 12 14 16 18 20)
          (make-cycle 4 29 71 61 53))

    (list (make-cycle 120 22 24 26 28)
          (make-cycle 5 30 82 74 15)
          (make-cycle 7 38 80 72 13)
          (make-cycle 21 23 25 27 29)
          (make-cycle 6 39 81 73 14))

    (list (make-cycle 30 32 34 36 38)
          (make-cycle 7 40 92 84 24)
          (make-cycle 9 48 90 82 22)
          (make-cycle 31 33 35 37 39)
          (make-cycle 8 49 91 83 23))

    (list (make-cycle 40 42 44 46 48)
          (make-cycle 9 50 114 94 34)
          (make-cycle 1 58 112 92 32)
          (make-cycle 41 43 45 47 49)
          (make-cycle 10 59 113 93 33))

    (list (make-cycle 50 52 54 56 58)
          (make-cycle 1 11 60 116 44)
          (make-cycle 3 19 68 114 42)
          (make-cycle 51 53 55 57 59)
          (make-cycle 2 20 69 115 43))

    (list (make-cycle 60 62 64 66 68)
          (make-cycle 19 70 106 118 56)
          (make-cycle 17 78 104 116 54)
          (make-cycle 61 63 65 67 69)
          (make-cycle 18 79 105 117 55))

    (list (make-cycle 70 72 74 76 78)
          (make-cycle 17 28 80 108 64)
          (make-cycle 15 26 88 106 62)
          (make-cycle 71 73 75 77 79)
          (make-cycle 16 27 89 107 63))

    (list (make-cycle 80 82 84 86 88)
          (make-cycle 26 38 90 100 76)
          (make-cycle 24 36 98 108 74)
          (make-cycle 81 83 85 87 89)
          (make-cycle 25 37 99 109 75))

    (list (make-cycle 90 92 94 96 98)
          (make-cycle 36 48 112 102 86)
          (make-cycle 34 46 110 100 84)
          (make-cycle 91 93 95 97 99)
          (make-cycle 35 47 111 101 85))

    (list (make-cycle 100 102 104 106 108)
          (make-cycle 98 110 66 78 88)
          (make-cycle 96 118 64 76 86)
          (make-cycle 101 103 105 107 109)
          (make-cycle 97 119 65 77 87))

    (list (make-cycle 110 112 114 116 118)
          (make-cycle 96 46 58 68 104)
          (make-cycle 94 44 56 66 102)
          (make-cycle 111 113 115 117 119)
          (make-cycle 95 45 57 67 103)))
   (* 12 ; faces
      10 ; stickers per face
      )))
