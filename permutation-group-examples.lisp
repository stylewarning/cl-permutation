(in-package #:cl-permutation)

;;; 120
(defparameter S_5
  (group-from
   '((2 1 3 4 5)
     (1 3 2 4 5)
     (1 2 4 3 5)
     (1 2 3 5 4))))

;;; 244823040
(defparameter mathieu-m25
  (group-from
   '((16 7 4 17 1 6 11 23 22 10 19 2 14 5 3 8 9 18 20 24 15 21 13 12)
     (24 21 10 22 9 23 8 7 5 3 18 20 14 13 19 17 16 11 15 12 2 4 6 1))))

;;; 3674160
(defparameter rubik-2x2
  (group-from
   '((2 4 1 3 17 18 7 8 5 6 11 12 9 10 15 16 13 14 19 20 21 22 23 24) ; U
     (1 10 3 12 5 6 7 8 9 22 11 24 15 13 16 14 4 18 2 20 21 19 23 17) ; R
     (1 2 13 15 5 4 7 3 10 12 9 11 22 14 21 16 17 18 19 20 6 8 23 24) ; F
     ;; (1 2 3 4 5 6 11 12 9 10 15 16 13 14 19 20 17 18 7 8 22 24 21 23) ; D
     ;; (9 2 11 4 6 8 5 7 21 10 23 12 13 14 15 16 17 3 19 1 20 22 18 24) ; L
     ;; (14 16 3 4 2 6 1 8 9 10 11 12 13 24 15 23 19 17 20 18 21 22 5 7) ; B
     )))

;;; 43252003274489856000
(defparameter rubik-3x3
  (group-from
   '((3 5 8 2 7 1 4 6 33 34 35 12 13 14 15 16 9 10 11 20 21 22 23 24 17
      18 19 28 29 30 31 32 25 26 27 36 37 38 39 40 41 42 43 44 45 46 47 48)
     (17 2 3 20 5 22 7 8 11 13 16 10 15 9 12 14 41 18 19 44 21 46 23 24
      25 26 27 28 29 30 31 32 33 34 6 36 4 38 39 1 40 42 43 37 45 35 47 48)
     (1 2 3 4 5 25 28 30 9 10 8 12 7 14 15 6 19 21 24 18 23 17 20 22 43
      26 27 42 29 41 31 32 33 34 35 36 37 38 39 40 11 13 16 44 45 46 47 48)
     (1 2 38 4 36 6 7 33 9 10 11 12 13 14 15 16 17 18 3 20 5 22 23 8 27
      29 32 26 31 25 28 30 48 34 35 45 37 43 39 40 41 42 19 44 21 46 47 24)
     (14 12 9 4 5 6 7 8 46 10 11 47 13 48 15 16 17 18 19 20 21 22 23 24
      25 26 1 28 2 30 31 3 35 37 40 34 39 33 36 38 41 42 43 44 45 32 29 27)
     (1 2 3 4 5 6 7 8 9 10 11 12 13 22 23 24 17 18 19 20 21 30 31 32 25
      26 27 28 29 38 39 40 33 34 35 36 37 14 15 16 43 45 48 42 47 41 44 46))))

;;; Doesn't work *as intended*
(defparameter rubik-4x4
  (group-from
   '((4 8 12 16 3 7 11 15 2 6 10 14 1 5 9 13
     65 66 67 68 21 22 23 24 25 26 27 28 29 30 31 32
     17 18 19 20 37 38 39 40 41 42 43 44 45 46 47 48
     33 34 35 36 53 54 55 56 57 58 59 60 61 62 63 64
     49 50 51 52 69 70 71 72 73 74 75 76 77 78 79 80
     81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96)
    (1 2 3 4 5 6 7 8 9 10 11 12 49 53 57 61
     17 18 19 16 21 22 23 15 25 26 27 14 29 30 31 13
     36 40 44 48 35 39 43 47 34 38 42 46 33 37 41 45
     84 50 51 52 83 54 55 56 82 58 59 60 81 62 63 64
     65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80
     20 24 28 32 85 86 87 88 89 90 91 92 93 94 95 96)
    (1 2 3 77 5 6 7 73 9 10 11 69 13 14 15 65
     17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32
     33 34 35 4 37 38 39 8 41 42 43 12 45 46 47 16
     52 56 60 64 51 55 59 63 50 54 58 62 49 53 57 61
     96 66 67 68 92 70 71 72 88 74 75 76 84 78 79 80
     81 82 83 36 85 86 87 40 89 90 91 44 93 94 95 48)
    (33 2 3 4 37 6 7 8 41 10 11 12 45 14 15 16
     20 24 28 32 19 23 27 31 18 22 26 30 17 21 25 29
     81 34 35 36 85 38 39 40 89 42 43 44 93 46 47 48
     49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64
     65 66 67 13 69 70 71 9 73 74 75 5 77 78 79 1
     80 82 83 84 76 86 87 88 72 90 91 92 68 94 95 96)
    (1 2 3 17 5 6 7 21 9 10 11 25 13 14 15 29
     96 18 19 20 92 22 23 24 88 26 27 28 84 30 31 32
     33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48
     49 50 51 4 53 54 55 8 57 58 59 12 61 62 63 16
     68 72 76 80 67 71 75 79 66 70 74 78 65 69 73 77
     81 82 83 64 85 86 87 60 89 90 91 56 93 94 95 52)
    (1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16
     17 18 19 20 21 22 23 24 25 26 27 28 45 46 47 48
     33 34 35 36 37 38 39 40 41 42 43 44 61 62 63 64
     49 50 51 52 53 54 55 56 57 58 59 60 77 78 79 80
     65 66 67 68 69 70 71 72 73 74 75 76 29 30 31 32
     84 88 92 96 83 87 91 95 82 86 90 94 81 85 89 93)
    (1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16
     17 18 19 20 69 70 71 72 25 26 27 28 29 30 31 32
     33 34 35 36 21 22 23 24 41 42 43 44 45 46 47 48
     49 50 51 52 37 38 39 40 57 58 59 60 61 62 63 64
     65 66 67 68 53 54 55 56 73 74 75 76 77 78 79 80
     81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96)
    (1 2 3 4 5 6 7 8 50 54 58 62 13 14 15 16
     17 18 12 20 21 22 11 24 25 26 10 28 29 30 9 32
     33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48
     49 88 51 52 53 87 55 56 57 86 59 60 61 85 63 64
     65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80
     81 82 83 84 19 23 27 31 89 90 91 92 93 94 95 96)
    (1 2 78 4 5 6 74 8 9 10 70 12 13 14 66 16
     17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32
     33 34 3 36 37 38 7 40 41 42 11 44 45 46 15 48
     49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64
     65 95 67 68 69 91 71 72 73 87 75 76 77 83 79 80
     81 82 35 84 85 86 39 88 89 90 43 92 93 94 47 96)
    (1 34 3 4 5 38 7 8 9 44 11 12 13 46 15 16
     17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32
     33 82 35 36 37 86 39 40 41 42 43 90 45 94 47 48
     49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64
     65 66 14 68 69 70 10 72 73 74 6 76 77 78 2 80
     81 79 83 84 85 75 87 88 89 71 91 92 93 67 95 96)
    (1 2 3 4 30 26 22 18 9 10 11 12 13 14 15 16
     17 89 19 20 21 90 23 24 25 91 27 28 29 92 31 32
     33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48
     49 50 5 52 53 54 6 56 57 7 59 60 61 62 8 64
     65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80
     81 82 83 84 85 86 87 88 63 58 55 51 93 94 95 96)
    (1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16
     17 18 19 20 21 22 23 24 41 42 43 44 29 30 31 32
     33 34 35 36 37 38 39 40 57 58 59 60 45 46 47 48
     49 50 51 52 53 54 55 56 73 74 75 76 61 62 63 64
     65 66 67 68 69 70 71 72 25 26 27 28 77 78 79 80
     81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96))))

;;; 100669616553523347122516032313645505168688116411019768627200000000000
(defparameter megaminx
  (labels ((cyc (&rest cycles)
             (from-cycles cycles 200)))
    (generate-perm-group
     (list
      (cyc (make-cycle 1 9 7 5 3)
           (make-cycle 50 40 30 200 11)
           (make-cycle 52 42 32 22 13)
           (make-cycle 2 10 8 6 4)
           (make-cycle 51 41 31 21 12))
      (cyc (make-cycle 11 13 15 17 19)
           (make-cycle 3 200 72 62 54)
           (make-cycle 5 28 70 60 52)
           (make-cycle 12 14 16 18 20)
           (make-cycle 4 29 71 61 53))
      (cyc (make-cycle 200 22 24 26 28)
           (make-cycle 5 30 82 74 15)
           (make-cycle 7 38 80 72 13)
           (make-cycle 21 23 25 27 29)
           (make-cycle 6 39 81 73 14))
      (cyc (make-cycle 30 32 34 36 38)
           (make-cycle 7 40 92 84 24)
           (make-cycle 9 48 90 82 22)
           (make-cycle 31 33 35 37 39)
           (make-cycle 8 49 91 83 23))
      (cyc (make-cycle 40 42 44 46 48)
           (make-cycle 9 50 114 94 34)
           (make-cycle 1 58 112 92 32)
           (make-cycle 41 43 45 47 49)
           (make-cycle 10 59 113 93 33))
      (cyc (make-cycle 50 52 54 56 58)
           (make-cycle 1 11 60 116 44)
           (make-cycle 3 19 68 114 42)
           (make-cycle 51 53 55 57 59)
           (make-cycle 2 20 69 115 43))
      (cyc (make-cycle 60 62 64 66 68)
           (make-cycle 19 70 106 118 56)
           (make-cycle 17 78 104 116 54)
           (make-cycle 61 63 65 67 69)
           (make-cycle 18 79 105 117 55))
      (cyc (make-cycle 70 72 74 76 78)
           (make-cycle 17 28 80 108 64)
           (make-cycle 15 26 88 106 62)
           (make-cycle 71 73 75 77 79)
           (make-cycle 16 27 89 107 63))
      (cyc (make-cycle 80 82 84 86 88)
           (make-cycle 26 38 90 100 76)
           (make-cycle 24 36 98 108 74)
           (make-cycle 81 83 85 87 89)
           (make-cycle 25 37 99 109 75))
      (cyc (make-cycle 90 92 94 96 98)
           (make-cycle 36 48 112 102 86)
           (make-cycle 34 46 110 100 84)
           (make-cycle 91 93 95 97 99)
           (make-cycle 35 47 111 101 85))
      (cyc (make-cycle 100 102 104 106 108)
           (make-cycle 98 110 66 78 88)
           (make-cycle 96 118 64 76 86)
           (make-cycle 101 103 105 107 109)
           (make-cycle 97 119 65 77 87))
      (cyc (make-cycle 110 112 114 116 118)
           (make-cycle 96 46 58 68 104)
           (make-cycle 94 44 56 66 102)
           (make-cycle 111 113 115 117 119)
           (make-cycle 95 45 57 67 103))))))
