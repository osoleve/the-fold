;;; basic-math.ss --- Arithmetic and Numeric Operations
;;;
;;; The Fold supports standard Scheme numeric operations.
;;;
;;; Run with: scheme --script docs/tutorials/examples/getting-started/basic-math.ss

;;; ====
;;; Basic Arithmetic
;;; ====

(display "=== Basic Arithmetic ===\n")

(format #t "2 + 3 = ~a~%" (+ 2 3))
(format #t "10 - 4 = ~a~%" (- 10 4))
(format #t "6 * 7 = ~a~%" (* 6 7))
(format #t "15 / 3 = ~a~%" (/ 15 3))

;;; Variadic: these operations take any number of arguments
(format #t "(+ 1 2 3 4 5) = ~a~%" (+ 1 2 3 4 5))
(format #t "(* 2 3 4) = ~a~%" (* 2 3 4))

;;; ====
;;; Integer Operations
;;; ====

(display "\n=== Integer Operations ===\n")

;;; quotient and remainder for integer division
(format #t "17 quotient 5 = ~a~%" (quotient 17 5))
(format #t "17 remainder 5 = ~a~%" (remainder 17 5))
(format #t "17 modulo 5 = ~a~%" (modulo 17 5))

;;; ====
;;; Floating Point
;;; ====

(display "\n=== Floating Point ===\n")

(format #t "3.14 * 2 = ~a~%" (* 3.14 2))
(format #t "1.0 / 3.0 = ~a~%" (/ 1.0 3.0))

;;; ====
;;; Mathematical Functions
;;; ====

(display "\n=== Mathematical Functions ===\n")

(format #t "sqrt(16) = ~a~%" (sqrt 16))
(format #t "expt(2, 10) = ~a~%" (expt 2 10))
(format #t "abs(-42) = ~a~%" (abs -42))
(format #t "max(3, 7, 2) = ~a~%" (max 3 7 2))
(format #t "min(3, 7, 2) = ~a~%" (min 3 7 2))

;;; ====
;;; Comparisons
;;; ====

(display "\n=== Comparisons ===\n")

(format #t "5 < 10 = ~a~%" (< 5 10))
(format #t "5 > 10 = ~a~%" (> 5 10))
(format #t "5 = 5 = ~a~%" (= 5 5))
(format #t "5 <= 5 = ~a~%" (<= 5 5))

;;; Chained comparisons
(format #t "1 < 2 < 3 = ~a~%" (< 1 2 3))

;;; ====
;;; Exact vs Inexact
;;; ====

(display "\n=== Exact vs Inexact ===\n")

;;; Scheme distinguishes exact (rational) from inexact (floating point)
(format #t "exact? 1/3 = ~a~%" (exact? 1/3))
(format #t "inexact? 0.333 = ~a~%" (inexact? 0.333))
(format #t "1/3 as inexact = ~a~%" (inexact 1/3))

(display "\nDone!\n")
