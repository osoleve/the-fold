;;; boundary/test-animation.ss — Tests for Animation and Easing
;;;
;;; Validates easing functions and animation utilities.

(load "boundary/ui/animation.ss")

(define test-count 0)
(define pass-count 0)
(define fail-count 0)

;;; Test assertion helper
(define (assert-equal name actual expected)
  (set! test-count (+ test-count 1))
  (if (equal? actual expected)
      (begin
       (set! pass-count (+ pass-count 1))
       (printf "  ✓ ~a\n" name))
      (begin
       (set! fail-count (+ fail-count 1))
       (printf "  ✗ ~a\n" name)
       (printf "    Expected: ~s\n" expected)
       (printf "    Actual:   ~s\n" actual))))

;;; Near assertion for floating point
(define (assert-near name actual expected epsilon)
  (set! test-count (+ test-count 1))
  (if (<= (abs (- actual expected)) epsilon)
      (begin
       (set! pass-count (+ pass-count 1))
       (printf "  ✓ ~a\n" name))
      (begin
       (set! fail-count (+ fail-count 1))
       (printf "  ✗ ~a\n" name)
       (printf "    Expected (near): ~s\n" expected)
       (printf "    Actual:         ~s\n" actual))))

;;; ====
;;; Linear Easing Tests
;;; ====

(printf "\n=== Testing linear ===\n")

(assert-near "linear 0.0" (linear 0.0) 0.0 0.0001)
(assert-near "linear 0.5" (linear 0.5) 0.5 0.0001)
(assert-near "linear 1.0" (linear 1.0) 1.0 0.0001)

;;; ====
;;; Quadratic Easing Tests
;;; ====

(printf "\n=== Testing quadratic ===\n")

(assert-near "ease-in-quad 0.0" (ease-in-quad 0.0) 0.0 0.0001)
(assert-near "ease-in-quad 0.5" (ease-in-quad 0.5) 0.25 0.0001)
(assert-near "ease-in-quad 1.0" (ease-in-quad 1.0) 1.0 0.0001)

(assert-near "ease-out-quad 0.0" (ease-out-quad 0.0) 0.0 0.0001)
(assert-near "ease-out-quad 0.5" (ease-out-quad 0.5) 0.75 0.0001)
(assert-near "ease-out-quad 1.0" (ease-out-quad 1.0) 1.0 0.0001)

(assert-near "ease-in-out-quad 0.0" (ease-in-out-quad 0.0) 0.0 0.0001)
(assert-near "ease-in-out-quad 0.5" (ease-in-out-quad 0.5) 0.5 0.0001)
(assert-near "ease-in-out-quad 1.0" (ease-in-out-quad 1.0) 1.0 0.0001)

;;; ====
;;; Animation Utilities Tests
;;; ====

(printf "\n=== Testing animation utilities ===\n")

(assert-near "animate linear" (animate linear 0.5 10 20) 15.0 0.0001)
(assert-near "animate ease-in-quad" (animate ease-in-quad 0.5 10 20) 12.5 0.0001)

(assert-near "animate-clamped below" (animate-clamped linear -0.5 10 20) 10.0 0.0001)
(assert-near "animate-clamped above" (animate-clamped linear 1.5 10 20) 20.0 0.0001)

(assert-near "frame->t 0" (frame->t 0 100) 0.0 0.0001)
(assert-near "frame->t 50" (frame->t 50 100) 0.5 0.0001)
(assert-near "frame->t 100" (frame->t 100 100) 1.0 0.0001)
(assert-near "frame->t overflow" (frame->t 150 100) 1.0 0.0001)

(assert-near "loop-t 0.0" (loop-t 0.0) 0.0 0.0001)
(assert-near "loop-t 0.25" (loop-t 0.25) 0.5 0.0001)
(assert-near "loop-t 0.5" (loop-t 0.5) 1.0 0.0001)
(assert-near "loop-t 0.75" (loop-t 0.75) 0.5 0.0001)
(assert-near "loop-t 1.0" (loop-t 1.0) 0.0 0.0001)

;;; ====
;;; Test Summary
;;; ====

(printf "\n====\n")
(printf "Test Results:\n")
(printf "  Total:  ~a\n" test-count)
(printf "  Passed: ~a\n" pass-count)
(printf "  Failed: ~a\n" fail-count)

(if (= fail-count 0)
    (printf "\n✓ All tests passed!\n\n")
    (printf "\n✗ Some tests failed\n\n"))

;;; Return success/failure
(= fail-count 0)
