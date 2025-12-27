;;; Tests for easing functions

(import (chezscheme)
        (shell easing))

;; Test linear interpolation
(assert (= (lerp 0 100 0.0) 0))
(assert (= (lerp 0 100 0.5) 50))
(assert (= (lerp 0 100 1.0) 100))

;; Test that easing functions are in valid range
(define (test-easing-range name ease-fn)
  (let loop ([i 0])
    (when (<= i 100)
      (let* ([t (/ i 100.0)]
             [result (ease-fn t)])
        (assert (<= 0 result))
        (assert (<= result 1))
        (loop (+ i 1))))))

(test-easing-range "linear" ease-linear)
(test-easing-range "in-quad" ease-in-quad)
(test-easing-range "out-quad" ease-out-quad)
(test-easing-range "in-out-quad" ease-in-out-quad)
(test-easing-range "in-cubic" ease-in-cubic)
(test-easing-range "out-cubic" ease-out-cubic)
(test-easing-range "in-out-cubic" ease-in-out-cubic)
(test-easing-range "in-sine" ease-in-sine)
(test-easing-range "out-sine" ease-out-sine)
(test-easing-range "in-out-sine" ease-in-out-sine)

;; Test boundary conditions
(assert (= (ease-in-quad 0) 0))
(assert (= (ease-in-quad 1) 1))
(assert (= (ease-out-quad 0) 0))
(assert (= (ease-out-quad 1) 1))

;; Test interpolation with easing
(let ([result (interpolate 0 100 0.5 ease-in-quad)])
  (assert (= result 25))) ; ease-in-quad at 0.5 is 0.25

(display "All easing tests passed!\n")
