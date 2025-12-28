;;; Tests for graphics primitives

(import (chezscheme)
        (shell layout)
        (shell easing)
        (shell graphics-primitives))

;;; ============================================================
;;; Helper Functions for Testing
;;; ============================================================

;;; test-canvas-has-char : Canvas × Nat × Nat × Char → Bool
;;; Check if canvas has expected character at position
(define (test-canvas-has-char canvas x y expected-ch)
  (char=? (canvas-ref canvas x y) expected-ch))

;;; count-chars : Canvas × Char → Nat
;;; Count occurrences of a character in canvas
(define (count-chars canvas ch)
  (let ([w (canvas-width canvas)]
        [h (canvas-height canvas)])
    (let loop-y ([y 0] [count 0])
      (if (>= y h)
          count
          (let loop-x ([x 0] [count count])
            (if (>= x w)
                (loop-y (+ y 1) count)
                (loop-x (+ x 1)
                       (if (char=? (canvas-ref canvas x y) ch)
                           (+ count 1)
                           count))))))))

;;; print-canvas : Canvas → Void
;;; Helper to visualize canvas for debugging
(define (print-canvas canvas)
  (display (canvas->string canvas))
  (newline))

;;; ============================================================
;;; Test Character Intensity Palettes
;;; ============================================================

(display "Testing character intensity palettes...\n")

;; Test palette lengths
(assert (= (length intensity-palette-simple) 10))
(assert (= (length intensity-palette-blocks) 5))
(assert (= (length intensity-palette-dots) 6))

;; Test get-intensity-char boundaries
(assert (char=? (get-intensity-char intensity-palette-simple 0.0) #\space))
(assert (char=? (get-intensity-char intensity-palette-simple 1.0) #\@))

;; Test clamping
(assert (char=? (get-intensity-char intensity-palette-simple -0.5) #\space))
(assert (char=? (get-intensity-char intensity-palette-simple 1.5) #\@))

;; Test mid-range
(let ([mid-char (get-intensity-char intensity-palette-simple 0.5)])
  (assert (char? mid-char)))

(display "  ✓ Character intensity palettes work correctly\n")

;;; ============================================================
;;; Test Line Drawing
;;; ============================================================

(display "Testing line drawing...\n")

;; Test horizontal line
(let* ([canvas (make-canvas 10 5)]
       [start (point 1 2)]
       [end (point 8 2)]
       [result (draw-line canvas start end #\-)])
  (assert (test-canvas-has-char result 1 2 #\-))
  (assert (test-canvas-has-char result 5 2 #\-))
  (assert (test-canvas-has-char result 8 2 #\-))
  (assert (char=? (canvas-ref result 1 1) #\space)))

;; Test vertical line
(let* ([canvas (make-canvas 10 10)]
       [start (point 5 2)]
       [end (point 5 7)]
       [result (draw-line canvas start end #\|)])
  (assert (test-canvas-has-char result 5 2 #\|))
  (assert (test-canvas-has-char result 5 5 #\|))
  (assert (test-canvas-has-char result 5 7 #\|)))

;; Test diagonal line
(let* ([canvas (make-canvas 10 10)]
       [start (point 1 1)]
       [end (point 5 5)]
       [result (draw-line canvas start end #\*)])
  (assert (test-canvas-has-char result 1 1 #\*))
  (assert (test-canvas-has-char result 3 3 #\*))
  (assert (test-canvas-has-char result 5 5 #\*)))

;; Test single point line
(let* ([canvas (make-canvas 10 10)]
       [start (point 3 3)]
       [end (point 3 3)]
       [result (draw-line canvas start end #\X)])
  (assert (test-canvas-has-char result 3 3 #\X)))

(display "  ✓ Line drawing works correctly\n")

;;; ============================================================
;;; Test Line Gradient
;;; ============================================================

(display "Testing gradient line drawing...\n")

;; Test that gradient line uses palette characters
(let* ([canvas (make-canvas 20 5)]
       [start (point 1 2)]
       [end (point 18 2)]
       [palette '(#\. #\: #\# #\@)]
       [result (draw-line-gradient canvas start end palette)])
  ;; Should have characters from the palette
  (assert (> (count-chars result #\.) 0))
  (assert (> (count-chars result #\@) 0)))

(display "  ✓ Gradient line drawing works correctly\n")

;;; ============================================================
;;; Test Rounded Box Drawing
;;; ============================================================

(display "Testing rounded box drawing...\n")

;; Test ASCII rounded box
(let* ([canvas (make-canvas 10 6)]
       [rect (make-rect (point 1 1) 8 4)]
       [result (draw-rounded-box canvas rect 'ascii)])
  ;; Check corners
  (assert (test-canvas-has-char result 1 1 #\/))
  (assert (test-canvas-has-char result 8 1 #\\))
  (assert (test-canvas-has-char result 1 4 #\\))
  (assert (test-canvas-has-char result 8 4 #\/))
  ;; Check edges
  (assert (test-canvas-has-char result 2 1 #\-))
  (assert (test-canvas-has-char result 1 2 #\|)))

;; Test light rounded box
(let* ([canvas (make-canvas 10 6)]
       [rect (make-rect (point 1 1) 8 4)]
       [result (draw-rounded-box canvas rect 'light)])
  ;; Check corners (light rounded)
  (assert (test-canvas-has-char result 1 1 #\╭))
  (assert (test-canvas-has-char result 8 1 #\╮))
  (assert (test-canvas-has-char result 1 4 #\╰))
  (assert (test-canvas-has-char result 8 4 #\╯)))

;; Test small box (should still work)
(let* ([canvas (make-canvas 5 5)]
       [rect (make-rect (point 0 0) 3 3)]
       [result (draw-rounded-box canvas rect 'ascii)])
  (assert (> (count-chars result #\/) 0)))

(display "  ✓ Rounded box drawing works correctly\n")

;;; ============================================================
;;; Test Circle Drawing
;;; ============================================================

(display "Testing circle drawing...\n")

;; Test circle outline
(let* ([canvas (make-canvas 20 20)]
       [center (point 10 10)]
       [radius 5]
       [result (draw-circle canvas center radius #\o)])
  ;; Should have circle characters
  (assert (> (count-chars result #\o) 0))
  ;; Center should be empty
  (assert (char=? (canvas-ref result 10 10) #\space)))

;; Test filled circle
(let* ([canvas (make-canvas 20 20)]
       [center (point 10 10)]
       [radius 5]
       [result (draw-filled-circle canvas center radius #\*)])
  ;; Should have many filled characters
  (assert (> (count-chars result #\*) 20))
  ;; Center should be filled
  (assert (test-canvas-has-char result 10 10 #\*)))

;; Test small circle
(let* ([canvas (make-canvas 10 10)]
       [center (point 5 5)]
       [radius 2]
       [result (draw-circle canvas center radius #\O)])
  (assert (> (count-chars result #\O) 0)))

(display "  ✓ Circle drawing works correctly\n")

;;; ============================================================
;;; Test Gradient Fills
;;; ============================================================

(display "Testing gradient fills...\n")

;; Test horizontal gradient
(let* ([canvas (make-canvas 20 10)]
       [rect (make-rect (point 2 2) 16 6)]
       [palette intensity-palette-simple]
       [result (gradient-fill-horizontal canvas rect 0.0 1.0 ease-linear palette)])
  ;; Should have characters from palette
  (assert (> (count-chars result #\space) 0))
  (assert (> (count-chars result #\@) 0))
  ;; Left side should be darker (space)
  (assert (char=? (canvas-ref result 2 4) #\space))
  ;; Right side should be brighter
  (let ([right-char (canvas-ref result 17 4)])
    (assert (not (char=? right-char #\space)))))

;; Test vertical gradient
(let* ([canvas (make-canvas 20 20)]
       [rect (make-rect (point 2 2) 16 16)]
       [palette intensity-palette-blocks]
       [result (gradient-fill-vertical canvas rect 0.0 1.0 ease-linear palette)])
  ;; Top should be lighter than bottom with our intensity scale
  (assert (> (count-chars result #\space) 0)))

;; Test vertical gradient with easing
(let* ([canvas (make-canvas 20 20)]
       [rect (make-rect (point 2 2) 16 16)]
       [palette intensity-palette-simple]
       [result (gradient-fill-vertical canvas rect 0.0 1.0 ease-in-quad palette)])
  ;; Should create non-linear gradient
  (assert (> (count-chars result #\space) 0)))

;; Test radial gradient
(let* ([canvas (make-canvas 30 30)]
       [center (point 15 15)]
       [radius 10]
       [palette intensity-palette-simple]
       [result (gradient-fill-radial canvas center radius palette ease-linear)])
  ;; Center should be brightest
  (let ([center-char (canvas-ref result 15 15)])
    (assert (not (char=? center-char #\space))))
  ;; Should have gradient characters
  (assert (> (count-chars result #\@) 0)))

;; Test radial gradient with easing
(let* ([canvas (make-canvas 30 30)]
       [center (point 15 15)]
       [radius 10]
       [palette intensity-palette-blocks]
       [result (gradient-fill-radial canvas center radius palette ease-out-quad)])
  ;; Center should be filled
  (assert (not (char=? (canvas-ref result 15 15) #\space))))

;; Test general gradient-fill function
(let* ([canvas (make-canvas 20 10)]
       [rect (make-rect (point 2 2) 16 6)]
       [palette intensity-palette-simple]
       [result (gradient-fill canvas rect 0.0 1.0 ease-linear palette 'horizontal)])
  (assert (> (count-chars result #\@) 0)))

(display "  ✓ Gradient fills work correctly\n")

;;; ============================================================
;;; Test Integration with Easing
;;; ============================================================

(display "Testing integration with easing functions...\n")

;; Test gradient with different easing functions
(let* ([canvas (make-canvas 40 10)]
       [rect (make-rect (point 1 1) 38 8)]
       [palette intensity-palette-simple])
  ;; Linear easing
  (let ([result (gradient-fill-horizontal canvas rect 0.0 1.0 ease-linear palette)])
    (assert (> (count-chars result #\@) 0)))
  ;; Ease-in
  (let ([result (gradient-fill-horizontal canvas rect 0.0 1.0 ease-in-cubic palette)])
    (assert (> (count-chars result #\@) 0)))
  ;; Ease-out
  (let ([result (gradient-fill-horizontal canvas rect 0.0 1.0 ease-out-cubic palette)])
    (assert (> (count-chars result #\@) 0)))
  ;; Ease-in-out
  (let ([result (gradient-fill-horizontal canvas rect 0.0 1.0 ease-in-out-sine palette)])
    (assert (> (count-chars result #\@) 0))))

(display "  ✓ Easing integration works correctly\n")

;;; ============================================================
;;; Visual Tests (Optional - for manual verification)
;;; ============================================================

(display "\n=== Visual Tests (for manual inspection) ===\n\n")

;; Display horizontal gradient
(display "Horizontal gradient with ease-in-out-cubic:\n")
(let* ([canvas (make-canvas 60 5)]
       [rect (make-rect (point 0 0) 60 5)]
       [result (gradient-fill-horizontal canvas rect 0.0 1.0 ease-in-out-cubic intensity-palette-simple)])
  (print-canvas result))

;; Display vertical gradient
(display "Vertical gradient with ease-out-quad:\n")
(let* ([canvas (make-canvas 30 15)]
       [rect (make-rect (point 0 0) 30 15)]
       [result (gradient-fill-vertical canvas rect 0.0 1.0 ease-out-quad intensity-palette-blocks)])
  (print-canvas result))

;; Display circle
(display "Circle with radius 8:\n")
(let* ([canvas (make-canvas 25 19)]
       [center (point 12 9)]
       [result (draw-circle canvas center 8 #\●)])
  (print-canvas result))

;; Display filled circle
(display "Filled circle with radius 6:\n")
(let* ([canvas (make-canvas 21 15)]
       [center (point 10 7)]
       [result (draw-filled-circle canvas center 6 #\▓)])
  (print-canvas result))

;; Display rounded box
(display "Rounded box (light style):\n")
(let* ([canvas (make-canvas 30 10)]
       [rect (make-rect (point 5 2) 20 6)]
       [result (draw-rounded-box canvas rect 'light)])
  (print-canvas result))

;; Display radial gradient
(display "Radial gradient with ease-in-sine:\n")
(let* ([canvas (make-canvas 40 20)]
       [center (point 20 10)]
       [result (gradient-fill-radial canvas center 12 intensity-palette-simple ease-in-sine)])
  (print-canvas result))

;; Display line with gradient
(display "Gradient line:\n")
(let* ([canvas (make-canvas 50 10)]
       [start (point 5 5)]
       [end (point 45 5)]
       [result (draw-line-gradient canvas start end intensity-palette-simple)])
  (print-canvas result))

;; Display diagonal lines
(display "Diagonal lines forming an X:\n")
(let* ([canvas (make-canvas 20 20)]
       [result (draw-line canvas (point 2 2) (point 17 17) #\/)]
       [result (draw-line result (point 17 2) (point 2 17) #\\)])
  (print-canvas result))

;;; ============================================================
;;; Summary
;;; ============================================================

(display "\n")
(display "======================================\n")
(display "All graphics primitives tests passed!\n")
(display "======================================\n")
