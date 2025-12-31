; statistics.ss - Statistical utility functions
; Part of The Fold prelude

; ============================================
; Descriptive statistics
; ============================================

; percentile: Get value at given percentile (0-100)
; (percentile 50 '(1 2 3 4 5)) => 3 (median)
(define (percentile p lst)
  (if (null? lst)
      0
      (let ((sorted (sort lst)))
        (let ((n (length lst)))
          (let ((idx (floor (/ (* p (- n 1)) 100))))
            (nth idx sorted))))))

; quartiles: Get 25th, 50th, and 75th percentiles
; (quartiles '(1 2 3 4 5)) => (2 3 4)
(define (quartiles lst)
  (list (percentile 25 lst)
        (percentile 50 lst)
        (percentile 75 lst)))

; interquartile-range: Difference between Q3 and Q1
; (interquartile-range '(1 2 3 4 5 6 7 8 9)) => 4
(define (interquartile-range lst)
  (let ((qs (quartiles lst)))
    (- (caddr qs) (car qs))))

; median: Middle value (50th percentile)
; (median '(1 2 3 4 5)) => 3
(define (median lst)
  (percentile 50 lst))

; ============================================
; Module exports
; ============================================

(module-exports
  percentile
  quartiles
  interquartile-range
  median)
