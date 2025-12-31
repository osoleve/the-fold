; numeric-util.ss - Numeric utility functions
; Part of The Fold prelude

; ============================================
; List-based numeric operations
; ============================================

; clamp-list: Clamp all values in list to range
; (clamp-list 0 10 '(-5 5 15)) => (0 5 10)
(define (clamp-list lo hi lst)
  (map (fn (x) (max lo (min hi x))) lst))

; normalize: Scale list values to 0-1 range
; (normalize '(0 50 100)) => (0 0.5 1)
(define (normalize lst)
  (if (null? lst)
      '()
      (let ((lo (foldl min (car lst) lst))
            (hi (foldl max (car lst) lst)))
        (if (= lo hi)
            (map (fn (_) 0) lst)
            (map (fn (x) (/ (- x lo) (- hi lo))) lst)))))

; ============================================
; Cumulative operations
; ============================================

; running-sum: Cumulative sum
; (running-sum '(1 2 3 4)) => (1 3 6 10)
(define (running-sum lst)
  (cdr (scanl + 0 lst)))

; running-product: Cumulative product
; (running-product '(1 2 3 4)) => (1 2 6 24)
(define (running-product lst)
  (cdr (scanl * 1 lst)))

; differences: Consecutive differences
; (differences '(1 3 6 10)) => (2 3 4)
(define (differences lst)
  (if (null? lst)
      '()
      (if (null? (cdr lst))
          '()
          (zip-with - (cdr lst) lst))))

; ============================================
; Vector operations
; ============================================

; magnitude: Vector magnitude (Euclidean norm)
; (magnitude '(3 4)) => 5
(define (magnitude v)
  (sqrt (foldl + 0 (map (fn (x) (* x x)) v))))

; normalize-vector: Normalize vector to unit length
; (normalize-vector '(3 4)) => (0.6 0.8)
(define (normalize-vector v)
  (let ((mag (magnitude v)))
    (if (= mag 0)
        v
        (map (fn (x) (/ x mag)) v))))

; dot-product: Dot product of vectors
; (dot-product '(1 2 3) '(4 5 6)) => 32
(define (dot-product u v)
  (foldl + 0 (zip-with * u v)))

; dot: Alias for dot-product
(define dot dot-product)

; cross-product: Cross product of 3D vectors
; (cross-product '(1 0 0) '(0 1 0)) => (0 0 1)
(define (cross-product u v)
  (list (- (* (cadr u) (caddr v)) (* (caddr u) (cadr v)))
        (- (* (caddr u) (car v)) (* (car u) (caddr v)))
        (- (* (car u) (cadr v)) (* (cadr u) (car v))))))

; ============================================
; Module exports
; ============================================

(module-exports
  clamp-list
  normalize
  running-sum
  running-product
  differences
  magnitude
  normalize-vector
  dot-product
  dot
  cross-product)
