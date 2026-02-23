;;; lattice/linalg/test-iteration.ss — Tests for Iteration Macros
;;;
;;; Verifies that iteration macros expand to equivalent code as hand-written loops.

(load "core/lang/module.ss")
(load "core/testing/test-framework.ss")
(load "lattice/linalg/iteration.ss")

;;; ============================================================================
;;; vec-map-idx Tests
;;; ============================================================================

(test-group vec-map-idx

  (define-test add-1-to-each
    (let ([v (vector 1 2 3 4 5)])
      (assert-equal (vector 2 3 4 5 6)
                    (vec-map-idx i v (+ (vector-ref v i) 1)))))

  (define-test multiply-by-index
    (let ([v (vector 0 1 2 3 4)])
      (assert-equal (vector 0 1 4 9 16)
                    (vec-map-idx i v (* (vector-ref v i) i))))))

;;; ============================================================================
;;; vec-fold-idx Tests
;;; ============================================================================

(test-group vec-fold-idx

  (define-test sum-elements
    (let ([v (vector 1 2 3 4 5)])
      (assert-equal 15
                    (vec-fold-idx sum 0 i v (+ sum (vector-ref v i))))))

  (define-test weighted-sum
    ;; 1*0 + 2*1 + 3*2 + 4*3 + 5*4 = 0 + 2 + 6 + 12 + 20 = 40
    (let ([v (vector 1 2 3 4 5)])
      (assert-equal 40
                    (vec-fold-idx sum 0 i v (+ sum (* (vector-ref v i) i)))))))

;;; ============================================================================
;;; vec-fold Tests
;;; ============================================================================

(test-group vec-fold

  (define-test sum
    (let ([v (vector 1 2 3 4 5)])
      (assert-equal 15
                    (vec-fold sum 0 x v (+ sum x)))))

  (define-test product
    (let ([v (vector 1 2 3 4 5)])
      (assert-equal 120
                    (vec-fold prod 1 x v (* prod x))))))

;;; ============================================================================
;;; vec-zip-map-idx Tests
;;; ============================================================================

(test-group vec-zip-map-idx

  (define-test add-vectors
    (let ([v1 (vector 1 2 3)]
          [v2 (vector 10 20 30)])
      (assert-equal (vector 11 22 33)
                    (vec-zip-map-idx i v1 v2 (+ (vector-ref v1 i) (vector-ref v2 i))))))

  (define-test multiply-vectors
    (let ([v1 (vector 1 2 3)]
          [v2 (vector 4 5 6)])
      (assert-equal (vector 4 10 18)
                    (vec-zip-map-idx i v1 v2 (* (vector-ref v1 i) (vector-ref v2 i)))))))

;;; ============================================================================
;;; vec-any? Tests
;;; ============================================================================

(test-group vec-any

  (define-test element-greater-than-3
    (let ([v (vector 1 2 3 4 5)])
      (assert-true (vec-any? x v (> x 3)))))

  (define-test no-element-greater-than-10
    (let ([v (vector 1 2 3)])
      (assert-false (vec-any? x v (> x 10))))))

;;; ============================================================================
;;; vec-all? Tests
;;; ============================================================================

(test-group vec-all

  (define-test all-positive
    (let ([v (vector 1 2 3 4 5)])
      (assert-true (vec-all? x v (> x 0)))))

  (define-test not-all-greater-than-2
    (let ([v (vector 1 2 3 4 5)])
      (assert-false (vec-all? x v (> x 2))))))

;;; ============================================================================
;;; range-fold Tests
;;; ============================================================================

(test-group range-fold

  (define-test sum-1-to-10
    (assert-equal 55
                  (range-fold sum 0 i 1 11 (+ sum i))))

  (define-test factorial-5
    (assert-equal 120
                  (range-fold prod 1 i 1 6 (* prod i)))))

;;; ============================================================================
;;; vec-fold-reverse Tests
;;; ============================================================================

(test-group vec-fold-reverse

  (define-test build-list
    (let ([v (vector 1 2 3)])
      (assert-equal '(1 2 3)
                    (vec-fold-reverse lst '() i v (cons (vector-ref v i) lst))))))

;;; ============================================================================
;;; matrix-do! Tests
;;; ============================================================================

(test-group matrix-do

  (define-test fill-with-formula
    (let ([data (make-vector 6 0)]
          [rows 2]
          [cols 3])
      (matrix-do! i j rows cols
                  (vector-set! data (+ (* i cols) j) (+ (* i 10) j)))
      (assert-equal (vector 0 1 2 10 11 12) data))))

;;; ============================================================================
;;; dot-product-loop Tests
;;; ============================================================================

(test-group dot-product-loop

  (define-test basic-dot-product
    ;; 1*4 + 2*5 + 3*6 = 4 + 10 + 18 = 32
    (let ([a (vector 1 2 3)]
          [b (vector 4 5 6)])
      (assert-equal 32
                    (dot-product-loop k 3 (vector-ref a k) (vector-ref b k))))))

;;; ============================================================================
;;; vec-tabulate Tests
;;; ============================================================================

(test-group vec-tabulate

  (define-test squares
    (assert-equal (vector 0 1 4 9 16)
                  (vec-tabulate 5 i (* i i))))

  (define-test empty-vector
    (assert-equal (vector)
                  (vec-tabulate 0 i i)))

  (define-test identity-copy
    (let ([v (vector 10 20 30)])
      (assert-equal (vector 10 20 30)
                    (vec-tabulate 3 i (vector-ref v i)))))

  (define-test computed-from-source
    ;; forward difference: u[i+1] - u[i]
    (let ([u (vector 0.0 1.0 4.0 9.0)])
      (assert-equal (vector 1.0 3.0 5.0)
                    (vec-tabulate 3 i (- (vector-ref u (+ i 1))
                                         (vector-ref u i)))))))

;;; ============================================================================
;;; vec-scan Tests
;;; ============================================================================

(test-group vec-scan

  (define-test prefix-sums
    ;; init=0, acc = acc + i => 0, 1, 3, 6, 10
    (assert-equal (vector 0 1 3 6 10)
                  (vec-scan 5 0 i acc (+ acc i))))

  (define-test single-element
    (assert-equal (vector 42)
                  (vec-scan 1 42 i acc (+ acc 1))))

  (define-test exponential-smoothing-pattern
    ;; SES: s_t = alpha*x_t + (1-alpha)*s_{t-1}
    ;; alpha=0.5, xs=[10 20 30 40]
    ;; s0=10, s1=0.5*20+0.5*10=15, s2=0.5*30+0.5*15=22.5, s3=0.5*40+0.5*22.5=31.25
    (let* ([xs (vector 10 20 30 40)]
           [alpha 0.5]
           [result (vec-scan 4 (vector-ref xs 0) t prev
                    (+ (* alpha (vector-ref xs t))
                       (* (- 1 alpha) prev)))])
      (assert-true (< (abs (- (vector-ref result 0) 10.0)) 1e-10))
      (assert-true (< (abs (- (vector-ref result 1) 15.0)) 1e-10))
      (assert-true (< (abs (- (vector-ref result 2) 22.5)) 1e-10))
      (assert-true (< (abs (- (vector-ref result 3) 31.25)) 1e-10))))

  (define-test fibonacci
    ;; Scan producing fib-like: 1, 1, 2, 3, 5, 8
    ;; Here we need to track two values — but scan only tracks one acc.
    ;; So track a pair: (fib_n . fib_{n-1})
    (let ([result (vec-scan 6 (cons 1 0) i acc
                   (cons (+ (car acc) (cdr acc)) (car acc)))])
      (assert-equal 1 (car (vector-ref result 0)))
      (assert-equal 1 (car (vector-ref result 1)))
      (assert-equal 2 (car (vector-ref result 2)))
      (assert-equal 3 (car (vector-ref result 3)))
      (assert-equal 5 (car (vector-ref result 4)))
      (assert-equal 8 (car (vector-ref result 5))))))

;;; ============================================================================
;;; Run All Tests
;;; ============================================================================

(run-all-tests)
