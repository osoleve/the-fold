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
;;; Run All Tests
;;; ============================================================================

(run-all-tests)
