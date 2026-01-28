;;; lattice/linalg/test-vec.ss — Tests for Vector Operations

(load "core/testing/test-framework.ss")
(load "lattice/linalg/vec.ss")

;; Helper for error-return functions (vs exception-raising)
(define (error-result? x)
  (and (pair? x) (eq? (car x) 'error)))

;;; ============================================================================
;;; Vector Construction Tests
;;; ============================================================================

(test-group construction

  (define-test make-vec-creates-vector
    (assert-equal (vector 0 0 0 0 0)
                  (make-vec 5 0)))

  (define-test make-vec-with-init
    (assert-equal (vector 7 7 7)
                  (make-vec 3 7)))

  (define-test vec-from-list-basic
    (assert-equal (vector 1 2 3 4 5)
                  (vec-from-list '(1 2 3 4 5))))

  (define-test vec-variadic
    (assert-equal (vector 1 2 3)
                  (vec 1 2 3)))

  (define-test vec-to-list
    (assert-equal '(1 2 3)
                  (vec->list (vec 1 2 3)))))

;;; ============================================================================
;;; Vector Accessors Tests
;;; ============================================================================

(test-group accessors

  (define-test vec-ref-basic
    (assert-equal 3
                  (vec-ref (vec 1 2 3 4 5) 2)))

  (define-test vec-length-basic
    (assert-equal 5
                  (vec-length (vec 1 2 3 4 5))))

  (define-test vec-empty-true
    (assert-true (vec-empty? (vec))))

  (define-test vec-empty-false
    (assert-false (vec-empty? (vec 1))))

  (define-test vec-first-basic
    (assert-equal 1
                  (vec-first (vec 1 2 3))))

  (define-test vec-last-basic
    (assert-equal 3
                  (vec-last (vec 1 2 3))))

  (define-test vec-ref-out-of-bounds
    (assert-true (error-result? (vec-ref (vec 1 2 3) 10))))

  (define-test vec-first-empty
    (assert-true (error-result? (vec-first (vec))))))

;;; ============================================================================
;;; Vector Transformations Tests
;;; ============================================================================

(test-group transformations

  (define-test vec-map-double
    (assert-equal (vector 2 4 6)
                  (vec-map (lambda (x) (* x 2)) (vec 1 2 3))))

  (define-test vec-fold-sum
    (assert-equal 15
                  (vec-fold + 0 (vec 1 2 3 4 5))))

  (define-test vec-fold-right-cons
    (assert-equal '(1 2 3)
                  (vec-fold-right cons '() (vec 1 2 3))))

  (define-test vec-zip-with-add
    (assert-equal (vector 5 7 9)
                  (vec-zip-with + (vec 1 2 3) (vec 4 5 6))))

  (define-test vec-zip-with-dimension-mismatch
    (assert-true (error-result? (vec-zip-with + (vec 1 2) (vec 1 2 3)))))

  (define-test vec-append-basic
    (assert-equal (vector 1 2 3 4 5 6)
                  (vec-append (vec 1 2 3) (vec 4 5 6))))

  (define-test vec-take-basic
    (assert-equal (vector 1 2)
                  (vec-take (vec 1 2 3 4 5) 2)))

  (define-test vec-drop-basic
    (assert-equal (vector 3 4 5)
                  (vec-drop (vec 1 2 3 4 5) 2)))

  (define-test vec-slice-basic
    (assert-equal (vector 2 3 4)
                  (vec-slice (vec 1 2 3 4 5) 1 4)))

  (define-test vec-reverse-basic
    (assert-equal (vector 5 4 3 2 1)
                  (vec-reverse (vec 1 2 3 4 5)))))

;;; ============================================================================
;;; Vector Arithmetic Tests
;;; ============================================================================

(test-group arithmetic

  (define-test vec-add-basic
    (assert-equal (vector 5 7 9)
                  (vec-add (vec 1 2 3) (vec 4 5 6))))

  (define-test vec-sub-basic
    (assert-equal (vector -3 -3 -3)
                  (vec-sub (vec 1 2 3) (vec 4 5 6))))

  (define-test vec-mul-hadamard
    (assert-equal (vector 4 10 18)
                  (vec-mul (vec 1 2 3) (vec 4 5 6))))

  (define-test vec-scale-basic
    (assert-equal (vector 2 4 6)
                  (vec-scale 2 (vec 1 2 3))))

  (define-test vec-negate-basic
    (assert-equal (vector -1 -2 -3)
                  (vec-negate (vec 1 2 3)))))

;;; ============================================================================
;;; Products and Norms Tests
;;; ============================================================================

(test-group norms

  (define-test vec-dot-basic
    (assert-equal 32
                  (vec-dot (vec 1 2 3) (vec 4 5 6))))

  (define-test vec-dot-same-vector
    (assert-equal 14
                  (vec-dot (vec 1 2 3) (vec 1 2 3))))

  (define-test vec-norm-squared-basic
    (assert-equal 14
                  (vec-norm-squared (vec 1 2 3))))

  (define-test vec-norm-unit
    (assert-equal 1
                  (vec-norm (vec 1 0 0))))

  (define-test vec-norm-l1-basic
    (assert-equal 6
                  (vec-norm-l1 (vec 1 -2 3))))

  (define-test vec-norm-linf-basic
    (assert-equal 5
                  (vec-norm-linf (vec 1 -5 3))))

  (define-test vec-norm-linf-empty
    (assert-true (error-result? (vec-norm-linf (vec)))))

  (define-test vec-normalize-preserves-direction
    (let ([normalized (vec-normalize (vec 3 4 0))])
      (assert-true (vec-approx-equal? normalized (vec 0.6 0.8 0.0)))))

  (define-test vec-normalize-zero-vector
    (assert-true (error-result? (vec-normalize (vec 0 0 0)))))

  (define-test vec-distance-basic
    (assert-equal 5
                  (vec-distance (vec 0 0) (vec 3 4)))))

;;; ============================================================================
;;; Vector Comparisons Tests
;;; ============================================================================

(test-group comparisons

  (define-test vec-equal-same
    (assert-true (vec-equal? (vec 1 2 3) (vec 1 2 3))))

  (define-test vec-equal-different
    (assert-false (vec-equal? (vec 1 2 3) (vec 1 2 4))))

  (define-test vec-equal-different-length
    (assert-false (vec-equal? (vec 1 2 3) (vec 1 2))))

  (define-test vec-approx-equal-within-epsilon
    (assert-true (vec-approx-equal? (vec 1.0 2.0) (vec 1.00000000001 2.0))))

  (define-test vec-approx-equal-outside-epsilon
    (assert-false (vec-approx-equal? (vec 1.0 2.0) (vec 1.1 2.0)))))

;;; ============================================================================
;;; Vector Utilities Tests
;;; ============================================================================

(test-group utilities

  (define-test vec-sum-basic
    (assert-equal 15
                  (vec-sum (vec 1 2 3 4 5))))

  (define-test vec-product-basic
    (assert-equal 120
                  (vec-product (vec 1 2 3 4 5))))

  (define-test vec-mean-basic
    (assert-equal 3
                  (vec-mean (vec 1 2 3 4 5))))

  (define-test vec-min-basic
    (assert-equal 1
                  (vec-min (vec 3 1 4 1 5))))

  (define-test vec-max-basic
    (assert-equal 5
                  (vec-max (vec 3 1 4 1 5))))

  (define-test vec-argmin-basic
    (assert-equal 1
                  (vec-argmin (vec 3 1 4 1 5))))

  (define-test vec-argmax-basic
    (assert-equal 4
                  (vec-argmax (vec 3 1 4 1 5)))))

;;; ============================================================================
;;; Special Vectors Tests
;;; ============================================================================

(test-group special-vectors

  (define-test vec-zeros-basic
    (assert-equal (vector 0 0 0)
                  (vec-zeros 3)))

  (define-test vec-ones-basic
    (assert-equal (vector 1 1 1)
                  (vec-ones 3)))

  (define-test vec-range-basic
    (assert-equal (vector 0 1 2 3 4)
                  (vec-range 5)))

  (define-test vec-linspace-integers
    (assert-equal (vector 0 1 2 3 4)
                  (vec-linspace 0 4 5)))

  (define-test vec-linspace-equally-spaced
    (let ([ls (vec-linspace 0 1 5)])
      (assert-true (vec-approx-equal? ls (vec 0 0.25 0.5 0.75 1.0)))))

  (define-test vec-unit-basic
    (assert-equal (vector 0 0 1 0)
                  (vec-unit 4 2))))

;;; ============================================================================
;;; Run All Tests
;;; ============================================================================

(run-all-tests)
