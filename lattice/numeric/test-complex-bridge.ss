;;; lattice/numeric/test-complex-bridge.ss — Tests for Complex Bridge
;;; Run with: scheme --script lattice/numeric/test-complex-bridge.ss

(load "core/lang/module.ss")
(load "core/testing/test-framework.ss")
(load "lattice/numeric/complex-bridge.ss")

;;; ============================================================
;;; Helpers
;;; ============================================================

(define (approx-equal? a b tol)
  (< (abs (- a b)) tol))

(define eps 1e-10)

;;; ============================================================
;;; Conversion: Custom → Native
;;; ============================================================

(test-group "custom-to-native"
  (define-test "complex->native basic"
    (let ([z (complex->native (make-complex 3 4))])
      (assert-true (number? z))
      (assert-true (approx-equal? 3.0 (real-part z) eps))
      (assert-true (approx-equal? 4.0 (imag-part z) eps))))

  (define-test "complex->native zero imaginary"
    (let ([z (complex->native (make-complex 5 0))])
      (assert-true (number? z))
      (assert-true (approx-equal? 5.0 (real-part z) eps))))

  (define-test "complex->native negative"
    (let ([z (complex->native (make-complex -1 -2))])
      (assert-true (approx-equal? -1.0 (real-part z) eps))
      (assert-true (approx-equal? -2.0 (imag-part z) eps))))

  (define-test "complex-compute from custom"
    (let ([z (complex-compute (make-complex 3 4))])
      (assert-true (approx-equal? 3.0 (real-part z) eps))))

  (define-test "complex-compute from real"
    (let ([z (complex-compute 5)])
      (assert-true (number? z))
      (assert-true (approx-equal? 5.0 (real-part z) eps))))

  (define-test "complex-compute from native complex"
    (let* ([native (make-rectangular 1.0 2.0)]
           [z (complex-compute native)])
      (assert-true (approx-equal? 1.0 (real-part z) eps))
      (assert-true (approx-equal? 2.0 (imag-part z) eps))))

  (define-test "complex-compute errors on non-number"
    (assert-error (lambda () (complex-compute "not a number")))))

;;; ============================================================
;;; Conversion: Native → Custom
;;; ============================================================

(test-group "native-to-custom"
  (define-test "native->complex basic"
    (let ([c (native->complex (make-rectangular 3.0 4.0))])
      (assert-true (custom-complex? c))
      (assert-true (approx-equal? 3.0 (complex-real c) eps))
      (assert-true (approx-equal? 4.0 (complex-imag c) eps))))

  (define-test "complex-store from native"
    (let ([c (complex-store (make-rectangular 1.0 2.0))])
      (assert-true (custom-complex? c))
      (assert-true (approx-equal? 1.0 (complex-real c) eps))))

  (define-test "complex-store from real"
    (let ([c (complex-store 5.0)])
      (assert-true (custom-complex? c))
      (assert-equal 5.0 (complex-real c))
      (assert-equal 0 (complex-imag c))))

  (define-test "complex-store from already custom"
    (let* ([orig (make-complex 3 4)]
           [stored (complex-store orig)])
      (assert-true (custom-complex? stored))
      (assert-equal 3 (complex-real stored))))

  (define-test "complex-store errors on non-number"
    (assert-error (lambda () (complex-store "bad")))))

;;; ============================================================
;;; Roundtrip
;;; ============================================================

(test-group "roundtrip"
  (define-test "custom -> native -> custom"
    (let* ([orig (make-complex 3 4)]
           [native (complex->native orig)]
           [back (native->complex native)])
      (assert-true (approx-equal? 3.0 (complex-real back) eps))
      (assert-true (approx-equal? 4.0 (complex-imag back) eps))))

  (define-test "compute -> store roundtrip"
    (let* ([orig (make-complex 7 -3)]
           [native (complex-compute orig)]
           [back (complex-store native)])
      (assert-true (approx-equal? 7.0 (complex-real back) eps))
      (assert-true (approx-equal? -3.0 (complex-imag back) eps)))))

;;; ============================================================
;;; Bridged Arithmetic
;;; ============================================================

(test-group "bridged-arithmetic"
  (define c1 (make-complex 1 2))
  (define c2 (make-complex 3 4))

  (define-test "c+ addition"
    (let ([result (c+ c1 c2)])
      (assert-true (custom-complex? result))
      (assert-true (approx-equal? 4.0 (complex-real result) eps))
      (assert-true (approx-equal? 6.0 (complex-imag result) eps))))

  (define-test "c- subtraction"
    (let ([result (c- c1 c2)])
      (assert-true (approx-equal? -2.0 (complex-real result) eps))
      (assert-true (approx-equal? -2.0 (complex-imag result) eps))))

  (define-test "c* multiplication"
    ;; (1+2i)(3+4i) = 3+4i+6i+8i^2 = 3+10i-8 = -5+10i
    (let ([result (c* c1 c2)])
      (assert-true (approx-equal? -5.0 (complex-real result) eps))
      (assert-true (approx-equal? 10.0 (complex-imag result) eps))))

  (define-test "c/ division"
    ;; (1+2i)/(3+4i) = (1+2i)(3-4i)/25 = (3-4i+6i-8i^2)/25 = (11+2i)/25
    (let ([result (c/ c1 c2)])
      (assert-true (approx-equal? 0.44 (complex-real result) 0.01))
      (assert-true (approx-equal? 0.08 (complex-imag result) 0.01))))

  (define-test "c+ identity"
    (let ([result (c+ c1 (make-complex 0 0))])
      (assert-true (approx-equal? 1.0 (complex-real result) eps))
      (assert-true (approx-equal? 2.0 (complex-imag result) eps))))

  (define-test "c* identity"
    (let ([result (c* c1 (make-complex 1 0))])
      (assert-true (approx-equal? 1.0 (complex-real result) eps))
      (assert-true (approx-equal? 2.0 (complex-imag result) eps)))))

;;; ============================================================
;;; Bridged Transcendentals
;;; ============================================================

(test-group "bridged-transcendentals"
  (define-test "c-exp of 0"
    (let ([result (c-exp (make-complex 0 0))])
      (assert-true (approx-equal? 1.0 (complex-real result) eps))
      (assert-true (approx-equal? 0.0 (complex-imag result) eps))))

  (define-test "c-sqrt of 4+0i"
    (let ([result (c-sqrt (make-complex 4 0))])
      (assert-true (approx-equal? 2.0 (complex-real result) eps))
      (assert-true (approx-equal? 0.0 (complex-imag result) eps))))

  (define-test "c-sqrt of -1 is i"
    (let ([result (c-sqrt (make-complex -1 0))])
      (assert-true (approx-equal? 0.0 (complex-real result) eps))
      (assert-true (approx-equal? 1.0 (complex-imag result) eps))))

  (define-test "c-log of e^1"
    (let ([result (c-log (c-exp (make-complex 1 0)))])
      (assert-true (approx-equal? 1.0 (complex-real result) eps))
      (assert-true (approx-equal? 0.0 (complex-imag result) eps))))

  (define-test "c-pow squares correctly"
    ;; (1+i)^2 = 1 + 2i + i^2 = 2i
    (let ([result (c-pow (make-complex 1 1) (make-complex 2 0))])
      (assert-true (approx-equal? 0.0 (complex-real result) 1e-6))
      (assert-true (approx-equal? 2.0 (complex-imag result) 1e-6)))))

;;; ============================================================
;;; Comparison
;;; ============================================================

(test-group "comparison"
  (define-test "c-magnitude of 3+4i is 5"
    (assert-true (approx-equal? 5.0 (c-magnitude (make-complex 3 4)) eps)))

  (define-test "c-magnitude of 0 is 0"
    (assert-true (approx-equal? 0.0 (c-magnitude (make-complex 0 0)) eps)))

  (define-test "c-angle of positive real is 0"
    (assert-true (approx-equal? 0.0 (c-angle (make-complex 5 0)) eps)))

  (define-test "c=? equal"
    (assert-true (c=? (make-complex 3 4) (make-complex 3 4))))

  (define-test "c=? unequal"
    (assert-false (c=? (make-complex 3 4) (make-complex 3 5)))))

;;; ============================================================
;;; Predicates and Utilities
;;; ============================================================

(test-group "utilities"
  (define-test "custom-complex? on custom"
    (assert-true (custom-complex? (make-complex 1 2))))

  (define-test "custom-complex? on non-complex"
    (assert-false (custom-complex? 42))
    (assert-false (custom-complex? '(list 1 2))))

  (define-test "ensure-complex from custom"
    (let ([c (ensure-complex (make-complex 3 4))])
      (assert-true (custom-complex? c))
      (assert-equal 3 (complex-real c))))

  (define-test "ensure-complex from real"
    (let ([c (ensure-complex 5)])
      (assert-true (custom-complex? c))
      (assert-equal 5 (complex-real c))
      (assert-equal 0 (complex-imag c))))

  (define-test "ensure-complex from native"
    (let ([c (ensure-complex (make-rectangular 1.0 2.0))])
      (assert-true (custom-complex? c))
      (assert-true (approx-equal? 1.0 (complex-real c) eps)))))

;;; ============================================================
;;; Batch Operations
;;; ============================================================

(test-group "batch"
  (define-test "complex-compute-all"
    (let* ([inputs (list (make-complex 1 2) (make-complex 3 4))]
           [natives (complex-compute-all inputs)])
      (assert-equal 2 (length natives))
      (assert-true (number? (car natives)))))

  (define-test "complex-store-all"
    (let* ([natives (list (make-rectangular 1.0 2.0) (make-rectangular 3.0 4.0))]
           [customs (complex-store-all natives)])
      (assert-equal 2 (length customs))
      (assert-true (custom-complex? (car customs)))))

  (define-test "with-native-complex roundtrip"
    (let* ([inputs (list (make-complex 1 0) (make-complex 2 0) (make-complex 3 0))]
           [results (with-native-complex inputs
                      (lambda (ns) (map (lambda (n) (* n n)) ns)))])
      (assert-equal 3 (length results))
      (assert-true (approx-equal? 1.0 (complex-real (car results)) eps))
      (assert-true (approx-equal? 4.0 (complex-real (cadr results)) eps))
      (assert-true (approx-equal? 9.0 (complex-real (caddr results)) eps)))))

;;; ============================================================
;;; Run Tests
;;; ============================================================

(run-all-tests-and-exit)
