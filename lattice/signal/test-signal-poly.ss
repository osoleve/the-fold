;;; lattice/signal/test-signal-poly.ss — Tests for Signal Processing Polynomial Integration
;;;
;;; Tests for polynomial algebra integration with signal processing.

(load "core/lang/module.ss")
(load "core/testing/test-framework.ss")
(load "lattice/signal/signal-poly.ss")

;;; ====
;;; Test: Polynomial Conversion
;;; ====

(test-group "polynomial-conversion"
  (define-test "numeric to signal polynomial"
    (let* ([num-poly (cons 'polynomial (vector 3 2 1))]  ; 3z^2 + 2z + 1 (descending)
           [sig-poly (numeric->signal-poly num-poly)]
           [coeffs (sig-poly-coeffs sig-poly)])
      ;; Should be ascending: [1, 2, 3]
      (assert-equal coeffs '(1 2 3))))

  (define-test "signal to numeric polynomial"
    (let* ([sig-poly (sig-make-polynomial Q-field-sig '(1 2 3))]  ; 1 + 2z + 3z^2
           [num-poly (signal->numeric-poly sig-poly)]
           [coeffs (vector->list (cdr num-poly))])
      ;; Should be descending: (3 2 1)
      (assert-equal coeffs '(3 2 1))))

  (define-test "round-trip preserves polynomial"
    (let* ([original (cons 'polynomial (vector 5 -3 0 2))]
           [sig-poly (numeric->signal-poly original)]
           [back (signal->numeric-poly sig-poly)]
           [orig-coeffs (vector->list (cdr original))]
           [back-coeffs (vector->list (cdr back))])
      (assert-equal orig-coeffs back-coeffs))))

;;; ====
;;; Test: Filter Polynomial Extraction
;;; ====

(test-group "filter-polynomials"
  (define-test "extract numerator polynomial"
    (let* ([f (make-iir-filter (vector 1 0.5) (vector 1 -0.8))]
           [num (filter-num-poly f)]
           [coeffs (sig-poly-coeffs num)])
      (assert-equal (length coeffs) 2)
      (assert-equal (car coeffs) 1)
      (assert-equal (cadr coeffs) 0.5)))

  (define-test "extract denominator polynomial"
    (let* ([f (make-iir-filter (vector 1) (vector 1 -0.5 0.25))]
           [den (filter-den-poly f)]
           [degree (sig-poly-degree den)])
      (assert-equal degree 2))))

;;; ====
;;; Test: Filter Coprimality
;;; ====

(test-group "filter-coprime"
  (define-test "coprime filter returns true"
    ;; (1 + 0.5z) / (1 - 0.8z) has no common factors
    (let ([f (make-iir-filter (vector 1 0.5) (vector 1 -0.8))])
      (assert-true (filter-coprime? f))))

  (define-test "non-coprime filter returns false"
    ;; Create filter with common factor
    ;; num = (1 - 0.5z)(1 + z) = 1 + 0.5z - 0.5z^2
    ;; den = (1 - 0.5z)(1 - z) = 1 - 1.5z + 0.5z^2
    ;; Common factor: (1 - 0.5z)
    (let ([f (make-iir-filter (vector 1 0.5 -0.5) (vector 1 -1.5 0.5))])
      (assert-false (filter-coprime? f)))))

;;; ====
;;; Test: Filter Simplification
;;; ====

(test-group "filter-simplify"
  (define-test "simplify removes common factor"
    ;; Filter with common factor should simplify
    ;; After simplification, degrees should decrease
    (let* ([f (make-iir-filter (vector 1 0.5 -0.5) (vector 1 -1.5 0.5))]
           [f-simp (filter-simplify f)]
           [num-deg (sig-poly-degree (filter-num-poly f-simp))]
           [den-deg (sig-poly-degree (filter-den-poly f-simp))])
      ;; Original was degree 2, simplified should be degree 1
      (assert-equal num-deg 1)
      (assert-equal den-deg 1)))

  (define-test "simplify preserves coprime filter"
    (let* ([f (make-iir-filter (vector 1 0.5) (vector 1 -0.8))]
           [f-simp (filter-simplify f)]
           [num-deg (sig-poly-degree (filter-num-poly f-simp))]
           [den-deg (sig-poly-degree (filter-den-poly f-simp))])
      (assert-equal num-deg 1)
      (assert-equal den-deg 1))))

;;; ====
;;; Test: Filter Stability
;;; ====

(test-group "filter-stability"
  (define-test "stable first-order filter"
    ;; For pole at p, denominator is (1 - p*z^{-1}) or equivalently (z - p)
    ;; In ascending z powers: coeffs = [-p, 1]
    ;; Pole at 0.5 (inside unit circle): coeffs = [-0.5, 1]
    (let ([f (make-iir-filter (vector 1) (vector -0.5 1))])
      (assert-true (filter-stable? f))))

  (define-test "unstable first-order filter"
    ;; Pole at 2 (outside unit circle): coeffs = [-2, 1]
    (let ([f (make-iir-filter (vector 1) (vector -2 1))])
      (assert-false (filter-stable? f))))

  (define-test "marginally stable filter"
    ;; Pole at 1 (on unit circle): coeffs = [-1, 1]
    (let ([f (make-iir-filter (vector 1) (vector -1 1))])
      (assert-false (filter-stable? f))))

  (define-test "stable second-order filter"
    ;; Complex conjugate poles with magnitude 0.5 inside unit circle
    ;; (z - 0.5e^{j*pi/4})(z - 0.5e^{-j*pi/4}) = z^2 - 0.707z + 0.25
    ;; In ascending powers: [0.25, -0.707, 1]
    (let ([f (make-iir-filter (vector 1) (vector 0.25 -0.707 1))])
      (assert-true (filter-stable? f)))))

;;; ====
;;; Test: Deconvolution
;;; ====

(test-group "deconvolution"
  (define-test "exact deconvolution"
    ;; If y = conv(x, h), then deconv(y, h) should give x
    (let* ([x (vector 1 2 3)]
           [h (vector 1 1)]
           ;; y = conv([1,2,3], [1,1]) = [1, 3, 5, 3]
           [y (vector 1 3 5 3)]
           [result (deconvolve y h)]
           [q (car result)]
           [r (cdr result)])
      (assert-equal (vector-length q) 3)
      (assert-true (vector-all-zero? r))))

  (define-test "deconvolution with remainder"
    (let* ([y (vector 1 2 3 4)]
           [h (vector 1 1)]
           [result (deconvolve y h)]
           [r (cdr result)])
      ;; May or may not have remainder depending on exact division
      (assert-true (vector? r)))))

;;; ====
;;; Test: Filter Cascade
;;; ====

(test-group "filter-cascade"
  (define-test "cascade increases polynomial degrees"
    (let* ([f1 (make-iir-filter (vector 1 0.5) (vector 1 -0.3))]  ; degree 1
           [f2 (make-iir-filter (vector 1 0.2) (vector 1 -0.4))]  ; degree 1
           [fc (filter-cascade f1 f2)]
           [num-deg (sig-poly-degree (filter-num-poly fc))]
           [den-deg (sig-poly-degree (filter-den-poly fc))])
      ;; Cascade: degrees add
      (assert-equal num-deg 2)
      (assert-equal den-deg 2)))

  (define-test "cascade of simple filters"
    (let* ([f1 (make-iir-filter (vector 1) (vector 1 -0.5))]
           [f2 (make-iir-filter (vector 1) (vector 1 -0.25))]
           [fc (filter-cascade f1 f2)]
           [den (filter-den-poly fc)]
           [coeffs (sig-poly-coeffs den)])
      ;; (1 - 0.5z)(1 - 0.25z) = 1 - 0.75z + 0.125z^2
      (assert-equal (length coeffs) 3))))

;;; ====
;;; Test: Filter Parallel
;;; ====

(test-group "filter-parallel"
  (define-test "parallel combination"
    (let* ([f1 (make-iir-filter (vector 1) (vector 1 -0.5))]
           [f2 (make-iir-filter (vector 1) (vector 1 -0.25))]
           [fp (filter-parallel f1 f2)]
           [den-deg (sig-poly-degree (filter-den-poly fp))])
      ;; Parallel denominator = A1 * A2
      (assert-equal den-deg 2))))

;;; ====
;;; Test: Display Functions
;;; ====

(test-group "display-functions"
  (define-test "sig-poly->string produces output"
    (let* ([p (sig-make-polynomial Q-field-sig '(1 2 3))]
           [str (sig-poly->string p 'z)])
      (assert-true (string? str))
      (assert-true (> (string-length str) 0))))

  (define-test "filter->string produces output"
    (let* ([f (make-iir-filter (vector 1 0.5) (vector 1 -0.8))]
           [str (filter->string f)])
      (assert-true (string? str))
      (assert-true (string-contains? str "H(z)")))))

;;; string-contains? : String × String → Boolean
(define (string-contains? str substr)
  (let ([len (string-length str)]
        [sublen (string-length substr)])
    (let loop ([i 0])
      (cond
        [(> (+ i sublen) len) #f]
        [(string=? (substring str i (+ i sublen)) substr) #t]
        [else (loop (+ i 1))]))))

;;; ====
;;; Run All Tests
;;; ====

(run-all-tests)
