;;; lattice/statistics/timeseries/test-ar-poly.ss — Tests for AR/MA Polynomial Integration
;;;
;;; Tests for polynomial algebra integration with time series.

(load "core/testing/test-framework.ss")
(load "lattice/statistics/timeseries/ar-poly.ss")

;;; ============================================================
;;; Test: AR Characteristic Polynomial
;;; ============================================================

(test-group "ar-char-poly"
  (define-test "AR(1) characteristic polynomial"
    ;; AR(1) with phi_1 = 0.5
    ;; Char poly: 1 - 0.5*z
    (let* ([phi '#(0.5)]
           [poly (ar-char-poly-vec phi)]
           [coeffs (poly-coeffs poly)])
      ;; Ascending order: [1, -0.5]
      (assert-equal (length coeffs) 2)
      (assert-equal (car coeffs) 1)
      (assert-equal (cadr coeffs) -0.5)))

  (define-test "AR(2) characteristic polynomial"
    ;; AR(2) with phi_1 = 0.5, phi_2 = 0.3
    ;; Char poly: 1 - 0.5*z - 0.3*z^2
    (let* ([phi '#(0.5 0.3)]
           [poly (ar-char-poly-vec phi)]
           [coeffs (poly-coeffs poly)])
      (assert-equal (length coeffs) 3)
      (assert-equal (car coeffs) 1)
      (assert-equal (cadr coeffs) -0.5)
      (assert-equal (caddr coeffs) -0.3)))

  (define-test "empty AR coefficients give constant 1"
    (let* ([phi '#()]
           [poly (ar-char-poly-vec phi)]
           [coeffs (poly-coeffs poly)])
      (assert-equal coeffs '(1)))))

;;; ============================================================
;;; Test: MA Characteristic Polynomial
;;; ============================================================

(test-group "ma-char-poly"
  (define-test "MA(1) characteristic polynomial"
    ;; MA(1) with theta_1 = 0.3
    ;; Char poly: 1 + 0.3*z
    (let* ([theta '#(0.3)]
           [poly (ma-char-poly theta)]
           [coeffs (poly-coeffs poly)])
      (assert-equal (length coeffs) 2)
      (assert-equal (car coeffs) 1)
      (assert-equal (cadr coeffs) 0.3)))

  (define-test "MA(2) characteristic polynomial"
    ;; MA(2) with theta_1 = 0.3, theta_2 = 0.2
    ;; Char poly: 1 + 0.3*z + 0.2*z^2
    (let* ([theta '#(0.3 0.2)]
           [poly (ma-char-poly theta)]
           [coeffs (poly-coeffs poly)])
      (assert-equal (length coeffs) 3)
      (assert-equal (car coeffs) 1)
      (assert-equal (cadr coeffs) 0.3)
      (assert-equal (caddr coeffs) 0.2))))

;;; ============================================================
;;; Test: AR Companion Polynomial
;;; ============================================================

(test-group "ar-companion-poly"
  (define-test "AR(1) companion polynomial"
    ;; AR(1) with phi_1 = 0.5
    ;; Companion: z - 0.5 (roots inside unit circle for stability)
    (let* ([phi '#(0.5)]
           [poly (ar-companion-poly phi)]
           [coeffs (poly-coeffs poly)])
      ;; Ascending order: [-0.5, 1]
      (assert-equal (length coeffs) 2)
      (assert-equal (cadr coeffs) 1)))

  (define-test "AR(2) companion polynomial"
    ;; AR(2) with phi_1 = 0.3, phi_2 = 0.2
    ;; Companion: z^2 - 0.3*z - 0.2
    (let* ([phi '#(0.3 0.2)]
           [poly (ar-companion-poly phi)]
           [coeffs (poly-coeffs poly)]
           [degree (poly-degree poly)])
      (assert-equal degree 2)
      (assert-equal (list-ref coeffs 2) 1))))  ; Leading coeff is 1

;;; ============================================================
;;; Test: ARMA Common Factors
;;; ============================================================

(test-group "arma-common-factors"
  (define-test "coprime AR and MA have GCD degree 0"
    ;; AR: 1 - 0.5*z, MA: 1 + 0.3*z
    (let* ([phi '#(0.5)]
           [theta '#(0.3)]
           [gcd-poly (arma-common-factors phi theta)])
      (assert-equal (poly-degree gcd-poly) 0)))

  (define-test "arma-reducible? false for coprime"
    (let* ([phi '#(0.5)]
           [theta '#(0.3)])
      (assert-false (arma-reducible? phi theta)))))

;;; ============================================================
;;; Test: Stability Analysis
;;; ============================================================

(test-group "ar-stability"
  (define-test "AR(1) with |phi| < 1 is stable"
    (assert-true (ar-stable-simple? '#(0.5))))

  (define-test "AR(1) with |phi| >= 1 is unstable"
    (assert-false (ar-stable-simple? '#(1.0)))
    (assert-false (ar-stable-simple? '#(-1.0))))

  (define-test "AR(2) stability conditions"
    ;; Stable AR(2): phi_1 + phi_2 < 1, phi_2 - phi_1 < 1, |phi_2| < 1
    (assert-true (ar-stable-simple? '#(0.3 0.2)))
    ;; phi_1 = 0.5, phi_2 = 0.6 -> phi_1 + phi_2 = 1.1 > 1 (unstable)
    (assert-false (ar-stable-simple? '#(0.5 0.6)))))

;;; ============================================================
;;; Test: MA Invertibility
;;; ============================================================

(test-group "ma-invertibility"
  (define-test "MA(1) with |theta| < 1 is invertible"
    (assert-true (ma-invertible-simple? '#(0.5))))

  (define-test "MA(1) with |theta| >= 1 is not invertible"
    (assert-false (ma-invertible-simple? '#(1.0)))
    (assert-false (ma-invertible-simple? '#(-1.0))))

  (define-test "MA(2) invertibility conditions"
    (assert-true (ma-invertible-simple? '#(0.3 0.2)))))

;;; ============================================================
;;; Test: Forecast Weights (Psi-weights)
;;; ============================================================

(test-group "forecast-weights"
  (define-test "AR(1) psi-weights"
    ;; For AR(1) with phi_1, psi_j = phi_1^j
    (let* ([phi '#(0.5)]
           [psi (ar-forecast-weights phi 4)])
      ;; psi_0 = 1, psi_1 = 0.5, psi_2 = 0.25, psi_3 = 0.125, psi_4 = 0.0625
      (assert-equal (length psi) 5)
      (assert-equal (list-ref psi 0) 1)
      (assert-equal (list-ref psi 1) 0.5)
      (assert-equal (list-ref psi 2) 0.25)))

  (define-test "AR(2) psi-weights follow recurrence"
    ;; psi_j = phi_1*psi_{j-1} + phi_2*psi_{j-2}
    (let* ([phi '#(0.5 0.3)]
           [psi (ar-forecast-weights phi 3)])
      (assert-equal (list-ref psi 0) 1)
      ;; psi_1 = 0.5 * 1 = 0.5
      (assert-equal (list-ref psi 1) 0.5)
      ;; psi_2 = 0.5 * 0.5 + 0.3 * 1 = 0.55
      (assert-equal (list-ref psi 2) 0.55))))

;;; ============================================================
;;; Test: Coefficient Conversion
;;; ============================================================

(test-group "coeff-conversion"
  (define-test "algebra-poly->ar-coeffs round trip"
    (let* ([phi '#(0.5 0.3)]
           [poly (ar-char-poly-vec phi)]
           [phi-back (algebra-poly->ar-coeffs poly)])
      (assert-equal (vector-length phi-back) 2)
      (assert-equal (vector-ref phi-back 0) 0.5)
      (assert-equal (vector-ref phi-back 1) 0.3)))

  (define-test "algebra-poly->ma-coeffs round trip"
    (let* ([theta '#(0.3 0.2)]
           [poly (ma-char-poly theta)]
           [theta-back (algebra-poly->ma-coeffs poly)])
      (assert-equal (vector-length theta-back) 2)
      (assert-equal (vector-ref theta-back 0) 0.3)
      (assert-equal (vector-ref theta-back 1) 0.2))))

;;; ============================================================
;;; Test: Display Functions
;;; ============================================================

(test-group "display-functions"
  (define-test "ar-char-poly->string produces output"
    (let* ([phi '#(0.5 0.3)]
           [str (ar-char-poly->string phi)])
      (assert-true (string? str))
      (assert-true (> (string-length str) 0))))

  (define-test "ma-char-poly->string produces output"
    (let* ([theta '#(0.3 0.2)]
           [str (ma-char-poly->string theta)])
      (assert-true (string? str))
      (assert-true (> (string-length str) 0))))

  (define-test "arma-model->string produces output"
    (let* ([phi '#(0.5)]
           [theta '#(0.3)]
           [str (arma-model->string phi theta)])
      (assert-true (string? str))
      (assert-true (> (string-length str) 0)))))

;;; ============================================================
;;; Run All Tests
;;; ============================================================

(run-all-tests)
