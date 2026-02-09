;;; lattice/control-systems/test-stability.ss — Stability Analysis Tests

(load "core/base/prelude.ss")
(load "lattice/control-systems/stability.ss")

;;; ====
;;; Test Framework
;;; ====

(define tests-passed 0)
(define tests-failed 0)

(define (test name expected actual)
  (if (equal? expected actual)
      (begin
       (set! tests-passed (+ tests-passed 1))
       (printf "  [PASS] ~a\n" name))
      (begin
       (set! tests-failed (+ tests-failed 1))
       (printf "  [FAIL] ~a\n" name)
       (printf "      Expected: ~s\n" expected)
       (printf "      Got:      ~s\n" actual))))

(define (test-approx name expected actual tolerance)
  (if (< (abs (- expected actual)) tolerance)
      (begin
       (set! tests-passed (+ tests-passed 1))
       (printf "  [PASS] ~a\n" name))
      (begin
       (set! tests-failed (+ tests-failed 1))
       (printf "  [FAIL] ~a\n" name)
       (printf "      Expected: ~s (within ~s)\n" expected tolerance)
       (printf "      Got:      ~s\n" actual))))

(printf "\n=== Stability Analysis Tests ===\n\n")

;;; ====
;;; Pole-Based Stability Tests
;;; ====

(printf "--- Pole-Based Stability ---\n")

;; Stable first-order system: x' = -2x
(let* ([A (make-matrix 1 1 -2)]
       [B (make-matrix 1 1 1)]
       [C (make-matrix 1 1 1)]
       [D (make-matrix 1 1 0)]
       [sys (make-ss A B C D)])
      (test "first-order stable" #t (ss-stable? sys)))

;; Unstable first-order system: x' = 2x
(let* ([A (make-matrix 1 1 2)]
       [B (make-matrix 1 1 1)]
       [C (make-matrix 1 1 1)]
       [D (make-matrix 1 1 0)]
       [sys (make-ss A B C D)])
      (test "first-order unstable" #f (ss-stable? sys)))

;; Stable transfer function: H(s) = 1/(s+1)
(let ([tf (tf-from-lists '(1) '(1 1))])
     (test "tf stable (pole at -1)" #t (tf-stable? tf)))

;; Unstable transfer function: H(s) = 1/(s-1)
(let ([tf (tf-from-lists '(1) '(1 -1))])
     (test "tf unstable (pole at 1)" #f (tf-stable? tf)))

;; Second-order stable: H(s) = 1/(s^2 + 2s + 2)
;; Poles at -1 ± j
(let ([tf (tf-from-lists '(1) '(1 2 2))])
     (test "2nd order stable (complex poles)" #t (tf-stable? tf)))

;; Marginally stable: pole at origin
(let* ([A (make-matrix 1 1 0)]
       [B (make-matrix 1 1 1)]
       [C (make-matrix 1 1 1)]
       [D (make-matrix 1 1 0)]
       [sys (make-ss A B C D)])
      (test "marginally stable" #t (ss-marginally-stable? sys)))

;; Discrete stability
(test "discrete stable (|z|<1)" #t (discrete-stable? (list (make-complex 0.5 0.3))))
(test "discrete unstable (|z|>1)" #f (discrete-stable? (list (make-complex 1.2 0))))

;;; ====
;;; Routh-Hurwitz Tests
;;; ====

(printf "\n--- Routh-Hurwitz Criterion ---\n")

;; s + 1 = 0: stable
(let ([p (poly-from-list '(1 1))])
     (test "RH: s+1 stable" #t (routh-hurwitz-stable? p)))

;; s - 1 = 0: unstable
(let ([p (poly-from-list '(1 -1))])
     (test "RH: s-1 unstable" #f (routh-hurwitz-stable? p)))

;; s^2 + 2s + 1 = 0: stable (double pole at -1)
(let ([p (poly-from-list '(1 2 1))])
     (test "RH: s^2+2s+1 stable" #t (routh-hurwitz-stable? p)))

;; s^2 + s - 2 = 0: unstable (roots at 1, -2)
(let ([p (poly-from-list '(1 1 -2))])
     (test "RH: s^2+s-2 unstable" #f (routh-hurwitz-stable? p)))

;; s^3 + 2s^2 + 3s + 4: stable (classic example)
(let ([p (poly-from-list '(1 2 3 4))])
     (test "RH: 3rd order stable" #t (routh-hurwitz-stable? p)))

;; s^3 + s^2 - 2s - 1: count RHP roots
(let ([p (poly-from-list '(1 1 -2 -1))])
     (test "RH: count RHP > 0" #t (> (routh-hurwitz-count-rhp p) 0)))

;;; ====
;;; Lyapunov Stability Tests
;;; ====

(printf "\n--- Lyapunov Stability ---\n")

;; 1x1 stable system
(let* ([A (make-matrix 1 1 -1)]
       [Q (make-matrix 1 1 1)]
       [P (lyapunov-solve-continuous A Q)])
      (test "lyapunov 1x1: P is matrix" #t (matrix? P))
      (test "lyapunov 1x1: P positive" #t (> (matrix-ref P 0 0) 0)))

;; Lyapunov stability check
(let* ([A (make-matrix 1 1 -2)]
       [B (make-matrix 1 1 1)]
       [C (make-matrix 1 1 1)]
       [D (make-matrix 1 1 0)]
       [sys (make-ss A B C D)])
      (test "lyapunov stable?" #t (lyapunov-stable? sys)))

;; Discrete Lyapunov
(let* ([A (make-matrix 1 1 0.5)]
       [Q (make-matrix 1 1 1)]
       [P (lyapunov-solve-discrete A Q)])
      (test "discrete lyapunov: P is matrix" #t (matrix? P))
      ;; P = Q/(1-a^2) = 1/(1-0.25) = 1.333...
      (test-approx "discrete lyapunov: P value" 1.333 (matrix-ref P 0 0) 0.01))

;;; ====
;;; Root Locus Tests
;;; ====

(printf "\n--- Root Locus ---\n")

;; Simple integrator: G(s) = 1/s
;; Closed-loop: s + K = 0 → pole at -K
(let* ([tf (tf-from-lists '(1) '(1 0))]
       [gains '(1 2 5 10)]
       [locus (root-locus-points tf gains)])
      (test "root locus: 4 gain values" 4 (length locus))
      ;; At K=1, pole should be at -1
      (let ([poles-k1 (car locus)])
           (test "root locus: K=1 pole count" 1 (length poles-k1))
           (test-approx "root locus: K=1 pole at -1" -1.0 (complex-real (car poles-k1)) 0.01)))

;; Asymptotes for H(s) = 1/(s(s+1)(s+2))
;; 3 poles, 0 zeros → 3 asymptotes at 60°, 180°, 300°
(let* ([tf (tf-from-lists '(1) '(1 3 2 0))]
       [asymp (root-locus-asymptotes tf)]
       [angles (car asymp)]
       [centroid (cadr asymp)])
      (test "asymptotes: 3 angles" 3 (length angles))
      ;; Centroid = (0 + (-1) + (-2)) / 3 = -1
      (test-approx "asymptotes: centroid" -1.0 centroid 0.01))

;;; ====
;;; Gain and Phase Margin Tests
;;; ====

(printf "\n--- Gain/Phase Margins ---\n")

;; Third-order system that has phase crossover at -180°
;; H(s) = 1/((s+1)^3) = 1/(s^3 + 3s^2 + 3s + 1)
;; Phase crosses -180° at w where each (s+1) contributes -60°
(let* ([tf (tf-from-lists '(1) '(1 3 3 1))]
       [gm (gain-margin tf)]
       [pm (phase-margin tf)])
      ;; Should have finite gain margin
      (test "3rd order: gm exists" #t (or (eq? (car gm) 'infinite)
                                          (number? (car gm))))
      ;; Phase margin test
      (test "3rd order: pm result" #t (or (eq? (car pm) 'infinite)
                                          (number? (car pm)))))

;; Simple first-order: H(s) = 10/(s+1)
;; Always stable in closed-loop, infinite gain margin
(let* ([tf (tf-from-lists '(10) '(1 1))]
       [margins (stability-margins tf)])
      ;; Very high gain margin expected (or infinite)
      (test "1st order: high GM" #t (or (eq? (car margins) 'infinite)
                                        (> (car margins) 20))))

;;; ====
;;; Nyquist Criterion Tests
;;; ====

(printf "\n--- Nyquist Criterion ---\n")

;; Stable open-loop → should pass Nyquist test
(let ([tf (tf-from-lists '(1) '(1 1))])
     (test "nyquist: stable OL" #t (nyquist-stable? tf)))

;; Nyquist plot points
(let* ([tf (tf-from-lists '(1) '(1 1 1))]
       [pts (nyquist-plot-points tf 10)])
      (test "nyquist points count" 10 (length pts))
      (test "nyquist points are complex" #t (complex? (car pts))))

;;; ====
;;; BIBO Stability Tests
;;; ====

(printf "\n--- BIBO Stability ---\n")

;; BIBO stable continuous
(let ([tf (tf-from-lists '(1 2) '(1 3 2))])  ; Stable poles at -1, -2
     (test "BIBO stable continuous" #t (bibo-stable? tf)))

;; BIBO unstable
(let ([tf (tf-from-lists '(1) '(1 -1))])  ; Pole at +1
     (test "BIBO unstable" #f (bibo-stable? tf)))

;; BIBO stable discrete
(let ([tf (tf-from-lists '(0.5) '(1 -0.8))])  ; Pole at 0.8 < 1
     (test "BIBO discrete stable" #t (bibo-stable-discrete? tf)))

;; BIBO unstable discrete
(let ([tf (tf-from-lists '(1) '(1 -1.2))])  ; Pole at 1.2 > 1
     (test "BIBO discrete unstable" #f (bibo-stable-discrete? tf)))

;;; ====
;;; Controllability/Observability Tests
;;; ====

(printf "\n--- Controllability/Observability ---\n")

;; Controllable system (canonical form)
(let* ([A (matrix-from-lists '((0 1) (-2 -3)))]
       [B (matrix-from-lists '((0) (1)))]
       [C (matrix-from-lists '((1 0)))]
       [D (matrix-from-lists '((0)))]
       [sys (make-ss A B C D)])
      (test "controllability matrix is matrix" #t
            (matrix? (controllability-matrix sys)))
      (test "controllable canonical form" #t (controllable? sys))
      (test "observable canonical form" #t (observable? sys)))

;; Observability matrix test
(let* ([A (matrix-from-lists '((0 1) (-2 -3)))]
       [B (matrix-from-lists '((0) (1)))]
       [C (matrix-from-lists '((1 0)))]
       [D (matrix-from-lists '((0)))]
       [sys (make-ss A B C D)]
       [O (observability-matrix sys)])
      (test "observability matrix rows" 2 (matrix-rows O))
      (test "observability matrix cols" 2 (matrix-cols O)))

;;; ====
;;; Polynomial Derivative Tests
;;; ====

(printf "\n--- Polynomial Derivative ---\n")

;; d/dx(x^2 + 2x + 1) = 2x + 2
(let* ([p (poly-from-list '(1 2 1))]
       [dp (poly-derivative p)]
       [coeffs (poly-coeffs dp)])
      (test "derivative degree" 1 (poly-degree dp))
      (test-approx "derivative: leading" 2.0 (vector-ref coeffs 0) 1e-10)
      (test-approx "derivative: constant" 2.0 (vector-ref coeffs 1) 1e-10))

;; d/dx(5x^3) = 15x^2
(let* ([p (poly-from-list '(5 0 0 0))]
       [dp (poly-derivative p)])
      (test "cubic derivative degree" 2 (poly-degree dp))
      (test-approx "cubic derivative leading" 15.0 (poly-leading dp) 1e-10))

;; d/dx(constant) = 0
(let* ([p (poly-from-list '(7))]
       [dp (poly-derivative p)])
      (test "constant derivative is zero" 0 (poly-degree dp)))

;;; ====
;;; Integration Tests (Cross-Module)
;;; ====

(printf "\n--- Integration Tests ---\n")

;; Create system, check stability multiple ways
(let* ([tf (tf-from-lists '(1) '(1 3 2))]  ; 1/((s+1)(s+2))
       [poles (tf-poles tf)])
      ;; Should be stable by all methods
      (test "integration: tf-stable?" #t (tf-stable? tf))
      (test "integration: routh-hurwitz" #t (routh-hurwitz-stable? (tf-den tf)))
      (test "integration: bibo" #t (bibo-stable? tf)))

;; Stability report generation
(let* ([tf (tf-from-lists '(1) '(1 2 1))]
       [report (stability-report tf)])
      (test "report is string" #t (string? report))
      (test "report not empty" #t (> (string-length report) 0)))

;;; ====
;;; Edge Cases
;;; ====

(printf "\n--- Edge Cases ---\n")

;; Constant transfer function
(let ([tf (tf-from-lists '(5) '(1))])
     (test "constant tf stable" #t (tf-stable? tf)))

;; Very small system (1x1)
(let* ([A (make-matrix 1 1 -0.5)]
       [B (make-matrix 1 1 1)]
       [C (make-matrix 1 1 1)]
       [D (make-matrix 1 1 0)]
       [sys (make-ss A B C D)]
       [poles (ss-poles sys)])
      (test "1x1 poles count" 1 (length poles))
      (test-approx "1x1 pole value" -0.5 (complex-real (car poles)) 1e-10))

;;; ====
;;; Summary
;;; ====

(printf "\n====\n")
(printf "                    TEST RESULTS\n")
(printf "====\n\n")
(printf "Tests passed: ~a\n" tests-passed)
(printf "Tests failed: ~a\n" tests-failed)
(printf "Total tests:  ~a\n" (+ tests-passed tests-failed))
(printf "\n")

(if (= tests-failed 0)
    (printf "[SUCCESS] All stability analysis tests passed.\n")
    (printf "[FAILURE] Some tests failed!\n"))
