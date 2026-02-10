;;; lattice/control-systems/test-z-transform.ss — Discrete Transfer Function Tests

(load "core/lang/module.ss")
(load "core/base/prelude.ss")
(load "lattice/control-systems/z-transform.ss")

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

(printf "\n=== Discrete Transfer Function (Z-Transform) Tests ===\n\n")

;;; ====
;;; Construction Tests
;;; ====

(printf "--- Construction ---\n")

;; Simple first-order: H(z) = 1/(z-0.5), Ts=0.1
(let ([dtf (dtf-from-lists '(1) '(1 -0.5) 0.1)])
     (test "dtf? on valid DTF" #t (dtf? dtf))
     (test "order of 1/(z-0.5)" 1 (dtf-order dtf))
     (test-approx "sample time" 0.1 (dtf-Ts dtf) 1e-10)
     (test "proper?" #t (dtf-proper? dtf)))

;; Second-order: H(z) = z/(z^2 - 1.5z + 0.5)
(let ([dtf (dtf-from-lists '(1 0) '(1 -1.5 0.5) 0.1)])
     (test "order of 2nd order" 2 (dtf-order dtf))
     (test "relative degree" 1 (dtf-relative-degree dtf)))

;; DTF from coeffs
(let ([dtf (dtf-from-coeffs (vector 2 3) (vector 1 -0.8 0.15) 0.05)])
     (test "dtf-from-coeffs is dtf" #t (dtf? dtf))
     (test-approx "dtf-from-coeffs Ts" 0.05 (dtf-Ts dtf) 1e-10))

;;; ====
;;; Poles and Zeros Tests
;;; ====

(printf "\n--- Poles and Zeros ---\n")

;; H(z) = 1/(z-0.5) has pole at z=0.5
(let* ([dtf (dtf-from-lists '(1) '(1 -0.5) 0.1)]
       [poles (dtf-poles dtf)])
      (test "single pole count" 1 (length poles))
      (test-approx "pole at 0.5" 0.5 (complex-real (car poles)) 1e-6))

;; H(z) = (z-0.3)/(z-0.5) has zero at z=0.3
(let* ([dtf (dtf-from-lists '(1 -0.3) '(1 -0.5) 0.1)]
       [zeros (dtf-zeros dtf)]
       [poles (dtf-poles dtf)])
      (test "zero count" 1 (length zeros))
      (test "pole count" 1 (length poles))
      (test-approx "zero at 0.3" 0.3 (complex-real (car zeros)) 1e-6)
      (test-approx "pole at 0.5" 0.5 (complex-real (car poles)) 1e-6))

;; Complex poles: z^2 - z + 0.5 has complex poles
(let* ([dtf (dtf-from-lists '(1) '(1 -1 0.5) 0.1)]
       [poles (dtf-poles dtf)]
       [real-sum (apply + (map complex-real poles))]
       [prod (apply * (map complex-magnitude poles))])
      (test "complex poles count" 2 (length poles))
      ;; Sum of poles = 1, product = 0.5
      (test-approx "poles real sum" 1.0 real-sum 0.01)
      ;; Magnitude product should be sqrt(0.5) * sqrt(0.5) = 0.5
      ;; But since poles are complex conjugates with |z|^2 = 0.5
      (test-approx "poles magnitude product" 0.5 (* (complex-magnitude (car poles))
                                                    (complex-magnitude (cadr poles))) 0.01))

;;; ====
;;; Evaluation Tests
;;; ====

(printf "\n--- Evaluation ---\n")

;; H(z) = 1/(z-0.5) at z=1: H(1) = 1/(1-0.5) = 2
(let ([dtf (dtf-from-lists '(1) '(1 -0.5) 0.1)])
     (test-approx "DC gain H(1)" 2.0 (dtf-gain dtf) 1e-10)
     (test-approx "eval at z=2" 0.6667 (dtf-eval-real dtf 2) 0.001))

;; Pole at z=1: DC gain is infinite
(let ([dtf (dtf-from-lists '(1) '(1 -1) 0.1)])
     (test "DC gain with pole at z=1" 'infinite (dtf-gain dtf)))

;;; ====
;;; Frequency Response Tests
;;; ====

(printf "\n--- Frequency Response ---\n")

;; First-order lowpass discrete filter
;; H(z) = 0.1/(z - 0.9), simple exponential decay
(let* ([dtf (dtf-from-lists '(0.1) '(1 -0.9) 0.1)]
       [freqs (vector 0.1 1.0 10.0)]
       [mag (dtf-magnitude dtf freqs)])
      (test "magnitude at low freq > high freq" #t (> (vector-ref mag 0) (vector-ref mag 2))))

;; Check phase behavior
(let* ([dtf (dtf-from-lists '(0.1) '(1 -0.9) 0.1)]
       [freqs (vector 1.0)]
       [phase (dtf-phase dtf freqs)])
      ;; Phase should be negative for typical lowpass
      (test "phase is real number" #t (number? (vector-ref phase 0))))

;;; ====
;;; System Connection Tests
;;; ====

(printf "\n--- System Connections ---\n")

;; Series: two first-order systems
(let* ([h1 (dtf-from-lists '(1) '(1 -0.5) 0.1)]
       [h2 (dtf-from-lists '(1) '(1 -0.8) 0.1)]
       [hs (dtf-series h1 h2)])
      (test "series: dtf?" #t (dtf? hs))
      (test "series: order" 2 (dtf-order hs))
      ;; DC gain: 2 * 5 = 10
      (test-approx "series: DC gain" 10.0 (dtf-gain hs) 1e-6))

;; Parallel: H1 + H2
(let* ([h1 (dtf-from-lists '(1) '(1 -0.5) 0.1)]
       [h2 (dtf-from-lists '(1) '(1 -0.8) 0.1)]
       [hp (dtf-parallel h1 h2)])
      (test "parallel: dtf?" #t (dtf? hp))
      (test "parallel: order" 2 (dtf-order hp))
      ;; DC gain: 2 + 5 = 7
      (test-approx "parallel: DC gain" 7.0 (dtf-gain hp) 1e-6))

;; Sample time mismatch should error
(let* ([h1 (dtf-from-lists '(1) '(1 -0.5) 0.1)]
       [h2 (dtf-from-lists '(1) '(1 -0.8) 0.2)]  ; Different Ts
       [hs (dtf-series h1 h2)])
      (test "series: sample time mismatch" 'error (car hs)))

;; Unity feedback
(let* ([G (dtf-from-lists '(1) '(1 -0.5) 0.1)]  ; G = 1/(z-0.5)
       [Hcl (dtf-unity-feedback G)])
      ;; Closed-loop: G/(1+G) = 1/(z-0.5+1) = 1/(z+0.5)
      (test "feedback: dtf?" #t (dtf? Hcl))
      (let ([poles (dtf-poles Hcl)])
           ;; Pole at z = -0.5 (from z - 0.5 + 1 = z + 0.5)
           ;; Actually, 1/(z-0.5) / (1 + 1/(z-0.5)) = 1/((z-0.5) + 1) = 1/(z + 0.5)
           (test "feedback: pole count" 1 (length poles))))

;;; ====
;;; Stability Tests
;;; ====

(printf "\n--- Stability ---\n")

;; Stable: pole inside unit circle
(let ([dtf (dtf-from-lists '(1) '(1 -0.5) 0.1)])
     (test "stable (pole at 0.5)" #t (dtf-stable? dtf)))

;; Stable: pole at origin
(let ([dtf (dtf-from-lists '(1) '(1 0) 0.1)])  ; pole at z=0
     (test "stable (pole at 0)" #t (dtf-stable? dtf)))

;; Unstable: pole outside unit circle
(let ([dtf (dtf-from-lists '(1) '(1 -1.5) 0.1)])  ; pole at z=1.5
     (test "unstable (pole at 1.5)" #f (dtf-stable? dtf)))

;; Marginally stable: pole on unit circle
(let ([dtf (dtf-from-lists '(1) '(1 -1) 0.1)])  ; pole at z=1
     (test "marginally stable (pole at 1)" #f (dtf-stable? dtf)))

;; Multiple poles inside unit circle
(let ([dtf (dtf-from-lists '(1) '(1 -1 0.5) 0.1)])  ; complex poles with |z|<1
     (test "stable with complex poles" #t (dtf-stable? dtf)))

;;; ====
;;; DSS ↔ DTF Conversion Tests
;;; ====

(printf "\n--- DSS <-> DTF Conversion ---\n")

;; Create DSS from ZOH, convert to DTF
(let* ([A (make-matrix 1 1 0.9)]
       [B (make-matrix 1 1 0.1)]
       [C (make-matrix 1 1 1)]
       [D (make-matrix 1 1 0)]
       [Ts 0.1]
       [dsys (make-dss A B C D Ts)]
       [dtf (dss->dtf dsys)])
      (test "dss->dtf: is dtf?" #t (dtf? dtf))
      (test "dss->dtf: order" 1 (dtf-order dtf))
      (test-approx "dss->dtf: Ts preserved" Ts (dtf-Ts dtf) 1e-10)
      ;; Pole should be at 0.9
      (let ([poles (dtf-poles dtf)])
           (test "dss->dtf: pole count" 1 (length poles))
           (test-approx "dss->dtf: pole value" 0.9 (complex-real (car poles)) 0.01)))

;; Convert DTF to DSS
(let* ([dtf (dtf-from-lists '(0.1) '(1 -0.9) 0.1)]
       [dsys (dtf->dss dtf)])
      (test "dtf->dss: is dss?" #t (dss? dsys))
      (test "dtf->dss: order" 1 (dss-order dsys))
      (test-approx "dtf->dss: Ts preserved" 0.1 (dss-Ts dsys) 1e-10))

;; Round-trip: DTF -> DSS -> DTF
(let* ([dtf1 (dtf-from-lists '(0.1 0.05) '(1 -0.9 0.2) 0.1)]
       [dsys (dtf->dss dtf1)]
       [dtf2 (dss->dtf dsys)])
      (test "round-trip: order preserved" (dtf-order dtf1) (dtf-order dtf2))
      (test-approx "round-trip: Ts preserved" (dtf-Ts dtf1) (dtf-Ts dtf2) 1e-10)
      ;; DC gains should match
      (let ([g1 (dtf-gain dtf1)]
            [g2 (dtf-gain dtf2)])
           (if (and (number? g1) (number? g2))
               (test-approx "round-trip: DC gain preserved" g1 g2 0.01)
               (test "round-trip: both infinite DC gain" g1 g2))))

;;; ====
;;; Edge Cases
;;; ====

(printf "\n--- Edge Cases ---\n")

;; Static gain (order 0)
(let ([dtf (dtf-from-lists '(5) '(1) 0.1)])
     (test "static gain: order" 0 (dtf-order dtf))
     (test-approx "static gain: DC gain" 5.0 (dtf-gain dtf) 1e-10)
     (test "static gain: stable" #t (dtf-stable? dtf)))

;; High-order system
(let ([dtf (dtf-from-lists '(1) '(1 -1.5 0.8 -0.15) 0.1)])
     (test "order 3: dtf?" #t (dtf? dtf))
     (test "order 3: order" 3 (dtf-order dtf)))

;;; ====
;;; Display Tests
;;; ====

(printf "\n--- Display ---\n")

(let ([dtf (dtf-from-lists '(1 0.5) '(1 -0.9) 0.1)])
     (let ([str (dtf->string dtf)])
          (test "dtf->string not empty" #t (> (string-length str) 0))))

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
    (printf "[SUCCESS] All discrete transfer function tests passed.\n")
    (printf "[FAILURE] Some tests failed!\n"))
