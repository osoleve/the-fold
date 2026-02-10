;;; lattice/control-systems/test-transfer-function.ss — Transfer Function Tests

(load "core/lang/module.ss")
(load "core/base/prelude.ss")
(load "lattice/control-systems/transfer-function.ss")

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

(define (test-complex-approx name expected actual tolerance)
  (let ([real-diff (abs (- (complex-real expected) (complex-real actual)))]
        [imag-diff (abs (- (complex-imag expected) (complex-imag actual)))])
       (if (and (< real-diff tolerance) (< imag-diff tolerance))
           (begin
            (set! tests-passed (+ tests-passed 1))
            (printf "  [PASS] ~a\n" name))
           (begin
            (set! tests-failed (+ tests-failed 1))
            (printf "  [FAIL] ~a\n" name)
            (printf "      Expected: ~s + ~si\n" (complex-real expected) (complex-imag expected))
            (printf "      Got:      ~s + ~si\n" (complex-real actual) (complex-imag actual))))))

(printf "\n=== Transfer Function Tests ===\n\n")

;;; ====
;;; Construction Tests
;;; ====

(printf "--- Construction ---\n")

;; Simple first-order: H(s) = 1/(s+1)
(let ([tf (tf-from-lists '(1) '(1 1))])
     (test "tf? on valid TF" #t (tf? tf))
     (test "order of 1/(s+1)" 1 (tf-order tf))
     (test "relative degree" 1 (tf-relative-degree tf))
     (test "proper?" #t (tf-proper? tf))
     (test "strictly-proper?" #t (tf-strictly-proper? tf)))

;; Second-order: H(s) = (s+1)/(s^2+2s+1)
(let ([tf (tf-from-lists '(1 1) '(1 2 1))])
     (test "order of 2nd order" 2 (tf-order tf))
     (test "relative degree 2nd order" 1 (tf-relative-degree tf)))

;; Improper: H(s) = s/(1)
(let ([tf (tf-from-lists '(1 0) '(1))])
     (test "improper relative degree" -1 (tf-relative-degree tf))
     (test "improper proper?" #f (tf-proper? tf)))

;; tf-from-coeffs
(let ([tf (tf-from-coeffs (vector 2 3) (vector 1 4 5))])
     (test "tf-from-coeffs is tf" #t (tf? tf)))

;;; ====
;;; Poles and Zeros Tests
;;; ====

(printf "\n--- Poles and Zeros ---\n")

;; H(s) = 1/(s+1) has pole at s=-1
(let* ([tf (tf-from-lists '(1) '(1 1))]
       [poles (tf-poles tf)])
      (test "single pole count" 1 (length poles))
      (test-approx "pole at -1" -1.0 (complex-real (car poles)) 1e-10))

;; H(s) = (s+2)/(s+1) has zero at s=-2, pole at s=-1
(let* ([tf (tf-from-lists '(1 2) '(1 1))]
       [zeros (tf-zeros tf)]
       [poles (tf-poles tf)])
      (test "zero count" 1 (length zeros))
      (test "pole count" 1 (length poles))
      (test-approx "zero at -2" -2.0 (complex-real (car zeros)) 1e-10)
      (test-approx "pole at -1" -1.0 (complex-real (car poles)) 1e-10))

;; Complex poles: H(s) = 1/(s^2+2s+5) has poles at -1+/-2i
(let* ([tf (tf-from-lists '(1) '(1 2 5))]
       [poles (tf-poles tf)]
       [real-sum (apply + (map complex-real poles))]
       [imag-sum (apply + (map complex-imag poles))])
      (test "complex poles count" 2 (length poles))
      (test-approx "complex poles real sum" -2.0 real-sum 1e-6)
      (test-approx "complex poles imag sum" 0.0 imag-sum 1e-6))

;; Construct from poles/zeros
(let* ([tf (tf-from-poles-zeros
            (list (make-complex -1 0) (make-complex -2 0))  ; poles
            (list (make-complex -3 0))                       ; zeros
            1)]                                              ; gain
       [poles (tf-poles tf)]
       [zeros (tf-zeros tf)])
      (test "from-poles-zeros: pole count" 2 (length poles))
      (test "from-poles-zeros: zero count" 1 (length zeros)))

;;; ====
;;; Evaluation Tests
;;; ====

(printf "\n--- Evaluation ---\n")

;; H(s) = 1/(s+1)
(let ([tf (tf-from-lists '(1) '(1 1))])
     ;; At s=0: H(0) = 1/1 = 1
     (test-approx "DC gain" 1.0 (tf-dc-gain tf) 1e-10)
     ;; At s=1: H(1) = 1/2 = 0.5
     (test-approx "eval at s=1" 0.5 (tf-eval-real tf 1) 1e-10)
     ;; At s=j (s = 0+1i): H(j) = 1/(1+j)
     (let ([resp (tf-eval tf (make-complex 0 1))])
          ;; |1/(1+j)| = 1/sqrt(2) ≈ 0.707
          (test-approx "magnitude at s=j" 0.7071 (complex-magnitude resp) 0.001)
          ;; angle(1/(1+j)) = -45 degrees = -pi/4
          (test-approx "phase at s=j" -0.7854 (complex-angle resp) 0.01)))

;; Pole at origin: H(s) = 1/s
(let ([tf (tf-from-lists '(1) '(1 0))])
     (test "DC gain with pole at origin" 'infinite (tf-dc-gain tf)))

;;; ====
;;; Frequency Response Tests
;;; ====

(printf "\n--- Frequency Response ---\n")

;; First-order lowpass: H(s) = 1/(s+1)
;; Cutoff at w=1, -3dB magnitude, -45 deg phase
(let* ([tf (tf-from-lists '(1) '(1 1))]
       [freqs (vector 0.1 1.0 10.0)]
       [mag (tf-magnitude tf freqs)]
       [phase (tf-phase tf freqs)])
      ;; At w=1 (cutoff): |H| = 1/sqrt(2)
      (test-approx "magnitude at cutoff" 0.7071 (vector-ref mag 1) 0.01)
      ;; At w=1: phase = -45 deg = -pi/4
      (test-approx "phase at cutoff" -0.7854 (vector-ref phase 1) 0.01)
      ;; At w=0.1: |H| ≈ 1
      (test-approx "magnitude at low freq" 1.0 (vector-ref mag 0) 0.01)
      ;; At w=10: |H| ≈ 0.1
      (test-approx "magnitude at high freq" 0.0995 (vector-ref mag 2) 0.01))

;; Bode data
(let* ([tf (tf-from-lists '(1) '(1 1))]
       [bode (tf-bode-data tf -1 1 3)]
       [freqs (car bode)]
       [mag-db (cadr bode)]
       [phase-deg (caddr bode)])
      (test "bode freqs length" 3 (vector-length freqs))
      (test "bode mag-db length" 3 (vector-length mag-db))
      (test "bode phase-deg length" 3 (vector-length phase-deg))
      ;; At w=1 (index 1): mag should be -3dB
      (test-approx "bode -3dB at cutoff" -3.01 (vector-ref mag-db 1) 0.1)
      ;; At w=1: phase should be -45 deg
      (test-approx "bode -45deg at cutoff" -45.0 (vector-ref phase-deg 1) 1.0))

;;; ====
;;; System Connection Tests
;;; ====

(printf "\n--- System Connections ---\n")

;; Series: 1/(s+1) * 1/(s+2) = 1/((s+1)(s+2)) = 1/(s^2+3s+2)
(let* ([h1 (tf-from-lists '(1) '(1 1))]
       [h2 (tf-from-lists '(1) '(1 2))]
       [hs (tf-series h1 h2)])
      (test "series order" 2 (tf-order hs))
      (test-approx "series DC gain" 0.5 (tf-dc-gain hs) 1e-10))

;; Parallel: 1/(s+1) + 1/(s+2) = (2s+3)/((s+1)(s+2))
(let* ([h1 (tf-from-lists '(1) '(1 1))]
       [h2 (tf-from-lists '(1) '(1 2))]
       [hp (tf-parallel h1 h2)])
      (test "parallel order" 2 (tf-order hp))
      ;; DC gain: (2*0+3)/(0+1)(0+2) = 3/2 = 1.5
      (test-approx "parallel DC gain" 1.5 (tf-dc-gain hp) 1e-10))

;; Unity feedback: H/(1+H) where H = K/(s+p)
;; Closed-loop = K/(s+p+K)
(let* ([K 10] [p 2]
       [G (tf-from-lists (list K) (list 1 p))]
       [Hcl (tf-unity-feedback G)])
      ;; Closed-loop pole should be at -(p+K) = -12
      (let ([poles (tf-poles Hcl)])
           (test "feedback pole count" 1 (length poles))
           (test-approx "feedback pole" -12.0 (complex-real (car poles)) 1e-6))
      ;; DC gain = K/(p+K) = 10/12 ≈ 0.833
      (test-approx "feedback DC gain" 0.8333 (tf-dc-gain Hcl) 0.001))

;;; ====
;;; Stability Tests
;;; ====

(printf "\n--- Stability ---\n")

;; Stable: poles in LHP
(let ([tf (tf-from-lists '(1) '(1 2 1))])  ; poles at -1, -1
     (test "stable system" #t (tf-stable? tf)))

;; Unstable: pole in RHP
(let ([tf (tf-from-lists '(1) '(1 -1))])  ; pole at +1
     (test "unstable system" #f (tf-stable? tf)))

;; Marginally stable: pole on imaginary axis
;; For practical control, poles with Re=0 are considered unstable
(let ([tf (tf-from-lists '(1) '(1 0 1))])  ; poles at +/-j
     (test "marginally stable (Re=0 is unstable)" #f (tf-stable? tf)))

;;; ====
;;; Display Tests
;;; ====

(printf "\n--- Display ---\n")

(let ([tf (tf-from-lists '(1 1) '(1 2 1))])
     (let ([str (tf->string tf)])
          (test "tf->string not empty" #t (> (string-length str) 0))))

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
    (printf "[SUCCESS] All transfer function tests passed.\n")
    (printf "[FAILURE] Some tests failed!\n"))
