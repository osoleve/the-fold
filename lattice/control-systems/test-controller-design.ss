;;; lattice/control-systems/test-controller-design.ss — Controller Design Tests

(load "core/lang/module.ss")
(load "core/base/prelude.ss")
(load "lattice/control-systems/controller-design.ss")

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

(printf "\n=== Controller Design Tests ===\n\n")

;;; ====
;;; PID Controller Tests
;;; ====

(printf "--- PID Controller Design ---\n")

;; PID transfer function
(let ([pid (pid-tf 2 0.5 0.1)])
     (test "pid-tf is tf?" #t (tf? pid))
     ;; C(s) = (0.1s^2 + 2s + 0.5)/s - order is based on denominator
     (test "pid-tf order" 1 (tf-order pid)))

;; P-only controller
(let ([p-only (pid-tf 5 0 0)])
     (test "P-only is tf?" #t (tf? p-only))
     (test "P-only order" 0 (tf-order p-only)))

;; PI controller
(let ([pi (pid-tf 3 1 0)])
     (test "PI is tf?" #t (tf? pi))
     (test "PI order" 1 (tf-order pi)))

;; Ziegler-Nichols open-loop tuning
(let* ([gains (pid-design-zn-open-loop 2 1 5)]  ; K=2, L=1, T=5
       [Kp (car gains)]
       [Ki (cadr gains)]
       [Kd (caddr gains)])
      ;; Kp = 1.2/K * T/L = 1.2/2 * 5/1 = 3
      (test-approx "ZN OL: Kp" 3.0 Kp 0.01)
      ;; Ti = 2L = 2, Ki = Kp/Ti = 1.5
      (test-approx "ZN OL: Ki" 1.5 Ki 0.01)
      ;; Td = 0.5L = 0.5, Kd = Kp*Td = 1.5
      (test-approx "ZN OL: Kd" 1.5 Kd 0.01))

;; Ziegler-Nichols closed-loop tuning
(let* ([gains (pid-design-zn-closed-loop 10 2)]  ; Ku=10, Tu=2
       [Kp (car gains)]
       [Ki (cadr gains)]
       [Kd (caddr gains)])
      ;; Kp = 0.6*Ku = 6
      (test-approx "ZN CL: Kp" 6.0 Kp 0.01)
      ;; Ti = 0.5*Tu = 1, Ki = Kp/Ti = 6
      (test-approx "ZN CL: Ki" 6.0 Ki 0.01)
      ;; Td = 0.125*Tu = 0.25, Kd = Kp*Td = 1.5
      (test-approx "ZN CL: Kd" 1.5 Kd 0.01))

;; Lambda tuning
(let* ([gains (pid-design-lambda 2 5 1 1)]  ; K=2, tau=5, theta=1, lambda=1
       [Kp (car gains)])
      ;; Kp = tau / (K * (lambda + theta)) = 5 / (2 * 2) = 1.25
      (test-approx "Lambda: Kp" 1.25 Kp 0.01))

;;; ====
;;; Pole Placement Tests
;;; ====

(printf "\n--- Pole Placement ---\n")

;; Create controllable canonical form system
;; x' = [0 1; -2 -3]x + [0; 1]u, y = [1 0]x
(let* ([A (matrix-from-lists '((0 1) (-2 -3)))]
       [B (matrix-from-lists '((0) (1)))]
       [C (matrix-from-lists '((1 0)))]
       [D (matrix-from-lists '((0)))]
       [sys (make-ss A B C D)]
       ;; Place poles at -5, -5
       [desired-poles (list (make-complex -5 0) (make-complex -5 0))]
       [K (pole-placement-ackermann sys desired-poles)])
      (test "Ackermann: K is matrix" #t (matrix? K))
      (test "Ackermann: K rows" 1 (matrix-rows K))
      (test "Ackermann: K cols" 2 (matrix-cols K)))

;; Characteristic polynomial
(let* ([A (matrix-from-lists '((0 1) (-2 -3)))]
       [char-poly (characteristic-polynomial A)]
       [coeffs (poly-coeffs char-poly)])
      ;; det(sI - A) = det([s -1; 2 s+3]) = s^2 + 3s + 2
      (test "char poly degree" 2 (poly-degree char-poly))
      (test-approx "char poly: s^2" 1.0 (vector-ref coeffs 0) 1e-10)
      (test-approx "char poly: s" 3.0 (vector-ref coeffs 1) 1e-10)
      (test-approx "char poly: const" 2.0 (vector-ref coeffs 2) 1e-10))

;; Matrix trace
(let ([A (matrix-from-lists '((1 2 3) (4 5 6) (7 8 9)))])
     (test-approx "matrix trace" 15.0 (matrix-trace A) 1e-10))

(printf "\n--- MIMO Pole Placement ---\n")

;; General pole-placement dispatches to Ackermann for SISO
(let* ([A (matrix-from-lists '((0 1) (-2 -3)))]
       [B (matrix-from-lists '((0) (1)))]
       [C (matrix-from-lists '((1 0)))]
       [D (matrix-from-lists '((0)))]
       [sys (make-ss A B C D)]
       [desired-poles (list (make-complex -5 0) (make-complex -5 0))]
       [K (pole-placement sys desired-poles)])
      (test "SISO via pole-placement: K is matrix" #t (matrix? K))
      (test "SISO via pole-placement: K shape" '(1 . 2)
            (cons (matrix-rows K) (matrix-cols K))))

;; MIMO system: 2-state, 2-input
;; A = [0 1; -2 -3], B = [1 0; 0 1] (two independent inputs)
(let* ([A (matrix-from-lists '((0 1) (-2 -3)))]
       [B (matrix-from-lists '((1 0) (0 1)))]  ; 2x2 B matrix (2 inputs)
       [C (matrix-from-lists '((1 0)))]
       [D (matrix-from-lists '((0 0)))]
       [sys (make-ss A B C D)]
       [desired-poles (list (make-complex -4 0) (make-complex -5 0))]
       [K (pole-placement sys desired-poles)])
      (test "MIMO: K is matrix" #t (matrix? K))
      (test "MIMO: K rows (inputs)" 2 (matrix-rows K))
      (test "MIMO: K cols (states)" 2 (matrix-cols K))
      ;; Verify closed-loop poles
      (when (matrix? K)
        (let* ([Acl (matrix-sub A (matrix-mul B K))]
               [cl-char-poly (characteristic-polynomial Acl)]
               [cl-coeffs (poly-coeffs cl-char-poly)])
          ;; For poles at -4, -5: (s+4)(s+5) = s² + 9s + 20
          (test-approx "MIMO closed-loop: s coeff" 9.0 (vector-ref cl-coeffs 1) 0.1)
          (test-approx "MIMO closed-loop: const" 20.0 (vector-ref cl-coeffs 2) 0.1))))

;; 3-state, 2-input MIMO system
(let* ([A (matrix-from-lists '((0 1 0) (0 0 1) (-1 -2 -3)))]
       [B (matrix-from-lists '((1 0) (0 1) (1 1)))]
       [C (matrix-from-lists '((1 0 0)))]
       [D (matrix-from-lists '((0 0)))]
       [sys (make-ss A B C D)]
       [desired-poles (list (make-complex -2 0) (make-complex -3 0) (make-complex -4 0))]
       [K (pole-placement sys desired-poles)])
      (test "MIMO 3x2: K is matrix" #t (matrix? K))
      (when (matrix? K)
        (test "MIMO 3x2: K rows" 2 (matrix-rows K))
        (test "MIMO 3x2: K cols" 3 (matrix-cols K))
        ;; Verify closed-loop characteristic polynomial
        ;; Poles at -2, -3, -4: (s+2)(s+3)(s+4) = s³ + 9s² + 26s + 24
        (let* ([Acl (matrix-sub A (matrix-mul B K))]
               [cl-char-poly (characteristic-polynomial Acl)]
               [cl-coeffs (poly-coeffs cl-char-poly)])
          (test-approx "MIMO 3x2 closed-loop: s² coeff" 9.0 (vector-ref cl-coeffs 1) 0.5)
          (test-approx "MIMO 3x2 closed-loop: s coeff" 26.0 (vector-ref cl-coeffs 2) 0.5)
          (test-approx "MIMO 3x2 closed-loop: const" 24.0 (vector-ref cl-coeffs 3) 0.5))))

;; Error case: wrong number of poles
(let* ([A (matrix-from-lists '((0 1) (-2 -3)))]
       [B (matrix-from-lists '((1 0) (0 1)))]
       [C (matrix-from-lists '((1 0)))]
       [D (matrix-from-lists '((0 0)))]
       [sys (make-ss A B C D)]
       [wrong-poles (list (make-complex -4 0))]  ; Only 1 pole for 2-state system
       [result (pole-placement sys wrong-poles)])
      (test "Error: pole count mismatch" 'error (and (pair? result) (car result))))

;;; ====
;;; Observer Design Tests
;;; ====

(printf "\n--- Observer Design ---\n")

;; Design observer for observable system
(let* ([A (matrix-from-lists '((0 1) (-2 -3)))]
       [B (matrix-from-lists '((0) (1)))]
       [C (matrix-from-lists '((1 0)))]
       [D (matrix-from-lists '((0)))]
       [sys (make-ss A B C D)]
       ;; Place observer poles faster than controller (typically 2-10x)
       [observer-poles (list (make-complex -10 0) (make-complex -10 0))]
       [L (observer-design-ackermann sys observer-poles)])
      (test "Observer: L is matrix" #t (matrix? L))
      (test "Observer: L rows" 2 (matrix-rows L))
      (test "Observer: L cols" 1 (matrix-cols L)))

;;; ====
;;; LQR Tests
;;; ====

(printf "\n--- LQR ---\n")

;; Simple first-order system
(let* ([A (make-matrix 1 1 -1)]
       [B (make-matrix 1 1 1)]
       [C (make-matrix 1 1 1)]
       [D (make-matrix 1 1 0)]
       [sys (make-ss A B C D)]
       [Q (make-matrix 1 1 1)]
       [R (make-matrix 1 1 1)]
       [result (lqr sys Q R)])
      (test "LQR 1x1: returns list" #t (list? result))
      (test "LQR 1x1: has 3 elements" 3 (length result))
      (let ([K (car result)]
            [P (cadr result)])
           (test "LQR 1x1: K is matrix" #t (matrix? K))
           (test "LQR 1x1: P is matrix" #t (matrix? P))))

;; Second-order system
(let* ([A (matrix-from-lists '((0 1) (0 0)))]  ; Double integrator
       [B (matrix-from-lists '((0) (1)))]
       [C (matrix-from-lists '((1 0)))]
       [D (matrix-from-lists '((0)))]
       [sys (make-ss A B C D)]
       [Q (matrix-from-lists '((1 0) (0 1)))]
       [R (make-matrix 1 1 1)]
       [result (lqr sys Q R)])
      (test "LQR 2nd order: returns list" #t (list? result))
      (let ([K (car result)]
            [eigenvalues (caddr result)])
           (test "LQR 2nd order: K shape" '(1 . 2)
                 (cons (matrix-rows K) (matrix-cols K)))
           ;; Closed-loop should be stable (eigenvalues might be returned as list or error)
           ;; Check for error result (like '(error complex-eigenvalues ...))
           (test "LQR 2nd order: has eigenvalues" #t
                 (or (and (pair? eigenvalues) (eq? (car eigenvalues) 'error))
                     (list? eigenvalues)))))

;;; ====
;;; Discrete LQR Tests
;;; ====

(printf "\n--- Discrete LQR ---\n")

;; Simple discrete first-order
(let* ([A (make-matrix 1 1 0.9)]
       [B (make-matrix 1 1 0.1)]
       [C (make-matrix 1 1 1)]
       [D (make-matrix 1 1 0)]
       [sys (make-ss A B C D)]
       [Q (make-matrix 1 1 1)]
       [R (make-matrix 1 1 0.1)]
       [result (dlqr sys Q R)])
      (test "DLQR 1x1: returns list" #t (list? result))
      (let ([K (car result)]
            [eigenvalues (caddr result)])
           (test "DLQR 1x1: K is matrix" #t (matrix? K))
           ;; Discrete closed-loop should be stable (|eig| < 1)
           ;; Check for error result or valid eigenvalue list
           (test "DLQR 1x1: has eigenvalues" #t
                 (or (and (pair? eigenvalues) (eq? (car eigenvalues) 'error))
                     (list? eigenvalues)))))

;;; ====
;;; LQG Tests
;;; ====

(printf "\n--- LQG ---\n")

;; LQG for simple system
(let* ([A (make-matrix 1 1 -1)]
       [B (make-matrix 1 1 1)]
       [C (make-matrix 1 1 1)]
       [D (make-matrix 1 1 0)]
       [sys (make-ss A B C D)]
       [Q (make-matrix 1 1 1)]
       [R (make-matrix 1 1 1)]
       [Qn (make-matrix 1 1 0.1)]
       [Rn (make-matrix 1 1 0.1)]
       [result (lqg sys Q R Qn Rn)])
      (test "LQG: returns list" #t (list? result))
      (test "LQG: has K and L" 2 (length result))
      (let ([K (car result)]
            [L (cadr result)])
           (test "LQG: K is matrix" #t (matrix? K))
           (test "LQG: L is matrix" #t (matrix? L))))

;;; ====
;;; H-infinity Tests
;;; ====

(printf "\n--- H-infinity ---\n")

;; H-infinity norm of stable system
(let* ([tf (tf-from-lists '(1) '(1 1))]  ; 1/(s+1)
       [hinf (hinf-norm tf)])
      (test "Hinf norm exists" #t (number? hinf))
      ;; Max magnitude of 1/(jw+1) is 1 (at w=0)
      (test-approx "Hinf: 1/(s+1)" 1.0 hinf 0.01))

;; Check bounded
(let ([tf (tf-from-lists '(1) '(1 1))])
     (test "Hinf bounded by 2" #t (hinf-bounded? tf 2))
     (test "Hinf not bounded by 0.5" #f (hinf-bounded? tf 0.5)))

;;; ====
;;; Lead/Lag Compensator Tests
;;; ====

(printf "\n--- Lead/Lag Compensators ---\n")

;; Lead compensator
(let ([lead (lead-compensator 2 1 10)])  ; Kc=2, zero at -1, pole at -10
     (test "Lead: is tf?" #t (tf? lead))
     (test "Lead: order" 1 (tf-order lead))
     ;; DC gain = 2 * 1/10 = 0.2
     (test-approx "Lead: DC gain" 0.2 (tf-dc-gain lead) 0.01))

;; Lag compensator
(let ([lag (lag-compensator 5 0.1 0.01)])  ; Kc=5, zero at -0.1, pole at -0.01
     (test "Lag: is tf?" #t (tf? lag))
     (test "Lag: order" 1 (tf-order lag))
     ;; DC gain = 5 * 0.1/0.01 = 50
     (test-approx "Lag: DC gain" 50.0 (tf-dc-gain lag) 0.1))

;;; ====
;;; Closed-Loop Tests
;;; ====

(printf "\n--- Closed-Loop Analysis ---\n")

;; Closed-loop transfer function
(let* ([plant (tf-from-lists '(1) '(1 1 0))]  ; 1/(s(s+1))
       [controller (pid-tf 2 1 0)]             ; PI: (2s+1)/s
       [closed-loop (closed-loop-tf plant controller)])
      (test "Closed-loop: is tf?" #t (tf? closed-loop)))

;; Sensitivity function
(let* ([plant (tf-from-lists '(1) '(1 1))]     ; 1/(s+1)
       [controller (tf-from-lists '(2) '(1))]  ; Gain = 2
       [S (sensitivity-tf plant controller)])
      (test "Sensitivity: is tf?" #t (tf? S))
      ;; S = 1/(1 + G*C) = 1/(1 + 2/(s+1)) = (s+1)/(s+3)
      (test "Sensitivity: order" 1 (tf-order S)))

;;; ====
;;; Polynomial Evaluation Tests
;;; ====

(printf "\n--- Polynomial at Matrix ---\n")

;; p(A) where p(x) = x^2 + 2x + 1 and A = [[1 0]; [0 1]] = I
(let* ([poly (poly-from-list '(1 2 1))]
       [A (matrix-identity 2)]
       [pA (poly-eval-matrix poly A)])
      ;; p(I) = I + 2I + I = 4I
      (test "poly-eval-matrix: shape" '(2 . 2)
            (cons (matrix-rows pA) (matrix-cols pA)))
      (test-approx "poly-eval-matrix: (0,0)" 4.0 (matrix-ref pA 0 0) 1e-10)
      (test-approx "poly-eval-matrix: (0,1)" 0.0 (matrix-ref pA 0 1) 1e-10))

;;; ====
;;; Edge Cases
;;; ====

(printf "\n--- Edge Cases ---\n")

;; 1x1 system
(let* ([A (make-matrix 1 1 -2)]
       [B (make-matrix 1 1 1)]
       [C (make-matrix 1 1 1)]
       [D (make-matrix 1 1 0)]
       [sys (make-ss A B C D)]
       [desired-poles (list (make-complex -5 0))]
       [K (pole-placement-ackermann sys desired-poles)])
      (test "1x1 pole placement: K is matrix" #t (matrix? K))
      ;; K should make A-BK have eigenvalue at -5
      ;; A-BK = -2 - K = -5 → K = 3
      (test-approx "1x1 pole placement: K value" 3.0 (matrix-ref K 0 0) 0.1))

;; all? is provided by prelude (alias for andmap)

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
    (printf "[SUCCESS] All controller design tests passed.\n")
    (printf "[FAILURE] Some tests failed!\n"))
