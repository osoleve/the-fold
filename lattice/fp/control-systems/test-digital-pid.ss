;;; lattice/fp/control-systems/test-digital-pid.ss — Digital PID Tests

(load "core/base/prelude.ss")
(load "lattice/fp/control-systems/digital-pid.ss")

;;; ============================================================
;;; Test Framework
;;; ============================================================

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

(printf "\n=== Digital PID Controller Tests ===\n\n")

;;; ============================================================
;;; Construction Tests
;;; ============================================================

(printf "--- Construction ---\n")

(let ([pid (make-digital-pid 1.0 0.5 0.1 0.01)])
     (test "digital-pid?" #t (digital-pid? pid))
     (test-approx "Kp accessor" 1.0 (pid-Kp pid) 1e-10)
     (test-approx "Ki accessor" 0.5 (pid-Ki pid) 1e-10)
     (test-approx "Kd accessor" 0.1 (pid-Kd pid) 1e-10)
     (test-approx "Ts accessor" 0.01 (pid-Ts pid) 1e-10))

;;; ============================================================
;;; State Tests
;;; ============================================================

(printf "\n--- State ---\n")

(let ([state (make-pid-state)])
     (test "pid-state?" #t (pid-state? state))
     (test-approx "initial integral" 0.0 (pid-integral state) 1e-10)
     (test-approx "initial prev-error" 0.0 (pid-prev-error state) 1e-10))

;;; ============================================================
;;; P-Only Controller Tests
;;; ============================================================

(printf "\n--- P-Only Controller ---\n")

;; P controller: output = Kp * error
(let* ([pid (make-digital-pid 2.0 0 0 0.1)]  ; Kp=2, Ki=0, Kd=0
       [state (make-pid-state)]
       [result (pid-step pid state 5.0)]  ; error = 5
       [output (car result)])
      (test-approx "P-only: output = Kp*e" 10.0 output 1e-10))

;; P controller with different gains
(let* ([pid (make-digital-pid 0.5 0 0 0.1)]
       [state (make-pid-state)]
       [result (pid-step pid state -4.0)]  ; negative error
       [output (car result)])
      (test-approx "P-only: negative error" -2.0 output 1e-10))

;;; ============================================================
;;; PI Controller Tests
;;; ============================================================

(printf "\n--- PI Controller ---\n")

;; PI controller: accumulates integral
(let* ([pid (make-digital-pid 1.0 2.0 0 0.1)]  ; Kp=1, Ki=2, Ts=0.1
       [state0 (make-pid-state)]
       ;; First step with error = 1
       [result1 (pid-step pid state0 1.0)]
       [output1 (car result1)]
       [state1 (cdr result1)]
       ;; P = 1*1 = 1, I = 2 * (0 + 1*0.1) = 0.2
       ;; Total = 1.2
       )
      (test-approx "PI step 1: P + I" 1.2 output1 1e-10)
      ;; Second step with same error
      (let* ([result2 (pid-step pid state1 1.0)]
             [output2 (car result2)])
            ;; P = 1, I = 2 * (0.1 + 1*0.1) = 0.4
            ;; Total = 1.4
            (test-approx "PI step 2: integral accumulates" 1.4 output2 1e-10)))

;;; ============================================================
;;; PID Controller Tests
;;; ============================================================

(printf "\n--- PID Controller ---\n")

;; Full PID controller
(let* ([Kp 1.0] [Ki 0.5] [Kd 0.2] [Ts 0.1]
       [pid (make-digital-pid Kp Ki Kd Ts)]
       [state0 (make-pid-state)]
       ;; First step: error = 2
       [result1 (pid-step pid state0 2.0)]
       [output1 (car result1)]
       [state1 (cdr result1)])
      ;; P = 1*2 = 2
      ;; I = 0.5 * (0 + 2*0.1) = 0.1
      ;; D = 0.2 * (2-0)/0.1 = 4
      ;; Total = 6.1
      (test-approx "PID step 1" 6.1 output1 1e-10)
      ;; Second step: error = 1 (decreasing)
      (let* ([result2 (pid-step pid state1 1.0)]
             [output2 (car result2)])
            ;; P = 1*1 = 1
            ;; I = 0.5 * (0.2 + 1*0.1) = 0.15
            ;; D = 0.2 * (1-2)/0.1 = -2
            ;; Total = -0.85
            (test-approx "PID step 2: derivative negative" -0.85 output2 1e-10)))

;;; ============================================================
;;; Anti-Windup Tests
;;; ============================================================

(printf "\n--- Anti-Windup ---\n")

;; Test output clamping
(let* ([pid (make-digital-pid 10.0 0 0 0.1)]  ; High P gain
       [state (make-pid-state)]
       [result (pid-step-antiwindup pid state 5.0 -10 10)]  ; Clamp to [-10, 10]
       [output (car result)])
      ;; Without clamp: 10*5 = 50
      ;; With clamp: 10
      (test-approx "antiwindup: output clamped high" 10.0 output 1e-10))

(let* ([pid (make-digital-pid 10.0 0 0 0.1)]
       [state (make-pid-state)]
       [result (pid-step-antiwindup pid state -5.0 -10 10)]
       [output (car result)])
      ;; Without clamp: -50
      ;; With clamp: -10
      (test-approx "antiwindup: output clamped low" -10.0 output 1e-10))

;; Test that integral doesn't wind up when saturated
(let* ([pid (make-digital-pid 0.5 10.0 0 0.1)]  ; High Ki
       [state0 (make-pid-state)]
       ;; Saturating step
       [result1 (pid-step-antiwindup pid state0 100.0 -10 10)]
       [output1 (car result1)]
       [state1 (cdr result1)]
       ;; Check integral didn't accumulate
       [integral1 (pid-integral state1)])
      (test-approx "antiwindup: saturated output" 10.0 output1 1e-10)
      ;; Integral should not have accumulated much since saturated
      (test-approx "antiwindup: integral frozen" 0.0 integral1 1e-10))

;;; ============================================================
;;; Filtered Derivative Tests
;;; ============================================================

(printf "\n--- Filtered Derivative ---\n")

;; Filtered derivative should smooth noisy signals
(let* ([pid (make-digital-pid 0 0 1.0 0.1)]  ; D-only controller
       [state0 (make-filtered-pid-state)]
       [N 10]  ; Filter coefficient
       ;; Step change in error
       [result1 (pid-step-filtered pid state0 10.0 N)]
       [output1 (car result1)]
       [state1 (cdr result1)])
      ;; Output should be less than unfiltered: D = Kd/Ts * delta_e = 1/0.1 * 10 = 100
      ;; Filtered will be smoothed
      (test "filtered derivative: output is finite" #t (and (number? output1) (not (infinite? output1))))
      (test "filtered derivative: less than unfiltered" #t (< (abs output1) 100)))

;;; ============================================================
;;; Velocity Form Tests
;;; ============================================================

(printf "\n--- Velocity Form ---\n")

;; Velocity form should give same steady-state behavior
(let* ([pid (make-digital-pid 1.0 0.5 0 0.1)]
       [state0 (make-velocity-pid-state)]
       ;; Apply constant error
       [result1 (pid-step-velocity pid state0 1.0)]
       [output1 (car result1)]
       [state1 (cdr result1)]
       [result2 (pid-step-velocity pid state1 1.0)]
       [output2 (car result2)])
      ;; Velocity form accumulates output
      (test "velocity: output increases with integral" #t (> output2 output1)))

;;; ============================================================
;;; Ziegler-Nichols Tuning Tests
;;; ============================================================

(printf "\n--- Ziegler-Nichols Tuning ---\n")

;; P controller tuning
(let* ([Ku 10] [Tu 2.0] [Ts 0.1]
       [pid-p (ziegler-nichols-tune Ku Tu 'P Ts)])
      (test "ZN P: is pid?" #t (digital-pid? pid-p))
      (test-approx "ZN P: Kp = 0.5*Ku" 5.0 (pid-Kp pid-p) 1e-10)
      (test-approx "ZN P: Ki = 0" 0.0 (pid-Ki pid-p) 1e-10)
      (test-approx "ZN P: Kd = 0" 0.0 (pid-Kd pid-p) 1e-10))

;; PI controller tuning
(let* ([Ku 10] [Tu 2.0] [Ts 0.1]
       [pid-pi (ziegler-nichols-tune Ku Tu 'PI Ts)])
      (test "ZN PI: is pid?" #t (digital-pid? pid-pi))
      ;; Kp = 0.45 * Ku = 4.5
      (test-approx "ZN PI: Kp = 0.45*Ku" 4.5 (pid-Kp pid-pi) 1e-10)
      ;; Ti = Tu/1.2 = 1.667, Ki = Kp/Ti = 4.5/1.667 = 2.7
      (test-approx "ZN PI: Ki = Kp/(Tu/1.2)" 2.7 (pid-Ki pid-pi) 0.01))

;; PID controller tuning
(let* ([Ku 10] [Tu 2.0] [Ts 0.1]
       [pid-pid (ziegler-nichols-tune Ku Tu 'PID Ts)])
      (test "ZN PID: is pid?" #t (digital-pid? pid-pid))
      ;; Kp = 0.6 * Ku = 6
      (test-approx "ZN PID: Kp = 0.6*Ku" 6.0 (pid-Kp pid-pid) 1e-10)
      ;; Ti = Tu/2 = 1, Ki = Kp/Ti = 6
      (test-approx "ZN PID: Ki" 6.0 (pid-Ki pid-pid) 1e-10)
      ;; Td = Tu/8 = 0.25, Kd = Kp*Td = 1.5
      (test-approx "ZN PID: Kd" 1.5 (pid-Kd pid-pid) 1e-10))

;;; ============================================================
;;; Tyreus-Luyben Tuning Tests
;;; ============================================================

(printf "\n--- Tyreus-Luyben Tuning ---\n")

(let* ([Ku 10] [Tu 2.0] [Ts 0.1]
       [pid (tyreus-luyben-tune Ku Tu Ts)])
      (test "TL: is pid?" #t (digital-pid? pid))
      ;; Kp = Ku/3.2 = 3.125
      (test-approx "TL: Kp = Ku/3.2" 3.125 (pid-Kp pid) 1e-10)
      ;; Ti = 2.2*Tu = 4.4, Ki = Kp/Ti = 0.71
      (test-approx "TL: Ki" 0.71 (pid-Ki pid) 0.01))

;;; ============================================================
;;; Simulation Tests
;;; ============================================================

(printf "\n--- Simulation ---\n")

;; Simulate P-only controller tracking a constant setpoint
(let* ([pid (make-digital-pid 0.5 0 0 0.1)]
       [state (make-pid-state)]
       [setpoint 10.0]
       ;; Simulate with measurements approaching setpoint
       [measurements '(0 5 7.5 8.75 9.375)]
       [result (pid-simulate pid state setpoint measurements)]
       [outputs (car result)]
       [errors (cdr result)])
      (test "simulate: correct output count" 5 (length outputs))
      (test "simulate: correct error count" 5 (length errors))
      ;; First output: 0.5 * (10 - 0) = 5
      (test-approx "simulate: first output" 5.0 (car outputs) 1e-10)
      ;; Last error should be smallest
      (test "simulate: error decreasing" #t (< (abs (car (reverse errors)))
                                               (abs (car errors)))))

;;; ============================================================
;;; Reset Tests
;;; ============================================================

(printf "\n--- Reset ---\n")

(let* ([pid (make-digital-pid 1 1 0 0.1)]
       [state0 (make-pid-state)]
       ;; Run a few steps to accumulate state
       [result1 (pid-step pid state0 5)]
       [state1 (cdr result1)]
       [result2 (pid-step pid state1 5)]
       [state2 (cdr result2)]
       ;; Reset
       [state-reset (pid-reset state2)])
      (test-approx "reset: integral cleared" 0.0 (pid-integral state-reset) 1e-10)
      (test-approx "reset: prev-error cleared" 0.0 (pid-prev-error state-reset) 1e-10))

;; Reset with specific values (bumpless transfer)
(let ([state (pid-reset-to 5.0 2.0)])
     (test-approx "reset-to: integral" 5.0 (pid-integral state) 1e-10)
     (test-approx "reset-to: prev-error" 2.0 (pid-prev-error state) 1e-10))

;;; ============================================================
;;; Display Tests
;;; ============================================================

(printf "\n--- Display ---\n")

(let* ([pid (make-digital-pid 1.5 0.8 0.2 0.01)]
       [str (pid->string pid)])
      (test "pid->string not empty" #t (> (string-length str) 0)))

;;; ============================================================
;;; Summary
;;; ============================================================

(printf "\n================================================================\n")
(printf "                    TEST RESULTS\n")
(printf "================================================================\n\n")
(printf "Tests passed: ~a\n" tests-passed)
(printf "Tests failed: ~a\n" tests-failed)
(printf "Total tests:  ~a\n" (+ tests-passed tests-failed))
(printf "\n")

(if (= tests-failed 0)
    (printf "[SUCCESS] All digital PID tests passed.\n")
    (printf "[FAILURE] Some tests failed!\n"))
