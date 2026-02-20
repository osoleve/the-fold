(load "core/testing/test-framework.ss")
(load "lattice/sim/dynamics/ode-adaptive.ss")

;;; Helper functions for tests
(define (f-const t y) '(1))
(define (f-exp t y) y)

;;; ====
;;; DP45 Single Step
;;; ====

(test-group "dp45-step"

  (define-test "constant derivative is exact"
    (let* ([result (dp45-step f-const 0 '(0) 0.1 1e-6)]
           [y-new (car result)]
           [err (cadr result)])
      (assert-true (< (abs (- (car y-new) 0.1)) 1e-12))
      (assert-true (< err 1e-12))))

  (define-test "exponential growth step is accurate"
    (let* ([result (dp45-step f-exp 0 '(1.0) 0.1 1e-6)]
           [y-new (car result)]
           [err (cadr result)]
           [exact (exp 0.1)])
      (assert-true (< (abs (- (car y-new) exact)) 1e-8))))

  (define-test "returns three elements"
    (let ([result (dp45-step f-const 0 '(0) 0.1 1e-6)])
      (assert-equal 3 (length result))))

  (define-test "suggested dt is positive"
    (let* ([result (dp45-step f-exp 0 '(1.0) 0.1 1e-6)]
           [dt-new (caddr result)])
      (assert-true (> dt-new 0)))))

;;; ====
;;; Options
;;; ====

(test-group "options"

  (define-test "default options have required keys"
    (let ([opts (default-ode-opts)])
      (assert-true (pair? (assq 'tolerance opts)))
      (assert-true (pair? (assq 'max-steps opts)))
      (assert-true (pair? (assq 'min-dt opts)))
      (assert-true (pair? (assq 'max-dt-growth opts)))))

  (define-test "ode-opt reads from explicit opts first"
    (assert-equal 1e-3 (ode-opt '((tolerance . 1e-3)) 'tolerance)))

  (define-test "ode-opt falls back to defaults"
    (assert-equal 1e-6 (ode-opt '() 'tolerance)))

  (define-test "ode-opt errors on unknown key"
    (assert-error (lambda () (ode-opt '() 'bogus-key)))))

;;; ====
;;; Adaptive Integration — Simple Systems
;;; ====

(test-group "integrate-simple"

  (define-test "constant derivative"
    (let* ([result (ode-solve (lambda (t y) '(2.0)) 0.0 '(0.0) 1.0)]
           [final (ode-result-final-state result)])
      (assert-true (< (abs (- (car final) 2.0)) 1e-6))))

  (define-test "exponential growth"
    (let* ([result (ode-solve (lambda (t y) y) 0.0 '(1.0) 1.0)]
           [final (ode-result-final-state result)])
      (assert-true (< (abs (- (car final) (exp 1))) 1e-5))))

  (define-test "harmonic oscillator at half period"
    (let* ([pi 3.14159265358979]
           [result (ode-solve/tol
                     (lambda (t y)
                       (let ([x (car y)] [v (cadr y)])
                         (list v (- x))))
                     0.0 '(1.0 0.0) pi 1e-8)]
           [final (ode-result-final-state result)])
      (assert-true (< (abs (+ (car final) 1.0)) 1e-5))
      (assert-true (< (abs (cadr final)) 1e-5)))))

;;; ====
;;; Result Structure
;;; ====

(test-group "result-structure"

  (define-test "result has all expected keys"
    (let ([result (ode-solve (lambda (t y) '(1.0)) 0.0 '(0.0) 1.0)])
      (assert-true (pair? (assq 'trajectory result)))
      (assert-true (pair? (assq 'steps result)))
      (assert-true (pair? (assq 'rejections result)))
      (assert-true (pair? (assq 'final-dt result)))))

  (define-test "trajectory starts at initial condition"
    (let* ([result (ode-solve (lambda (t y) '(1.0)) 0.0 '(5.0) 1.0)]
           [traj (ode-result-trajectory result)]
           [first-point (car traj)])
      (assert-equal 0.0 (car first-point))
      (assert-equal '(5.0) (cdr first-point))))

  (define-test "trajectory ends near t-final"
    (let* ([result (ode-solve (lambda (t y) '(1.0)) 0.0 '(0.0) 2.5)]
           [traj (ode-result-trajectory result)]
           [last-point (list-ref traj (- (length traj) 1))])
      (assert-true (< (abs (- (car last-point) 2.5)) 1e-10))))

  (define-test "steps count is positive"
    (let ([result (ode-solve (lambda (t y) y) 0.0 '(1.0) 1.0)])
      (assert-true (> (ode-result-steps result) 0))))

  (define-test "no truncation on normal integration"
    (assert-false (ode-result-truncated? (ode-solve (lambda (t y) '(1.0)) 0.0 '(0.0) 1.0))))

  (define-test "times are monotonically increasing"
    (let* ([result (ode-solve (lambda (t y) y) 0.0 '(1.0) 1.0)]
           [times (ode-result-times result)])
      (let loop ([ts (cdr times)] [prev (car times)])
        (unless (null? ts)
          (assert-true (> (car ts) prev))
          (loop (cdr ts) (car ts)))))))

;;; ====
;;; Step Budget
;;; ====

(test-group "step-budget"

  (define-test "max-steps limits integration"
    (let* ([result (ode-adaptive-integrate
                     (lambda (t y) y)
                     0.0 '(1.0) 100.0
                     '((max-steps . 5) (initial-dt . 0.01)))]
           [steps (ode-result-steps result)])
      (assert-true (<= steps 5))
      (assert-true (ode-result-truncated? result)))))

;;; ====
;;; Tolerance
;;; ====

(test-group "tolerance"

  (define-test "tighter tolerance yields more steps"
    (let* ([f (lambda (t y) y)]
           [loose (ode-adaptive-integrate f 0.0 '(1.0) 1.0 '((tolerance . 1e-3)))]
           [tight (ode-adaptive-integrate f 0.0 '(1.0) 1.0 '((tolerance . 1e-9)))])
      (assert-true (> (ode-result-steps tight) (ode-result-steps loose)))))

  (define-test "tighter tolerance yields better accuracy"
    (let* ([f (lambda (t y) y)]
           [exact (exp 1)]
           [loose (ode-adaptive-integrate f 0.0 '(1.0) 1.0 '((tolerance . 1e-3)))]
           [tight (ode-adaptive-integrate f 0.0 '(1.0) 1.0 '((tolerance . 1e-9)))]
           [err-loose (abs (- (car (ode-result-final-state loose)) exact))]
           [err-tight (abs (- (car (ode-result-final-state tight)) exact))])
      (assert-true (< err-tight err-loose)))))

;;; ====
;;; ode-system Bridge
;;; ====

(test-group "ode-system-bridge"

  (define-test "integrates ode-system with vector output"
    (let* ([sys (harmonic-oscillator 1.0)]
           [pi 3.14159265358979]
           [result (ode-system-integrate sys 0.0 '(1.0 0.0) pi '((tolerance . 1e-8)))]
           [final (ode-result-final-state result)])
      (assert-true (< (abs (+ (car final) 1.0)) 1e-4))
      (assert-true (< (abs (cadr final)) 1e-4))))

  (define-test "integrates lorenz system"
    (let* ([sys (lorenz-system 10 28 8/3)]
           [result (ode-system-integrate sys 0.0 '(1.0 1.0 1.0) 0.1 '((tolerance . 1e-6)))]
           [final (ode-result-final-state result)])
      (assert-equal 3 (length final))
      (assert-true (< (abs (car final)) 100)))))

;;; ====
;;; Multi-Dimensional Systems
;;; ====

(test-group "multi-dimensional"

  (define-test "3D decoupled exponentials"
    (let* ([f (lambda (t y)
               (list (- (car y))
                     (* -2 (cadr y))
                     (* -3 (caddr y))))]
           [result (ode-solve/tol f 0.0 '(1.0 1.0 1.0) 1.0 1e-8)]
           [final (ode-result-final-state result)])
      (assert-true (< (abs (- (car final) (exp -1))) 1e-6))
      (assert-true (< (abs (- (cadr final) (exp -2))) 1e-6))
      (assert-true (< (abs (- (caddr final) (exp -3))) 1e-6)))))

(run-all-tests)
