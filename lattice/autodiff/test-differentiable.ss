;;; Test harness for differentiable.ss --- Differentiable Type Class

(load "lattice/autodiff/differentiable.ss")

(define passed 0)
(define failed 0)

(define (test name expected actual)
  (display "  ")
  (display name)
  (display ": ")
  (if (equal? expected actual)
      (begin
       (set! passed (+ passed 1))
       (display "ok"))
      (begin
       (set! failed (+ failed 1))
       (display "FAIL\n    expected: ")
       (write expected)
       (display "\n    got: ")
       (write actual)))
  (newline))

(define (test-approx name expected actual tolerance)
  (display "  ")
  (display name)
  (display ": ")
  (if (< (abs (- expected actual)) tolerance)
      (begin
       (set! passed (+ passed 1))
       (display "ok"))
      (begin
       (set! failed (+ failed 1))
       (display "FAIL\n    expected: ")
       (write expected)
       (display " +/- ")
       (write tolerance)
       (display "\n    got: ")
       (write actual)))
  (newline))

(define (test-ok name result)
  (display "  ")
  (display name)
  (display ": ")
  (if (and (pair? result) (eq? (car result) 'ok))
      (begin
       (set! passed (+ passed 1))
       (display "ok"))
      (begin
       (set! failed (+ failed 1))
       (display "FAIL\n    expected ok, got: ")
       (write result)))
  (newline))

(define (section name)
  (newline)
  (display "=== ")
  (display name)
  (display " ===")
  (newline))

;;; ============================================================
;;; Test Instance Resolution
;;; ============================================================

(section "Instance Resolution")

(test-ok "resolve Differentiable Real" (resolve-differentiable 'Real))
(test-ok "resolve Differentiable Dual" (resolve-differentiable 'Dual))
(test-ok "resolve Differentiable Traced" (resolve-differentiable 'Traced))
(test-ok "resolve Differentiable Hyperdual" (resolve-differentiable 'Hyperdual))

;;; ============================================================
;;; Test Real Instance
;;; ============================================================

(section "Real Instance")

(let* ([result (resolve-differentiable 'Real)]
       [inst (cadr result)]
       [lift (get-method inst 'lift)]
       [primal (get-method inst 'primal)]
       [tangent (get-method inst 'tangent)]
       [d+ (get-method inst 'd+)]
       [d* (get-method inst 'd*)])
      (test "lift 5" 5 (lift 5))
      (test "primal 3" 3 (primal 3))
      (test "tangent 7" 0 (tangent 7))
      (test "d+ 2 3" 5 (d+ 2 3))
      (test "d* 2 3" 6 (d* 2 3)))

;;; ============================================================
;;; Test Dual Instance
;;; ============================================================

(section "Dual Instance")

(let* ([result (resolve-differentiable 'Dual)]
       [inst (cadr result)]
       [lift (get-method inst 'lift)]
       [primal (get-method inst 'primal)]
       [tangent (get-method inst 'tangent)]
       [d+ (get-method inst 'd+)]
       [d* (get-method inst 'd*)]
       [d-sq (get-method inst 'd-sq)])
      ;; Lift creates dual with zero derivative
      (let ([d (lift 5)])
           (test "lift value" 5 (primal d))
           (test "lift deriv" 0 (tangent d)))
      ;; Variable has derivative 1
      (let ([x (dual-variable 3)])
           (test "var value" 3 (primal x))
           (test "var deriv" 1 (tangent x)))
      ;; d+ adds values and derivatives
      (let ([result (d+ (dual 2 1) (dual 3 2))])
           (test "d+ value" 5 (primal result))
           (test "d+ deriv" 3 (tangent result)))
      ;; d* uses product rule
      (let ([result (d* (dual 2 1) (dual 3 2))])
           (test "d* value" 6 (primal result))
           ;; d/dx(2*3) = 1*3 + 2*2 = 7
           (test "d* deriv" 7 (tangent result)))
      ;; d-sq: derivative of x^2 is 2x
      (let ([result (d-sq (dual 3 1))])
           (test "d-sq value" 9 (primal result))
           (test "d-sq deriv" 6 (tangent result))))

;;; ============================================================
;;; Test Traced Instance
;;; ============================================================

(section "Traced Instance")

(let* ([result (resolve-differentiable 'Traced)]
       [inst (cadr result)]
       [lift (get-method inst 'lift)]
       [primal (get-method inst 'primal)]
       [d+ (get-method inst 'd+)]
       [d* (get-method inst 'd*)])
      (let* ([tape (make-reverse-tape)]
             [x (make-traced-var 2 tape)]
             [y (make-traced-var 3 tape)]
             [z (d* x y)])
            (test "traced d* value" 6 (primal z))))

;;; ============================================================
;;; Test Hyperdual Instance
;;; ============================================================

(section "Hyperdual Instance")

(let* ([result (resolve-differentiable 'Hyperdual)]
       [inst (cadr result)]
       [lift (get-method inst 'lift)]
       [primal (get-method inst 'primal)]
       [d+ (get-method inst 'd+)]
       [d* (get-method inst 'd*)]
       [d-sq (get-method inst 'd-sq)])
      ;; Const has zero derivatives
      (let ([c (lift 5)])
           (test "hd lift value" 5 (primal c)))
      ;; d-sq of variable gives second derivative
      (let* ([x (hd-var1 3)]
             [result (d-sq x)])
            (test "hd d-sq value" 9 (primal result))
            (test "hd d-sq deriv1 (2x)" 6 (hd-deriv1 result))
            (test "hd d-sq deriv2" 0 (hd-deriv2 result))
            (test "hd d-sq deriv12 (d^2/dxdy = 0)" 0 (hd-deriv12 result))))

;;; ============================================================
;;; Test get-diff-method
;;; ============================================================

(section "get-diff-method")

(test "get d+ for Dual" #t (procedure? (get-diff-method 'Dual 'd+)))
(test "get d* for Real" #t (procedure? (get-diff-method 'Real 'd*)))
(test "get invalid method" #f (get-diff-method 'Dual 'nonexistent))

;;; ============================================================
;;; Test Generic d-pow
;;; ============================================================

(section "Generic d-pow")

(let ([inst (cadr (resolve-differentiable 'Dual))])
     ;; x^3 at x=2 should give value 8, derivative 12 (3*x^2)
     (let* ([x (dual-variable 2)]
            [result (d-pow inst x 3)])
           (test "d-pow x^3 value" 8 (dual-value result))
           (test "d-pow x^3 deriv" 12 (dual-deriv result)))
     ;; x^0 = 1 with derivative 0
     (let* ([x (dual-variable 5)]
            [result (d-pow inst x 0)])
           (test "d-pow x^0 value" 1 (dual-value result))
           (test "d-pow x^0 deriv" 0 (dual-deriv result)))
     ;; x^(-1) at x=2 should give value 0.5, derivative -0.25 (d/dx x^-1 = -x^-2)
     (let* ([x (dual-variable 2)]
            [result (d-pow inst x -1)])
           (test-approx "d-pow x^-1 value" 0.5 (dual-value result) 0.0001)
           (test-approx "d-pow x^-1 deriv" -0.25 (dual-deriv result) 0.0001))
     ;; x^(-2) at x=2 should give value 0.25, derivative -0.25 (d/dx x^-2 = -2*x^-3)
     (let* ([x (dual-variable 2)]
            [result (d-pow inst x -2)])
           (test-approx "d-pow x^-2 value" 0.25 (dual-value result) 0.0001)
           (test-approx "d-pow x^-2 deriv" -0.25 (dual-deriv result) 0.0001))
     ;; x^(-3) at x=2 should give value 0.125, derivative -0.1875 (d/dx x^-3 = -3*x^-4)
     (let* ([x (dual-variable 2)]
            [result (d-pow inst x -3)])
           (test-approx "d-pow x^-3 value" 0.125 (dual-value result) 0.0001)
           (test-approx "d-pow x^-3 deriv" -0.1875 (dual-deriv result) 0.0001)))

;;; ============================================================
;;; Test derivative-at
;;; ============================================================

(section "derivative-at")

;; f(x) = x^2, f'(x) = 2x
(let ([f (lambda (x) (dual-sq x))])
     (test-approx "d/dx x^2 at 3" 6 (derivative-at f 3) 0.0001)
     (test-approx "d/dx x^2 at 5" 10 (derivative-at f 5) 0.0001))

;; f(x) = sin(x), f'(x) = cos(x)
(let ([f (lambda (x) (dual-sin x))])
     (test-approx "d/dx sin(x) at 0" 1 (derivative-at f 0) 0.0001))

;;; ============================================================
;;; Test extended-classes
;;; ============================================================

(section "extended-classes")

(test "has Differentiable" #t (if (assq 'Differentiable extended-classes) #t #f))
(test "has Functor" #t (if (assq 'Functor extended-classes) #t #f))

;;; ============================================================
;;; Summary
;;; ============================================================

(newline)
(display "======================")
(newline)
(display "Passed: ")
(display passed)
(newline)
(display "Failed: ")
(display failed)
(newline)

(if (= failed 0)
    (display "\nAll Differentiable type class tests passed.\n")
    (display "\nSome tests failed!\n"))
