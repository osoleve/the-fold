;;; core/util/test-debug-fuel.ss — Tests for Fuel-Tracking Debugger
;;;
;;; Tests the fuel tracking extensions to the debugger.

(load "core/util/debug.ss")

(display "Fuel-Tracking Debugger Tests\n")
(display "=============================\n\n")

(define tests-passed 0)
(define tests-failed 0)

(define (test name expected actual)
  (if (equal? expected actual)
      (begin
       (set! tests-passed (+ tests-passed 1))
       (display "  + ") (display name) (newline))
      (begin
       (set! tests-failed (+ tests-failed 1))
       (display "  X ") (display name)
       (display " - expected ") (write expected)
       (display ", got ") (write actual)
       (newline))))

;;; ============================================================
;;; Fuel Debugger Creation
;;; ============================================================

(display "Fuel Debugger Creation:\n")

(let ([d (make-fuel-debugger '(+ 1 2) empty-env 1000)])
     (test "fuel-debugger?" #t (fuel-debugger? d))
     (test "debugger-fuel-budget" 1000 (debugger-fuel-budget d))
     (test "debugger-fuel-trace empty" '() (debugger-fuel-trace d))
     (test "debugger-call-stack empty" '() (debugger-call-stack d))
     (test "debugger-fuel-used initially 0" 0 (debugger-fuel-used d))
     (test "debugger-fuel-pct initially 0" 0 (debugger-fuel-pct d)))

;;; ============================================================
;;; Step with Fuel Tracking
;;; ============================================================

(display "\nStep with Fuel Tracking:\n")

;; Simple value
(let* ([d (make-fuel-debugger 42 empty-env 1000)]
       [d2 (step-with-fuel d)])
      (test "step value completes" 'complete (debugger-status d2))
      (test "step value result" 42 (debugger-expr d2))
      (test "step has fuel-trace entry" 1 (length (debugger-fuel-trace d2)))
      (test "fuel-used > 0" #t (> (debugger-fuel-used d2) 0)))

;; Primitive operation
(let* ([d (make-fuel-debugger '(prim 'add 1 2) empty-env 1000)]
       [d2 (step-with-fuel d)])
      (test "step prim result" 3 (debugger-expr d2))
      (test "step prim has trace" 1 (length (debugger-fuel-trace d2))))

;; Let binding
(let* ([d (make-fuel-debugger '(let ((x 5)) x) empty-env 1000)]
       [d2 (continue-with-fuel d)])
      (test "let result" 5 (debugger-expr d2))
      (test "let status" 'complete (debugger-status d2))
      (test "let fuel-trace non-empty" #t (pair? (debugger-fuel-trace d2))))

;;; ============================================================
;;; Fuel Analysis
;;; ============================================================

(display "\nFuel Analysis:\n")

;; expr-type classification
(test "expr-type literal" 'literal (expr-type 42))
(test "expr-type lambda" 'lambda (expr-type '(fn (x) x)))
(test "expr-type let" 'let (expr-type '(let ((x 1)) x)))
(test "expr-type if" 'if (expr-type '(if #t 1 2)))
(test "expr-type prim" 'prim (expr-type '(prim 'add 1 2)))
(test "expr-type application" 'application (expr-type '(foo x)))

;; fuel-by-expr-type
(let* ([d (make-fuel-debugger '(let ((x (prim 'add 1 2))) x) empty-env 1000)]
       [d2 (continue-with-fuel d)]
       [by-type (fuel-by-expr-type d2)])
      (test "by-type is alist" #t (list? by-type)))

;; fuel-summary
(let* ([d (make-fuel-debugger '(prim 'add 1 2) empty-env 1000)]
       [d2 (continue-with-fuel d)]
       [summary (fuel-summary d2)])
      (test "summary has budget" #t (pair? (assq 'budget summary)))
      (test "summary has used" #t (pair? (assq 'used summary)))
      (test "summary has remaining" #t (pair? (assq 'remaining summary)))
      (test "summary has steps" #t (pair? (assq 'steps summary))))

;; fuel-hotspots
(let* ([d (make-fuel-debugger '(let ((x 1)) (prim 'add x x)) empty-env 1000)]
       [d2 (continue-with-fuel d)]
       [hotspots (fuel-hotspots d2 5)])
      (test "hotspots is list" #t (list? hotspots)))

;;; ============================================================
;;; Step-Over (Next)
;;; ============================================================

(display "\nStep-Over (Next):\n")

;; call-depth
(test "depth of literal" 0 (call-depth 42))
(test "depth of var" 0 (call-depth 'x))
(test "depth of prim" 1 (call-depth '(prim 'add 1 2)))
(test "depth of nested" 2 (call-depth '(prim 'add (prim 'sub 1 2) 3)))

;; next function
(let* ([d (make-fuel-debugger '(prim 'add 1 2) empty-env 1000)]
       [d2 (next d)])
      (test "next completes simple" 'complete (debugger-status d2)))

;;; ============================================================
;;; Continue with Fuel Tracking
;;; ============================================================

(display "\nContinue with Fuel:\n")

;; Simple expression
(let* ([d (make-fuel-debugger '(prim 'mul 3 4) empty-env 1000)]
       [d2 (continue-with-fuel d)])
      (test "continue result" 12 (debugger-expr d2))
      (test "continue status" 'complete (debugger-status d2)))

;; With breakpoint on top-level form
;; Note: breakpoints check the current expression before stepping,
;; so we break on 'let (the top-level form), not nested 'prim
(let* ([d (make-fuel-debugger '(let ((x 1)) (prim 'add x x)) empty-env 1000)]
       [d2 (add-breakpoint d (break-on-form 'let))]
       [d3 (continue-with-fuel d2)])
      (test "breakpoint hit on let" 'breakpoint (debugger-status d3)))

;;; ============================================================
;;; Integration with Original Debugger
;;; ============================================================

(display "\nIntegration:\n")

;; fuel-debugger works with original functions
(let* ([d (make-fuel-debugger '(prim 'add 1 2) empty-env 1000)]
       [d2 (step d)])  ; Original step
      (test "original step works" 'complete (debugger-status d2)))

;; undo works with fuel debugger
(let* ([d (make-fuel-debugger '(let ((x 1)) x) empty-env 1000)]
       [d2 (step-with-fuel d)]
       [d3 (undo d2)])
      (test "undo restores expr" '(let ((x 1)) x) (debugger-expr d3)))

;;; ============================================================
;;; Summary
;;; ============================================================

(newline)
(display "=============================\n")
(display (format "Tests: ~a passed, ~a failed\n" tests-passed tests-failed))
(when (> tests-failed 0)
      (exit 1))
