;;; fabric/stitches/test-debug.ss — Tests for Stepping/Debugger
;;;
;;; Tests debugger state, stepping, breakpoints, and tracing.

(load "fabric/stitches/debug.ss")

(display "Debugger Tests
")
(display "==============

")

(define tests-passed 0)
(define tests-failed 0)

(define (test name expected actual)
  (if (equal? expected actual)
      (begin
       (set! tests-passed (+ tests-passed 1))
       (display "  ✓ ") (display name) (newline))
      (begin
       (set! tests-failed (+ tests-failed 1))
       (display "  ✗ ") (display name)
       (display " — expected ") (write expected)
       (display ", got ") (write actual)
       (newline))))

;;; ============================================================
;;; Debugger Creation
;;; ============================================================

(display "Debugger Creation:
")

(let ([d (make-debugger 42)])
     (test "debugger?" #t (debugger? d))
     (test "initial expr" 42 (debugger-expr d))
     (test "initial status" 'ready (debugger-status d))
     (test "initial fuel > 0" #t (> (debugger-fuel d) 0))
     (test "initial trace empty" '() (debugger-trace d))
     (test "initial history empty" '() (debugger-history d)))

;;; ============================================================
;;; Single Step
;;; ============================================================

(display "
Single Step:
")

;; Simple value - one step completes
(let* ([d (make-debugger 42)]
       [d2 (step d)])
      (test "step value completes" 'complete (debugger-status d2))
      (test "step value result" 42 (debugger-expr d2))
      (test "step adds to trace" 1 (length (debugger-trace d2)))
      (test "step adds to history" 1 (length (debugger-history d2))))

;; Lambda creates closure
(let* ([d (make-debugger '(fn (x) x))]
       [d2 (step d)])
      (test "step lambda status" 'complete (debugger-status d2))
      (test "step lambda creates closure" 'closure (car (debugger-expr d2))))

;; Variable lookup
(let* ([d (make-debugger 'x '((x . 10)) 1000)]
       [d2 (step d)])
      (test "step var status" 'complete (debugger-status d2))
      (test "step var value" 10 (debugger-expr d2)))

;;; ============================================================
;;; Step-N
;;; ============================================================

(display "
Step-N:
")

;; (+ 1 2) requires multiple steps
(let* ([d (make-debugger '(prim 'add 1 2))]
       [d2 (step-n d 10)])
      (test "step-n completes" 'complete (debugger-status d2))
      (test "step-n result" 3 (debugger-expr d2)))

;; Let expression
(let* ([d (make-debugger '(let ((x 5)) (prim 'add x 1)))]
       [d2 (step-n d 20)])
      (test "step-n let result" 6 (debugger-expr d2)))

;;; ============================================================
;;; Breakpoints
;;; ============================================================

(display "
Breakpoints:
")

;; Add breakpoint
(let* ([d (make-debugger '(let ((x 1)) x))]
       [d2 (add-breakpoint d (break-on-form 'let))])
      (test "add-breakpoint adds" 1 (length (debugger-breakpoints d2))))

;; Break on form
(let* ([d (make-debugger '(prim 'add (prim 'mul 2 3) 1))]
       [d2 (add-breakpoint d (break-on-form 'prim))]
       [d3 (continue d2)])
      (test "break on form status" 'breakpoint (debugger-status d3)))

;; Clear breakpoints
(let* ([d (make-debugger 42)]
       [d2 (add-breakpoint d (break-on-form 'let))]
       [d3 (clear-breakpoints d2)])
      (test "clear-breakpoints" 0 (length (debugger-breakpoints d3))))

;;; ============================================================
;;; Continue
;;; ============================================================

(display "
Continue:
")

;; Continue to completion
(let* ([d (make-debugger '(prim 'add 1 2))]
       [d2 (continue d)])
      (test "continue completes" 'complete (debugger-status d2))
      (test "continue result" 3 (debugger-expr d2)))

;; Factorial
(let* ([d (make-debugger
           '(fix fact (fn (n)
                          (if (prim 'eq? n 0)
                              1
                              (prim 'mul n (fact (prim 'sub n 1)))))))]
       [d2 (continue d)])
      (test "continue factorial closure" 'complete (debugger-status d2))
      (test "continue creates closure" 'closure (car (debugger-expr d2))))

;;; ============================================================
;;; History and Undo
;;; ============================================================

(display "
History and Undo:
")

;; Undo single step
(let* ([d (make-debugger '(prim 'add 1 2))]
       [d2 (step d)]
       [d3 (undo d2)])
      (test "undo restores expr" (debugger-expr d) (debugger-expr d3))
      (test "undo reduces history" 0 (length (debugger-history d3))))

;; Undo multiple steps - use an expression that requires multiple steps
(let* ([d (make-debugger '(let ((x 1)) (let ((y 2)) (prim 'add x y))))]
       [d2 (step-n d 5)]   ; Will take several steps
       [steps-before (length (debugger-history d2))]
       [d3 (undo-n d2 2)]
       [steps-after (length (debugger-history d3))])
      ;; After undo-n 2, history should be reduced by 2 (or 0 if less than 2 in history)
      (test "undo-n reduces history" #t
            (or (= steps-after (max 0 (- steps-before 2)))
                (= steps-after 0))))

;; Reset
(let* ([d (make-debugger '(prim 'add 1 2))]
       [d2 (step-n d 5)]
       [d3 (reset d2)])
      (test "reset clears history" 0 (length (debugger-history d3)))
      (test "reset clears trace" 0 (length (debugger-trace d3))))

;;; ============================================================
;;; Inspection
;;; ============================================================

(display "
Inspection:
")

(let* ([d (make-debugger '(prim 'add 1 2))]
       [d2 (step-n d 3)]
       [info (inspect d2)])
      (test "inspect has status" #t (pair? (assq 'status info)))
      (test "inspect has expression" #t (pair? (assq 'expression info)))
      (test "inspect has fuel" #t (pair? (assq 'fuel-remaining info)))
      (test "inspect has steps" #t (pair? (assq 'steps-taken info))))

;; Trace inspection
(let* ([d (make-debugger '(prim 'add 1 2))]
       [d2 (step-n d 5)]
       [trace (inspect-trace d2)])
      (test "trace non-empty" #t (pair? trace)))

;;; ============================================================
;;; Trace Expression
;;; ============================================================

(display "
Trace Expression:
")

(let ([trace (trace-expr '(prim 'add 1 2) 100)])
     (test "trace-expr returns list" #t (list? trace))
     (test "trace-expr has steps" #t (> (length trace) 0)))

;;; ============================================================
;;; Error Handling
;;; ============================================================

(display "
Error Handling:
")

;; Unbound variable
(let* ([d (make-debugger 'undefined)]
       [d2 (step d)])
      (test "error on unbound" 'error (debugger-status d2)))

;; Out of fuel - actually call the infinite loop
(let* ([expr '((fix loop (fn (x) (loop x))) 0)]  ; Call the loop with argument
       [d (make-debugger expr)]
       [d2 (debugger-set d 'fuel 50)]  ; Low fuel to trigger out-of-fuel
       [d3 (continue d2)])  ; Continue until done or out of fuel
      (test "out of fuel status" 'out-of-fuel (debugger-status d3)))

;;; ============================================================
;;; Complex Examples
;;; ============================================================

(display "
Complex Examples:
")

;; Map function with debugging
(let* ([d (make-debugger
           '(let ((double (fn (x) (prim 'mul x 2))))
             (let ((xs (prim 'list 1 2 3)))
                  (fix map-double (fn (lst)
                                      (if (prim 'null? lst)
                                          '()
                                          (prim 'cons (double (prim 'car lst))
                                                (map-double (prim 'cdr lst)))))))))]
       [d2 (run-debug (debugger-expr d) 1000)])
      (test "map double status" 'complete (debugger-status d2)))

;;; ============================================================
;;; Summary
;;; ============================================================

(newline)
(display "==================
")
(display (string-append "Passed: " (number->string tests-passed) "
"))
(display (string-append "Failed: " (number->string tests-failed) "
"))

(if (= tests-failed 0)
    (display "
✓ All debugger tests passed!
")
    (display "
✗ Some tests failed!
"))
