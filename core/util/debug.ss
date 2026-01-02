;;; fabric/stitches/debug.ss — Stepping and Debugger for The Fold
;;;
;;; Interactive debugging capabilities built on fuel suspension:
;;;   - Single-step execution
;;;   - Step-n execution
;;;   - Breakpoints on forms
;;;   - Execution tracing
;;;   - State inspection
;;;   - Resume from suspension
;;;
;;; Debugger State:
;;;   (debugger expr env fuel breakpoints trace history)
;;;
;;; This is Core code: pure evaluation with debugger wrapper.
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - eval.ss

(load "core/base/prelude.ss")
(load "core/lang/eval.ss")

;;; ============================================================
;;; Debugger State
;;; ============================================================

;;; A debugger session tracks:
;;;   - current expression
;;;   - current environment
;;;   - remaining fuel
;;;   - breakpoints (list of predicates)
;;;   - trace log (list of steps taken)
;;;   - history (list of previous states for undo)

(define (make-debugger expr . opts)
  (let ([env (if (and (pair? opts) (pair? (car opts)))
                 (car opts)
                 empty-env)]
        [fuel (if (and (pair? opts) (pair? (cdr opts)))
                  (cadr opts)
                  10000)])
       `(debugger
         (expr . ,expr)
         (env . ,env)
         (fuel . ,fuel)
         (breakpoints . ())
         (trace . ())
         (history . ())
         (status . ready))))

(define (debugger? d)
  (and (pair? d) (eq? (car d) 'debugger)))

(define (debugger-get d key)
  (let ([entry (assq key (cdr d))])
       (and entry (cdr entry))))

(define (debugger-expr d) (debugger-get d 'expr))
(define (debugger-env d) (debugger-get d 'env))
(define (debugger-fuel d) (debugger-get d 'fuel))
(define (debugger-breakpoints d) (debugger-get d 'breakpoints))
(define (debugger-trace d) (debugger-get d 'trace))
(define (debugger-history d) (debugger-get d 'history))
(define (debugger-status d) (debugger-get d 'status))

(define (debugger-set d key value)
  (cons 'debugger
        (map (lambda (entry)
                     (if (eq? (car entry) key)
                         (cons key value)
                         entry))
             (cdr d))))

(define (debugger-update d updates)
  (fold-left (lambda (d update)
                     (debugger-set d (car update) (cdr update)))
             d
             updates))

;;; ============================================================
;;; Single Step
;;; ============================================================

;;; *step-fuel* : Fuel budget per debugger step
;;; Each step gets enough fuel to complete one "logical" reduction.
(define *step-fuel* 100)

;;; step : Debugger → Debugger
;;; Execute one reduction phase with per-step fuel budget.
(define (step d)
  (let ([expr (debugger-expr d)]
        [env (debugger-env d)]
        [fuel (debugger-fuel d)]
        [trace (debugger-trace d)]
        [history (debugger-history d)])
       
       (if (zero? fuel)
           (debugger-set d 'status 'out-of-fuel)
           
           ;; Execute with step-fuel budget (min of available and step budget)
           (let* ([step-budget (min fuel *step-fuel*)]
                  [result (eval-expr expr env step-budget)])
                 (cond
                  ;; Completed
                  [(eq? (car result) 'ok)
                   (let* ([value (cadr result)]
                          [remaining (caddr result)]
                          [used (- step-budget remaining)])
                         (debugger-update d
                                          `((expr . ,value)
                                            (fuel . ,(- fuel used))
                                            (trace . ,(cons `(step ,expr -> ,value) trace))
                                            (history . ,(cons (list expr env fuel) history))
                                            (status . ,(if (value? value) 'complete 'ready)))))]
                  
                  ;; Suspended (expr reduced but not to value - ran out of step budget)
                  [(eq? (car result) 'suspended)
                   (let ([new-expr (cadr result)]
                         [new-env (caddr result)])
                        (debugger-update d
                                         `((expr . ,new-expr)
                                           (env . ,new-env)
                                           (fuel . ,(- fuel step-budget))
                                           (trace . ,(cons `(step ,expr -> suspended) trace))
                                           (history . ,(cons (list expr env fuel) history))
                                           (status . ready))))]
                  
                  ;; Error
                  [else
                   (debugger-update d
                                    `((trace . ,(cons `(error ,(cadr result) ,(caddr result)) trace))
                                      (status . error)
                                      (error . ,result)))])))))

;;; step-n : Debugger × Nat → Debugger
;;; Execute n reduction steps.
(define (step-n d n)
  (if (or (zero? n)
          (eq? (debugger-status d) 'complete)
          (eq? (debugger-status d) 'error)
          (eq? (debugger-status d) 'out-of-fuel)
          (eq? (debugger-status d) 'breakpoint))
      d
      (step-n (step d) (- n 1))))

;;; ============================================================
;;; Run to Completion / Breakpoint
;;; ============================================================

;;; run-until : Debugger × (Expr → Bool) → Debugger
;;; Run until predicate matches or completion/error.
(define (run-until d pred)
  (let loop ([d d])
       (let ([status (debugger-status d)]
             [expr (debugger-expr d)])
            (cond
             [(eq? status 'complete) d]
             [(eq? status 'error) d]
             [(eq? status 'out-of-fuel) d]
             [(pred expr)
              (debugger-set d 'status 'breakpoint)]
             [else
              (loop (step d))]))))

;;; continue : Debugger → Debugger
;;; Run until completion, error, or breakpoint.
(define (continue d)
  (let ([breakpoints (debugger-breakpoints d)])
       (if (null? breakpoints)
           ;; No breakpoints - run to completion
           (run-until d (lambda (e) #f))
           ;; Check breakpoints
           (run-until d (lambda (e)
                                (ormap (lambda (bp) (bp e)) breakpoints))))))

;;; run-debug : Expr × Fuel → Debugger
;;; Create and run a debugger session.
(define (run-debug expr fuel)
  (continue (make-debugger expr empty-env fuel)))

;;; ============================================================
;;; Breakpoints
;;; ============================================================

;;; add-breakpoint : Debugger × (Expr → Bool) → Debugger
;;; Add a breakpoint predicate.
(define (add-breakpoint d pred)
  (let ([bps (debugger-breakpoints d)])
       (debugger-set d 'breakpoints (cons pred bps))))

;;; clear-breakpoints : Debugger → Debugger
(define (clear-breakpoints d)
  (debugger-set d 'breakpoints '()))

;;; Common breakpoint predicates:

;;; break-on-form : Symbol → (Expr → Bool)
;;; Break when expression starts with given form.
(define (break-on-form sym)
  (lambda (e)
          (and (pair? e) (eq? (car e) sym))))

;;; break-on-var : Symbol → (Expr → Bool)
;;; Break when expression is a variable reference.
(define (break-on-var sym)
  (lambda (e) (eq? e sym)))

;;; break-on-call : Symbol → (Expr → Bool)
;;; Break when calling a specific function.
(define (break-on-call name)
  (lambda (e)
          (and (pair? e)
               (or (eq? (car e) name)
                   (and (eq? (car e) 'call)
                        (eq? (cadr e) name))))))

;;; break-on-value : → (Expr → Bool)
;;; Break when expression is a value.
(define (break-on-value)
  (lambda (e) (value? e)))

;;; ============================================================
;;; History and Undo
;;; ============================================================

;;; undo : Debugger → Debugger
;;; Undo the last step.
(define (undo d)
  (let ([history (debugger-history d)])
       (if (null? history)
           d
           (let ([prev (car history)])
                (debugger-update d
                                 `((expr . ,(car prev))
                                   (env . ,(cadr prev))
                                   (fuel . ,(caddr prev))
                                   (history . ,(cdr history))
                                   (status . ready)))))))

;;; undo-n : Debugger × Nat → Debugger
;;; Undo n steps.
(define (undo-n d n)
  (if (zero? n)
      d
      (undo-n (undo d) (- n 1))))

;;; reset : Debugger → Debugger
;;; Reset to initial state (from history).
(define (reset d)
  (let ([history (debugger-history d)])
       (if (null? history)
           d
           (let ([initial (last history)])
                (debugger-update d
                                 `((expr . ,(car initial))
                                   (env . ,(cadr initial))
                                   (fuel . ,(caddr initial))
                                   (history . ())
                                   (trace . ())
                                   (status . ready)))))))

;;; Helper: get last element
(define (last lst)
  (if (null? (cdr lst))
      (car lst)
      (last (cdr lst))))

;;; ============================================================
;;; Inspection
;;; ============================================================

;;; inspect : Debugger → Alist
;;; Get human-readable state.
(define (inspect d)
  `((status . ,(debugger-status d))
    (expression . ,(debugger-expr d))
    (fuel-remaining . ,(debugger-fuel d))
    (steps-taken . ,(length (debugger-trace d)))
    (history-depth . ,(length (debugger-history d)))
    (breakpoints . ,(length (debugger-breakpoints d)))))

;;; inspect-env : Debugger → Alist
;;; Get environment bindings.
(define (inspect-env d)
  (debugger-env d))

;;; inspect-trace : Debugger → List
;;; Get execution trace (most recent first).
(define (inspect-trace d)
  (debugger-trace d))

;;; inspect-trace-last : Debugger × Nat → List
;;; Get last n trace entries.
(define (inspect-trace-last d n)
  (take n (debugger-trace d)))

;;; trace-expr : Expr × Fuel → List
;;; Evaluate and return full trace.
(define (trace-expr expr fuel)
  (let ([d (run-debug expr fuel)])
       (reverse (debugger-trace d))))

;;; ============================================================
;;; Fuel-Tracking Debugger Extension
;;; ============================================================
;;;
;;; Extended debugger state with detailed fuel tracking:
;;;   - fuel-trace: List of (expr fuel-before fuel-after fuel-consumed)
;;;   - fuel-budget: Initial fuel budget
;;;   - call-stack: Current call stack with fuel annotations

;;; make-fuel-debugger : Expr × Env × Fuel → Debugger
;;; Create a debugger with fuel tracking extensions.
(define (make-fuel-debugger expr env fuel)
  `(debugger
    (expr . ,expr)
    (env . ,env)
    (fuel . ,fuel)
    (breakpoints . ())
    (trace . ())
    (history . ())
    (status . ready)
    (fuel-budget . ,fuel)
    (fuel-trace . ())
    (call-stack . ())))

;;; fuel-debugger? : Debugger → Boolean
(define (fuel-debugger? d)
  (and (debugger? d)
       (pair? (assq 'fuel-budget (cdr d)))))

;;; debugger-fuel-budget : Debugger → Nat
(define (debugger-fuel-budget d)
  (or (debugger-get d 'fuel-budget)
      (debugger-fuel d)))

;;; debugger-fuel-trace : Debugger → List
(define (debugger-fuel-trace d)
  (or (debugger-get d 'fuel-trace) '()))

;;; debugger-call-stack : Debugger → List
(define (debugger-call-stack d)
  (or (debugger-get d 'call-stack) '()))

;;; debugger-fuel-used : Debugger → Nat
;;; Calculate total fuel consumed.
(define (debugger-fuel-used d)
  (- (debugger-fuel-budget d) (debugger-fuel d)))

;;; debugger-fuel-pct : Debugger → Number
;;; Calculate percentage of fuel used.
(define (debugger-fuel-pct d)
  (let ([budget (debugger-fuel-budget d)])
       (if (zero? budget)
           0
           (* 100.0 (/ (debugger-fuel-used d) budget)))))

;;; ============================================================
;;; Fuel-Tracking Step
;;; ============================================================

;;; step-with-fuel : Debugger → Debugger
;;; Execute one step and record fuel consumption details.
(define (step-with-fuel d)
  (let ([expr (debugger-expr d)]
        [env (debugger-env d)]
        [fuel (debugger-fuel d)]
        [fuel-trace (debugger-fuel-trace d)]
        [trace (debugger-trace d)]
        [history (debugger-history d)]
        [call-stack (debugger-call-stack d)])
       
       (if (zero? fuel)
           (debugger-set d 'status 'out-of-fuel)
           
           (let* ([step-budget (min fuel *step-fuel*)]
                  [fuel-before fuel]
                  [result (eval-expr expr env step-budget)])
                 (cond
                  ;; Completed
                  [(eq? (car result) 'ok)
                   (let* ([value (cadr result)]
                          [remaining (caddr result)]
                          [used (- step-budget remaining)]
                          [fuel-after (- fuel used)]
                          [fuel-entry `(,expr ,fuel-before ,fuel-after ,used)])
                         (debugger-update d
                                          `((expr . ,value)
                                            (fuel . ,fuel-after)
                                            (trace . ,(cons `(step ,expr -> ,value (fuel ,used)) trace))
                                            (fuel-trace . ,(cons fuel-entry fuel-trace))
                                            (history . ,(cons (list expr env fuel) history))
                                            (status . ,(if (value? value) 'complete 'ready)))))]
                  
                  ;; Suspended
                  [(eq? (car result) 'suspended)
                   (let* ([new-expr (cadr result)]
                          [new-env (caddr result)]
                          [fuel-after (- fuel step-budget)]
                          [fuel-entry `(,expr ,fuel-before ,fuel-after ,step-budget)])
                         (debugger-update d
                                          `((expr . ,new-expr)
                                            (env . ,new-env)
                                            (fuel . ,fuel-after)
                                            (trace . ,(cons `(step ,expr -> suspended (fuel ,step-budget)) trace))
                                            (fuel-trace . ,(cons fuel-entry fuel-trace))
                                            (history . ,(cons (list expr env fuel) history))
                                            (status . ready))))]
                  
                  ;; Error
                  [else
                   (debugger-update d
                                    `((trace . ,(cons `(error ,(cadr result) ,(caddr result)) trace))
                                      (status . error)
                                      (error . ,result)))])))))

;;; ============================================================
;;; Fuel Analysis
;;; ============================================================

;;; fuel-by-expr-type : Debugger → Alist
;;; Group fuel consumption by expression type.
(define (fuel-by-expr-type d)
  (let ([fuel-trace (debugger-fuel-trace d)])
       (let loop ([entries fuel-trace] [acc '()])
            (if (null? entries)
                acc
                (let* ([entry (car entries)]
                       [expr (car entry)]
                       [fuel-used (cadddr entry)]
                       [type (expr-type expr)]
                       [existing (assq type acc)])
                      (loop (cdr entries)
                            (if existing
                                (map (lambda (e)
                                             (if (eq? (car e) type)
                                                 (cons type (+ fuel-used (cdr e)))
                                                 e))
                                     acc)
                                (cons (cons type fuel-used) acc))))))))

;;; expr-type : Expr → Symbol
;;; Classify expression for fuel analysis.
(define (expr-type expr)
  (cond
   [(not (pair? expr)) 'literal]
   [(symbol? expr) 'variable]
   [else
    (let ([head (car expr)])
         (cond
          [(eq? head 'fn) 'lambda]
          [(eq? head 'fix) 'fix]
          [(eq? head 'let) 'let]
          [(eq? head 'if) 'if]
          [(eq? head 'case) 'case]
          [(eq? head 'prim) 'prim]
          [(eq? head 'call) 'call]
          [(eq? head 'quote) 'quote]
          [else 'application]))]))

;;; fuel-hotspots : Debugger × Nat → List
;;; Get top N fuel-consuming steps.
(define (fuel-hotspots d n)
  (let* ([fuel-trace (debugger-fuel-trace d)]
         [sorted (list-sort (lambda (a b)
                                    (> (cadddr a) (cadddr b)))
                            fuel-trace)])
        (take-up-to-debug n sorted)))

;;; take-up-to-debug : Nat × List → List
(define (take-up-to-debug n lst)
  (if (or (zero? n) (null? lst))
      '()
      (cons (car lst) (take-up-to-debug (- n 1) (cdr lst)))))

;;; fuel-summary : Debugger → Alist
;;; Get fuel consumption summary.
(define (fuel-summary d)
  (let ([budget (debugger-fuel-budget d)]
        [remaining (debugger-fuel d)]
        [used (debugger-fuel-used d)]
        [pct (debugger-fuel-pct d)]
        [by-type (fuel-by-expr-type d)]
        [steps (length (debugger-trace d))])
       `((budget . ,budget)
         (remaining . ,remaining)
         (used . ,used)
         (percentage . ,pct)
         (steps . ,steps)
         (by-type . ,by-type))))

;;; ============================================================
;;; Step-Over (Next)
;;; ============================================================

;;; next : Debugger → Debugger
;;; Step over: execute until the current expression fully evaluates
;;; without descending into sub-expressions during display.
(define (next d)
  (let ([initial-depth (call-depth (debugger-expr d))])
       (step-until-depth d initial-depth)))

;;; call-depth : Expr → Nat
;;; Estimate nesting depth of expression.
(define (call-depth expr)
  (cond
   [(not (pair? expr)) 0]
   [(memq (car expr) '(fn fix quote)) 0]
   [else
    (+ 1 (apply max (cons 0 (map call-depth (cdr expr)))))]))

;;; step-until-depth : Debugger × Nat → Debugger
;;; Step until expression depth decreases or completion.
;;; Guards against stepping when debugger is already in terminal state.
(define (step-until-depth d target-depth)
  ;; Check terminal states before stepping
  (let ([status (debugger-status d)])
       (if (memq status '(complete error out-of-fuel breakpoint))
           d
           (let loop ([d (step-with-fuel d)])
                (let ([status (debugger-status d)]
                      [current-depth (call-depth (debugger-expr d))])
                     (cond
                      [(eq? status 'complete) d]
                      [(eq? status 'error) d]
                      [(eq? status 'out-of-fuel) d]
                      [(eq? status 'breakpoint) d]
                      [(<= current-depth target-depth) d]
                      [else (loop (step-with-fuel d))]))))))

;;; ============================================================
;;; Continue with Fuel Tracking
;;; ============================================================

;;; continue-with-fuel : Debugger → Debugger
;;; Run to completion/breakpoint with fuel tracking.
(define (continue-with-fuel d)
  (let ([breakpoints (debugger-breakpoints d)])
       (if (null? breakpoints)
           (run-until-fuel d (lambda (e) #f))
           (run-until-fuel d (lambda (e)
                                     (ormap (lambda (bp) (bp e)) breakpoints))))))

;;; run-until-fuel : Debugger × (Expr → Bool) → Debugger
;;; Run with fuel tracking until predicate matches.
(define (run-until-fuel d pred)
  (let loop ([d d])
       (let ([status (debugger-status d)]
             [expr (debugger-expr d)])
            (cond
             [(eq? status 'complete) d]
             [(eq? status 'error) d]
             [(eq? status 'out-of-fuel) d]
             [(pred expr)
              (debugger-set d 'status 'breakpoint)]
             [else
              (loop (step-with-fuel d))]))))
