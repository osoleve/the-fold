(load "core/base/prelude.ss")
(load "core/lang/eval.ss")

(doc 'module 'debug)
(doc 'description "Stepping and Debugger for The Fold. Interactive debugging capabilities built on fuel suspension: single-step execution, step-n execution, breakpoints on forms, execution tracing, state inspection, resume from suspension. Debugger State: (debugger expr env fuel breakpoints trace history).")
(doc 'layer 'core)

(doc 'section 'debugger-state)

(define (make-debugger expr . opts)
  (doc 'type (-> Expr Env Nat Debugger))
  (doc 'description "Create a debugger session. Tracks: current expression, current environment, remaining fuel, breakpoints (list of predicates), trace log (list of steps taken), history (list of previous states for undo).")
  (doc 'export #t)
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
         (future . ())
         (status . ready))))

(define (debugger? d)
  (doc 'type (-> Any Bool))
  (doc 'description "Test if value is a debugger session.")
  (doc 'export #t)
  (and (pair? d) (eq? (car d) 'debugger)))

(define (debugger-get d key)
  (doc 'type (-> Debugger Symbol Any))
  (doc 'description "Get field from debugger session.")
  (doc 'export #t)
  (let ([entry (assq key (cdr d))])
       (and entry (cdr entry))))

(doc debugger-expr 'type (-> Debugger Expr))
(doc debugger-expr 'description "Get current expression from debugger.")
(doc debugger-expr 'export #t)
(define (debugger-expr d) (debugger-get d 'expr))

(doc debugger-env 'type (-> Debugger Env))
(doc debugger-env 'description "Get current environment from debugger.")
(doc debugger-env 'export #t)
(define (debugger-env d) (debugger-get d 'env))

(doc debugger-fuel 'type (-> Debugger Nat))
(doc debugger-fuel 'description "Get remaining fuel from debugger.")
(doc debugger-fuel 'export #t)
(define (debugger-fuel d) (debugger-get d 'fuel))

(doc debugger-breakpoints 'type (-> Debugger (List Predicate)))
(doc debugger-breakpoints 'description "Get breakpoints from debugger.")
(doc debugger-breakpoints 'export #t)
(define (debugger-breakpoints d) (debugger-get d 'breakpoints))

(doc debugger-trace 'type (-> Debugger (List TraceEntry)))
(doc debugger-trace 'description "Get execution trace from debugger.")
(doc debugger-trace 'export #t)
(define (debugger-trace d) (debugger-get d 'trace))

(doc debugger-history 'type (-> Debugger (List DebuggerState)))
(doc debugger-history 'description "Get state history from debugger for undo.")
(doc debugger-history 'export #t)
(define (debugger-history d) (debugger-get d 'history))

(doc debugger-future 'type (-> Debugger (List DebuggerState)))
(doc debugger-future 'description "Get future states from debugger for redo.")
(doc debugger-future 'export #t)
(define (debugger-future d) (or (debugger-get d 'future) '()))

(doc debugger-status 'type (-> Debugger Symbol))
(doc debugger-status 'description "Get current status from debugger.")
(doc debugger-status 'export #t)
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

(doc 'section 'single-step)

(doc *step-fuel* 'type Nat)
(doc *step-fuel* 'description "Fuel budget per debugger step. Each step gets enough fuel to complete one logical reduction.")
(doc *step-fuel* 'export #t)
(define *step-fuel* 100)

(define (step d)
  (doc 'type (-> Debugger Debugger))
  (doc 'description "Execute one reduction phase with per-step fuel budget. Saves full state (including trace) to history for proper time-travel.")
  (doc 'export #t)
  (let ([expr (debugger-expr d)]
        [env (debugger-env d)]
        [fuel (debugger-fuel d)]
        [trace (debugger-trace d)]
        [history (debugger-history d)])
       
       (if (zero? fuel)
           (debugger-set d 'status 'out-of-fuel)
           
           ;; Execute with step-fuel budget (min of available and step budget)
           (let* ([step-budget (min fuel *step-fuel*)]
                  [result (eval-expr expr env step-budget)]
                  ;; Save full state for proper undo/redo
                  [saved-state (list expr env fuel trace)])
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
                                            (history . ,(cons saved-state history))
                                            (future . ())  ;; Clear future on new step
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
                                           (history . ,(cons saved-state history))
                                           (future . ())  ;; Clear future on new step
                                           (status . ready))))]
                  
                  ;; Error
                  [else
                   (debugger-update d
                                    `((trace . ,(cons `(error ,(cadr result) ,(caddr result)) trace))
                                      (future . ())
                                      (status . error)
                                      (error . ,result)))])))))

(define (step-n d n)
  (doc 'type (-> Debugger Nat Debugger))
  (doc 'description "Execute n reduction steps.")
  (doc 'export #t)
  (if (or (zero? n)
          (eq? (debugger-status d) 'complete)
          (eq? (debugger-status d) 'error)
          (eq? (debugger-status d) 'out-of-fuel)
          (eq? (debugger-status d) 'breakpoint))
      d
      (step-n (step d) (- n 1))))

(doc 'section 'run-to-completion)

(define (run-until d pred)
  (doc 'type (-> Debugger (-> Expr Bool) Debugger))
  (doc 'description "Run debugger until predicate is satisfied or completion.")
  (doc 'export #t)
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

(define (continue d)
  (doc 'type (-> Debugger Debugger))
  (doc 'description "Continue execution until breakpoint or completion.")
  (doc 'export #t)
  (let ([breakpoints (debugger-breakpoints d)])
       (if (null? breakpoints)
           ;; No breakpoints - run to completion
           (run-until d (lambda (e) #f))
           ;; Check breakpoints
           (run-until d (lambda (e)
                                (ormap (lambda (bp) (bp e)) breakpoints))))))

(define (run-debug expr fuel)
  (doc 'type (-> Expr Nat Debugger))
  (doc 'description "Run expression with debugger to completion.")
  (doc 'export #t)
  (continue (make-debugger expr empty-env fuel)))

(doc 'section 'breakpoints)

(define (add-breakpoint d pred)
  (doc 'type (-> Debugger (-> Expr Bool) Debugger))
  (doc 'description "Add a breakpoint predicate to debugger.")
  (doc 'export #t)
  (let ([bps (debugger-breakpoints d)])
       (debugger-set d 'breakpoints (cons pred bps))))

(define (clear-breakpoints d)
  (doc 'type (-> Debugger Debugger))
  (doc 'description "Remove all breakpoints from debugger.")
  (doc 'export #t)
  (debugger-set d 'breakpoints '()))

(define (break-on-form sym)
  (doc 'type (-> Symbol (-> Expr Bool)))
  (doc 'description "Create a breakpoint predicate that triggers on forms starting with the given symbol.")
  (doc 'export #t)
  (lambda (e)
          (and (pair? e) (eq? (car e) sym))))

(define (break-on-var sym)
  (doc 'type (-> Symbol (-> Expr Bool)))
  (doc 'description "Create a breakpoint predicate that triggers on variable references.")
  (doc 'export #t)
  (lambda (e) (eq? e sym)))

(define (break-on-call name)
  (doc 'type (-> Symbol (-> Expr Bool)))
  (doc 'description "Create a breakpoint predicate that triggers on function calls.")
  (doc 'export #t)
  (lambda (e)
          (and (pair? e)
               (or (eq? (car e) name)
                   (and (eq? (car e) 'call)
                        (eq? (cadr e) name))))))

(define (break-on-value)
  (doc 'type (-> (-> Expr Bool)))
  (doc 'description "Create a breakpoint predicate that triggers on value expressions.")
  (doc 'export #t)
  (lambda (e) (value? e)))

;;; ====
;;; History, Undo, and Redo (Time-Travel)
;;; ====

;;; undo : Debugger → Debugger
;;; Go back one step, pushing current state to future for redo.
;;; Restores full state including trace, fuel-trace, watch-events for proper time-travel.
(define (undo d)
  (let ([history (debugger-history d)]
        [future (debugger-future d)]
        [expr (debugger-expr d)]
        [env (debugger-env d)]
        [fuel (debugger-fuel d)]
        [trace (debugger-trace d)]
        [fuel-trace (debugger-fuel-trace d)]
        [watch-events (debugger-watch-events d)])
       (if (null? history)
           d
           (let* ([prev (car history)]
                  [prev-len (length prev)]
                  ;; Save current state for redo (6-element format for fuel debugger)
                  [current-state (list expr env fuel trace fuel-trace watch-events)]
                  ;; Handle different history formats:
                  ;; 3-element (legacy): (expr env fuel)
                  ;; 4-element (basic step): (expr env fuel trace)
                  ;; 6-element (fuel debugger): (expr env fuel trace fuel-trace watch-events)
                  [prev-trace (if (> prev-len 3) (list-ref prev 3) '())]
                  [prev-fuel-trace (if (> prev-len 4) (list-ref prev 4) '())]
                  [prev-watch-events (if (> prev-len 5) (list-ref prev 5) '())])
                 (debugger-update d
                                  `((expr . ,(car prev))
                                    (env . ,(cadr prev))
                                    (fuel . ,(caddr prev))
                                    (trace . ,prev-trace)
                                    (fuel-trace . ,prev-fuel-trace)
                                    (watch-events . ,prev-watch-events)
                                    (history . ,(cdr history))
                                    (future . ,(cons current-state future))
                                    (status . ready)))))))

;;; undo-n : Debugger × Nat → Debugger
(define (undo-n d n)
  (if (zero? n)
      d
      (undo-n (undo d) (- n 1))))

;;; redo : Debugger → Debugger
;;; Go forward one step in the future stack.
;;; Restores full state including trace, fuel-trace, watch-events for proper time-travel.
(define (redo d)
  (let ([future (debugger-future d)]
        [history (debugger-history d)]
        [expr (debugger-expr d)]
        [env (debugger-env d)]
        [fuel (debugger-fuel d)]
        [trace (debugger-trace d)]
        [fuel-trace (debugger-fuel-trace d)]
        [watch-events (debugger-watch-events d)])
       (if (null? future)
           d
           (let* ([next-state (car future)]
                  [next-len (length next-state)]
                  ;; Save current state for undo (6-element format for fuel debugger)
                  [current-state (list expr env fuel trace fuel-trace watch-events)]
                  ;; Handle different future formats:
                  ;; 3-element (legacy): (expr env fuel)
                  ;; 4-element (basic step): (expr env fuel trace)
                  ;; 6-element (fuel debugger): (expr env fuel trace fuel-trace watch-events)
                  [next-trace (if (> next-len 3) (list-ref next-state 3) '())]
                  [next-fuel-trace (if (> next-len 4) (list-ref next-state 4) '())]
                  [next-watch-events (if (> next-len 5) (list-ref next-state 5) '())])
                 (debugger-update d
                                  `((expr . ,(car next-state))
                                    (env . ,(cadr next-state))
                                    (fuel . ,(caddr next-state))
                                    (trace . ,next-trace)
                                    (fuel-trace . ,next-fuel-trace)
                                    (watch-events . ,next-watch-events)
                                    (history . ,(cons current-state history))
                                    (future . ,(cdr future))
                                    (status . ready)))))))

;;; redo-n : Debugger × Nat → Debugger
(define (redo-n d n)
  (if (zero? n)
      d
      (redo-n (redo d) (- n 1))))

;;; can-undo? : Debugger → Boolean
(define (can-undo? d)
  (not (null? (debugger-history d))))

;;; can-redo? : Debugger → Boolean
(define (can-redo? d)
  (not (null? (debugger-future d))))

;;; reset : Debugger → Debugger
;;; Reset to initial state, clearing history, future, and all trace data.
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
                                   (future . ())
                                   (trace . ())
                                   (fuel-trace . ())
                                   (watch-events . ())
                                   (status . ready)))))))

;;; last provided by prelude

;;; ====
;;; Inspection
;;; ====

;;; inspect : Debugger → Alist
(define (inspect d)
  `((status . ,(debugger-status d))
    (expression . ,(debugger-expr d))
    (fuel-remaining . ,(debugger-fuel d))
    (steps-taken . ,(length (debugger-trace d)))
    (history-depth . ,(length (debugger-history d)))
    (future-depth . ,(length (debugger-future d)))
    (breakpoints . ,(length (debugger-breakpoints d)))
    (watches . ,(length (debugger-watches d)))))

;;; inspect-env : Debugger → Env
(define (inspect-env d)
  (debugger-env d))

;;; inspect-trace : Debugger → (List Sexp)
(define (inspect-trace d)
  (debugger-trace d))

;;; inspect-trace-last : Debugger × Nat → (List Sexp)
(define (inspect-trace-last d n)
  (take n (debugger-trace d)))

;;; trace-expr : Expr × Nat → (List Sexp)
(define (trace-expr expr fuel)
  (let ([d (run-debug expr fuel)])
       (reverse (debugger-trace d))))

;;; ====
;;; Watch Expressions
;;; ====

;;; add-watch : Debugger × Symbol → Debugger
;;; Add a variable to the watch list.
(define (add-watch d var)
  (let ([watches (debugger-watches d)])
       (if (memq var watches)
           d  ;; Already watching
           (debugger-set d 'watches (cons var watches)))))

;;; remove-watch : Debugger × Symbol → Debugger
;;; Remove a variable from the watch list.
(define (remove-watch d var)
  (let ([watches (debugger-watches d)])
       (debugger-set d 'watches (filter (lambda (w) (not (eq? w var))) watches))))

;;; clear-watches : Debugger → Debugger
;;; Remove all watches.
(define (clear-watches d)
  (debugger-set d 'watches '()))

;;; env-lookup-safe : Env × Symbol → Value | 'unbound
;;; Safely look up a variable in the environment.
(define (env-lookup-safe env var)
  (let loop ([e env])
       (cond
        [(null? e) 'unbound]
        [(eq? (caar e) var) (cdar e)]
        [else (loop (cdr e))])))

;;; check-watches : (List Symbol) × Env × Env × Nat × (List WatchEvent) → (List WatchEvent)
;;; Check watched variables for changes and return updated event log.
(define (check-watches watches old-env new-env step-num events)
  (let loop ([ws watches] [evs events])
       (if (null? ws)
           evs
           (let* ([var (car ws)]
                  [old-val (env-lookup-safe old-env var)]
                  [new-val (env-lookup-safe new-env var)])
                 (if (equal? old-val new-val)
                     (loop (cdr ws) evs)
                     (loop (cdr ws)
                           (cons `(watch-event
                                   (step-number . ,step-num)
                                   (variable . ,var)
                                   (old-value . ,old-val)
                                   (new-value . ,new-val))
                                 evs)))))))

;;; get-recent-watch-events : Debugger × Nat → (List WatchEvent)
;;; Get the N most recent watch events.
(define (get-recent-watch-events d n)
  (take n (debugger-watch-events d)))

;; filter provided by prelude

;;; ====
;;; Fuel-Tracking Debugger Extension
;;; ====
;;;
;;; Extended debugger state with detailed fuel tracking:
;;;   - fuel-trace: List of (expr fuel-before fuel-after fuel-consumed)
;;;   - fuel-budget: Initial fuel budget

;;; make-fuel-debugger : Expr × Env × Nat → Debugger
;;; Extended debugger with fuel tracking, redo capability, and watch expressions.
(define (make-fuel-debugger expr env fuel)
  `(debugger
    (expr . ,expr)
    (env . ,env)
    (fuel . ,fuel)
    (breakpoints . ())
    (trace . ())
    (history . ())
    (future . ())           ;; For redo (forward time-travel)
    (watches . ())          ;; Watch expressions
    (watch-events . ())     ;; Watch event log
    (status . ready)
    (fuel-budget . ,fuel)
    (fuel-trace . ())))

;;; fuel-debugger? : Debugger → Boolean
(define (fuel-debugger? d)
  (and (debugger? d)
       (pair? (assq 'fuel-budget (cdr d)))))

;;; debugger-fuel-budget : Debugger → Nat
(define (debugger-fuel-budget d)
  (or (debugger-get d 'fuel-budget)
      (debugger-fuel d)))

;;; debugger-fuel-trace : Debugger → (List Sexp)
(define (debugger-fuel-trace d)
  (or (debugger-get d 'fuel-trace) '()))

;;; debugger-watches : Debugger → (List Symbol)
;;; Get watched variables.
(define (debugger-watches d)
  (or (debugger-get d 'watches) '()))

;;; debugger-watch-events : Debugger → (List WatchEvent)
;;; Get watch event log.
(define (debugger-watch-events d)
  (or (debugger-get d 'watch-events) '()))

;;; debugger-fuel-used : Debugger → Nat
(define (debugger-fuel-used d)
  (- (debugger-fuel-budget d) (debugger-fuel d)))

;;; debugger-fuel-pct : Debugger → Number
(define (debugger-fuel-pct d)
  (let ([budget (debugger-fuel-budget d)])
       (if (zero? budget)
           0
           (* 100.0 (/ (debugger-fuel-used d) budget)))))

;;; ====
;;; Fuel-Tracking Step
;;; ====

;;; step-with-fuel : Debugger → Debugger
;;; Execute one step with fuel tracking, watch events, and timeline branching.
;;; Saves full state (including trace, fuel-trace, watch-events) for proper time-travel.
(define (step-with-fuel d)
  (let ([expr (debugger-expr d)]
        [env (debugger-env d)]
        [fuel (debugger-fuel d)]
        [fuel-trace (debugger-fuel-trace d)]
        [trace (debugger-trace d)]
        [history (debugger-history d)]
        [watches (debugger-watches d)]
        [watch-events (debugger-watch-events d)]
        [step-num (+ 1 (length (debugger-trace d)))])
       
       (if (zero? fuel)
           (debugger-set d 'status 'out-of-fuel)
           
           ;; Save full state BEFORE stepping for proper undo/redo
           (let* ([saved-state (list expr env fuel trace fuel-trace watch-events)]
                  [step-budget (min fuel *step-fuel*)]
                  [fuel-before fuel]
                  [result (eval-expr expr env step-budget)])
                 (cond
                  ;; Completed
                  [(eq? (car result) 'ok)
                   (let* ([value (cadr result)]
                          [remaining (caddr result)]
                          [used (- step-budget remaining)]
                          [fuel-after (- fuel used)]
                          [fuel-entry `(,expr ,fuel-before ,fuel-after ,used)]
                          ;; Check watched variables (env unchanged in ok case)
                          [new-watch-events (check-watches watches env env step-num watch-events)])
                         (debugger-update d
                                          `((expr . ,value)
                                            (fuel . ,fuel-after)
                                            (trace . ,(cons `(step ,expr -> ,value (fuel ,used)) trace))
                                            (fuel-trace . ,(cons fuel-entry fuel-trace))
                                            (history . ,(cons saved-state history))
                                            (future . ())  ;; Clear future on new step (timeline branch)
                                            (watch-events . ,new-watch-events)
                                            (status . ,(if (value? value) 'complete 'ready)))))]
                  
                  ;; Suspended
                  [(eq? (car result) 'suspended)
                   (let* ([new-expr (cadr result)]
                          [new-env (caddr result)]
                          [fuel-after (- fuel step-budget)]
                          [fuel-entry `(,expr ,fuel-before ,fuel-after ,step-budget)]
                          ;; Check watched variables for changes
                          [new-watch-events (check-watches watches env new-env step-num watch-events)])
                         (debugger-update d
                                          `((expr . ,new-expr)
                                            (env . ,new-env)
                                            (fuel . ,fuel-after)
                                            (trace . ,(cons `(step ,expr -> suspended (fuel ,step-budget)) trace))
                                            (fuel-trace . ,(cons fuel-entry fuel-trace))
                                            (history . ,(cons saved-state history))
                                            (future . ())  ;; Clear future on new step (timeline branch)
                                            (watch-events . ,new-watch-events)
                                            (status . ready))))]
                  
                  ;; Error
                  [else
                   (debugger-update d
                                    `((trace . ,(cons `(error ,(cadr result) ,(caddr result)) trace))
                                      (future . ())  ;; Clear future on error too
                                      (status . error)
                                      (error . ,result)))])))))

;;; ====
;;; Fuel Analysis
;;; ====

;;; fuel-by-expr-type : Debugger → Alist
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

;;; fuel-hotspots : Debugger × Nat → (List Sexp)
(define (fuel-hotspots d n)
  (let* ([fuel-trace (debugger-fuel-trace d)]
         [sorted (list-sort (lambda (a b)
                                    (> (cadddr a) (cadddr b)))
                            fuel-trace)])
        (take n sorted)))

;;; fuel-summary : Debugger → Alist
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

;;; ====
;;; Step-Over (Next)
;;; ====

;;; next : Debugger → Debugger
(define (next d)
  (let ([initial-depth (call-depth (debugger-expr d))])
       (step-until-depth d initial-depth)))

;;; call-depth : Expr → Nat
(define (call-depth expr)
  (cond
   [(not (pair? expr)) 0]
   [(memq (car expr) '(fn fix quote)) 0]
   [else
    (+ 1 (apply max (cons 0 (map call-depth (cdr expr)))))]))

;;; step-until-depth : Debugger × Nat → Debugger
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

;;; ====
;;; Continue with Fuel Tracking
;;; ====

;;; continue-with-fuel : Debugger → Debugger
(define (continue-with-fuel d)
  (let ([breakpoints (debugger-breakpoints d)])
       (if (null? breakpoints)
           (run-until-fuel d (lambda (e) #f))
           (run-until-fuel d (lambda (e)
                                     (ormap (lambda (bp) (bp e)) breakpoints))))))

;;; run-until-fuel : Debugger × (Expr → Boolean) → Debugger
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

;;; ====
;;; Explain-Why Traces
;;; ====
;;;
;;; Build causal explanations for evaluation results.
;;; Explains WHY an expression evaluated to a particular value
;;; by tracing through the evaluation history.

;;; explain-reason : Expr → Symbol
;;; Determine the reason type for an expression's evaluation.
(define (explain-reason expr)
  (cond
   [(not (pair? expr))
    (if (symbol? expr) 'lookup 'literal)]
   [else
    (let ([head (car expr)])
         (cond
          [(eq? head 'quote) 'literal]
          [(eq? head 'fn) 'literal]
          [(eq? head 'if) 'conditional]
          [(eq? head 'case) 'conditional]
          [(eq? head 'let) 'let-binding]
          [(eq? head 'prim) 'primitive]
          [(eq? head 'call) 'application]
          [else 'application]))]))

;;; make-explanation : Expr × Value × Symbol × (List Explanation) → Explanation
(define (make-explanation expr result reason children)
  `(explanation
    (expression . ,expr)
    (result . ,result)
    (reason . ,reason)
    (children . ,children)))

;;; explain-current : Debugger → Explanation
;;; Explain the current expression and result.
(define (explain-current d)
  (let* ([expr (debugger-expr d)]
         [status (debugger-status d)]
         [trace (debugger-trace d)]
         [reason (explain-reason expr)])
        (make-explanation
         expr
         (if (eq? status 'complete) expr 'incomplete)
         reason
         '())))

;;; explain-step-entry : TraceEntry → Explanation
;;; Convert a single trace entry to an explanation.
(define (explain-step-entry entry)
  (if (and (pair? entry) (eq? (car entry) 'step))
      (let* ([from-expr (cadr entry)]
             [to-expr (cadddr entry)]  ;; Skip the '->' symbol
             [reason (explain-reason from-expr)])
            (make-explanation from-expr to-expr reason '()))
      #f))

;;; explain-trace : Debugger → (List Explanation)
;;; Build explanations from the execution trace.
(define (explain-trace d)
  (let ([trace (reverse (debugger-trace d))])
       (filter-map-explain explain-step-entry trace)))

;;; filter-map-explain : (α → β | #f) × (List α) → (List β)
(define (filter-map-explain f lst)
  (let loop ([l lst] [acc '()])
       (if (null? l)
           (reverse acc)
           (let ([result (f (car l))])
                (if result
                    (loop (cdr l) (cons result acc))
                    (loop (cdr l) acc))))))

;;; explain-result : Debugger → Explanation
;;; Build a complete explanation tree for how we got to the current result.
(define (explain-result d)
  (let* ([expr (debugger-expr d)]
         [status (debugger-status d)]
         [trace (reverse (debugger-trace d))]
         [children (filter-map-explain explain-step-entry trace)])
        (if (null? children)
            (make-explanation expr expr (explain-reason expr) '())
            (let ([first-step (car children)])
                 (make-explanation
                  (cdr (assq 'expression (cdr first-step)))  ;; Skip 'explanation tag
                  expr
                  'evaluation-sequence
                  children)))))

;;; let-binds-var? : Expr × Symbol → Boolean
;;; Check if a let expression binds a specific variable.
;;; let syntax: (let ((var1 val1) (var2 val2) ...) body)
(define (let-binds-var? let-expr var)
  (and (pair? let-expr)
       (eq? (car let-expr) 'let)
       (pair? (cdr let-expr))
       (let ([bindings (cadr let-expr)])
            (and (list? bindings)
                 (ormap (lambda (binding)
                                (and (pair? binding)
                                     (eq? (car binding) var)))
                        bindings)))))

;;; explain-binding : Debugger × Symbol → Explanation | #f
;;; Explain how a binding got its value by searching the trace.
;;; Now properly verifies the variable is actually bound by the let expression.
(define (explain-binding d var)
  (let ([env (debugger-env d)]
        [trace (debugger-trace d)])
       (let ([val (env-lookup-safe env var)])
            (if (eq? val 'unbound)
                #f
                ;; Search trace for let-bindings that introduced THIS variable
                (let loop ([entries trace])
                     (if (null? entries)
                         (make-explanation var val 'initial-binding '())
                         (let ([entry (car entries)])
                              (if (and (pair? entry)
                                       (eq? (car entry) 'step)
                                       (let ([from-expr (cadr entry)])
                                            (let-binds-var? from-expr var)))
                                  (make-explanation var val 'let-binding
                                                    (list (explain-step-entry entry)))
                                  (loop (cdr entries))))))))))

;;; ====
;;; Structured Trace Export
;;; ====
;;;
;;; Export trace data in machine-readable S-expression format.

;;; export-trace : Debugger → TraceExport
;;; Export the full trace as a structured S-expression.
(define (export-trace d)
  (let* ([trace (reverse (debugger-trace d))]
         [history (debugger-history d)]
         [initial-expr (if (null? history)
                           (debugger-expr d)
                           (car (last history)))]
         [final-result (if (eq? (debugger-status d) 'complete)
                           (debugger-expr d)
                           'incomplete)]
         [steps (build-trace-steps trace 1)])
        `(trace-export
          (initial-expression . ,initial-expr)
          (final-result . ,final-result)
          (fuel-budget . ,(debugger-fuel-budget d))
          (fuel-used . ,(debugger-fuel-used d))
          (status . ,(debugger-status d))
          (step-count . ,(length steps))
          (steps . ,steps)
          (watch-events . ,(debugger-watch-events d)))))

;;; build-trace-steps : (List TraceEntry) × Nat → (List TraceStep)
(define (build-trace-steps entries num)
  (if (null? entries)
      '()
      (let ([entry (car entries)])
           (if (and (pair? entry) (eq? (car entry) 'step))
               (cons (build-trace-step entry num)
                     (build-trace-steps (cdr entries) (+ num 1)))
               (build-trace-steps (cdr entries) num)))))

;;; build-trace-step : TraceEntry × Nat → TraceStep
(define (build-trace-step entry num)
  (let* ([from-expr (cadr entry)]
         [to-expr (cadddr entry)]
         [fuel-info (if (> (length entry) 4) (cddddr entry) '())]
         [fuel-consumed (if (and (pair? fuel-info)
                                 (pair? (car fuel-info))
                                 (eq? (caar fuel-info) 'fuel))
                            (cadar fuel-info)
                            0)])
        `(trace-step
          (number . ,num)
          (from . ,from-expr)
          (to . ,to-expr)
          (fuel-consumed . ,fuel-consumed)
          (reason . ,(explain-reason from-expr)))))

;;; trace->sexp : Debugger → Sexp
;;; Alias for export-trace.
(define trace->sexp export-trace)
