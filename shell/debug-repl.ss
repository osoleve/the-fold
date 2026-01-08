;;; shell/debug-repl.ss — REPL Debugger Commands
;;;
;;; Interactive debugging commands:
;;;   (debug expr)     - Start debugging
;;;   (step)           - Single step
;;;   (next)           - Step over
;;;   (continue)       - Run to breakpoint/completion
;;;   (break 'fn)      - Set breakpoint
;;;   (inspect)        - Show environment
;;;   (fuel)           - Show fuel status
;;;   (trace)          - Show call stack
;;;
;;; This is Shell code: REPL interface for debugger.
;;;
;;; Dependencies:
;;;   - shell/session-debugger.ss
;;;   - shell/fuel-viz.ss (optional, for rich display)

(load "shell/session-debugger.ss")

;;; ============================================================
;;; Configuration
;;; ============================================================

(define *default-debug-fuel* 10000)

;;; ============================================================
;;; Display Helpers
;;; ============================================================

;;; truncate-expr : Expr × Nat → String
(define (truncate-expr-str expr max-len)
  (let* ([str (format "~a" expr)]
         [len (string-length str)])
        (if (> len max-len)
            (string-append (substring str 0 (- max-len 3)) "...")
            str)))

;;; status-symbol : Symbol → String
(define (status-symbol status)
  (case status
        [(ready) ">>"]
        [(complete) "OK"]
        [(error) "!!"]
        [(out-of-fuel) "XX"]
        [(breakpoint) "**"]
        [else "??"]))

;;; fuel-bar : Nat × Nat × Nat → String
;;; Create ASCII fuel bar with clamping for safety.
(define (fuel-bar used total width)
  (let* ([pct (if (zero? total) 0 (min 1.0 (/ used total)))]
         [filled (exact (min width (max 0 (round (* width pct)))))]
         [empty (exact (max 0 (- width filled)))])
        (string-append
         "["
         (make-string filled #\#)
         (make-string empty #\-)
         "]")))

;;; ============================================================
;;; Debug Command
;;; ============================================================

;;; cmd-debug : Expr [× Fuel] → void
;;; Start a new debug session.
(define cmd-debug
  (case-lambda
   [(expr) (cmd-debug expr *default-debug-fuel*)]
   [(expr fuel)
    (let ([dbg (start-debug-session! expr empty-env fuel)])
         (display "\n")
         (display "  ============================================\n")
         (display "           DEBUG SESSION STARTED\n")
         (display "  ============================================\n\n")
         (display (format "  Expression: ~a\n" (truncate-expr-str expr 50)))
         (display (format "  Fuel budget: ~a\n" fuel))
         (display "  Breakpoints: (none)\n\n")
         (display "  [Ready] Use (step) to begin, (help 'debug) for commands.\n\n")
         (void))]))

;;; ============================================================
;;; Step Command
;;; ============================================================

;;; cmd-step : → void
;;; Execute one step.
(define (cmd-step)
  (let* ([dbg (require-debugger!)]
         [dbg* (step-with-fuel dbg)]
         [status (debugger-status dbg*)]
         [expr (debugger-expr dbg*)]
         [steps (length (debugger-trace dbg*))]
         [fuel-trace (debugger-fuel-trace dbg*)])
        
        (set-session-debugger! dbg*)
        
        (display "\n")
        (display (format "  Step #~a ~a\n" steps (status-symbol status)))
        (display "  ────────────────────────────────────────────\n")
        (display (format "  Expression: ~a\n" (truncate-expr-str expr 50)))
        
        ;; Show fuel consumed this step
        (when (pair? fuel-trace)
              (let ([last-entry (car fuel-trace)])
                   (display (format "  Fuel: ~a consumed, ~a remaining ~a ~a%\n"
                                    (cadddr last-entry)
                                    (debugger-fuel dbg*)
                                    (fuel-bar (debugger-fuel-used dbg*)
                                              (debugger-fuel-budget dbg*)
                                              20)
                                    (round (debugger-fuel-pct dbg*))))))
        
        ;; Show environment if not empty
        (let ([env (debugger-env dbg*)])
             (unless (null? env)
                     (display "\n  Environment:\n")
                     (for-each
                      (lambda (binding)
                              (display (format "    ~a = ~a\n"
                                               (car binding)
                                               (truncate-expr-str (cdr binding) 40))))
                      (take-up-to-repl 5 env))))
        
        ;; Status message
        (case status
              [(complete)
               (display "\n  [Complete] Evaluation finished.\n")]
              [(error)
               (display "\n  [Error] Evaluation failed.\n")
               (display (format "    ~a\n" (debugger-get dbg* 'error)))]
              [(out-of-fuel)
               (display "\n  [Out of Fuel] Increase budget with (debug expr fuel).\n")]
              [(breakpoint)
               (display "\n  [Breakpoint] Hit a breakpoint.\n")])
        
        (display "\n")
        (void)))

;;; take-up-to-repl : Nat × List → List
(define (take-up-to-repl n lst)
  (if (or (zero? n) (null? lst))
      '()
      (cons (car lst) (take-up-to-repl (- n 1) (cdr lst)))))

;;; ============================================================
;;; Next Command (Step Over)
;;; ============================================================

;;; cmd-next : → void
;;; Step over the current expression.
(define (cmd-next)
  (let* ([dbg (require-debugger!)]
         [dbg* (next dbg)])
        
        (set-session-debugger! dbg*)
        
        (let ([status (debugger-status dbg*)]
              [expr (debugger-expr dbg*)]
              [steps (length (debugger-trace dbg*))])
             
             (display "\n")
             (display (format "  Step #~a ~a (stepped over)\n" steps (status-symbol status)))
             (display "  ────────────────────────────────────────────\n")
             (display (format "  Expression: ~a\n" (truncate-expr-str expr 50)))
             (display (format "  Fuel: ~a used, ~a remaining ~a\n"
                              (debugger-fuel-used dbg*)
                              (debugger-fuel dbg*)
                              (fuel-bar (debugger-fuel-used dbg*)
                                        (debugger-fuel-budget dbg*)
                                        20)))
             (display "\n")
             (void))))

;;; ============================================================
;;; Continue Command
;;; ============================================================

;;; cmd-continue : → void
;;; Run to completion or breakpoint.
(define (cmd-continue)
  (let* ([dbg (require-debugger!)]
         [dbg* (continue-with-fuel dbg)]
         [status (debugger-status dbg*)]
         [expr (debugger-expr dbg*)]
         [steps (length (debugger-trace dbg*))])
        
        (set-session-debugger! dbg*)
        
        (display "\n")
        (case status
              [(complete)
               (display "  ============================================\n")
               (display "             EVALUATION COMPLETE\n")
               (display "  ============================================\n\n")
               (display (format "  Result: ~a\n" (truncate-expr-str expr 50)))
               (display (format "  Steps: ~a\n" steps))
               (display (format "  Fuel: ~a used of ~a (~a%)\n"
                                (debugger-fuel-used dbg*)
                                (debugger-fuel-budget dbg*)
                                (round (debugger-fuel-pct dbg*))))]
              [(breakpoint)
               (display "  ============================================\n")
               (display "               BREAKPOINT HIT\n")
               (display "  ============================================\n\n")
               (display (format "  Expression: ~a\n" (truncate-expr-str expr 50)))
               (display (format "  Fuel: ~a used ~a\n"
                                (debugger-fuel-used dbg*)
                                (fuel-bar (debugger-fuel-used dbg*)
                                          (debugger-fuel-budget dbg*)
                                          20)))]
              [(error)
               (display "  ============================================\n")
               (display "                   ERROR\n")
               (display "  ============================================\n\n")
               (display (format "  ~a\n" (debugger-get dbg* 'error)))]
              [(out-of-fuel)
               (display "  ============================================\n")
               (display "               OUT OF FUEL\n")
               (display "  ============================================\n\n")
               (display (format "  Expression: ~a\n" (truncate-expr-str expr 50)))
               (display "  Increase fuel budget with (debug expr fuel)\n")])
        
        (display "\n")
        (void)))

;;; ============================================================
;;; Breakpoint Commands
;;; ============================================================

;;; cmd-break : Symbol → void
;;; Set a breakpoint on a function name.
(define (cmd-break fn-name)
  (let* ([dbg (require-debugger!)]
         [dbg* (add-breakpoint dbg (break-on-call fn-name))])
        (set-session-debugger! dbg*)
        (display (format "  Breakpoint added: break on '~a'\n" fn-name))
        (void)))

;;; cmd-clear-breaks : → void
;;; Clear all breakpoints.
(define (cmd-clear-breaks)
  (let* ([dbg (require-debugger!)]
         [dbg* (clear-breakpoints dbg)])
        (set-session-debugger! dbg*)
        (display "  All breakpoints cleared.\n")
        (void)))

;;; ============================================================
;;; Inspect Command
;;; ============================================================

;;; cmd-inspect : → void
;;; Show current environment bindings.
(define (cmd-inspect)
  (let* ([dbg (require-debugger!)]
         [env (debugger-env dbg)])
        
        (display "\n")
        (display "  ENVIRONMENT BINDINGS\n")
        (display "  ────────────────────────────────────────────\n")
        
        (if (null? env)
            (display "  (empty environment)\n")
            (for-each
             (lambda (binding)
                     (display (format "  ~a = ~a\n"
                                      (car binding)
                                      (truncate-expr-str (cdr binding) 50))))
             env))
        
        (display "\n")
        (void)))

;;; ============================================================
;;; Fuel Command
;;; ============================================================

;;; cmd-fuel : → void
;;; Show detailed fuel consumption.
(define (cmd-fuel)
  (let* ([dbg (require-debugger!)]
         [summary (fuel-summary dbg)])
        
        (display "\n")
        (display "  ============================================\n")
        (display "              FUEL CONSUMPTION\n")
        (display "  ============================================\n\n")
        
        (display (format "  Budget:    ~a\n" (cdr (assq 'budget summary))))
        (display (format "  Used:      ~a\n" (cdr (assq 'used summary))))
        (display (format "  Remaining: ~a\n" (cdr (assq 'remaining summary))))
        (display (format "  Steps:     ~a\n\n" (cdr (assq 'steps summary))))
        
        (display (format "  ~a ~a%\n\n"
                         (fuel-bar (cdr (assq 'used summary))
                                   (cdr (assq 'budget summary))
                                   40)
                         (round (cdr (assq 'percentage summary)))))
        
        ;; By expression type
        (let ([by-type (cdr (assq 'by-type summary))])
             (unless (null? by-type)
                     (display "  By Expression Type:\n")
                     (for-each
                      (lambda (entry)
                              (display (format "    ~a: ~a fuel\n"
                                               (car entry)
                                               (cdr entry))))
                      by-type)))
        
        (display "\n")
        (void)))

;;; ============================================================
;;; Trace Command
;;; ============================================================

;;; cmd-trace : → void
;;; Show execution trace (call stack).
(define (cmd-trace)
  (let* ([dbg (require-debugger!)]
         [trace (debugger-trace dbg)]
         [recent (take-up-to-repl 15 trace)])
        
        (display "\n")
        (display "  EXECUTION TRACE (most recent first)\n")
        (display "  ────────────────────────────────────────────\n")
        
        (if (null? trace)
            (display "  (no steps taken yet)\n")
            (let ([n (length trace)])
                 (for-each
                  (lambda (entry)
                          (display (format "  #~a ~a\n"
                                           n
                                           (truncate-expr-str entry 55)))
                          (set! n (- n 1)))
                  recent)
                 (when (> (length trace) 15)
                       (display (format "  ... (~a more entries)\n"
                                        (- (length trace) 15))))))
        
        (display "\n")
        (void)))

;;; ============================================================
;;; Undo Commands
;;; ============================================================

;;; cmd-undo : → void
;;; Undo the last step.
(define (cmd-undo)
  (let* ([dbg (require-debugger!)]
         [dbg* (undo dbg)])
        (set-session-debugger! dbg*)
        (display (format "  Undid last step. Now at: ~a\n"
                         (truncate-expr-str (debugger-expr dbg*) 50)))
        (display (format "  Can redo: ~a\n" (if (can-redo? dbg*) "yes" "no")))
        (void)))

;;; cmd-redo : [Nat] → void
;;; Redo steps (go forward in timeline).
(define cmd-redo
  (case-lambda
   [()
    (let* ([dbg (require-debugger!)])
          (if (can-redo? dbg)
              (let ([dbg* (redo dbg)])
                   (set-session-debugger! dbg*)
                   (display (format "  Redid step. Now at: ~a\n"
                                    (truncate-expr-str (debugger-expr dbg*) 50)))
                   (display (format "  Can redo: ~a\n" (if (can-redo? dbg*) "yes" "no"))))
              (display "  Nothing to redo.\n"))
          (void))]
   [(n)
    (let* ([dbg (require-debugger!)]
           [dbg* (redo-n dbg n)])
          (set-session-debugger! dbg*)
          (display (format "  Redid ~a step(s). Now at: ~a\n"
                           n
                           (truncate-expr-str (debugger-expr dbg*) 50)))
          (void))]))

;;; cmd-reset : → void
;;; Reset to initial state.
(define (cmd-reset)
  (let* ([dbg (require-debugger!)]
         [dbg* (reset dbg)])
        (set-session-debugger! dbg*)
        (display "  Reset to initial state.\n")
        (void)))

;;; ============================================================
;;; Watch Commands
;;; ============================================================

;;; cmd-watch : Symbol → void
;;; Add a variable to the watch list.
(define (cmd-watch var)
  (let* ([dbg (require-debugger!)]
         [dbg* (add-watch dbg var)])
        (set-session-debugger! dbg*)
        (display (format "  Watching: ~a\n" var))
        ;; Show current value if bound
        (let ([val (env-lookup-safe (debugger-env dbg*) var)])
             (if (eq? val 'unbound)
                 (display "  (currently unbound)\n")
                 (display (format "  Current value: ~a\n" (truncate-expr-str val 50)))))
        (void)))

;;; cmd-unwatch : Symbol → void
;;; Remove a variable from the watch list.
(define (cmd-unwatch var)
  (let* ([dbg (require-debugger!)]
         [dbg* (remove-watch dbg var)])
        (set-session-debugger! dbg*)
        (display (format "  Stopped watching: ~a\n" var))
        (void)))

;;; cmd-watches : → void
;;; List all watched variables.
(define (cmd-watches)
  (let* ([dbg (require-debugger!)]
         [watches (debugger-watches dbg)]
         [env (debugger-env dbg)])
        (display "\n")
        (display "  WATCHED VARIABLES\n")
        (display "  ────────────────────────────────────────────\n")
        (if (null? watches)
            (display "  (no watches set)\n")
            (for-each
             (lambda (var)
                     (let ([val (env-lookup-safe env var)])
                          (display (format "  ~a = ~a\n"
                                           var
                                           (if (eq? val 'unbound)
                                               "<unbound>"
                                               (truncate-expr-str val 40))))))
             watches))
        ;; Show recent watch events
        (let ([events (get-recent-watch-events dbg 5)])
             (unless (null? events)
                     (display "\n  Recent watch events:\n")
                     (for-each
                      (lambda (ev)
                              (let ([var (cdr (assq 'variable (cdr ev)))]
                                    [old (cdr (assq 'old-value (cdr ev)))]
                                    [new (cdr (assq 'new-value (cdr ev)))]
                                    [step (cdr (assq 'step-number (cdr ev)))])
                                   (display (format "    #~a: ~a: ~a -> ~a\n"
                                                    step var
                                                    (truncate-expr-str old 20)
                                                    (truncate-expr-str new 20)))))
                      events)))
        (display "\n")
        (void)))

;;; ============================================================
;;; Explain Commands
;;; ============================================================

;;; cmd-explain : [Symbol] → void
;;; Explain the current result or a specific binding.
(define cmd-explain
  (case-lambda
   [()
    (let* ([dbg (require-debugger!)]
           [explanation (explain-result dbg)])
          (display "\n")
          (display "  WHY DID WE GET THIS RESULT?\n")
          (display "  ════════════════════════════════════════════\n\n")
          (display-explanation explanation 0)
          (display "\n")
          (void))]
   [(var)
    (let* ([dbg (require-debugger!)]
           [explanation (explain-binding dbg var)])
          (display "\n")
          (if explanation
              (begin
               (display (format "  WHY DOES ~a HAVE THIS VALUE?\n" var))
               (display "  ════════════════════════════════════════════\n\n")
               (display-explanation explanation 0))
              (display (format "  Variable ~a is not bound.\n" var)))
          (display "\n")
          (void))]))

;;; display-explanation : Explanation × Nat → void
;;; Pretty-print an explanation tree.
(define (display-explanation exp depth)
  (let* ([expr (cdr (assq 'expression (cdr exp)))]
         [result (cdr (assq 'result (cdr exp)))]
         [reason (cdr (assq 'reason (cdr exp)))]
         [children (cdr (assq 'children (cdr exp)))]
         [indent (make-string (* depth 2) #\space)])
        (display (format "  ~a~a: ~a\n" indent (reason-description reason)
                         (truncate-expr-str expr 40)))
        (unless (equal? result 'incomplete)
                (display (format "  ~a  → ~a\n" indent (truncate-expr-str result 40))))
        (for-each (lambda (child) (display-explanation child (+ depth 1)))
                  children)))

;;; reason-description : Symbol → String
(define (reason-description reason)
  (case reason
        [(literal) "Literal value"]
        [(lookup) "Variable lookup"]
        [(application) "Function application"]
        [(conditional) "Conditional branch"]
        [(primitive) "Primitive operation"]
        [(let-binding) "Let binding"]
        [(initial-binding) "Initial binding"]
        [(evaluation-sequence) "Evaluation sequence"]
        [else (symbol->string reason)]))

;;; cmd-why : → void
;;; Shorthand for (explain).
(define (cmd-why)
  (cmd-explain))

;;; ============================================================
;;; Export Commands
;;; ============================================================

;;; cmd-export-trace : [String] → void
;;; Export structured trace data.
(define cmd-export-trace
  (case-lambda
   [()
    (let* ([dbg (require-debugger!)]
           [exported (export-trace dbg)])
          (display "\n")
          (display "  STRUCTURED TRACE EXPORT\n")
          (display "  ════════════════════════════════════════════\n\n")
          (pretty-print-sexp exported 2)
          (display "\n")
          (void))]
   [(filename)
    (let* ([dbg (require-debugger!)]
           [exported (export-trace dbg)])
          (call-with-output-file filename
                                 (lambda (port)
                                         (write exported port)
                                         (newline port)))
          (display (format "  Trace exported to: ~a\n" filename))
          (void))]))

;;; pretty-print-sexp : Sexp × Nat → void
;;; Simple pretty printer for S-expressions.
(define (pretty-print-sexp sexp indent)
  (let ([ind (make-string indent #\space)])
       (cond
        [(and (pair? sexp)
              (symbol? (car sexp))
              (pair? (cdr sexp))
              (pair? (cadr sexp))
              (eq? (caadr sexp) (car sexp)))
         ;; It's a tagged record, print nicely
         (display (format "~a(~a\n" ind (car sexp)))
         (for-each (lambda (field)
                           (display (format "~a  (~a . ~a)\n"
                                            ind
                                            (car field)
                                            (truncate-expr-str (cdr field) 50))))
                   (cdr sexp))
         (display (format "~a)\n" ind))]
        [(pair? sexp)
         (display (format "~a~a\n" ind (truncate-expr-str sexp 70)))]
        [else
         (display (format "~a~a\n" ind sexp))])))

;;; ============================================================
;;; Session Management
;;; ============================================================

;;; cmd-quit-debug : → void
;;; Quit the current debug session.
(define (cmd-quit-debug)
  (end-debug-session!)
  (display "  Debug session ended.\n")
  (void))

;;; ============================================================
;;; Aliases for REPL convenience
;;; ============================================================

(define debug cmd-debug)
(define dbg-step cmd-step)
(define dbg-next cmd-next)
(define dbg-continue cmd-continue)
(define dbg-break cmd-break)
(define dbg-inspect cmd-inspect)
(define dbg-fuel cmd-fuel)
(define dbg-trace cmd-trace)
(define dbg-undo cmd-undo)
(define dbg-redo cmd-redo)
(define dbg-reset cmd-reset)
(define dbg-quit cmd-quit-debug)
;; Watch commands
(define watch cmd-watch)
(define unwatch cmd-unwatch)
(define watches cmd-watches)
;; Explain commands
(define explain cmd-explain)
(define why cmd-why)
;; Export commands (export-trace shadows core function - use dbg-export-trace)
(define dbg-export-trace cmd-export-trace)
