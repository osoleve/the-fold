(load "boundary/debug/session-debugger.ss")

(doc 'module 'debug-repl)
(doc 'description "REPL Debugger Commands - Interactive debugging interface")
(doc 'layer 'boundary)
(doc 'purity 'partial)
(doc 'dependencies '(boundary/debug/session-debugger))

(doc 'section 'overview)
(doc 'note "Interactive debugging commands:
  (debug expr)     - Start debugging
  (step)           - Single step
  (next)           - Step over
  (continue)       - Run to breakpoint/completion
  (break 'fn)      - Set breakpoint
  (inspect)        - Show environment
  (fuel)           - Show fuel status
  (trace)          - Show call stack")

(doc 'section 'configuration)

(define *default-debug-fuel* 10000)
(doc *default-debug-fuel* 'type 'Nat)
(doc *default-debug-fuel* 'description "Default fuel budget for debug sessions")

(doc 'section 'display-helpers)

(define (truncate-expr-str expr max-len)
  (doc 'type '(-> Expr Nat String))
  (doc 'description "Truncate expression string to max-len with ellipsis")
  (let* ([str (format "~a" expr)]
         [len (string-length str)])
        (if (> len max-len)
            (string-append (substring str 0 (- max-len 3)) "...")
            str)))

(define (status-symbol status)
  (doc 'type '(-> Symbol String))
  (doc 'description "Convert debugger status to display symbol")
  (case status
        [(ready) ">>"]
        [(complete) "OK"]
        [(error) "!!"]
        [(out-of-fuel) "XX"]
        [(breakpoint) "**"]
        [else "??"]))

(define (fuel-bar used total width)
  (doc 'type '(-> Nat Nat Nat String))
  (doc 'description "Create ASCII fuel bar with clamping for safety")
  (let* ([pct (if (zero? total) 0 (min 1.0 (/ used total)))]
         [filled (exact (min width (max 0 (round (* width pct)))))]
         [empty (exact (max 0 (- width filled)))])
        (string-append
         "["
         (make-string filled #\#)
         (make-string empty #\-)
         "]")))

(doc 'section 'debug-command)

(doc cmd-debug 'export #t)
(doc cmd-debug 'type '(case-lambda (-> Expr void) (-> Expr Fuel void)))
(doc cmd-debug 'description "Start a new debug session")
(define cmd-debug
  (case-lambda
   [(expr) (cmd-debug expr *default-debug-fuel*)]
   [(expr fuel)
    (let ([dbg (start-debug-session! expr empty-env fuel)])
         (display "\n")
         (display "  ====\n")
         (display "           DEBUG SESSION STARTED\n")
         (display "  ====\n\n")
         (display (format "  Expression: ~a\n" (truncate-expr-str expr 50)))
         (display (format "  Fuel budget: ~a\n" fuel))
         (display "  Breakpoints: (none)\n\n")
         (display "  [Ready] Use (step) to begin, (help 'debug) for commands.\n\n")
         (void))]))

(doc 'section 'step-command)

(doc cmd-step 'export #t)
(define (cmd-step)
  (doc 'type '(-> void))
  (doc 'description "Execute one step in debugger")
  (let* ([dbg (require-debugger!)]
         [dbg* (step-with-fuel dbg)]
         [status (debugger-status dbg*)]
         [expr (debugger-expr dbg*)]
         [steps (length (debugger-trace dbg*))]
         [fuel-trace (debugger-fuel-trace dbg*)])

        (set-session-debugger! dbg*)

        (display "\n")
        (display (format "  Step #~a ~a\n" steps (status-symbol status)))
        (display "  --------------------------------------------\n")
        (display (format "  Expression: ~a\n" (truncate-expr-str expr 50)))

        (when (pair? fuel-trace)
              (let ([last-entry (car fuel-trace)])
                   (display (format "  Fuel: ~a consumed, ~a remaining ~a ~a%\n"
                                    (cadddr last-entry)
                                    (debugger-fuel dbg*)
                                    (fuel-bar (debugger-fuel-used dbg*)
                                              (debugger-fuel-budget dbg*)
                                              20)
                                    (round (debugger-fuel-pct dbg*))))))

        (let ([env (debugger-env dbg*)])
             (unless (null? env)
                     (display "\n  Environment:\n")
                     (for-each
                      (lambda (binding)
                              (display (format "    ~a = ~a\n"
                                               (car binding)
                                               (truncate-expr-str (cdr binding) 40))))
                      (take 5 env))))

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

(doc 'section 'next-command)

(doc cmd-next 'export #t)
(define (cmd-next)
  (doc 'type '(-> void))
  (doc 'description "Step over the current expression")
  (let* ([dbg (require-debugger!)]
         [dbg* (next dbg)])

        (set-session-debugger! dbg*)

        (let ([status (debugger-status dbg*)]
              [expr (debugger-expr dbg*)]
              [steps (length (debugger-trace dbg*))])

             (display "\n")
             (display (format "  Step #~a ~a (stepped over)\n" steps (status-symbol status)))
             (display "  --------------------------------------------\n")
             (display (format "  Expression: ~a\n" (truncate-expr-str expr 50)))
             (display (format "  Fuel: ~a used, ~a remaining ~a\n"
                              (debugger-fuel-used dbg*)
                              (debugger-fuel dbg*)
                              (fuel-bar (debugger-fuel-used dbg*)
                                        (debugger-fuel-budget dbg*)
                                        20)))
             (display "\n")
             (void))))

(doc 'section 'continue-command)

(doc cmd-continue 'export #t)
(define (cmd-continue)
  (doc 'type '(-> void))
  (doc 'description "Run to completion or breakpoint")
  (let* ([dbg (require-debugger!)]
         [dbg* (continue-with-fuel dbg)]
         [status (debugger-status dbg*)]
         [expr (debugger-expr dbg*)]
         [steps (length (debugger-trace dbg*))])

        (set-session-debugger! dbg*)

        (display "\n")
        (case status
              [(complete)
               (display "  ====\n")
               (display "             EVALUATION COMPLETE\n")
               (display "  ====\n\n")
               (display (format "  Result: ~a\n" (truncate-expr-str expr 50)))
               (display (format "  Steps: ~a\n" steps))
               (display (format "  Fuel: ~a used of ~a (~a%)\n"
                                (debugger-fuel-used dbg*)
                                (debugger-fuel-budget dbg*)
                                (round (debugger-fuel-pct dbg*))))]
              [(breakpoint)
               (display "  ====\n")
               (display "               BREAKPOINT HIT\n")
               (display "  ====\n\n")
               (display (format "  Expression: ~a\n" (truncate-expr-str expr 50)))
               (display (format "  Fuel: ~a used ~a\n"
                                (debugger-fuel-used dbg*)
                                (fuel-bar (debugger-fuel-used dbg*)
                                          (debugger-fuel-budget dbg*)
                                          20)))]
              [(error)
               (display "  ====\n")
               (display "                   ERROR\n")
               (display "  ====\n\n")
               (display (format "  ~a\n" (debugger-get dbg* 'error)))]
              [(out-of-fuel)
               (display "  ====\n")
               (display "               OUT OF FUEL\n")
               (display "  ====\n\n")
               (display (format "  Expression: ~a\n" (truncate-expr-str expr 50)))
               (display "  Increase fuel budget with (debug expr fuel)\n")])

        (display "\n")
        (void)))

(doc 'section 'breakpoint-commands)

(doc cmd-break 'export #t)
(define (cmd-break fn-name)
  (doc 'type '(-> Symbol void))
  (doc 'description "Set a breakpoint on a function name")
  (let* ([dbg (require-debugger!)]
         [dbg* (add-breakpoint dbg (break-on-call fn-name))])
        (set-session-debugger! dbg*)
        (display (format "  Breakpoint added: break on '~a'\n" fn-name))
        (void)))

(doc cmd-clear-breaks 'export #t)
(define (cmd-clear-breaks)
  (doc 'type '(-> void))
  (doc 'description "Clear all breakpoints")
  (let* ([dbg (require-debugger!)]
         [dbg* (clear-breakpoints dbg)])
        (set-session-debugger! dbg*)
        (display "  All breakpoints cleared.\n")
        (void)))

(doc 'section 'inspect-command)

(doc cmd-inspect 'export #t)
(define (cmd-inspect)
  (doc 'type '(-> void))
  (doc 'description "Show current environment bindings")
  (let* ([dbg (require-debugger!)]
         [env (debugger-env dbg)])

        (display "\n")
        (display "  ENVIRONMENT BINDINGS\n")
        (display "  --------------------------------------------\n")

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

(doc 'section 'fuel-command)

(doc cmd-fuel 'export #t)
(define (cmd-fuel)
  (doc 'type '(-> void))
  (doc 'description "Show detailed fuel consumption")
  (let* ([dbg (require-debugger!)]
         [summary (fuel-summary dbg)])

        (display "\n")
        (display "  ====\n")
        (display "              FUEL CONSUMPTION\n")
        (display "  ====\n\n")

        (display (format "  Budget:    ~a\n" (cdr (assq 'budget summary))))
        (display (format "  Used:      ~a\n" (cdr (assq 'used summary))))
        (display (format "  Remaining: ~a\n" (cdr (assq 'remaining summary))))
        (display (format "  Steps:     ~a\n\n" (cdr (assq 'steps summary))))

        (display (format "  ~a ~a%\n\n"
                         (fuel-bar (cdr (assq 'used summary))
                                   (cdr (assq 'budget summary))
                                   40)
                         (round (cdr (assq 'percentage summary)))))

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

(doc 'section 'trace-command)

(doc cmd-trace 'export #t)
(define (cmd-trace)
  (doc 'type '(-> void))
  (doc 'description "Show execution trace (call stack)")
  (let* ([dbg (require-debugger!)]
         [trace (debugger-trace dbg)]
         [recent (take 15 trace)])

        (display "\n")
        (display "  EXECUTION TRACE (most recent first)\n")
        (display "  --------------------------------------------\n")

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

(doc 'section 'undo-commands)

(doc cmd-undo 'export #t)
(define (cmd-undo)
  (doc 'type '(-> void))
  (doc 'description "Undo the last step")
  (let* ([dbg (require-debugger!)]
         [dbg* (undo dbg)])
        (set-session-debugger! dbg*)
        (display (format "  Undid last step. Now at: ~a\n"
                         (truncate-expr-str (debugger-expr dbg*) 50)))
        (display (format "  Can redo: ~a\n" (if (can-redo? dbg*) "yes" "no")))
        (void)))

(doc cmd-redo 'export #t)
(doc cmd-redo 'type '(case-lambda (-> void) (-> Nat void)))
(doc cmd-redo 'description "Redo steps (go forward in timeline)")
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

(doc cmd-reset 'export #t)
(define (cmd-reset)
  (doc 'type '(-> void))
  (doc 'description "Reset to initial state")
  (let* ([dbg (require-debugger!)]
         [dbg* (reset dbg)])
        (set-session-debugger! dbg*)
        (display "  Reset to initial state.\n")
        (void)))

(doc 'section 'watch-commands)

(doc cmd-watch 'export #t)
(define (cmd-watch var)
  (doc 'type '(-> Symbol void))
  (doc 'description "Add a variable to the watch list")
  (let* ([dbg (require-debugger!)]
         [dbg* (add-watch dbg var)])
        (set-session-debugger! dbg*)
        (display (format "  Watching: ~a\n" var))
        (let ([val (env-lookup-safe (debugger-env dbg*) var)])
             (if (eq? val 'unbound)
                 (display "  (currently unbound)\n")
                 (display (format "  Current value: ~a\n" (truncate-expr-str val 50)))))
        (void)))

(doc cmd-unwatch 'export #t)
(define (cmd-unwatch var)
  (doc 'type '(-> Symbol void))
  (doc 'description "Remove a variable from the watch list")
  (let* ([dbg (require-debugger!)]
         [dbg* (remove-watch dbg var)])
        (set-session-debugger! dbg*)
        (display (format "  Stopped watching: ~a\n" var))
        (void)))

(doc cmd-watches 'export #t)
(define (cmd-watches)
  (doc 'type '(-> void))
  (doc 'description "List all watched variables")
  (let* ([dbg (require-debugger!)]
         [watches (debugger-watches dbg)]
         [env (debugger-env dbg)])
        (display "\n")
        (display "  WATCHED VARIABLES\n")
        (display "  --------------------------------------------\n")
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

(doc 'section 'explain-commands)

(doc cmd-explain 'export #t)
(doc cmd-explain 'type '(case-lambda (-> void) (-> Symbol void)))
(doc cmd-explain 'description "Explain the current result or a specific binding")
(define cmd-explain
  (case-lambda
   [()
    (let* ([dbg (require-debugger!)]
           [explanation (explain-result dbg)])
          (display "\n")
          (display "  WHY DID WE GET THIS RESULT?\n")
          (display "  ============================================\n\n")
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
               (display "  ============================================\n\n")
               (display-explanation explanation 0))
              (display (format "  Variable ~a is not bound.\n" var)))
          (display "\n")
          (void))]))

(define (display-explanation exp depth)
  (doc 'type '(-> Explanation Nat void))
  (doc 'description "Pretty-print an explanation tree")
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

(define (reason-description reason)
  (doc 'type '(-> Symbol String))
  (doc 'description "Convert reason symbol to human-readable description")
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

(doc cmd-why 'export #t)
(define (cmd-why)
  (doc 'type '(-> void))
  (doc 'description "Shorthand for (explain)")
  (cmd-explain))

(doc 'section 'export-commands)

(doc cmd-export-trace 'export #t)
(doc cmd-export-trace 'type '(case-lambda (-> void) (-> String void)))
(doc cmd-export-trace 'description "Export structured trace data")
(define cmd-export-trace
  (case-lambda
   [()
    (let* ([dbg (require-debugger!)]
           [exported (export-trace dbg)])
          (display "\n")
          (display "  STRUCTURED TRACE EXPORT\n")
          (display "  ============================================\n\n")
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

(define (pretty-print-sexp sexp indent)
  (doc 'type '(-> Sexp Nat void))
  (doc 'description "Simple pretty printer for S-expressions")
  (let ([ind (make-string indent #\space)])
       (cond
        [(and (pair? sexp)
              (symbol? (car sexp))
              (pair? (cdr sexp))
              (pair? (cadr sexp))
              (eq? (caadr sexp) (car sexp)))
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

(doc 'section 'session-management)

(doc cmd-quit-debug 'export #t)
(define (cmd-quit-debug)
  (doc 'type '(-> void))
  (doc 'description "Quit the current debug session")
  (end-debug-session!)
  (display "  Debug session ended.\n")
  (void))

(doc 'section 'aliases)

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
(define watch cmd-watch)
(define unwatch cmd-unwatch)
(define watches cmd-watches)
(define explain cmd-explain)
(define why cmd-why)
(define dbg-export-trace cmd-export-trace)
