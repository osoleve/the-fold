;;; shell/eval-repl.ss — Typed Evaluation REPL Commands
;;;
;;; Provides REPL commands for the compilation pipeline:
;;;   (fold-eval expr)     - Evaluate a Fold expression
;;;   (fold-type expr)     - Type-check and show type
;;;   (fold-parse str)     - Parse a string and show AST
;;;   (fold-compile str)   - Full pipeline with diagnostics
;;;
;;; All errors display with source location context.
;;;
;;; Dependencies:
;;;   - core/compile.ss (the compilation pipeline)
;;;   - core/error.ss (error formatting)

;;; ============================================================
;;; Load Dependencies
;;; ============================================================

;; Ensure core is in path
(source-directories (cons "core" (source-directories)))

(load "fabric/stitches/compile.ss")
(load "fabric/stitches/error.ss")

;;; ============================================================
;;; Error Display with Source Context
;;; ============================================================

;;; Current source being processed (for error display)
(define *current-source* #f)

;;; display-fold-error : Result × String → void
;;; Display an error with source context if available.
(define (display-fold-error result source)
  (cond
    [(result-error? result)
     (let* ([phase (result-phase result)]
            [error-type (result-error-type result)]
            [details (result-error-details result)]
            ;; Find span in details (could be first or second element)
            [span (find-span-in-details details)]
            [other-details (remove-span-from-details details)])
       ;; Display location
       (when (and span (span? span) (> (span-line span) 0))
         (display (format-span span))
         (display ": "))
       ;; Display phase and error type
       (display "[")
       (display phase)
       (display "] ")
       (display (lookup-error-message phase error-type))
       ;; Display additional details
       (when (pair? other-details)
         (display ": ")
         (if (pair? (cdr other-details))
             (write other-details)
             (write (car other-details))))
       (newline)
       ;; Display source context
       (when (and span (span? span) (> (span-line span) 0) source)
         (display-source-context source (span-line span) (span-column span))))]
    [else
     (display "Error: ")
     (write result)
     (newline)]))

;;; find-span-in-details : List → Span | #f
(define (find-span-in-details details)
  (cond
    [(null? details) #f]
    [(span? (car details)) (car details)]
    [(pair? (cdr details)) (find-span-in-details (cdr details))]
    [else #f]))

;;; remove-span-from-details : List → List
(define (remove-span-from-details details)
  (filter (lambda (x) (not (span? x))) details))

;;; ============================================================
;;; Parse Command
;;; ============================================================

;;; fold-parse-string : String → void
;;; Parse a string and display the AST or error.
(define (fold-parse-string input)
  (set! *current-source* input)
  (let ([result (adapt-parse input)])
    (if (result-ok? result)
        (begin
          (display "Parsed AST:\n")
          (pretty-print (result-value result)))
        (display-fold-error result input))))

;;; fold-parse : Any → void
;;; Wrapper that handles both strings and quoted expressions.
(define-syntax fold-parse
  (syntax-rules ()
    [(_ str)
     (if (string? str)
         (fold-parse-string str)
         (begin
           (display "AST:\n")
           (pretty-print 'str)))]))

;;; ============================================================
;;; Type Check Command
;;; ============================================================

;;; fold-type-string : String → void
;;; Type-check a string and display the type or error.
(define (fold-type-string input)
  (set! *current-source* input)
  (let ([result (compile input 'infer)])
    (if (result-ok? result)
        (let ([typed-result (result-value result)]
              [context (result-context result)])
          (display "Type: ")
          (if (and (pair? context)
                   (pair? (car context))
                   (eq? (caar context) 'substitution))
              ;; Extract and apply substitution to get final type
              (let ([subst (cadar context)])
                (write typed-result)
                (newline))
              (begin
                (write typed-result)
                (newline))))
        (display-fold-error result input))))

;;; fold-type : Any → void
;;; Type-check an expression.
(define-syntax fold-type
  (syntax-rules ()
    [(_ expr)
     (fold-type-string (format "~s" 'expr))]))

;;; ============================================================
;;; Eval Command
;;; ============================================================

;;; fold-eval-string : String [× Nat] → void
;;; Evaluate a string and display the result or error.
(define (fold-eval-string input . fuel-arg)
  (set! *current-source* input)
  (let* ([fuel (if (null? fuel-arg) 10000 (car fuel-arg))]
         [result (compile input 'eval fuel)])
    (cond
      [(result-ok? result)
       (display "=> ")
       (write (result-value result))
       (newline)]
      [(result-suspended? result)
       (display "[suspended - out of fuel]\n")
       (display "Expression: ")
       (write (cadr result))
       (newline)]
      [else
       (display-fold-error result input)])))

;;; fold-eval : Any → void
;;; Evaluate an expression.
(define-syntax fold-eval
  (syntax-rules ()
    [(_ expr)
     (fold-eval-string (format "~s" 'expr))]
    [(_ expr fuel)
     (fold-eval-string (format "~s" 'expr) fuel)]))

;;; ============================================================
;;; Full Pipeline Command
;;; ============================================================

;;; fold-compile-string : String → void
;;; Run full pipeline with diagnostics at each phase.
(define (fold-compile-string input)
  (set! *current-source* input)
  (display "─── Parse ───\n")
  (let ([parse-result (adapt-parse input)])
    (if (result-ok? parse-result)
        (let ([ast (result-value parse-result)])
          (display "  ✓ Parsed\n")
          (display "─── Type Check ───\n")
          (let ([type-result (compile input 'infer)])
            (if (result-ok? type-result)
                (begin
                  (display "  ✓ Type: ")
                  (write (result-value type-result))
                  (newline)
                  (display "─── Evaluate ───\n")
                  (let ([eval-result (compile input 'eval 10000)])
                    (cond
                      [(result-ok? eval-result)
                       (display "  ✓ Result: ")
                       (write (result-value eval-result))
                       (newline)]
                      [(result-suspended? eval-result)
                       (display "  ⏸ Suspended (out of fuel)\n")]
                      [else
                       (display "  ✗ Eval error:\n")
                       (display-fold-error eval-result input)])))
                (begin
                  (display "  ✗ Type error:\n")
                  (display-fold-error type-result input)))))
        (begin
          (display "  ✗ Parse error:\n")
          (display-fold-error parse-result input)))))

;;; fold-compile : Any → void
(define-syntax fold-compile
  (syntax-rules ()
    [(_ expr)
     (fold-compile-string (format "~s" 'expr))]))

;;; ============================================================
;;; Help
;;; ============================================================

(define (fold-eval-help)
  (display "\n")
  (display "  ┌────────────────────────────────────────────────────────────────────┐\n")
  (display "  │                    FOLD EVALUATION COMMANDS                        │\n")
  (display "  └────────────────────────────────────────────────────────────────────┘\n")
  (display "\n")
  (display "  (fold-parse str)      Parse a string, show AST\n")
  (display "  (fold-type expr)      Type-check an expression\n")
  (display "  (fold-eval expr)      Evaluate an expression\n")
  (display "  (fold-eval expr n)    Evaluate with n fuel (default: 10000)\n")
  (display "  (fold-compile expr)   Full pipeline with diagnostics\n")
  (display "\n")
  (display "  Examples:\n")
  (display "    (fold-eval (+ 1 2))                ; => 3\n")
  (display "    (fold-type (fn (x) x))             ; Type: (a → a)\n")
  (display "    (fold-parse \"(let ((x 1)) x)\")    ; Show AST\n")
  (display "    (fold-compile (if #t 1 0))         ; Full diagnostics\n")
  (display "\n"))
