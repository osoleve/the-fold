(source-directories (cons "core" (source-directories)))

(load "core/lang/compile.ss")
(load "core/base/error.ss")

(doc 'module 'eval-repl)
(doc 'description "Typed Evaluation REPL Commands - provides REPL commands for the compilation pipeline")
(doc 'layer 'boundary)
(doc 'purity 'partial)
(doc 'note "All errors display with source location context")

(doc 'section 'error-display-with-source-context)

(doc *current-source* 'description "Current source being processed (for error display)")
(define *current-source* #f)

(doc display-fold-error 'type "Result × String → void")
(doc display-fold-error 'description "Display an error with source context if available")
(define (display-fold-error result source)
  (cond
   [(result-error? result)
    (let* ([phase (result-phase result)]
           [error-type (result-error-type result)]
           [details (result-error-details result)]
           [span (find-span-in-details details)]
           [other-details (remove-span-from-details details)])
          (when (and span (span? span) (> (span-line span) 0))
                (display (format-span span))
                (display ": "))
          (display "[")
          (display phase)
          (display "] ")
          (display (lookup-error-message phase error-type))
          (when (pair? other-details)
                (display ": ")
                (if (pair? (cdr other-details))
                    (write other-details)
                    (write (car other-details))))
          (newline)
          (when (and span (span? span) (> (span-line span) 0) source)
                (display-source-context source (span-line span) (span-column span))))]
   [else
    (display "Error: ")
    (write result)
    (newline)]))

(doc find-span-in-details 'type "List → Span | #f")
(define (find-span-in-details details)
  (cond
   [(null? details) #f]
   [(span? (car details)) (car details)]
   [(pair? (cdr details)) (find-span-in-details (cdr details))]
   [else #f]))

(doc remove-span-from-details 'type "List → List")
(define (remove-span-from-details details)
  (filter (lambda (x) (not (span? x))) details))

(doc 'section 'parse-command)

(doc fold-parse-string 'type "String → void")
(doc fold-parse-string 'description "Parse a string and display the AST or error")
(define (fold-parse-string input)
  (set! *current-source* input)
  (let ([result (adapt-parse input)])
       (if (result-ok? result)
           (begin
            (display "Parsed AST:\n")
            (pretty-print (result-value result)))
           (display-fold-error result input))))

(doc fold-parse 'description "Wrapper that handles both strings and quoted expressions")
(define-syntax fold-parse
  (syntax-rules ()
                [(_ str)
                 (if (string? str)
                     (fold-parse-string str)
                     (begin
                      (display "AST:\n")
                      (pretty-print 'str)))]))

(doc 'section 'type-check-command)

(doc fold-type-string 'type "String → void")
(doc fold-type-string 'description "Type-check a string and display the type or error")
(define (fold-type-string input)
  (set! *current-source* input)
  (let ([result (compile input 'to 'infer)])
       (if (result-ok? result)
           (let ([typed-result (result-value result)]
                 [context (result-context result)])
                (display "Type: ")
                (if (and (pair? context)
                         (pair? (car context))
                         (eq? (caar context) 'substitution))
                    (let ([subst (cadar context)])
                         (write typed-result)
                         (newline))
                    (begin
                     (write typed-result)
                     (newline))))
           (display-fold-error result input))))

(doc fold-type 'description "Type-check an expression")
(define-syntax fold-type
  (syntax-rules (quote)
                [(_ (quote expr))
                 (begin
                   (display "  Note: fold-type already quotes its argument. Stripping extra quote.\n")
                   (fold-type-string (format "~s" 'expr)))]
                [(_ expr)
                 (fold-type-string (format "~s" 'expr))]))

(doc 'section 'eval-command)

(doc fold-eval-string 'type "String [× Nat] → void")
(doc fold-eval-string 'description "Evaluate a string and display the result or error")
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

(doc fold-eval 'description "Evaluate an expression")
(define-syntax fold-eval
  (syntax-rules (quote)
                [(_ (quote expr))
                 (begin
                   (display "  Note: fold-eval already quotes its argument. Stripping extra quote.\n")
                   (fold-eval-string (format "~s" 'expr)))]
                [(_ (quote expr) fuel)
                 (begin
                   (display "  Note: fold-eval already quotes its argument. Stripping extra quote.\n")
                   (fold-eval-string (format "~s" 'expr) fuel))]
                [(_ expr)
                 (fold-eval-string (format "~s" 'expr))]
                [(_ expr fuel)
                 (fold-eval-string (format "~s" 'expr) fuel)]))

(doc 'section 'full-pipeline-command)

(doc fold-compile-string 'type "String → void")
(doc fold-compile-string 'description "Run full pipeline with diagnostics at each phase")
(define (fold-compile-string input)
  (set! *current-source* input)
  (display "--- Parse ---\n")
  (let ([parse-result (adapt-parse input)])
       (if (result-ok? parse-result)
           (let ([ast (result-value parse-result)])
                (display "  ✓ Parsed\n")
                (display "--- Type Check ---\n")
                (let ([type-result (compile input 'infer)])
                     (if (result-ok? type-result)
                         (begin
                          (display "  ✓ Type: ")
                          (write (result-value type-result))
                          (newline)
                          (display "--- Evaluate ---\n")
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

(doc fold-compile 'description "Full pipeline with diagnostics")
(define-syntax fold-compile
  (syntax-rules (quote)
                [(_ (quote expr))
                 (begin
                   (display "  Note: fold-compile already quotes its argument. Stripping extra quote.\n")
                   (fold-compile-string (format "~s" 'expr)))]
                [(_ expr)
                 (fold-compile-string (format "~s" 'expr))]))

(doc 'section 'help)

(doc fold-eval-help 'type "-> void")
(doc fold-eval-help 'description "Display help for fold evaluation commands")
(define (fold-eval-help)
  (display "\n")
  (display "  ---------------- FOLD EVALUATION COMMANDS -----------------\n")
  (display "\n")
  (display "  (fold-parse str)      Parse a string, show AST\n")
  (display "  (fold-type expr)      Type-check an expression\n")
  (display "  (fold-eval expr)      Evaluate an expression\n")
  (display "  (fold-eval expr n)    Evaluate with n fuel (default: 10000)\n")
  (display "  (fold-compile expr)   Full pipeline with diagnostics\n")
  (display "\n")
  (display "  Examples:\n")
  (display "    (fold-eval (prim 'add 1 2))        ; => 3\n")
  (display "    (fold-eval (prim 'eq? 1 1))        ; => #t (comparison)\n")
  (display "    (fold-eval (prim 'lt? 1 2))        ; => #t\n")
  (display "    (fold-type (fn (x) x))             ; Type: (a → a)\n")
  (display "    (fold-eval ((fn (x) x) 42))        ; => 42 (juxtaposition)\n")
  (display "    (fold-eval (call (fn (x) x) 42))   ; => 42 (explicit call)\n")
  (display "    (fold-eval (fix 'fact              ; Recursion via fix\n")
  (display "      (fn (self n)\n")
  (display "        (if (prim 'eq? n 0) 1\n")
  (display "          (prim 'mul n (call self (prim 'sub n 1)))))))\n")
  (display "    (fold-parse \"(let ((x 1)) x)\")    ; Show AST\n")
  (display "    (fold-compile (if #t 1 0))         ; Full diagnostics\n")
  (display "\n")
  (display "  Note: Fold uses (prim 'op ...) for operations, not bare +/*.\n")
  (display "  Use (list-primitives) to see all available primitives.\n")
  (display "\n"))
