;;; shell/tools/effect-lint.ss — Effect Typing and Linting
;;;
;;; Effect-aware analysis for The Fold:
;;;   - Track which functions have effects (IO, mutation, etc.)
;;;   - Enforce effect boundaries between pure/impure modules
;;;   - Lint for effect violations in core code
;;;   - Surface effect information in REPL tooling
;;;
;;; Effect Categories:
;;;   - Pure: No observable effects
;;;   - IO: File system, network, console
;;;   - Mutation: State modification
;;;   - Exception: May throw errors
;;;   - NonTerminating: May not terminate
;;;   - Async: Asynchronous operations
;;;
;;; This is Shell code: performs analysis with I/O.
;;;
;;; Dependencies:
;;;   - prelude.ss

(load "core/base/prelude.ss")

;;; ============================================================
;;; Effect Types
;;; ============================================================

;;; Effects are represented as symbols:
;;;   'pure - no effects
;;;   'io - input/output
;;;   'mutation - state changes
;;;   'exception - may throw
;;;   'nonterminating - may not halt
;;;   'async - asynchronous operations
;;;   'unknown - effect not determined

(define all-effects '(pure io mutation exception nonterminating async unknown))

;;; effect? : Any → Boolean
(define (effect? x)
  (memq x all-effects))

;;; effect-join : Effect × Effect → Effect
;;; Combine two effects (lattice join).
;;; Unknown is treated as "could be pure" - we return the worst KNOWN effect.
(define (effect-join e1 e2)
  (cond
   [(eq? e1 e2) e1]
   [(eq? e1 'pure) e2]
   [(eq? e2 'pure) e1]
   ;; If one is unknown, use the known effect (optimistic for local analysis)
   [(eq? e1 'unknown) e2]
   [(eq? e2 'unknown) e1]
   ;; Multiple different known effects - be conservative
   [else 'unknown]))

;;; effects-join : (List Effect) → Effect
(define (effects-join effects)
  (fold-left effect-join 'pure effects))

;;; effect<? : Effect × Effect → Boolean
;;; Effect ordering (pure < everything else).
(define (effect<? e1 e2)
  (cond
   [(eq? e1 e2) #f]
   [(eq? e1 'pure) #t]
   [(eq? e2 'pure) #f]
   [else #f]))

;;; ============================================================
;;; Effect Annotations Database
;;; ============================================================

;;; Known effect annotations for standard functions
(define *effect-database*
  (make-eq-hashtable))

;;; Initialize with known effects
(define (init-effect-database!)
  ;; Pure functions
  (for-each
   (lambda (name) (hashtable-set! *effect-database* name 'pure))
   '(+ - * / < > = <= >= and or not
     car cdr cons null? pair? list? length append reverse
     map filter fold-left fold-right
     string-length string-append substring string-ref
     vector-ref vector-length
     symbol->string string->symbol
     number->string string->number
     equal? eq? eqv?
     abs min max quotient remainder modulo
     floor ceiling round truncate
     sin cos tan asin acos atan sqrt exp log expt))
  
  ;; IO functions
  (for-each
   (lambda (name) (hashtable-set! *effect-database* name 'io))
   '(display newline printf format
     read write
     open-input-file open-output-file
     close-input-port close-output-port
     call-with-input-file call-with-output-file
     get-line get-char put-char put-string
     file-exists? delete-file rename-file
     current-directory directory-list
     system shell-command))
  
  ;; Mutation functions
  (for-each
   (lambda (name) (hashtable-set! *effect-database* name 'mutation))
   '(set! set-car! set-cdr!
     vector-set!
     hashtable-set! hashtable-delete!
     box-set!))
  
  ;; Exception functions
  (for-each
   (lambda (name) (hashtable-set! *effect-database* name 'exception))
   '(error raise assert
     guard catch-error)))

;;; lookup-effect : Symbol → Effect
(define (lookup-effect name)
  (hashtable-ref *effect-database* name 'unknown))

;;; annotate-effect! : Symbol × Effect → Void
(define (annotate-effect! name effect)
  (hashtable-set! *effect-database* name effect))

;;; ============================================================
;;; Effect Inference
;;; ============================================================

;;; infer-effect : Expr → Effect
;;; Infer the effect of an expression.
(define (infer-effect expr)
  (cond
   ;; Literals are pure
   [(or (number? expr) (boolean? expr) (string? expr) (char? expr))
    'pure]
   
   ;; Symbols: variable references are pure (not function calls)
   ;; Only function applications have effects
   [(symbol? expr)
    'pure]
   
   ;; Not an expression
   [(not (pair? expr))
    'pure]
   
   ;; Quote is pure
   [(eq? (car expr) 'quote)
    'pure]
   
   ;; Lambda: analyze body but function itself is pure
   [(memq (car expr) '(fn λ lambda))
    (let ([body-effect (infer-effect (caddr expr))])
         ;; Function definition is pure; calling it may have effects
         'pure)]
   
   ;; Let: effects of bindings and body
   [(eq? (car expr) 'let)
    (let* ([bindings (cadr expr)]
           [body (caddr expr)]
           [binding-effects (map (lambda (b) (infer-effect (cadr b))) bindings)]
           [body-effect (infer-effect body)])
          (effects-join (cons body-effect binding-effects)))]
   
   ;; If: effects of all branches
   [(eq? (car expr) 'if)
    (let* ([test-effect (infer-effect (cadr expr))]
           [then-effect (infer-effect (caddr expr))]
           [else-effect (if (pair? (cdddr expr))
                            (infer-effect (cadddr expr))
                            'pure)])
          (effects-join (list test-effect then-effect else-effect)))]
   
   ;; Begin: effects of all expressions
   [(eq? (car expr) 'begin)
    (effects-join (map infer-effect (cdr expr)))]
   
   ;; Set!: mutation effect
   [(eq? (car expr) 'set!)
    'mutation]
   
   ;; Application: effect of function + arguments
   [(pair? expr)
    (let* ([fn-name (car expr)]
           [fn-effect (if (symbol? fn-name)
                          (lookup-effect fn-name)
                          (infer-effect fn-name))]
           [arg-effects (map infer-effect (cdr expr))])
          (effects-join (cons fn-effect arg-effects)))]
   
   [else 'unknown]))

;;; ============================================================
;;; Effect Checking
;;; ============================================================

;;; check-pure : Expr → (List Violation)
;;; Check that an expression is pure, returning violations if not.
(define (check-pure expr)
  (check-effect-bound expr 'pure))

;;; check-effect-bound : Expr × Effect → (List Violation)
;;; Check that expression's effect is within the bound.
(define (check-effect-bound expr bound)
  (let ([effect (infer-effect expr)])
       (if (or (eq? effect 'pure) (eq? effect bound))
           '()
           (list `(effect-violation
                   (expression ,expr)
                   (expected ,bound)
                   (actual ,effect))))))

;;; ============================================================
;;; Module Effect Analysis
;;; ============================================================

;;; analyze-module-effects : String → ModuleEffects
;;; Analyze all definitions in a module for their effects.
;;; Uses multi-pass analysis to handle local function calls.
(define (analyze-module-effects path)
  (guard (e [else `((error . ,(format "Cannot read ~a" path)))])
         (let* ([content (call-with-input-file path get-string-all)]
                [port (open-input-string content)]
                [definitions (read-all-definitions port)]
                [names (map definition-name definitions)])
               ;; Pass 1: Register all local functions as 'pure' initially
               ;; (optimistic assumption - most local helpers are pure)
               (for-each (lambda (name) (annotate-effect! name 'pure)) names)
               ;; Pass 2: Analyze with local context available
               (let ([results (map analyze-definition-effect definitions)])
                    ;; Pass 3: Update database with actual effects
                    (for-each (lambda (r) (annotate-effect! (car r) (cdr r))) results)
                    ;; Pass 4: Re-analyze to propagate effects through call chains
                    (let ([final-results (map analyze-definition-effect definitions)])
                         ;; Clean up: remove local annotations to not pollute global db
                         (for-each (lambda (name) (hashtable-delete! *effect-database* name)) names)
                         final-results)))))

;;; definition-name : Expr → Symbol
;;; Extract the name from a definition.
(define (definition-name def)
  (if (pair? (cadr def))
      (caadr def)
      (cadr def)))

;;; read-all-definitions : Port → (List Expr)
(define (read-all-definitions port)
  (let loop ([defs '()])
       (let ([expr (read port)])
            (if (eof-object? expr)
                (reverse defs)
                (if (and (pair? expr) (eq? (car expr) 'define))
                    (loop (cons expr defs))
                    (loop defs))))))

;;; analyze-definition-effect : Expr → (Name × Effect)
(define (analyze-definition-effect def)
  (let* ([name (if (pair? (cadr def))
                   (caadr def)
                   (cadr def))]
         [body (caddr def)]
         [effect (infer-effect body)])
        (cons name effect)))

;;; ============================================================
;;; Effect Linting
;;; ============================================================

;;; lint-effects : String × Effect → (List Violation)
;;; Lint a module, checking that all functions respect the effect bound.
(define (lint-effects path expected-effect)
  (let ([analysis (analyze-module-effects path)])
       (if (and (pair? analysis) (assq 'error analysis))
           (list analysis)
           (filter-map
            (lambda (entry)
                    (let ([name (car entry)]
                          [effect (cdr entry)])
                         (if (effect-acceptable? effect expected-effect)
                             #f
                             `(effect-violation
                               (function ,name)
                               (expected ,expected-effect)
                               (actual ,effect)))))
            analysis))))

;;; effect-acceptable? : Effect × Effect → Boolean
;;; Is the actual effect acceptable given the expected bound?
(define (effect-acceptable? actual expected)
  (or (eq? actual 'pure)
      (eq? actual expected)
      (eq? expected 'unknown)))

;;; ============================================================
;;; REPL Commands
;;; ============================================================

;;; effect-of : Expr → Void
;;; Show the inferred effect of an expression.
(define (effect-of expr)
  (let ([effect (infer-effect expr)])
       (display "\n")
       (printf "  Effect: ~a\n" effect)
       (display "  ────────────────────────────────────────────\n")
       (case effect
             [(pure)
              (display "  ✓ Expression is pure (no side effects).\n")]
             [(io)
              (display "  ⚠ Expression performs I/O operations.\n")]
             [(mutation)
              (display "  ⚠ Expression mutates state.\n")]
             [(exception)
              (display "  ⚠ Expression may throw exceptions.\n")]
             [(nonterminating)
              (display "  ⚠ Expression may not terminate.\n")]
             [(async)
              (display "  ⚠ Expression performs async operations.\n")]
             [(unknown)
              (display "  ? Effect could not be determined.\n")])
       (display "\n")))

;;; lint-pure : String → Void
;;; Lint a file for purity violations.
(define (lint-pure path)
  (let ([violations (lint-effects path 'pure)])
       (display "\n")
       (printf "  Effect Lint: ~a\n" path)
       (display "  ════════════════════════════════════════════\n")
       (if (null? violations)
           (display "  ✓ All functions are pure.\n")
           (begin
            (printf "  ✗ Found ~a violation(s):\n\n" (length violations))
            (for-each
             (lambda (v)
                     (let ([fn (cadr (assq 'function (cdr v)))]
                           [expected (cadr (assq 'expected (cdr v)))]
                           [actual (cadr (assq 'actual (cdr v)))])
                          (printf "    ~a: expected ~a, has ~a\n"
                                  fn expected actual)))
             violations)))
       (display "\n")))

;;; show-module-effects : String → Void
;;; Display effect analysis for all functions in a module.
(define (show-module-effects path)
  (let ([analysis (analyze-module-effects path)])
       (display "\n")
       (printf "  Module Effects: ~a\n" path)
       (display "  ════════════════════════════════════════════\n")
       (cond
        [(and (pair? analysis) (assq 'error analysis))
         (printf "  Error: ~a\n" (cdr (assq 'error analysis)))]
        [(null? analysis)
         (display "  No definitions found.\n")]
        [else
         (let* ([pure-count (length (filter (lambda (e) (eq? (cdr e) 'pure)) analysis))]
                [impure (filter (lambda (e) (not (eq? (cdr e) 'pure))) analysis)])
               (printf "  Total: ~a definitions\n" (length analysis))
               (printf "  Pure: ~a\n" pure-count)
               (printf "  Impure: ~a\n\n" (length impure))
               
               (when (pair? impure)
                     (display "  Impure functions:\n")
                     (for-each
                      (lambda (entry)
                              (printf "    ~a: ~a\n" (car entry) (cdr entry)))
                      impure)))])
       (display "\n")))

;;; ============================================================
;;; Core vs Shell Boundary Checking
;;; ============================================================

;;; check-core-purity : → Void
;;; Verify that core/ modules are pure.
(define (check-core-purity)
  (display "\n")
  (display "  Core Purity Check\n")
  (display "  ════════════════════════════════════════════\n")
  (let* ([core-files (find-scheme-files "core")]
         [results (map (lambda (f) (cons f (lint-effects f 'pure))) core-files)]
         [violations (filter (lambda (r) (pair? (cdr r))) results)])
        (printf "  Scanned ~a core files.\n" (length core-files))
        (if (null? violations)
            (display "  ✓ All core modules are pure.\n")
            (begin
             (printf "  ✗ Found violations in ~a file(s):\n\n" (length violations))
             (for-each
              (lambda (r)
                      (printf "  ~a:\n" (car r))
                      (for-each
                       (lambda (v)
                               (when (pair? v)
                                     (let ([fn (assq 'function (cdr v))])
                                          (when fn
                                                (printf "    - ~a\n" (cadr fn))))))
                       (cdr r)))
              violations))))
  (display "\n"))

;;; find-scheme-files : String → (List String)
(define (find-scheme-files dir)
  (guard (e [else '()])
         (let loop ([pending (list dir)] [results '()])
              (if (null? pending)
                  results
                  (let ([current (car pending)])
                       (if (file-directory? current)
                           (let ([entries (map (lambda (e)
                                                       (string-append current "/" e))
                                               (directory-list current))])
                                (loop (append (cdr pending) entries) results))
                           (if (string-suffix? ".ss" current)
                               (loop (cdr pending) (cons current results))
                               (loop (cdr pending) results))))))))

;;; string-suffix? : String × String → Boolean
(define (string-suffix? suffix str)
  (let ([slen (string-length suffix)]
        [len (string-length str)])
       (and (>= len slen)
            (string=? suffix (substring str (- len slen) len)))))

;;; ============================================================
;;; Effect Annotations
;;; ============================================================

;;; @pure : Symbol → Void
;;; Annotate a function as pure.
(define (@pure name)
  (annotate-effect! name 'pure)
  (printf "Annotated ~a as pure.\n" name))

;;; @io : Symbol → Void
;;; Annotate a function as having IO effects.
(define (@io name)
  (annotate-effect! name 'io)
  (printf "Annotated ~a as io.\n" name))

;;; @mutation : Symbol → Void
;;; Annotate a function as having mutation effects.
(define (@mutation name)
  (annotate-effect! name 'mutation)
  (printf "Annotated ~a as mutation.\n" name))

;;; ============================================================
;;; Help
;;; ============================================================

(define (effect-help)
  (display "\n")
  (display "  Effect Typing and Linting Commands\n")
  (display "  ────────────────────────────────────────────\n")
  (display "  (effect-of expr)           - Show expression's effect\n")
  (display "  (lint-pure path)           - Check file for purity\n")
  (display "  (show-module-effects path) - Show all effects in module\n")
  (display "  (check-core-purity)        - Verify core/ is pure\n")
  (display "\n")
  (display "  Annotations:\n")
  (display "    (@pure name)      - Mark function as pure\n")
  (display "    (@io name)        - Mark function as having IO\n")
  (display "    (@mutation name)  - Mark function as mutating state\n")
  (display "\n")
  (display "  Effect categories:\n")
  (display "    pure          - No side effects\n")
  (display "    io            - Input/output operations\n")
  (display "    mutation      - State modification\n")
  (display "    exception     - May throw errors\n")
  (display "    nonterminating - May not halt\n")
  (display "    async         - Asynchronous operations\n")
  (display "\n")
  (display "  Examples:\n")
  (display "    (effect-of '(+ 1 2))        ;; → pure\n")
  (display "    (effect-of '(display \"hi\")) ;; → io\n")
  (display "    (lint-pure \"core/types/types.ss\")\n")
  (display "\n"))

;;; Initialize database
(init-effect-database!)

;;; filter-map helper
(define (filter-map f lst)
  (let loop ([items lst] [result '()])
       (if (null? items)
           (reverse result)
           (let ([mapped (f (car items))])
                (if mapped
                    (loop (cdr items) (cons mapped result))
                    (loop (cdr items) result))))))

(display "Effect lint loaded. Use (effect-help) for usage.\n")
