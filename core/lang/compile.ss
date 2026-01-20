(load "core/base/prelude.ss")
(load "core/lang/parse.ss")
(load "core/lang/span.ss")
(load "core/lang/fold-parse.ss")
(load "core/blocks/normalize.ss")
(load "core/blocks/expand.ss")
(load "core/types/types.ss")
(load "core/types/infer.ss")
(load "core/lang/eval.ss")

(doc 'module 'compile)
(doc 'description "Unified Compilation Pipeline. Threads an expression through all compilation phases: read → parse → normalize → expand → infer → eval.")
(doc 'layer 'core)
(doc 'requires '(prelude parse span fold-parse normalize expand types infer eval))

(doc 'note "Result Types

Result = (ok value context)
       | (error phase error-type details...)
       | (suspended expr env)  ; eval only

Each phase is optional — caller specifies where to stop.
All results use a consistent Result type with error threading.

This is Core code: pure, total, assumes perfect input.")

(doc 'section 'phases)

(doc *phases* 'type (List Symbol))
(doc *phases* 'description "Phase tags in order: read parse normalize expand infer eval.")
(doc *phases* 'export #t)
(define *phases* '(read parse normalize expand infer eval))

(define (phase-index phase)
  (doc 'type (-> Symbol Nat))
  (doc 'description "Return the index of a phase (0-based).")
  (doc 'export #t)
  (let loop ([phases *phases*] [i 0])
       (cond
        [(null? phases) (error 'phase-index "Unknown phase" phase)]
        [(eq? (car phases) phase) i]
        [else (loop (cdr phases) (+ i 1))])))

(define (phase<=? p1 p2)
  (doc 'type (-> Symbol Symbol Boolean))
  (doc 'description "True if phase1 comes before or equals phase2.")
  (doc 'export #t)
  (<= (phase-index p1) (phase-index p2)))

(doc 'section 'result-type)

(define (result-ok value . context)
  (doc 'type (-> α Context... (Result α Error)))
  (doc 'description "Construct a successful result with value and optional context.")
  (doc 'export #t)
  `(ok ,value ,@context))

(define (result-error phase error-type . details)
  (doc 'type (-> Symbol Symbol α... (Result β Error)))
  (doc 'description "Construct an error result with phase, error type, and optional details.")
  (doc 'export #t)
  `(error ,phase ,error-type ,@details))

(define (result-suspended expr env)
  (doc 'type (-> Expr Env (Suspended Expr Env)))
  (doc 'description "Construct a suspended result (eval only).")
  (doc 'export #t)
  `(suspended ,expr ,env))

(define (result-ok? r)
  (doc 'type (-> α Boolean))
  (doc 'description "Test if result is successful.")
  (doc 'export #t)
  (and (pair? r) (eq? (car r) 'ok)))

(define (result-error? r)
  (doc 'type (-> α Boolean))
  (doc 'description "Test if result is an error.")
  (doc 'export #t)
  (and (pair? r) (eq? (car r) 'error)))

(define (result-suspended? r)
  (doc 'type (-> α Boolean))
  (doc 'description "Test if result is suspended.")
  (doc 'export #t)
  (and (pair? r) (eq? (car r) 'suspended)))

(define (result-value r)
  (doc 'type (-> (Result α Error) α))
  (doc 'description "Extract value from successful result.")
  (doc 'export #t)
  (if (result-ok? r)
      (cadr r)
      (error 'result-value "Not an ok result" r)))

(define (result-context r)
  (doc 'type (-> (Result α Error) (List α)))
  (doc 'description "Extract context from successful result.")
  (doc 'export #t)
  (if (result-ok? r)
      (cddr r)
      (error 'result-context "Not an ok result" r)))

(define (result-phase r)
  (doc 'type (-> (Result α Error) Symbol))
  (doc 'description "Extract phase from error result.")
  (doc 'export #t)
  (if (result-error? r)
      (cadr r)
      (error 'result-phase "Not an error result" r)))

(define (result-error-type r)
  (doc 'type (-> (Result α Error) Symbol))
  (doc 'description "Extract error type from error result.")
  (doc 'export #t)
  (if (result-error? r)
      (caddr r)
      (error 'result-error-type "Not an error result" r)))

(define (result-error-details r)
  (doc 'type (-> (Result α Error) (List α)))
  (doc 'description "Extract error details from error result.")
  (doc 'export #t)
  (if (result-error? r)
      (cdddr r)
      (error 'result-error-details "Not an error result" r)))

(doc 'section 'threading)

(define (result-bind r f)
  (doc 'type (-> (Result α Error) (-> α (Result β Error)) (Result β Error)))
  (doc 'description "Monadic bind: continue with f if ok, propagate error otherwise.")
  (doc 'export #t)
  (cond
   [(result-ok? r) (f (result-value r))]
   [(result-error? r) r]
   [(result-suspended? r) r]
   [else (result-error 'bind 'invalid-result r)]))

(define (result-map r f)
  (doc 'type (-> (Result α Error) (-> α β) (Result β Error)))
  (doc 'description "Functor map: apply f to value if ok, propagate error otherwise.")
  (doc 'export #t)
  (cond
   [(result-ok? r) (result-ok (f (result-value r)))]
   [(result-error? r) r]
   [(result-suspended? r) r]
   [else (result-error 'map 'invalid-result r)]))

(doc >>= 'type (-> (Result α Error) (-> α (Result β Error)) (Result β Error)))
(doc >>= 'description "Alias for result-bind.")
(doc >>= 'export #t)
(define >>= result-bind)

(doc 'section 'adapters)

(doc 'note "Each adapter converts phase-specific results to unified Results.")

(define (adapt-read input)
  (doc 'type (-> String (Result Expr Error)))
  (doc 'description "Read phase: convert string to S-expression.")
  (doc 'export #t)
  (guard (ex [else (result-error 'read 'read-error (ex))])
         (let ([expr (read (open-input-string input))])
              (if (eof-object? expr)
                  (result-error 'read 'empty-input)
                  (result-ok expr)))))

(define (adapt-parse input . file-arg)
  (doc 'type (-> String String... (Result Expr Error)))
  (doc 'description "Parse phase using the Fold parser with source spans. Returns stripped AST (spans removed) for compatibility with downstream phases. Full spanned AST available via parse-fold-expr directly.")
  (doc 'export #t)
  (let* ([file (if (null? file-arg) "<input>" (car file-arg))]
         [result (parse-fold-expr input file)])
        (cond
         [(eq? (car result) 'ok)
          ;; Success: strip spans for downstream compatibility
          (result-ok (strip-spans (cadr result)))]
         [(eq? (car result) 'error)
          ;; Error: preserve span information
          (let ([expected (caddr result)]
                [span (cadddr result)])
               (result-error 'parse 'syntax-error expected span))]
         [else (result-error 'parse 'unknown-error result)])))

(define (adapt-normalize expr)
  (doc 'type (-> Expr (Result Expr Error)))
  (doc 'description "Normalize phase: convert to de Bruijn form.")
  (doc 'export #t)
  (guard (ex [else (result-error 'normalize 'normalize-error (ex))])
         (result-ok (normalize expr))))

(define (adapt-expand expr)
  (doc 'type (-> Expr (Result Expr Error)))
  (doc 'description "Expand phase: convert from de Bruijn back to named form. Uses fresh symbol supply.")
  (doc 'export #t)
  (guard (ex [else (result-error 'expand 'expand-error (ex))])
         (result-ok (expand-fresh expr))))

(define (adapt-infer expr)
  (doc 'type (-> Expr (Result Type Error)))
  (doc 'description "Infer phase: type inference.")
  (doc 'export #t)
  (reset-fresh!)  ; Reset type variable counter
  (let ([result (infer expr empty-tenv)])
       (cond
        [(and (pair? result) (eq? (car result) 'ok))
         (result-ok (cadr result) `(substitution ,(caddr result)))]
        [(and (pair? result) (eq? (car result) 'error))
         (apply result-error 'infer (cdr result))]
        [else (result-error 'infer 'unexpected-result result)])))

(define (adapt-eval expr fuel)
  (doc 'type (-> Expr Nat (Result Value Error)))
  (doc 'description "Eval phase: evaluation with fuel.")
  (doc 'note "run returns (ok value) or (suspended expr) or (error tag info)")
  (doc 'export #t)
  (let ([result (run expr fuel)])
       (cond
        [(and (pair? result) (eq? (car result) 'ok))
         ;; run returns (ok value) without remaining fuel
         (result-ok (cadr result))]
        [(and (pair? result) (eq? (car result) 'error))
         (apply result-error 'eval (cdr result))]
        [(and (pair? result) (eq? (car result) 'suspended))
         ;; run returns (suspended expr) without env
         (result-suspended (cadr result) '())]
        [else (result-error 'eval 'unexpected-result result)])))

(doc 'section 'pipeline)

(doc *default-fuel* 'type Nat)
(doc *default-fuel* 'description "Default fuel for evaluation: 10000.")
(doc *default-fuel* 'export #t)
(define *default-fuel* 10000)

(define (compile input . options)
  (doc 'type (-> String α... (Result Value Error)))
  (doc 'description "Compile an input string through the pipeline.")
  (doc 'note "Options:
  #:to   - Phase to stop at (default: 'eval)
  #:fuel - Fuel for evaluation (default: 10000)

Returns the result of the final requested phase.")
  (doc 'export #t)
  (let* ([to-phase (option-get options 'to 'eval)]
         [fuel (option-get options 'fuel *default-fuel*)])
        (compile-pipeline input to-phase fuel)))

(define (option-get options key default)
  (doc 'type (-> (List α) Symbol β β))
  (doc 'description "Get option value or default.")
  (let loop ([opts options])
       (cond
        [(null? opts) default]
        [(null? (cdr opts)) default]
        [(eq? (car opts) key) (cadr opts)]
        [else (loop (cddr opts))])))

(define (compile-pipeline input to-phase fuel)
  (doc 'type (-> String Symbol Nat (Result Value Error)))
  (doc 'description "Internal pipeline runner.")
  ;; Phase 1: Read/Parse
  (let ([r1 (adapt-parse input)])
       (if (or (not (result-ok? r1)) (eq? to-phase 'read) (eq? to-phase 'parse))
           r1
           ;; Phase 2: Normalize (optional in full pipeline, but useful)
           (let* ([expr1 (result-value r1)]
                  ;; Skip normalize/expand for now - go straight to infer
                  ;; Normalization is more for CAS storage
                  [r2 (if (phase<=? to-phase 'expand)
                          (result-ok expr1)  ; Skip normalize
                          (result-ok expr1))])
                 (if (or (not (result-ok? r2)) (eq? to-phase 'normalize) (eq? to-phase 'expand))
                     r2
                     ;; Phase 3: Type Inference
                     (let ([r3 (adapt-infer (result-value r2))])
                          (if (or (not (result-ok? r3)) (eq? to-phase 'infer))
                              ;; Return type + original expression for eval
                              (if (result-ok? r3)
                                  (result-ok (result-value r3) `(expr ,(result-value r2)))
                                  r3)
                              ;; Phase 4: Evaluation
                              (adapt-eval (result-value r2) fuel))))))))

(doc 'section 'convenience)

(define (compile-to input phase)
  (doc 'type (-> String Symbol (Result Value Error)))
  (doc 'description "Compile to a specific phase.")
  (doc 'export #t)
  (compile input 'to phase))

(define (typeof input)
  (doc 'type (-> String Type))
  (doc 'description "Get the type of an expression.")
  (doc 'export #t)
  (let ([r (compile input 'to 'infer)])
       (if (result-ok? r)
           (result-value r)
           r)))

(define (eval-string input . args)
  (doc 'type (-> String Nat... Value))
  (doc 'description "Evaluate a string expression.")
  (doc 'export #t)
  (let* ([fuel (if (null? args) *default-fuel* (car args))]
         [r (compile input 'to 'eval 'fuel fuel)])
        (if (result-ok? r)
            (result-value r)
            r)))

(define (compile-file path)
  (doc 'type (-> String (Result Value Error)))
  (doc 'description "Compile a file (reads entire file, evaluates as sequence).")
  (doc 'export #t)
  (guard (ex [else (result-error 'read 'file-error path)])
         (let ([contents (call-with-input-file path
                                               (lambda (port)
                                                       (let loop ([exprs '()])
                                                            (let ([e (read port)])
                                                                 (if (eof-object? e)
                                                                     (reverse exprs)
                                                                     (loop (cons e exprs)))))))])
              (compile-exprs contents))))

(define (compile-exprs exprs)
  (doc 'type (-> (List Expr) (Result Value Error)))
  (doc 'description "Compile a list of expressions, returning last result.")
  (doc 'export #t)
  (if (null? exprs)
      (result-ok '())
      (let loop ([es exprs] [last-result (result-ok '())])
           (if (null? es)
               last-result
               (let ([r (compile-expr (car es))])
                    (if (result-ok? r)
                        (loop (cdr es) r)
                        r))))))

(define (compile-expr expr)
  (doc 'type (-> Expr (Result Value Error)))
  (doc 'description "Compile an already-parsed expression.")
  (doc 'export #t)
  (let ([r1 (adapt-infer expr)])
       (if (not (result-ok? r1))
           r1
           (adapt-eval expr *default-fuel*))))

(doc 'section 'formatting)

(define (format-result r)
  (doc 'type (-> (Result α Error) String))
  (doc 'description "Format a result for display.")
  (doc 'export #t)
  (cond
   [(result-ok? r)
    (format "~a" (result-value r))]
   [(result-error? r)
    (format "Error in ~a: ~a~a"
            (result-phase r)
            (result-error-type r)
            (if (null? (result-error-details r))
                ""
                (format " ~a" (result-error-details r))))]
   [(result-suspended? r)
    (format "Suspended: ~a" (cadr r))]
   [else (format "Unknown result: ~a" r)]))

(doc 'section 'info)

(define (show-pipeline)
  (doc 'type (-> Unit Void))
  (doc 'description "Display the compilation pipeline.")
  (doc 'export #t)
  (display "Compilation Pipeline:
")
  (display "  read → parse → normalize → expand → infer → eval
")
  (display "
")
  (display "Use (compile input 'to 'phase) to stop at any phase.
")
  (display "Use (typeof input) to get just the type.
")
  (display "Use (eval-string input) to evaluate.
"))
