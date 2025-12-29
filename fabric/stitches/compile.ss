;;; fabric/stitches/compile.ss — Unified Compilation Pipeline
;;;
;;; Threads an expression through all compilation phases:
;;;   read → parse → normalize → expand → infer → eval
;;;
;;; Each phase is optional — caller specifies where to stop.
;;; All results use a consistent Result type with error threading.
;;;
;;; Result = (ok value context)
;;;        | (error phase error-type details...)
;;;        | (suspended expr env)  ; eval only
;;;
;;; This is Core code: pure, total, assumes perfect input.
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - parse.ss
;;;   - normalize.ss
;;;   - expand.ss
;;;   - types.ss
;;;   - infer.ss
;;;   - eval.ss

(load "prelude.ss")
(load "parse.ss")
(load "span.ss")
(load "fold-parse.ss")
(load "normalize.ss")
(load "expand.ss")
(load "types.ss")
(load "infer.ss")
(load "eval.ss")

;;; ============================================================
;;; Compilation Phases
;;; ============================================================

;;; Phase tags (in order)
(define *phases* '(read parse normalize expand infer eval))

;;; phase-index : Symbol → Nat
;;; Return the index of a phase (0-based).
(define (phase-index phase)
  (let loop ([phases *phases*] [i 0])
       (cond
        [(null? phases) (error 'phase-index "Unknown phase" phase)]
        [(eq? (car phases) phase) i]
        [else (loop (cdr phases) (+ i 1))])))

;;; phase<=? : Symbol × Symbol → Boolean
;;; True if phase1 comes before or equals phase2.
(define (phase<=? p1 p2)
  (<= (phase-index p1) (phase-index p2)))

;;; ============================================================
;;; Unified Result Type
;;; ============================================================

;;; Result constructors
(define (result-ok value . context)
  `(ok ,value ,@context))

(define (result-error phase error-type . details)
  `(error ,phase ,error-type ,@details))

(define (result-suspended expr env)
  `(suspended ,expr ,env))

;;; Result predicates
(define (result-ok? r)
  (and (pair? r) (eq? (car r) 'ok)))

(define (result-error? r)
  (and (pair? r) (eq? (car r) 'error)))

(define (result-suspended? r)
  (and (pair? r) (eq? (car r) 'suspended)))

;;; Result accessors
(define (result-value r)
  (if (result-ok? r)
      (cadr r)
      (error 'result-value "Not an ok result" r)))

(define (result-context r)
  (if (result-ok? r)
      (cddr r)
      (error 'result-context "Not an ok result" r)))

(define (result-phase r)
  (if (result-error? r)
      (cadr r)
      (error 'result-phase "Not an error result" r)))

(define (result-error-type r)
  (if (result-error? r)
      (caddr r)
      (error 'result-error-type "Not an error result" r)))

(define (result-error-details r)
  (if (result-error? r)
      (cdddr r)
      (error 'result-error-details "Not an error result" r)))

;;; ============================================================
;;; Result Threading
;;; ============================================================

;;; result-bind : Result × (Value → Result) → Result
;;; Monadic bind: continue with f if ok, propagate error otherwise.
(define (result-bind r f)
  (cond
   [(result-ok? r) (f (result-value r))]
   [(result-error? r) r]
   [(result-suspended? r) r]
   [else (result-error 'bind 'invalid-result r)]))

;;; result-map : Result × (Value → Value) → Result
;;; Functor map: apply f to value if ok, propagate error otherwise.
(define (result-map r f)
  (cond
   [(result-ok? r) (result-ok (f (result-value r)))]
   [(result-error? r) r]
   [(result-suspended? r) r]
   [else (result-error 'map 'invalid-result r)]))

;;; >>= : Result × (Value → Result) → Result
;;; Alias for result-bind
(define >>= result-bind)

;;; ============================================================
;;; Phase Adapters
;;; ============================================================

;;; Each adapter converts phase-specific results to unified Results.

;;; adapt-read : String → Result
;;; Read phase: convert string to S-expression.
(define (adapt-read input)
  (guard (ex [else (result-error 'read 'read-error (ex))])
         (let ([expr (read (open-input-string input))])
              (if (eof-object? expr)
                  (result-error 'read 'empty-input)
                  (result-ok expr)))))

;;; adapt-parse : String [× String] → Result
;;; Parse phase using the Fold parser with source spans.
;;; Returns stripped AST (spans removed) for compatibility with downstream phases.
;;; Full spanned AST available via parse-fold-expr directly.
(define (adapt-parse input . file-arg)
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

;;; adapt-normalize : S-expr → Result
;;; Normalize phase: convert to de Bruijn form.
(define (adapt-normalize expr)
  (guard (ex [else (result-error 'normalize 'normalize-error (ex))])
         (result-ok (normalize expr))))

;;; adapt-expand : S-expr → Result
;;; Expand phase: convert from de Bruijn back to named form.
;;; Uses fresh symbol supply.
(define (adapt-expand expr)
  (guard (ex [else (result-error 'expand 'expand-error (ex))])
         (result-ok (expand-fresh expr))))

;;; adapt-infer : S-expr → Result
;;; Infer phase: type inference.
(define (adapt-infer expr)
  (reset-fresh!)  ; Reset type variable counter
  (let ([result (infer expr empty-tenv)])
       (cond
        [(and (pair? result) (eq? (car result) 'ok))
         (result-ok (cadr result) `(substitution ,(caddr result)))]
        [(and (pair? result) (eq? (car result) 'error))
         (apply result-error 'infer (cdr result))]
        [else (result-error 'infer 'unexpected-result result)])))

;;; adapt-eval : S-expr × Nat → Result
;;; Eval phase: evaluation with fuel.
;;; Note: run returns (ok value) or (suspended expr) or (error tag info)
(define (adapt-eval expr fuel)
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

;;; ============================================================
;;; Compilation Pipeline
;;; ============================================================

;;; Default fuel for evaluation
(define *default-fuel* 10000)

;;; compile : String [#:to Phase] [#:fuel Nat] → Result
;;; Compile an input string through the pipeline.
;;;
;;; Options:
;;;   #:to   - Phase to stop at (default: 'eval)
;;;   #:fuel - Fuel for evaluation (default: 10000)
;;;
;;; Returns the result of the final requested phase.
(define (compile input . options)
  (let* ([to-phase (option-get options 'to 'eval)]
         [fuel (option-get options 'fuel *default-fuel*)])
        (compile-pipeline input to-phase fuel)))

;;; option-get : Options × Symbol × Any → Any
;;; Get option value or default.
(define (option-get options key default)
  (let loop ([opts options])
       (cond
        [(null? opts) default]
        [(null? (cdr opts)) default]
        [(eq? (car opts) key) (cadr opts)]
        [else (loop (cddr opts))])))

;;; compile-pipeline : String × Phase × Fuel → Result
;;; Internal pipeline runner.
(define (compile-pipeline input to-phase fuel)
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

;;; ============================================================
;;; Convenience Functions
;;; ============================================================

;;; compile-to : String × Phase → Result
;;; Compile to a specific phase.
(define (compile-to input phase)
  (compile input 'to phase))

;;; typeof : String → Type | Error
;;; Get the type of an expression.
(define (typeof input)
  (let ([r (compile input 'to 'infer)])
       (if (result-ok? r)
           (result-value r)
           r)))

;;; eval-string : String [Fuel] → Value | Error
;;; Evaluate a string expression.
(define (eval-string input . args)
  (let* ([fuel (if (null? args) *default-fuel* (car args))]
         [r (compile input 'to 'eval 'fuel fuel)])
        (if (result-ok? r)
            (result-value r)
            r)))

;;; compile-file : Path → Result
;;; Compile a file (reads entire file, evaluates as sequence).
(define (compile-file path)
  (guard (ex [else (result-error 'read 'file-error path)])
         (let ([contents (call-with-input-file path
                                               (lambda (port)
                                                       (let loop ([exprs '()])
                                                            (let ([e (read port)])
                                                                 (if (eof-object? e)
                                                                     (reverse exprs)
                                                                     (loop (cons e exprs)))))))])
              (compile-exprs contents))))

;;; compile-exprs : (List S-expr) → Result
;;; Compile a list of expressions, returning last result.
(define (compile-exprs exprs)
  (if (null? exprs)
      (result-ok '())
      (let loop ([es exprs] [last-result (result-ok '())])
           (if (null? es)
               last-result
               (let ([r (compile-expr (car es))])
                    (if (result-ok? r)
                        (loop (cdr es) r)
                        r))))))

;;; compile-expr : S-expr → Result
;;; Compile an already-parsed expression.
(define (compile-expr expr)
  (let ([r1 (adapt-infer expr)])
       (if (not (result-ok? r1))
           r1
           (adapt-eval expr *default-fuel*))))

;;; ============================================================
;;; Error Formatting
;;; ============================================================

;;; format-result : Result → String
;;; Format a result for display.
(define (format-result r)
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

;;; ============================================================
;;; Pipeline Info
;;; ============================================================

;;; show-pipeline : → void
;;; Display the compilation pipeline.
(define (show-pipeline)
  (display "Compilation Pipeline:\n")
  (display "  read → parse → normalize → expand → infer → eval\n")
  (display "\n")
  (display "Use (compile input 'to 'phase) to stop at any phase.\n")
  (display "Use (typeof input) to get just the type.\n")
  (display "Use (eval-string input) to evaluate.\n"))
