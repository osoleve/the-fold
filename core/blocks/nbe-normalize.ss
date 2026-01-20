(load "core/base/prelude.ss")
(load "core/lang/nbe.ss")

(doc 'module 'nbe-normalize)
(doc 'description "NbE-based normalization for CAS hashing. Provides β-reduction, η-equivalence, projections, and conditionals.")
(doc 'layer 'core)

(doc 'section 'configuration)

(doc *nbe-cas-fuel* 'type Nat)
(doc *nbe-cas-fuel* 'description "Fuel limit for CAS normalization. 10000 steps ≈ 10ms for well-behaved code.")
(define *nbe-cas-fuel* 10000)

(doc 'section 'nbe-normalization)

(define (nbe-normalize-for-cas expr)
  (doc 'type (-> Any Any))
  (doc 'description "Normalize expression using NbE for CAS hashing. Idempotent, deterministic, total.")
  (doc 'export #t)
  ;; Use *nbe-cas-fuel* directly instead of nbe-normalize-safe's default
  ;; This allows CAS-specific fuel tuning independent of general NbE usage.
  (guard (ex [else expr])  ; Fall back on any error
         (let-values ([(result fuel) (normalize-closed-fuel expr *nbe-cas-fuel*)])
                     (if (> fuel 0) result expr))))

(doc 'section 'custom-fuel)

(define (nbe-normalize-with-fuel expr fuel)
  (doc 'type (-> Any Nat (Values Any Boolean)))
  (doc 'description "Normalize with custom fuel. Returns (values normalized-expr completed?).")
  (doc 'export #t)
  (guard (ex [else (values expr #f)])
         (let-values ([(result remaining) (normalize-closed-fuel expr fuel)])
                     (values result (> remaining 0)))))

(doc 'section 'utility)

(define (nbe-reducible? expr)
  (doc 'type (-> Any Boolean))
  (doc 'description "Heuristic check if expression might benefit from NbE normalization.")
  (doc 'export #t)
  (cond
   [(not (pair? expr)) #f]

   ;; Application of lambda - β-reducible
   [(and (pair? (car expr))
         (or (eq? (caar expr) 'fn)
             (eq? (caar expr) 'λ)))
    #t]

   ;; Projection of pair - reducible
   [(and (eq? (car expr) 'fst)
         (pair? (cdr expr))
         (pair? (cadr expr))
         (eq? (caadr expr) 'pair))
    #t]

   [(and (eq? (car expr) 'snd)
         (pair? (cdr expr))
         (pair? (cadr expr))
         (eq? (caadr expr) 'pair))
    #t]

   ;; Case on known sum - reducible
   [(and (eq? (car expr) 'case)
         (pair? (cdr expr))
         (pair? (cadr expr))
         (or (eq? (caadr expr) 'Left)
             (eq? (caadr expr) 'Right)))
    #t]

   ;; Conditional with literal condition - reducible
   [(and (eq? (car expr) 'if)
         (pair? (cdr expr))
         (boolean? (cadr expr)))
    #t]

   ;; Recurse into subexpressions
   [(eq? (car expr) 'quote) #f]
   [else (ormap nbe-reducible? expr)]))

(doc 'section 'diagnostics)

(define (nbe-trace-normalize expr max-steps)
  (doc 'type (-> Any Nat (List Any)))
  (doc 'description "Trace normalization process, returning intermediate forms. Expensive - diagnostics only.")
  (doc 'export #t)
  (let ([trace '()])
    (let loop ([e expr] [steps max-steps])
      (set! trace (cons e trace))
      (if (<= steps 0)
          (reverse trace)
          (let-values ([(result fuel) (normalize-closed-fuel e 100)])
                      (if (equal? result e)
                          (reverse trace)  ; Fixed point reached
                          (loop result (- steps 1))))))))

(doc 'section 'integration)

(doc nbe-normalize-payload 'type (-> Any Any))
(doc nbe-normalize-payload 'description "Primary entry point for normalization pipeline. Alias for nbe-normalize-for-cas.")
(doc nbe-normalize-payload 'export #t)
(define nbe-normalize-payload nbe-normalize-for-cas)

(define (nbe-complete? input output)
  (doc 'type (-> Any Any Boolean))
  (doc 'description "Check if NbE normalization was complete (no fuel exhaustion).")
  (doc 'export #t)
  (not (and (pair? output)
            (or (eq? (car output) 'stuck-readback)
                (eq? (car output) 'stuck-neutral)))))
