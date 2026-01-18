;;; core/blocks/nbe-normalize.ss — NbE-based Normalization for CAS Hashing
;;; @module nbe-normalize
;;; @requires prelude nbe
;;;
;;; This module provides NbE (Normalization by Evaluation) based
;;; normalization for the v3 CAS hashing pipeline.
;;;
;;; NbE normalization provides:
;;;   - β-reduction: ((fn (x) body) arg) → body[arg/x]
;;;   - η-equivalence: (fn (x) (f x)) ≡ f (when x not free in f)
;;;   - Pair projection: (fst (pair a b)) → a, (snd (pair a b)) → b
;;;   - Sum projection: (case (Left a) ...) → left-branch[a]
;;;   - Conditional reduction: (if #t a b) → a, (if #f a b) → b
;;;
;;; All these reductions happen intrinsically during NbE evaluation,
;;; eliminating the need for external rewrite rules and iteration.
;;;
;;; This is Core code: pure, total (with fuel bounding), assumes perfect input.
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - nbe.ss (fuel-bounded variants)

(load "core/base/prelude.ss")
(load "core/lang/nbe.ss")

;;; ====
;;; Configuration
;;; ====

;;; *nbe-cas-fuel* : Nat
;;; Fuel limit for CAS normalization.
;;; This value balances:
;;;   - High enough for practical code (nested functions, recursion)
;;;   - Low enough to catch divergence quickly (omega combinator)
;;;
;;; 10000 steps is ~10ms on modern hardware for well-behaved code.
(define *nbe-cas-fuel* 10000)

;;; ====
;;; NbE Normalization for CAS
;;; ====

;;; nbe-normalize-for-cas : S-expr → S-expr
;;; Normalize an expression using NbE for CAS hashing.
;;;
;;; This function:
;;;   1. Evaluates the expression to semantic values (β-reduction)
;;;   2. Reads back to normal form expressions
;;;   3. Falls back gracefully on errors or fuel exhaustion
;;;
;;; Properties:
;;;   - Idempotent: nbe-normalize-for-cas(nbe-normalize-for-cas(x)) = nbe-normalize-for-cas(x)
;;;   - Deterministic: same input → same output
;;;   - Total: always returns (may return original on failure)
;;;
;;; Example reductions:
;;;   ((fn (x) x) 5)              → 5
;;;   (fst (pair a b))            → a
;;;   (snd (pair a b))            → b
;;;   (case (Left 1) ...)         → [left branch with 1]
;;;   (if #t then else)           → then
;;;   ((fn (x) (x x)) (fn (x) ...)) → [original, stuck on fuel exhaustion]
(define (nbe-normalize-for-cas expr)
  ;; Use *nbe-cas-fuel* directly instead of nbe-normalize-safe's default
  ;; This allows CAS-specific fuel tuning independent of general NbE usage.
  (guard (ex [else expr])  ; Fall back on any error
         (let-values ([(result fuel) (normalize-closed-fuel expr *nbe-cas-fuel*)])
                     (if (> fuel 0) result expr))))

;;; ====
;;; NbE Normalization with Custom Fuel
;;; ====

;;; nbe-normalize-with-fuel : S-expr × Nat → (Values S-expr Boolean)
;;; Normalize with custom fuel, returning whether normalization completed.
;;;
;;; Returns (values normalized-expr completed?)
;;; where completed? is #t if fuel was sufficient, #f if exhausted.
;;;
;;; Use this for diagnostic purposes or when you need to know if
;;; the expression was fully reduced.
(define (nbe-normalize-with-fuel expr fuel)
  (guard (ex [else (values expr #f)])
         (let-values ([(result remaining) (normalize-closed-fuel expr fuel)])
                     (values result (> remaining 0)))))

;;; ====
;;; Utility: Check if Expression Needs NbE
;;; ====

;;; nbe-reducible? : S-expr → Boolean
;;; Heuristic check if an expression might benefit from NbE normalization.
;;; Returns #t if the expression contains reducible forms.
;;;
;;; This can be used to skip NbE for expressions that are already
;;; in normal form, though the overhead of NbE on normal forms is minimal.
(define (nbe-reducible? expr)
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

;;; ====
;;; Diagnostic: Trace Normalization Steps
;;; ====

;;; nbe-trace-normalize : S-expr × Nat → (List S-expr)
;;; Trace the normalization process, returning intermediate forms.
;;; Useful for debugging and understanding what NbE does.
;;;
;;; Note: This is expensive and should only be used for diagnostics.
(define (nbe-trace-normalize expr max-steps)
  (let ([trace '()])
    (let loop ([e expr] [steps max-steps])
      (set! trace (cons e trace))
      (if (<= steps 0)
          (reverse trace)
          (let-values ([(result fuel) (normalize-closed-fuel e 100)])
                      (if (equal? result e)
                          (reverse trace)  ; Fixed point reached
                          (loop result (- steps 1))))))))

;;; ====
;;; Integration Points
;;; ====

;;; The following functions provide the interface expected by
;;; the v3 normalization pipeline in normalize.ss.

;;; nbe-normalize-payload : S-expr → S-expr
;;; Primary entry point for the normalization pipeline.
;;; Alias for nbe-normalize-for-cas.
(define nbe-normalize-payload nbe-normalize-for-cas)

;;; nbe-complete? : S-expr × S-expr → Boolean
;;; Check if NbE normalization was complete (no fuel exhaustion).
;;; Compares input and output to detect stuck terms.
(define (nbe-complete? input output)
  (not (and (pair? output)
            (or (eq? (car output) 'stuck-readback)
                (eq? (car output) 'stuck-neutral)))))
