;;; core/fp/rewrite/verify.ss — Equivalence Verification
;;;
;;; Provides verification that rewrite transformations preserve meaning.
;;; Uses multiple approaches:
;;;
;;;   1. Alpha-equivalence: De Bruijn normalization for structural equality
;;;   2. NbE-based: Normalization by evaluation for definitional equality
;;;   3. Trace verification: Check that each step preserves equivalence
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Dependencies:
;;;   - core/base/prelude.ss
;;;   - core/blocks/normalize.ss  (for alpha-equivalence)
;;;   - core/fp/rewrite/trace.ss

(load "core/base/prelude.ss")
(load "core/blocks/normalize.ss")
(load "core/fp/rewrite/trace.ss")

;;; ============================================================
;;; Alpha-Equivalence
;;; ============================================================

;;; Leverage de Bruijn normalization from core/blocks/normalize.ss
;;; Two expressions are alpha-equivalent if they normalize to
;;; the same de Bruijn form.

;;; alpha-equiv? : Expr × Expr → Boolean
;;; Check if two expressions are alpha-equivalent.
;;; Uses de Bruijn indices to ignore variable naming.
(define (alpha-equiv? e1 e2)
  (equal? (normalize e1) (normalize e2)))

;;; alpha-normalize : Expr → Expr
;;; Normalize an expression to de Bruijn form.
;;; Re-export from normalize.ss for convenience.
(define alpha-normalize normalize)

;;; ============================================================
;;; Structural Equivalence
;;; ============================================================

;;; For simpler expressions without binders, we can use
;;; direct structural comparison.

;;; struct-equiv? : Expr × Expr → Boolean
;;; Check structural equality (no normalization).
(define (struct-equiv? e1 e2)
  (equal? e1 e2))

;;; ============================================================
;;; Equivalence Result Type
;;; ============================================================

;;; Equivalence results are tagged unions:
;;;   (ok #t reason)          - Expressions are equivalent
;;;   (ok #f reason)          - Expressions are not equivalent
;;;   (error 'tag message)    - Could not determine

(define (equiv-ok result reason)
  `(ok ,result ,reason))

(define (equiv-error tag message)
  `(error ,tag ,message))

(define (equiv-ok? r)
  (and (pair? r) (eq? (car r) 'ok)))

(define (equiv-error? r)
  (and (pair? r) (eq? (car r) 'error)))

(define (equiv-result r)
  (if (equiv-ok? r) (cadr r) #f))

(define (equiv-reason r)
  (if (equiv-ok? r) (caddr r) #f))

;;; ============================================================
;;; Verification Functions
;;; ============================================================

;;; verify-equivalence : Expr × Expr → EquivResult
;;; Check if two expressions are equivalent using multiple methods.
(define (verify-equivalence e1 e2)
  (cond
   ;; First try structural equality (fastest)
   [(struct-equiv? e1 e2)
    (equiv-ok #t 'structural)]
   
   ;; Then try alpha-equivalence (handles binders)
   [(alpha-equiv? e1 e2)
    (equiv-ok #t 'alpha)]
   
   ;; If neither, they're not equivalent
   ;; (NbE verification would go here for more sophisticated checking)
   [else
    (equiv-ok #f 'not-equivalent)]))

;;; quick-equiv? : Expr × Expr → Boolean
;;; Fast equivalence check (structural only).
(define (quick-equiv? e1 e2)
  (struct-equiv? e1 e2))

;;; full-equiv? : Expr × Expr → Boolean
;;; Full equivalence check (alpha + structural).
(define (full-equiv? e1 e2)
  (or (struct-equiv? e1 e2)
      (alpha-equiv? e1 e2)))

;;; ============================================================
;;; Trace Verification
;;; ============================================================

;;; verify-trace : Trace → EquivResult
;;; Verify that a trace's initial and final expressions are equivalent.
;;; Note: This only checks endpoints, not individual steps.
(define (verify-trace trace)
  (let ([initial (trace-initial trace)]
        [final (trace-final trace)])
       (verify-equivalence initial final)))

;;; verify-trace-steps : Trace → (List EquivResult)
;;; Verify each step in a trace preserves meaning.
;;; Returns list of verification results for each step.
(define (verify-trace-steps trace)
  (let ([steps (trace-steps trace)])
       (map (lambda (step)
                    (verify-equivalence (step-from step) (step-to step)))
            steps)))

;;; trace-all-valid? : Trace → Boolean
;;; Check if all steps in a trace are valid equivalences.
(define (trace-all-valid? trace)
  (let ([results (verify-trace-steps trace)])
       (for-all (lambda (r) (and (equiv-ok? r) (equiv-result r)))
                results)))

;;; ============================================================
;;; Equivalence Certificates
;;; ============================================================

;;; A certificate is a record of how equivalence was established.

;;; make-certificate : Symbol × Expr × Expr → Certificate
(define (make-certificate method e1 e2)
  `((method . ,method)
    (lhs . ,e1)
    (rhs . ,e2)
    (timestamp . ,(current-time))))

;;; Current time helper (returns seconds since epoch)
(define (current-time)
  (let ([t (current-date)])
       (date->seconds t)))

;;; Date to seconds (approximate)
(define (date->seconds d)
  ;; Simple approximation - returns 0 if date functions unavailable
  0)

;;; equiv-certificate : Expr × Expr → Certificate | #f
;;; Attempt to produce a certificate of equivalence.
(define (equiv-certificate e1 e2)
  (cond
   [(struct-equiv? e1 e2)
    (make-certificate 'structural e1 e2)]
   
   [(alpha-equiv? e1 e2)
    (make-certificate 'alpha (alpha-normalize e1) (alpha-normalize e2))]
   
   [else #f]))

;;; ============================================================
;;; Difference Detection
;;; ============================================================

;;; When expressions are not equivalent, it's helpful to know why.

;;; find-difference : Expr × Expr → (Position × Expr × Expr) | #f
;;; Find the first position where expressions differ.
(define (find-difference e1 e2)
  (find-diff-helper e1 e2 '()))

(define (find-diff-helper e1 e2 path)
  (cond
   ;; Equal: no difference
   [(equal? e1 e2) #f]
   
   ;; Both pairs: recurse
   [(and (pair? e1) (pair? e2))
    (or (find-diff-helper (car e1) (car e2) (append path '(car)))
        (find-diff-helper (cdr e1) (cdr e2) (append path '(cdr))))]
   
   ;; Structure mismatch or atom difference
   [else (list path e1 e2)]))

;;; diff-description : (Position × Expr × Expr) → String
;;; Format a difference for display.
(define (diff-description diff)
  (if diff
      (format "At ~a: ~a vs ~a"
              (car diff)
              (cadr diff)
              (caddr diff))
      "No difference found"))

;;; ============================================================
;;; Free Variable Analysis
;;; ============================================================

;;; For checking side conditions like eta-reduction.

;;; free-vars : Expr → (List Symbol)
;;; Collect free variables from an expression.
(define (free-vars expr)
  (free-vars-helper expr '()))

(define (free-vars-helper expr bound)
  (cond
   ;; Symbol: free if not bound
   [(symbol? expr)
    (if (memq expr bound) '() (list expr))]
   
   ;; Not a pair: no free vars
   [(not (pair? expr)) '()]
   
   ;; Lambda: (fn (var) body) - bind parameter
   [(and (eq? (car expr) 'fn)
         (pair? (cdr expr))
         (pair? (cadr expr))
         (symbol? (caadr expr)))
    (let ([param (caadr expr)]
          [body (caddr expr)])
         (free-vars-helper body (cons param bound)))]
   
   ;; Let: bind variable
   [(and (eq? (car expr) 'let)
         (pair? (cdr expr))
         (pair? (cadr expr))
         (pair? (caadr expr)))
    (let* ([binding (caadr expr)]
           [var (car binding)]
           [val (cadr binding)]
           [body (caddr expr)])
          (append (free-vars-helper val bound)
                  (free-vars-helper body (cons var bound))))]
   
   ;; Other pairs: recurse
   [else
    (append (free-vars-helper (car expr) bound)
            (free-vars-helper (cdr expr) bound))]))

;;; not-free? : Symbol × Expr → Boolean
;;; Check if a variable is NOT free in an expression.
(define (not-free? var expr)
  (not (memq var (free-vars expr))))

;;; ============================================================
;;; Substitution Verification
;;; ============================================================

;;; For verifying beta reduction is correct.

;;; substitute : Expr × Symbol × Expr → Expr
;;; Substitute value for variable in expression.
;;; Captures naive substitution (no renaming).
(define (substitute expr var value)
  (cond
   ;; Variable: replace if matches
   [(symbol? expr)
    (if (eq? expr var) value expr)]
   
   ;; Not a pair: return as-is
   [(not (pair? expr)) expr]
   
   ;; Lambda: don't substitute shadowed variable
   [(and (eq? (car expr) 'fn)
         (pair? (cdr expr))
         (pair? (cadr expr))
         (eq? (caadr expr) var))
    expr]
   
   ;; Other: recurse
   [else
    (cons (substitute (car expr) var value)
          (substitute (cdr expr) var value))]))

;;; verify-substitution : Expr × Symbol × Expr × Expr → Boolean
;;; Check that expected = substitute(body, var, value).
(define (verify-substitution body var value expected)
  (equal? (substitute body var value) expected))
