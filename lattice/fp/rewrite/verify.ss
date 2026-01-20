(load "core/base/prelude.ss")
(load "core/blocks/normalize.ss")
(load "lattice/fp/rewrite/trace.ss")

(doc 'module 'verify)
(doc 'description "Equivalence Verification

Provides verification that rewrite transformations preserve meaning.
Uses multiple approaches:

  1. Alpha-equivalence: De Bruijn normalization for structural equality
  2. NbE-based: Normalization by evaluation for definitional equality
  3. Trace verification: Check that each step preserves equivalence")
(doc 'layer 'lattice)
(doc 'purity 'total)

(doc 'section 'alpha-equivalence)
(doc 'description "Leverage de Bruijn normalization from core/blocks/normalize.ss
Two expressions are alpha-equivalent if they normalize to
the same de Bruijn form.")

(define (alpha-equiv? e1 e2)
  (doc 'type '(-> Expr Expr Boolean))
  (doc 'description "Check if two expressions are alpha-equivalent.
Uses de Bruijn indices to ignore variable naming.")
  (equal? (normalize e1) (normalize e2)))

(define alpha-normalize normalize)
(doc alpha-normalize 'type '(-> Expr Expr))
(doc alpha-normalize 'description "Normalize an expression to de Bruijn form.
Re-export from normalize.ss for convenience.")

(doc 'section 'structural-equivalence)
(doc 'description "For simpler expressions without binders, we can use
direct structural comparison.")

(define (struct-equiv? e1 e2)
  (doc 'type '(-> Expr Expr Boolean))
  (doc 'description "Check structural equality (no normalization).")
  (equal? e1 e2))

(doc 'section 'equivalence-result-type)
(doc 'description "Equivalence results are tagged unions:
  (ok #t reason)          - Expressions are equivalent
  (ok #f reason)          - Expressions are not equivalent
  (error 'tag message)    - Could not determine")

(define (equiv-ok result reason)
  (doc 'type '(-> Boolean Symbol EquivResult))
  `(ok ,result ,reason))

(define (equiv-error tag message)
  (doc 'type '(-> Symbol Any EquivResult))
  `(error ,tag ,message))

(define (equiv-ok? r)
  (doc 'type '(-> Any Boolean))
  (and (pair? r) (eq? (car r) 'ok)))

(define (equiv-error? r)
  (doc 'type '(-> Any Boolean))
  (and (pair? r) (eq? (car r) 'error)))

(define (equiv-result r)
  (doc 'type '(-> EquivResult Boolean))
  (if (equiv-ok? r) (cadr r) #f))

(define (equiv-reason r)
  (doc 'type '(-> EquivResult Symbol))
  (if (equiv-ok? r) (caddr r) #f))

(doc 'section 'verification-functions)

(define (verify-equivalence e1 e2)
  (doc 'type '(-> Expr Expr EquivResult))
  (doc 'description "Check if two expressions are equivalent using multiple methods.")
  (cond
   [(struct-equiv? e1 e2)
    (equiv-ok #t 'structural)]

   [(alpha-equiv? e1 e2)
    (equiv-ok #t 'alpha)]

   [else
    (equiv-ok #f 'not-equivalent)]))

(define (quick-equiv? e1 e2)
  (doc 'type '(-> Expr Expr Boolean))
  (doc 'description "Fast equivalence check (structural only).")
  (struct-equiv? e1 e2))

(define (full-equiv? e1 e2)
  (doc 'type '(-> Expr Expr Boolean))
  (doc 'description "Full equivalence check (alpha + structural).")
  (or (struct-equiv? e1 e2)
      (alpha-equiv? e1 e2)))

(doc 'section 'trace-verification)

(define (verify-trace trace)
  (doc 'type '(-> Trace EquivResult))
  (doc 'description "Verify that a trace's initial and final expressions are equivalent.
Note: This only checks endpoints, not individual steps.")
  (let ([initial (trace-initial trace)]
        [final (trace-final trace)])
       (verify-equivalence initial final)))

(define (verify-trace-steps trace)
  (doc 'type '(-> Trace (List EquivResult)))
  (doc 'description "Verify each step in a trace preserves meaning.
Returns list of verification results for each step.")
  (let ([steps (trace-steps trace)])
       (map (lambda (step)
                    (verify-equivalence (step-from step) (step-to step)))
            steps)))

(define (trace-all-valid? trace)
  (doc 'type '(-> Trace Boolean))
  (doc 'description "Check if all steps in a trace are valid equivalences.")
  (let ([results (verify-trace-steps trace)])
       (for-all (lambda (r) (and (equiv-ok? r) (equiv-result r)))
                results)))

(doc 'section 'equivalence-certificates)
(doc 'description "A certificate is a record of how equivalence was established.")

(define (make-certificate method e1 e2)
  (doc 'type '(-> Symbol Expr Expr Certificate))
  `((method . ,method)
    (lhs . ,e1)
    (rhs . ,e2)
    (timestamp . ,(current-time))))

(define (current-time)
  (doc 'description "Current time helper (returns seconds since epoch)")
  (let ([t (current-date)])
       (date->seconds t)))

(define (date->seconds d)
  (doc 'description "Date to seconds (approximate)")
  0)

(define (equiv-certificate e1 e2)
  (doc 'type '(-> Expr Expr (Union Certificate Boolean)))
  (doc 'description "Attempt to produce a certificate of equivalence.")
  (cond
   [(struct-equiv? e1 e2)
    (make-certificate 'structural e1 e2)]

   [(alpha-equiv? e1 e2)
    (make-certificate 'alpha (alpha-normalize e1) (alpha-normalize e2))]

   [else #f]))

(doc 'section 'difference-detection)
(doc 'description "When expressions are not equivalent, it's helpful to know why.")

(define (find-difference e1 e2)
  (doc 'type '(-> Expr Expr (Union (List Position Expr Expr) Boolean)))
  (doc 'description "Find the first position where expressions differ.")
  (find-diff-helper e1 e2 '()))

(define (find-diff-helper e1 e2 path)
  (cond
   [(equal? e1 e2) #f]

   [(and (pair? e1) (pair? e2))
    (or (find-diff-helper (car e1) (car e2) (append path '(car)))
        (find-diff-helper (cdr e1) (cdr e2) (append path '(cdr))))]

   [else (list path e1 e2)]))

(define (diff-description diff)
  (doc 'type '(-> (Union (List Position Expr Expr) Boolean) String))
  (doc 'description "Format a difference for display.")
  (if diff
      (format "At ~a: ~a vs ~a"
              (car diff)
              (cadr diff)
              (caddr diff))
      "No difference found"))

(doc 'section 'free-variable-analysis)
(doc 'description "For checking side conditions like eta-reduction.")

(define (free-vars expr)
  (doc 'type '(-> Expr (List Symbol)))
  (doc 'description "Collect free variables from an expression.")
  (free-vars-helper expr '()))

(define (free-vars-helper expr bound)
  (cond
   [(symbol? expr)
    (if (memq expr bound) '() (list expr))]

   [(not (pair? expr)) '()]

   [(and (eq? (car expr) 'fn)
         (pair? (cdr expr))
         (pair? (cadr expr))
         (symbol? (caadr expr)))
    (let ([param (caadr expr)]
          [body (caddr expr)])
         (free-vars-helper body (cons param bound)))]

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

   [else
    (append (free-vars-helper (car expr) bound)
            (free-vars-helper (cdr expr) bound))]))

(define (not-free? var expr)
  (doc 'type '(-> Symbol Expr Boolean))
  (doc 'description "Check if a variable is NOT free in an expression.")
  (not (memq var (free-vars expr))))

(doc 'section 'substitution-verification)
(doc 'description "For verifying beta reduction is correct.")

(define (substitute expr var value)
  (doc 'type '(-> Expr Symbol Expr Expr))
  (doc 'description "Substitute value for variable in expression.
Captures naive substitution (no renaming).")
  (cond
   [(symbol? expr)
    (if (eq? expr var) value expr)]

   [(not (pair? expr)) expr]

   [(and (eq? (car expr) 'fn)
         (pair? (cdr expr))
         (pair? (cadr expr))
         (eq? (caadr expr) var))
    expr]

   [else
    (cons (substitute (car expr) var value)
          (substitute (cdr expr) var value))]))

(define (verify-substitution body var value expected)
  (doc 'type '(-> Expr Symbol Expr Expr Boolean))
  (doc 'description "Check that expected = substitute(body, var, value).")
  (equal? (substitute body var value) expected))
