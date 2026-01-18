;;; core/blocks/normalize.ss — S-expression α-normalization via de Bruijn indices
;;; @module normalize
;;; @requires prelude
;;;
;;; Converts named variables to positional indices, ensuring that
;;; α-equivalent expressions produce identical canonical forms.
;;;
;;; (fn (x) x)           → (fn (dv 0))
;;; (fn (x) (fn (y) x))  → (fn (fn (dv 1)))
;;; (fn (x) (fn (y) y))  → (fn (fn (dv 0)))
;;;
;;; Binder forms recognized:
;;;   (fn (var) body)     - single-argument function
;;;   (let ((var val)) body) - single binding let
;;;   (fix (f) body)      - recursive binding
;;;
;;; (dv n) represents a de Bruijn variable with index n.
;;;
;;; This is Core code: pure, total, assumes perfect input.
;;;
;;; Dependencies:
;;;   - prelude.ss

(load "core/base/prelude.ss")

;;; ====
;;; Environment (prefixed to avoid collision with eval.ss)
;;; ====

;;; An environment is a list of symbols, with the innermost binding first.
;;; Index 0 refers to (car env), index 1 to (cadr env), etc.

;;; norm-env-empty : Env
;;; The empty environment with no bindings.
(define norm-env-empty '())

;;; norm-env-extend : Env × Symbol → Env
;;; Extend environment with a new symbol binding.
(define (norm-env-extend env sym)
  (cons sym env))

;;; norm-env-lookup : Env × Symbol → (Option Nat)
;;; Returns the de Bruijn index if found, #f otherwise.
(define (norm-env-lookup env sym)
  (let loop ([e env] [i 0])
       (cond
        [(null? e) #f]
        [(eq? (car e) sym) i]
        [else (loop (cdr e) (+ i 1))])))

;;; ====
;;; Normalization
;;; ====

;;; normalize : S-expr → S-expr
;;; Convert an expression to de Bruijn form.
(define (normalize expr)
  (normalize-with-env expr norm-env-empty))

;;; normalize-with-env : S-expr × Env → S-expr
;;; Convert expression to de Bruijn form using given environment.
(define (normalize-with-env expr env)
  (cond
   ;; Symbols: look up in environment
   [(symbol? expr)
    (let ([idx (norm-env-lookup env expr)])
         (if idx
             `(dv ,idx)      ; Bound variable → de Bruijn index
             expr))]          ; Free variable → keep as symbol
   
   ;; Non-list atoms pass through
   [(not (pair? expr)) expr]
   
   ;; (fn (var) body) → (fn normalized-body)
   [(and (eq? (car expr) 'fn)
         (pair? (cdr expr))
         (pair? (cadr expr))
         (symbol? (caadr expr)))
    (let* ([var (caadr expr)]
           [body (caddr expr)]
           [new-env (norm-env-extend env var)])
          `(fn ,(normalize-with-env body new-env)))]
   
   ;; (let ((var val)) body) → (let (normalized-val) normalized-body)
   [(and (eq? (car expr) 'let)
         (pair? (cdr expr))
         (pair? (cadr expr))
         (pair? (caadr expr)))
    (let* ([binding (caadr expr)]
           [var (car binding)]
           [val (cadr binding)]
           [body (caddr expr)]
           [new-env (norm-env-extend env var)])
          `(let (,(normalize-with-env val env))
            ,(normalize-with-env body new-env)))]
   
   ;; (fix (f) body) → (fix normalized-body)
   ;; The recursive name is bound in the body
   [(and (eq? (car expr) 'fix)
         (pair? (cdr expr))
         (pair? (cadr expr))
         (symbol? (caadr expr)))
    (let* ([f (caadr expr)]
           [body (caddr expr)]
           [new-env (norm-env-extend env f)])
          `(fix ,(normalize-with-env body new-env)))]
   
   ;; (quote datum) → (quote datum) unchanged
   [(eq? (car expr) 'quote) expr]
   
   ;; General list: normalize each element
   [else
    (map (lambda (e) (normalize-with-env e env)) expr)]))

;;; ====
;;; Free Variables
;;; ====

;;; free-vars : S-expr → (List Symbol)
;;; Collect free variables in the expression (before normalization).
(define (free-vars expr)
  (free-vars-with-env expr norm-env-empty))

;;; free-vars-with-env : S-expr × Env → (List Symbol)
;;; Collect free variables using given environment.
(define (free-vars-with-env expr env)
  (cond
   [(symbol? expr)
    (if (norm-env-lookup env expr)
        '()
        (list expr))]
   [(not (pair? expr)) '()]
   [(eq? (car expr) 'quote) '()]
   [(eq? (car expr) 'fn)
    (let* ([var (caadr expr)]
           [body (caddr expr)])
          (free-vars-with-env body (norm-env-extend env var)))]
   [(eq? (car expr) 'let)
    (let* ([binding (caadr expr)]
           [var (car binding)]
           [val (cadr binding)]
           [body (caddr expr)])
          (append (free-vars-with-env val env)
                  (free-vars-with-env body (norm-env-extend env var))))]
   [(eq? (car expr) 'fix)
    (let* ([f (caadr expr)]
           [body (caddr expr)])
          (free-vars-with-env body (norm-env-extend env f)))]
   [else
    (apply append (map (lambda (e) (free-vars-with-env e env)) expr))]))

;;; Note: unique is provided by prelude.ss

;;; ====
;;; Algebraic Normalization (Phase 1 - before α-normalization)
;;; ====
;;;
;;; Algebraic normalization canonicalizes expressions by exploiting
;;; mathematical properties of operations:
;;;   - Commutative: (+ a b) = (+ b a) → sort arguments
;;;   - Associative: (+ (+ a b) c) = (+ a b c) → flatten
;;;   - Parallel bindings: reorder independent let* bindings
;;;   - Pure sequences: reorder independent pure expressions in begin
;;;
;;; CRITICAL: Must be applied BEFORE α-normalization (de Bruijn conversion).
;;; Reordering bindings after de Bruijn conversion corrupts indices.

(load "core/blocks/op-properties.ss")
(load "core/blocks/canonical-order.ss")

;;; normalize-algebraic : S-expr → S-expr
;;; Algebraic canonicalization of an expression.
;;; Call this BEFORE normalize (α-normalization).
(define (normalize-algebraic expr)
  (cond
    ;; Atoms pass through unchanged
    [(not (pair? expr)) expr]

    ;; Quoted data: don't touch
    [(eq? (car expr) 'quote) expr]

    ;; Commutative + associative: flatten then sort
    [(and (op-commutative? (car expr))
          (op-associative? (car expr)))
     (let* ([op (car expr)]
            [args (cdr expr)]
            [flat (flatten-associative op args)]
            [norm-args (map normalize-algebraic flat)]
            [sorted (canonical-sort norm-args)])
       (cons op sorted))]

    ;; Commutative only: sort arguments (no flattening)
    [(op-commutative? (car expr))
     (let* ([norm-args (map normalize-algebraic (cdr expr))]
            [sorted (canonical-sort norm-args)])
       (cons (car expr) sorted))]

    ;; Associative only: flatten (preserve order)
    [(op-associative? (car expr))
     (let* ([flat (flatten-associative (car expr) (cdr expr))]
            [norm-args (map normalize-algebraic flat)])
       (cons (car expr) norm-args))]

    ;; Parallel let*: sort independent bindings
    [(eq? (car expr) 'let*)
     (normalize-parallel-let expr)]

    ;; Begin: sort if all expressions are pure
    [(eq? (car expr) 'begin)
     (normalize-begin expr)]

    ;; Function: recurse into body
    [(eq? (car expr) 'fn)
     `(fn ,(cadr expr) ,(normalize-algebraic (caddr expr)))]

    ;; Single let: recurse
    [(eq? (car expr) 'let)
     (let* ([binding (caadr expr)]
            [var (car binding)]
            [val (cadr binding)]
            [body (caddr expr)])
       `(let ((,var ,(normalize-algebraic val)))
          ,(normalize-algebraic body)))]

    ;; Fix: recurse into body
    [(eq? (car expr) 'fix)
     `(fix ,(cadr expr) ,(normalize-algebraic (caddr expr)))]

    ;; General list: recurse into each element
    [else
     (map normalize-algebraic expr)]))

;;; flatten-associative : Symbol × (List S-expr) → (List S-expr)
;;; Flatten nested applications of an associative operator.
;;; (+ (+ a b) c (+ d e)) → (a b c d e)
(define (flatten-associative op args)
  (apply append
         (map (lambda (arg)
                (if (and (pair? arg) (eq? (car arg) op))
                    (flatten-associative op (cdr arg))
                    (list arg)))
              args)))

;;; ====
;;; Parallel Binding Canonicalization
;;; ====

;;; normalize-parallel-let : S-expr → S-expr
;;; Reorder let* bindings that don't depend on each other.
;;; Bindings are sorted topologically (dependencies first),
;;; with a stable tiebreaker for independent bindings.
(define (normalize-parallel-let expr)
  (let* ([bindings (cadr expr)]
         [body (caddr expr)]
         [sorted (sort-bindings-by-deps bindings)]
         [norm-bindings (map (lambda (b)
                               (list (car b) (normalize-algebraic (cadr b))))
                             sorted)]
         [norm-body (normalize-algebraic body)])
    `(let* ,norm-bindings ,norm-body)))

;;; sort-bindings-by-deps : (List Binding) → (List Binding)
;;; Topologically sort bindings, respecting dependencies.
;;; Bindings without dependencies between them are sorted alphabetically.
(define (sort-bindings-by-deps bindings)
  (let* ([bound-vars (map car bindings)]
         [deps-map (map (lambda (b)
                          (let* ([var (car b)]
                                 [val (cadr b)]
                                 [used (free-vars val)]
                                 [deps (filter (lambda (v) (memq v bound-vars)) used)])
                            (cons var deps)))
                        bindings)])
    (topo-sort-stable bindings deps-map)))

;;; topo-sort-stable : (List Binding) × (List (Var . Deps)) → (List Binding)
;;; Topological sort with stable tiebreaker (alphabetical by var name).
(define (topo-sort-stable bindings deps-map)
  (define (lookup-deps var)
    (let ([entry (assq var deps-map)])
      (if entry (cdr entry) '())))

  (define (remove-var var deps-map)
    (map (lambda (entry)
           (cons (car entry)
                 (filter (lambda (d) (not (eq? d var))) (cdr entry))))
         (filter (lambda (entry) (not (eq? (car entry) var))) deps-map)))

  (define (find-ready deps-map)
    ;; Find bindings with no remaining dependencies
    (filter (lambda (entry) (null? (cdr entry))) deps-map))

  (let loop ([remaining bindings]
             [deps deps-map]
             [result '()])
    (if (null? remaining)
        (reverse result)
        (let* ([ready (find-ready deps)]
               ;; Sort ready bindings alphabetically for stability
               [ready-sorted (list-sort (lambda (a b)
                                          (symbol<? (car a) (car b)))
                                        ready)])
          (if (null? ready-sorted)
              ;; Cycle detected or no ready bindings - just return remaining
              ;; (This shouldn't happen with valid let* bindings)
              (append (reverse result) remaining)
              (let* ([next-var (caar ready-sorted)]
                     [next-binding (assq next-var remaining)]
                     [new-remaining (filter (lambda (b) (not (eq? (car b) next-var)))
                                            remaining)]
                     [new-deps (remove-var next-var deps)])
                (loop new-remaining new-deps (cons next-binding result))))))))

;;; ====
;;; Sequence Canonicalization
;;; ====

;;; normalize-begin : S-expr → S-expr
;;; Sort begin expressions if all subexpressions are provably pure.
;;; If any expression might have side effects, preserve original order.
(define (normalize-begin expr)
  (let* ([exprs (cdr expr)]
         [norm-exprs (map normalize-algebraic exprs)])
    (if (andmap expr-pure? norm-exprs)
        (cons 'begin (canonical-sort norm-exprs))
        (cons 'begin norm-exprs))))

;;; expr-pure? : S-expr → Bool
;;; Conservative purity check. Returns #t only for expressions
;;; that are DEFINITELY pure. Unknown expressions default to impure.
(define (expr-pure? expr)
  (cond
    ;; Literals are always pure
    [(or (number? expr) (boolean? expr) (string? expr) (char? expr)) #t]

    ;; Quoted data is pure
    [(and (pair? expr) (eq? (car expr) 'quote)) #t]

    ;; Lambda creation is pure (application might not be)
    [(and (pair? expr) (eq? (car expr) 'fn)) #t]

    ;; De Bruijn variable reference is pure
    [(and (pair? expr) (eq? (car expr) 'dv)) #t]

    ;; Known pure primitives (whitelist approach)
    [(and (pair? expr)
          (symbol? (car expr))
          (op-pure? (car expr)))
     (andmap expr-pure? (cdr expr))]

    ;; Unknown function calls: DEFAULT TO IMPURE
    ;; This is critical - (my-logger "msg") must not be reordered
    [(pair? expr) #f]

    ;; Symbols (free variables): conservative - might be effectful thunk
    [(symbol? expr) #f]

    [else #f]))

;;; ====
;;; Combined Normalization
;;; ====

;;; normalize-full : S-expr → S-expr
;;; Full normalization: algebraic canonicalization followed by α-normalization.
;;; Use this for version 1 content-addressed hashing.
(define (normalize-full expr)
  (normalize (normalize-algebraic expr)))

;;; ====
;;; Version 2 Normalization Pipeline
;;; ====
;;;
;;; Version 2 adds:
;;;   - Hash-consing for structural deduplication
;;;   - Identity/absorbing element elimination
;;;   - η-reduction for function canonicalization
;;;   - (Future: polynomial canonicalization)
;;;
;;; Order of operations (CRITICAL - order matters!):
;;;   1. η-reduction (while named variables exist)
;;;   2. Identity/absorbing elimination
;;;   3. Algebraic canonicalization (commutative sorting, etc.)
;;;   4. α-normalization (de Bruijn indices)
;;;   5. Hash-consing (structural deduplication)

(load "core/blocks/hash-cons.ss")
(load "core/blocks/poly-canon.ss")

;;; ====
;;; η-Reduction
;;; ====

;;; eta-reduce : S-expr → S-expr
;;; Transform (fn (x) (f x)) → f when x does not occur free in f.
;;; This canonicalizes point-free style wrappers.
(define (eta-reduce expr)
  (cond
    ;; Atoms pass through
    [(not (pair? expr)) expr]

    ;; Quoted data unchanged
    [(eq? (car expr) 'quote) expr]

    ;; (fn (x) (f x)) where x not free in f → f
    ;; NOTE: Only applies to exact two-element applications (f x).
    ;; Multi-argument forms like (+ 0 y) are NOT η-reducible in Scheme
    ;; because (+ 0 y) is not the same as ((+ 0) y).
    [(and (eq? (car expr) 'fn)
          (pair? (cdr expr))
          (pair? (cadr expr))
          (symbol? (caadr expr))
          (pair? (cddr expr))
          (let ([var (caadr expr)]
                [body (caddr expr)])
            (and (pair? body)              ; body is an application
                 (= (length body) 2)        ; exactly (f x), not (f x y ...)
                 (not (eq? (car body) 'fn)) ; not a nested lambda
                 (not (eq? (car body) 'let))
                 (not (eq? (car body) 'fix))
                 (not (eq? (car body) 'quote))
                 (eq? (cadr body) var)      ; argument is the bound var
                 (not (memq var (free-vars (car body))))))) ; var not free in f
     (eta-reduce (car (caddr expr)))]       ; return f

    ;; Recurse into fn body
    [(eq? (car expr) 'fn)
     `(fn ,(cadr expr) ,(eta-reduce (caddr expr)))]

    ;; Recurse into let
    [(eq? (car expr) 'let)
     (let* ([binding (caadr expr)]
            [var (car binding)]
            [val (cadr binding)]
            [body (caddr expr)])
       `(let ((,var ,(eta-reduce val)))
          ,(eta-reduce body)))]

    ;; Recurse into fix
    [(eq? (car expr) 'fix)
     `(fix ,(cadr expr) ,(eta-reduce (caddr expr)))]

    ;; General list: recurse
    [else (map eta-reduce expr)]))

;;; ====
;;; Identity/Absorbing Element Elimination
;;; ====

;;; eliminate-identities : S-expr → S-expr
;;; Remove identity elements from operations and simplify absorbing cases.
;;; Recursively processes the entire expression tree.
(define (eliminate-identities expr)
  (cond
    ;; Atoms pass through
    [(not (pair? expr)) expr]

    ;; Quoted data unchanged
    [(eq? (car expr) 'quote) expr]

    ;; Recurse into fn body
    [(eq? (car expr) 'fn)
     `(fn ,(cadr expr) ,(eliminate-identities (caddr expr)))]

    ;; Recurse into let
    [(eq? (car expr) 'let)
     (let* ([binding (caadr expr)]
            [var (car binding)]
            [val (cadr binding)]
            [body (caddr expr)])
       `(let ((,var ,(eliminate-identities val)))
          ,(eliminate-identities body)))]

    ;; Recurse into fix
    [(eq? (car expr) 'fix)
     `(fix ,(cadr expr) ,(eliminate-identities (caddr expr)))]

    ;; Operation with known identity/absorbing elements
    [(symbol? (car expr))
     (let* ([op (car expr)]
            [args (map eliminate-identities (cdr expr))]
            [identity (op-identity op)]
            [absorbing (op-absorbing op)])
       (cond
         ;; Check for absorbing element first
         [(and absorbing (any (lambda (a) (equal? a absorbing)) args))
          absorbing]

         ;; Remove identity elements
         [identity
          (let ([filtered (filter (lambda (a) (not (equal? a identity))) args)])
            (cond
              ;; All were identity → return identity
              [(null? filtered) identity]
              ;; Single element: only unwrap for truly unary-collapsible ops
              ;; NOTE: (- x) is unary negation, NOT (- x 0) with identity removed.
              ;; Only collapse to single element for ops where (op x) = x.
              [(and (null? (cdr filtered))
                    (not (eq? op '-)))  ; - has right identity only, not unary
               (car filtered)]
              ;; Multiple or non-collapsible single → reconstruct
              [else (cons op filtered)]))]

         ;; No identity/absorbing → reconstruct with recursed args
         [else (cons op args)]))]

    ;; General list: recurse
    [else (map eliminate-identities expr)]))

;;; any : (a → Bool) × (List a) → Bool
;;; Returns #t if predicate is true for any element.
(define (any pred lst)
  (cond
    [(null? lst) #f]
    [(pred (car lst)) #t]
    [else (any pred (cdr lst))]))

;;; ====
;;; Version 2 Algebraic Normalization
;;; ====

;;; normalize-algebraic-v2 : S-expr → S-expr
;;; Enhanced algebraic canonicalization with:
;;;   - η-reduction
;;;   - Identity/absorbing element elimination
;;;   - Polynomial canonicalization for arithmetic subtrees
;;;   - Standard algebraic canonicalization (commutative sorting, etc.)
(define (normalize-algebraic-v2 expr)
  (let* ([eta-reduced (eta-reduce expr)]
         [poly-canonicalized (poly-canonicalize-recursive eta-reduced)]
         [algebraically-sorted (normalize-algebraic poly-canonicalized)]
         [identities-eliminated (eliminate-identities algebraically-sorted)])
    identities-eliminated))

;;; poly-canonicalize-recursive : S-expr → S-expr
;;; Apply polynomial canonicalization recursively to all arithmetic subtrees.
(define (poly-canonicalize-recursive expr)
  (cond
    ;; Atoms pass through
    [(not (pair? expr)) expr]

    ;; Quoted data unchanged
    [(eq? (car expr) 'quote) expr]

    ;; Try polynomial canonicalization first
    [(arithmetic-expr? expr)
     (try-poly-canonicalize expr)]

    ;; Recurse into fn body
    [(eq? (car expr) 'fn)
     `(fn ,(cadr expr) ,(poly-canonicalize-recursive (caddr expr)))]

    ;; Recurse into let
    [(eq? (car expr) 'let)
     (let* ([binding (caadr expr)]
            [var (car binding)]
            [val (cadr binding)]
            [body (caddr expr)])
       `(let ((,var ,(poly-canonicalize-recursive val)))
          ,(poly-canonicalize-recursive body)))]

    ;; Recurse into fix
    [(eq? (car expr) 'fix)
     `(fix ,(cadr expr) ,(poly-canonicalize-recursive (caddr expr)))]

    ;; General list: recurse
    [else (map poly-canonicalize-recursive expr)]))

;;; ====
;;; Version 2 Combined Normalization
;;; ====

;;; normalize-v2 : S-expr → S-expr
;;; Full version 2 normalization pipeline:
;;;   η-reduction → poly-canon → algebraic → identity elimination → α → hash-cons
;;; Identity elimination is applied LAST in the algebraic phase because
;;; flattening may expose new identity opportunities (e.g., (+ (+ a 0) b)).
;;; Use this for version 2 content-addressed hashing.
(define (normalize-v2 expr)
  (hash-cons (normalize (normalize-algebraic-v2 expr))))

;;; normalize-v2-no-hashcons : S-expr → S-expr
;;; Version 2 without hash-consing (for testing/comparison).
(define (normalize-v2-no-hashcons expr)
  (normalize (normalize-algebraic-v2 expr)))

;;; ====
;;; Version 3 Normalization Pipeline
;;; ====
;;;
;;; Version 3 adds NbE (Normalization by Evaluation) to the pipeline.
;;; NbE provides intrinsic reductions that were previously missing or
;;; required external rewrite rules:
;;;
;;;   - β-reduction: ((fn (x) body) arg) → body[arg/x]
;;;   - η-equivalence: structurally (via readback)
;;;   - Pair projections: (fst (pair a b)) → a, (snd (pair a b)) → b
;;;   - Sum projections: (case (Left a) ...) → left-branch[a]
;;;   - Conditionals: (if #t a b) → a, (if #f a b) → b
;;;
;;; The v3 pipeline is:
;;;   NbE → V2 algebraic → α-normalization → Hash-consing
;;;
;;; NbE is applied FIRST because it performs semantic reductions that
;;; may expose opportunities for algebraic canonicalization.
;;;
;;; Order matters:
;;;   1. NbE: Semantic evaluation and readback (β, projections, case)
;;;   2. V2 algebraic: η-reduction, poly-canon, sorting, identity elim
;;;   3. α-norm: De Bruijn indices
;;;   4. Hash-cons: Structural deduplication
;;;
;;; Properties:
;;;   - Backward compatible: v3 identifies MORE equivalences than v2
;;;   - Monotonic: if v2-hash(a) = v2-hash(b), then v3-hash(a) = v3-hash(b)
;;;   - Idempotent: normalize-v3(normalize-v3(x)) = normalize-v3(x)
;;;   - Deterministic: same input → same output

(load "core/blocks/nbe-normalize.ss")

;;; normalize-v3 : S-expr → S-expr
;;; Full version 3 normalization pipeline:
;;;   NbE → η-reduction → poly-canon → algebraic → identity elim → α → hash-cons
;;;
;;; This is the most aggressive normalization mode, identifying the
;;; maximum number of semantic equivalences.
(define (normalize-v3 expr)
  (hash-cons
   (normalize
    (normalize-algebraic-v2
     (nbe-normalize-for-cas expr)))))

;;; normalize-v3-no-hashcons : S-expr → S-expr
;;; Version 3 without hash-consing (for testing/comparison).
(define (normalize-v3-no-hashcons expr)
  (normalize
   (normalize-algebraic-v2
    (nbe-normalize-for-cas expr))))

;;; normalize-v3-no-nbe : S-expr → S-expr
;;; Version 3 without NbE (for testing/comparison).
;;; Equivalent to normalize-v2.
(define (normalize-v3-no-nbe expr)
  (normalize-v2 expr))

;;; ====
;;; V3 Normalization Diagnostics
;;; ====

;;; normalize-v3-phases : S-expr → Alist
;;; Run v3 normalization and return intermediate results at each phase.
;;; Useful for debugging and understanding normalization behavior.
(define (normalize-v3-phases expr)
  (let* ([nbe-result (nbe-normalize-for-cas expr)]
         [alg-result (normalize-algebraic-v2 nbe-result)]
         [alpha-result (normalize alg-result)]
         [final-result (hash-cons alpha-result)])
    `((input . ,expr)
      (after-nbe . ,nbe-result)
      (after-algebraic . ,alg-result)
      (after-alpha . ,alpha-result)
      (final . ,final-result))))

;;; v3-equivalence-report : S-expr × S-expr → Alist
;;; Compare two expressions and report their normalization at each version.
;;; Useful for understanding when and why expressions become equivalent.
(define (v3-equivalence-report e1 e2)
  (let* ([e1-v1 (normalize-full e1)]
         [e2-v1 (normalize-full e2)]
         [e1-v2 (normalize-v2 e1)]
         [e2-v2 (normalize-v2 e2)]
         [e1-v3 (normalize-v3 e1)]
         [e2-v3 (normalize-v3 e2)])
    `((v1-equivalent . ,(equal? e1-v1 e2-v1))
      (v2-equivalent . ,(equal? e1-v2 e2-v2))
      (v3-equivalent . ,(equal? e1-v3 e2-v3))
      (e1-v1 . ,e1-v1)
      (e2-v1 . ,e2-v1)
      (e1-v2 . ,e1-v2)
      (e2-v2 . ,e2-v2)
      (e1-v3 . ,e1-v3)
      (e2-v3 . ,e2-v3))))
