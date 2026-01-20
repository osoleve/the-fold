(load "core/base/prelude.ss")

(doc 'module 'normalize)
(doc 'description "S-expression α-normalization via de Bruijn indices. Ensures α-equivalent expressions produce identical forms.")
(doc 'layer 'core)

(doc 'section 'environment)

(doc norm-env-empty 'type Env)
(doc norm-env-empty 'description "The empty environment with no bindings.")
(doc norm-env-empty 'export #t)
(define norm-env-empty '())

(define (norm-env-extend env sym)
  (doc 'type (-> Env Symbol Env))
  (doc 'description "Extend environment with a new symbol binding.")
  (doc 'export #t)
  (cons sym env))

(define (norm-env-lookup env sym)
  (doc 'type (-> Env Symbol (Maybe Nat)))
  (doc 'description "Returns the de Bruijn index if found, #f otherwise.")
  (doc 'export #t)
  (let loop ([e env] [i 0])
       (cond
        [(null? e) #f]
        [(eq? (car e) sym) i]
        [else (loop (cdr e) (+ i 1))])))

(doc 'section 'normalization)

(define (normalize expr)
  (doc 'type (-> Any Any))
  (doc 'description "Convert an expression to de Bruijn form.")
  (doc 'export #t)
  (normalize-with-env expr norm-env-empty))

(define (normalize-with-env expr env)
  (doc 'type (-> Any Env Any))
  (doc 'description "Convert expression to de Bruijn form using given environment.")
  (doc 'export #t)
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

   ;; (doc ...) → stripped in sequence contexts, kept otherwise
   ;; When kept, normalize to itself (data, not code)
   [(eq? (car expr) 'doc) expr]

   ;; (begin exprs...) → strip doc forms, handle empty
   [(eq? (car expr) 'begin)
    (let* ([body (cdr expr)]
           [normalized (map (lambda (e) (normalize-with-env e env)) body)]
           ;; Filter out doc forms (they're metadata, not semantics)
           [filtered (filter (lambda (x) (not (and (pair? x) (eq? (car x) 'doc)))) normalized)])
      (cond
       [(null? filtered) '(void)]           ; empty body → void literal
       [(= 1 (length filtered)) (car filtered)]  ; single expr → unwrap
       [else (cons 'begin filtered)]))]

   ;; General list: normalize each element
   [else
    (map (lambda (e) (normalize-with-env e env)) expr)]))

(doc 'section 'free-variables)

(define (free-vars expr)
  (doc 'type (-> Any (List Symbol)))
  (doc 'description "Collect free variables in the expression (before normalization).")
  (doc 'export #t)
  (free-vars-with-env expr norm-env-empty))

(define (free-vars-with-env expr env)
  (doc 'type (-> Any Env (List Symbol)))
  (doc 'description "Collect free variables using given environment.")
  (doc 'export #t)
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

(doc 'section 'algebraic-normalization)

(load "core/blocks/op-properties.ss")
(load "core/blocks/canonical-order.ss")

(define (normalize-algebraic expr)
  (doc 'type (-> Any Any))
  (doc 'description "Algebraic canonicalization of expression. Call BEFORE normalize (α-norm).")
  (doc 'export #t)
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

(define (flatten-associative op args)
  (doc 'type (-> Symbol (List Any) (List Any)))
  (doc 'description "Flatten nested applications of an associative operator.")
  (doc 'export #t)
  (apply append
         (map (lambda (arg)
                (if (and (pair? arg) (eq? (car arg) op))
                    (flatten-associative op (cdr arg))
                    (list arg)))
              args)))

(doc 'section 'parallel-binding-canonicalization)

(define (normalize-parallel-let expr)
  (doc 'type (-> Any Any))
  (doc 'description "Reorder let* bindings topologically with stable tiebreaker.")
  (doc 'export #t)
  (let* ([bindings (cadr expr)]
         [body (caddr expr)]
         [sorted (sort-bindings-by-deps bindings)]
         [norm-bindings (map (lambda (b)
                               (list (car b) (normalize-algebraic (cadr b))))
                             sorted)]
         [norm-body (normalize-algebraic body)])
    `(let* ,norm-bindings ,norm-body)))

(define (sort-bindings-by-deps bindings)
  (doc 'type (-> (List Binding) (List Binding)))
  (doc 'description "Topologically sort bindings with alphabetical tiebreaker.")
  (doc 'export #t)
  (let* ([bound-vars (map car bindings)]
         [deps-map (map (lambda (b)
                          (let* ([var (car b)]
                                 [val (cadr b)]
                                 [used (free-vars val)]
                                 [deps (filter (lambda (v) (memq v bound-vars)) used)])
                            (cons var deps)))
                        bindings)])
    (topo-sort-stable bindings deps-map)))

(doc topo-sort-stable 'type (-> (List Binding) (List (Var . Deps)) (List Binding)))
(doc topo-sort-stable 'description "Topological sort with alphabetical tiebreaker.")
(doc topo-sort-stable 'export #t)
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

(doc 'section 'sequence-canonicalization)

(define (normalize-begin expr)
  (doc 'type (-> Any Any))
  (doc 'description "Sort begin expressions if all subexpressions are provably pure.")
  (doc 'export #t)
  (let* ([exprs (cdr expr)]
         [norm-exprs (map normalize-algebraic exprs)])
    (if (andmap expr-pure? norm-exprs)
        (cons 'begin (canonical-sort norm-exprs))
        (cons 'begin norm-exprs))))

(define (expr-pure? expr)
  (doc 'type (-> Any Boolean))
  (doc 'description "Conservative purity check. Unknown expressions default to impure.")
  (doc 'export #t)
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

(doc 'section 'combined-normalization)

(define (normalize-full expr)
  (doc 'type (-> Any Any))
  (doc 'description "Full normalization: algebraic canonicalization then α-normalization. For v1 CAS hashing.")
  (doc 'export #t)
  (normalize (normalize-algebraic expr)))

(doc 'section 'version-2-normalization)

(load "core/blocks/hash-cons.ss")
(load "core/blocks/poly-canon.ss")

(doc 'section 'eta-reduction)

(define (eta-reduce expr)
  (doc 'type (-> Any Any))
  (doc 'description "Transform (fn (x) (f x)) → f when x not free in f. Canonicalizes point-free wrappers.")
  (doc 'export #t)
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

(doc 'section 'identity-elimination)

(define (eliminate-identities expr)
  (doc 'type (-> Any Any))
  (doc 'description "Remove identity elements from operations and simplify absorbing cases.")
  (doc 'export #t)
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

(define (any pred lst)
  (doc 'type (-> (-> Any Boolean) (List Any) Boolean))
  (doc 'description "Returns #t if predicate is true for any element.")
  (doc 'export #t)
  (cond
    [(null? lst) #f]
    [(pred (car lst)) #t]
    [else (any pred (cdr lst))]))

(doc 'section 'version-2-algebraic)

(define (normalize-algebraic-v2 expr)
  (doc 'type (-> Any Any))
  (doc 'description "Enhanced algebraic canonicalization with η-reduction, identity elim, poly-canon, and sorting.")
  (doc 'export #t)
  (let* ([eta-reduced (eta-reduce expr)]
         [poly-canonicalized (poly-canonicalize-recursive eta-reduced)]
         [algebraically-sorted (normalize-algebraic poly-canonicalized)]
         [identities-eliminated (eliminate-identities algebraically-sorted)])
    identities-eliminated))

(define (poly-canonicalize-recursive expr)
  (doc 'type (-> Any Any))
  (doc 'description "Apply polynomial canonicalization recursively to all arithmetic subtrees.")
  (doc 'export #t)
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

(doc 'section 'version-2-combined)

(define (normalize-v2 expr)
  (doc 'type (-> Any Any))
  (doc 'description "Full v2 normalization: η → poly-canon → algebraic → identity elim → α → hash-cons. For v2 CAS hashing.")
  (doc 'export #t)
  (hash-cons (normalize (normalize-algebraic-v2 expr))))

(define (normalize-v2-no-hashcons expr)
  (doc 'type (-> Any Any))
  (doc 'description "Version 2 without hash-consing (for testing/comparison).")
  (doc 'export #t)
  (normalize (normalize-algebraic-v2 expr)))

(doc 'section 'version-3-normalization)

(load "core/blocks/nbe-normalize.ss")

(define (normalize-v3 expr)
  (doc 'type (-> Any Any))
  (doc 'description "Full v3 normalization: NbE → η → poly-canon → algebraic → identity → α → hash-cons. Most aggressive mode.")
  (doc 'export #t)
  (hash-cons
   (normalize
    (normalize-algebraic-v2
     (nbe-normalize-for-cas expr)))))

(define (normalize-v3-no-hashcons expr)
  (doc 'type (-> Any Any))
  (doc 'description "Version 3 without hash-consing (for testing/comparison).")
  (doc 'export #t)
  (normalize
   (normalize-algebraic-v2
    (nbe-normalize-for-cas expr))))

(define (normalize-v3-no-nbe expr)
  (doc 'type (-> Any Any))
  (doc 'description "Version 3 without NbE (equivalent to normalize-v2).")
  (doc 'export #t)
  (normalize-v2 expr))

(doc 'section 'v3-diagnostics)

(define (normalize-v3-phases expr)
  (doc 'type (-> Any Alist))
  (doc 'description "Run v3 normalization and return intermediate results at each phase.")
  (doc 'export #t)
  (let* ([nbe-result (nbe-normalize-for-cas expr)]
         [alg-result (normalize-algebraic-v2 nbe-result)]
         [alpha-result (normalize alg-result)]
         [final-result (hash-cons alpha-result)])
    `((input . ,expr)
      (after-nbe . ,nbe-result)
      (after-algebraic . ,alg-result)
      (after-alpha . ,alpha-result)
      (final . ,final-result))))

(define (v3-equivalence-report e1 e2)
  (doc 'type (-> Any Any Alist))
  (doc 'description "Compare two expressions and report normalization at each version.")
  (doc 'export #t)
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
