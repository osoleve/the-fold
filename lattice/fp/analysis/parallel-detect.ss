(load "core/base/prelude.ss")

(doc 'module 'parallel-detect)
(doc 'description "Parallelization Opportunity Detection

Analyzes S-expressions to detect parallelization opportunities:
  - Independent let bindings: (let ([a (f x)] [b (g y)]) ...) where a, b don't depend on each other
  - Independent map operations: Multiple maps on different data
  - Par-eligible expressions: Expressions that could use (par ...) form
  - Fanout patterns: Single input feeding multiple independent computations")
(doc 'layer 'lattice)
(doc 'purity 'total)
(doc 'dependencies '(core/base/prelude.ss))
(doc 'exports '(detect-parallel-regions parallel-opportunity? par-type
                par-branches par-location par-estimated-speedup
                par-dependencies parallel-summary))

(doc 'section 'parallel-opportunity-data-structure)
(doc 'note "A ParallelOpportunity captures a detected parallelization pattern:
  (parallel-opportunity
    (type . Symbol)              ; Pattern: independent-let, independent-map, par-eligible, fanout
    (branches . (List Expr))     ; Independent branches that can run in parallel
    (location . Position)        ; Path in expression tree (list of indices)
    (estimated-speedup . Rational) ; Speedup factor (e.g., 2.0 for 2 branches)
    (dependencies . Graph))      ; Dependency info showing why branches are independent")

(define (make-parallel-opportunity type branches location estimated-speedup dependencies)
  (doc 'type '(-> Symbol (List Expr) Position Rational Graph ParallelOpportunity))
  (doc 'description "Create a new parallel opportunity record.")
  `(parallel-opportunity
    (type . ,type)
    (branches . ,branches)
    (location . ,location)
    (estimated-speedup . ,estimated-speedup)
    (dependencies . ,dependencies)))

(define (parallel-opportunity? x)
  (doc 'type '(-> Any Boolean))
  (doc 'description "Check if x is a ParallelOpportunity.")
  (and (pair? x)
       (eq? (car x) 'parallel-opportunity)
       (pair? (assq 'type (cdr x)))
       (pair? (assq 'branches (cdr x)))
       (pair? (assq 'location (cdr x)))
       #t))

(define (par-type opp)
  (doc 'type '(-> ParallelOpportunity Symbol))
  (cdr (assq 'type (cdr opp))))

(define (par-branches opp)
  (doc 'type '(-> ParallelOpportunity (List Expr)))
  (cdr (assq 'branches (cdr opp))))

(define (par-location opp)
  (doc 'type '(-> ParallelOpportunity Position))
  (cdr (assq 'location (cdr opp))))

(define (par-estimated-speedup opp)
  (doc 'type '(-> ParallelOpportunity Rational))
  (cdr (assq 'estimated-speedup (cdr opp))))

(define (par-dependencies opp)
  (doc 'type '(-> ParallelOpportunity DependencyGraph))
  (cdr (assq 'dependencies (cdr opp))))

(doc 'section 'dependency-graph)
(doc 'note "A DependencyGraph shows variable dependencies between expressions.
Structure: ((name . (List DependsOn)) ...)
Example: ((a) (b . (x)) (c . (a b)))  ; a has no deps, b depends on x, c depends on a and b")

(define (make-dep-graph entries)
  (doc 'type '(-> (List (Pair Symbol (List Symbol))) DependencyGraph))
  `(dep-graph . ,entries))

(define (dep-graph? x)
  (doc 'type '(-> Any Boolean))
  (and (pair? x) (eq? (car x) 'dep-graph)))

(define (dep-graph-entries g)
  (doc 'type '(-> DependencyGraph (List (Pair Symbol (List Symbol)))))
  (cdr g))

(define (dep-graph-lookup g sym)
  (doc 'type '(-> DependencyGraph Symbol (List Symbol)))
  (let ([entry (assq sym (dep-graph-entries g))])
       (if entry (cdr entry) '())))

(doc 'section 'free-variables-analysis)
(doc 'note "Binders recognized: fn, lambda, let, let*, letrec, fix")

(define (free-vars expr)
  (doc 'type '(-> Expr (List Symbol)))
  (doc 'description "Collect free variables in an expression.")
  (free-vars-env expr '()))

(define (free-vars-env expr bound)
  (doc 'type '(-> Expr (List Symbol) (List Symbol)))
  (doc 'description "Collect free variables with bound variable tracking.")
  (cond
   ;; Symbol: free if not in bound list
   [(symbol? expr)
    (if (memq expr bound) '() (list expr))]
   
   ;; Non-list atoms have no free vars
   [(not (pair? expr)) '()]
   
   ;; Quote: no free vars in quoted data
   [(eq? (car expr) 'quote) '()]
   
   ;; (fn (var ...) body) or (lambda (var ...) body)
   [(or (eq? (car expr) 'fn)
        (eq? (car expr) 'lambda))
    (if (and (pair? (cdr expr)) (pair? (cadr expr)))
        (let* ([params (cadr expr)]
               [body (cddr expr)]
               [new-bound (append (if (list? params) params (list params)) bound)])
              (append-map (lambda (e) (free-vars-env e new-bound)) body))
        '())]

   ;; (let ((var val) ...) body ...)
   [(eq? (car expr) 'let)
    (if (and (pair? (cdr expr)) (pair? (cadr expr)))
        (let* ([bindings (cadr expr)]
               [body (cddr expr)]
               [vars (map car bindings)]
               [vals (map cadr bindings)]
               ;; For regular let: vals are in outer scope, body in extended scope
               [val-fvs (append-map (lambda (v) (free-vars-env v bound)) vals)]
               [body-fvs (append-map (lambda (e) (free-vars-env e (append vars bound))) body)])
              (append val-fvs body-fvs))
        '())]
   
   ;; (let* ((var val) ...) body ...)
   [(eq? (car expr) 'let*)
    (if (and (pair? (cdr expr)) (pair? (cadr expr)))
        (let* ([bindings (cadr expr)]
               [body (cddr expr)])
              ;; let* bindings are sequential
              (let loop ([bs bindings] [cur-bound bound] [acc '()])
                   (if (null? bs)
                       (append acc
                               (append-map (lambda (e) (free-vars-env e cur-bound)) body))
                       (let* ([b (car bs)]
                              [var (car b)]
                              [val (cadr b)])
                             (loop (cdr bs)
                                   (cons var cur-bound)
                                   (append acc (free-vars-env val cur-bound)))))))
        '())]
   
   ;; (letrec ((var val) ...) body ...)
   [(eq? (car expr) 'letrec)
    (if (and (pair? (cdr expr)) (pair? (cadr expr)))
        (let* ([bindings (cadr expr)]
               [body (cddr expr)]
               [vars (map car bindings)]
               [vals (map cadr bindings)]
               ;; For letrec: all vars are in scope for both vals and body
               [new-bound (append vars bound)]
               [val-fvs (append-map (lambda (v) (free-vars-env v new-bound)) vals)]
               [body-fvs (append-map (lambda (e) (free-vars-env e new-bound)) body)])
              (append val-fvs body-fvs))
        '())]
   
   ;; (fix (f) body)
   [(eq? (car expr) 'fix)
    (if (and (pair? (cdr expr)) (pair? (cadr expr)))
        (let* ([f (caadr expr)]
               [body (caddr expr)])
              (free-vars-env body (cons f bound)))
        '())]
   
   ;; General list: union of free vars in all subexpressions
   [else
    (append-map (lambda (e) (free-vars-env e bound)) expr)]))

(define (unique-symbols lst)
  (doc 'type '(-> (List Symbol) (List Symbol)))
  (doc 'description "Remove duplicate symbols, preserving order.")
  (let loop ([items lst] [seen '()] [acc '()])
       (cond
        [(null? items) (reverse acc)]
        [(memq (car items) seen) (loop (cdr items) seen acc)]
        [else (loop (cdr items) (cons (car items) seen) (cons (car items) acc))])))

(doc 'section 'independence-analysis)
(doc 'note "Two expressions are independent if:
1. Neither's free vars include the other's defined vars
2. Neither writes to shared mutable state (assumed pure in Core)")

(define (sets-disjoint? xs ys)
  (doc 'type '(-> (List Symbol) (List Symbol) Boolean))
  (doc 'description "Check if two symbol lists have no common elements.")
  (not (ormap (lambda (x) (memq x ys)) xs)))

(define (bindings-independent? bindings)
  (doc 'type '(-> (List (Pair Symbol Expr)) (List (List Nat))))
  (doc 'description "Given let bindings ((var val) ...), return groups of independent binding indices.
Each group can be evaluated in parallel.")
  (let* ([n (length bindings)]
         [vars (map car bindings)]
         [vals (map cadr bindings)]
         [fvs-list (map free-vars vals)]
         ;; Build dependency matrix: deps[i] = indices of bindings that binding i depends on
         [deps (map (lambda (fvs)
                            (let loop ([i 0] [acc '()])
                                 (if (>= i n)
                                     acc
                                     (if (memq (list-ref vars i) fvs)
                                         (loop (+ i 1) (cons i acc))
                                         (loop (+ i 1) acc)))))
                    fvs-list)])
        ;; Find independent sets using a simple greedy approach
        (find-independent-groups n deps)))

(define (find-independent-groups n deps)
  (doc 'type '(-> Nat (List (List Nat)) (List (List Nat))))
  (doc 'description "Find groups of mutually independent indices.")
  ;; deps[i] = list of indices that i depends on
  ;; Two indices i,j are mutually independent if:
  ;;   - i not in deps[j] AND j not in deps[i]
  (let outer ([remaining (iota n)] [groups '()])
       (if (null? remaining)
           (filter (lambda (g) (> (length g) 1)) (reverse groups))
           (let ([i (car remaining)])
                (let inner ([rest (cdr remaining)] [group (list i)] [leftover '()])
                     (if (null? rest)
                         (outer (reverse leftover) (cons (reverse group) groups))
                         (let ([j (car rest)])
                              (let ([i-deps (list-ref deps i)]
                                    [j-deps (list-ref deps j)])
                                   ;; Check if j is independent of all current group members
                                   (if (and (not (memv i j-deps))
                                            (andmap (lambda (g-idx)
                                                            (and (not (memv g-idx (list-ref deps j)))
                                                                 (not (memv j (list-ref deps g-idx)))))
                                                    group))
                                       (inner (cdr rest) (cons j group) leftover)
                                       (inner (cdr rest) group (cons j leftover)))))))))))

(doc 'section 'pattern-detectors)

(define (detect-independent-let expr pos)
  (doc 'type '(-> Expr Position (List ParallelOpportunity)))
  (doc 'description "Detect let forms with independent bindings that can run in parallel.")
  (if (and (pair? expr)
           (eq? (car expr) 'let)
           (pair? (cdr expr))
           (pair? (cadr expr))
           (> (length (cadr expr)) 1))  ; Need at least 2 bindings
      (let* ([bindings (cadr expr)]
             [groups (bindings-independent? bindings)])
            (if (pair? groups)
                ;; Found independent groups
                (map (lambda (group)
                             (let* ([branch-exprs (map (lambda (i) (list-ref bindings i)) group)]
                                    [vars (map car bindings)]
                                    [fvs-list (map (lambda (b) (free-vars (cadr b))) branch-exprs)]
                                    [dep-entries (map (lambda (b fvs)
                                                              (cons (car b)
                                                                    (filter (lambda (v) (memq v vars)) fvs)))
                                                      branch-exprs fvs-list)]
                                    [speedup (estimate-speedup (length group) branch-exprs)])
                                   (make-parallel-opportunity
                                    'independent-let
                                    (map cadr branch-exprs)  ; Just the value expressions
                                    pos
                                    speedup
                                    (make-dep-graph dep-entries))))
                     groups)
                '()))
      '()))

(define (detect-independent-maps expr pos)
  (doc 'type '(-> Expr Position (List ParallelOpportunity)))
  (doc 'description "Detect multiple map calls on different data that could run in parallel.")
  (if (and (pair? expr)
           (pair? (cdr expr)))  ; At least 2 elements
      (let* ([elements (if (memq (car expr) '(begin list vector))
                           (cdr expr)
                           '())]
             [maps (filter-indexed (lambda (e i) (and (pair? e) (eq? (car e) 'map))) elements)]
             [map-exprs (map car maps)]
             [map-indices (map cdr maps)])
            (if (> (length maps) 1)
                ;; Check which maps are independent
                (let* ([map-fvs (map (lambda (m) (free-vars (caddr m))) map-exprs)]
                       [independent-pairs (find-independent-pairs map-fvs)])
                      (if (pair? independent-pairs)
                          (list (make-parallel-opportunity
                                 'independent-map
                                 map-exprs
                                 pos
                                 (estimate-speedup (length map-exprs) map-exprs)
                                 (make-dep-graph (map (lambda (m fvs i)
                                                              (cons i (unique-symbols fvs)))
                                                      map-exprs map-fvs map-indices))))
                          '()))
                '()))
      '()))

(define (filter-indexed pred lst)
  (doc 'type '(-> (-> α Nat Boolean) (List α) (List (Pair α Nat))))
  (doc 'description "Filter keeping track of original indices.")
  (let loop ([items lst] [i 0] [acc '()])
       (if (null? items)
           (reverse acc)
           (if (pred (car items) i)
               (loop (cdr items) (+ i 1) (cons (cons (car items) i) acc))
               (loop (cdr items) (+ i 1) acc)))))

(define (find-independent-pairs fvs-list)
  (doc 'type '(-> (List (List Symbol)) (List (Pair Nat Nat))))
  (doc 'description "Find pairs of indices whose free variable sets are disjoint.")
  (let ([n (length fvs-list)])
       (let outer ([i 0] [acc '()])
            (if (>= i n)
                acc
                (let inner ([j (+ i 1)] [pairs acc])
                     (if (>= j n)
                         (outer (+ i 1) pairs)
                         (if (sets-disjoint? (list-ref fvs-list i) (list-ref fvs-list j))
                             (inner (+ j 1) (cons (cons i j) pairs))
                             (inner (+ j 1) pairs))))))))

(define (detect-par-eligible expr pos)
  (doc 'type '(-> Expr Position (List ParallelOpportunity)))
  (doc 'description "Detect expressions that could benefit from (par a b) wrapping.
Specifically: function calls with independent argument computations.")
  (if (and (pair? expr)
           (symbol? (car expr))
           (> (length (cdr expr)) 1))  ; Function with 2+ args
      (let* ([args (cdr expr)]
             [arg-fvs (map free-vars args)]
             [independent-pairs (find-independent-pairs arg-fvs)])
            (if (pair? independent-pairs)
                (list (make-parallel-opportunity
                       'par-eligible
                       args
                       pos
                       (estimate-speedup (length args) args)
                       (make-dep-graph (map (lambda (arg fvs i)
                                                    (cons i (unique-symbols fvs)))
                                            args arg-fvs (iota (length args))))))
                '()))
      '()))

(define (detect-fanout expr pos)
  (doc 'type '(-> Expr Position (List ParallelOpportunity)))
  (doc 'description "Detect patterns where one input feeds multiple independent computations.
Pattern: (let ([x input]) (list (f x) (g x) (h x)))")
  (if (and (pair? expr)
           (eq? (car expr) 'let)
           (pair? (cdr expr))
           (pair? (cadr expr))
           (= (length (cadr expr)) 1)  ; Single binding
           (pair? (cddr expr)))
      (let* ([binding (caadr expr)]
             [var (car binding)]
             [body (caddr expr)])
            ;; Check if body is a list/vector of independent uses of var
            (if (and (pair? body)
                     (memq (car body) '(list vector values)))
                (let* ([uses (cdr body)]
                       [uses-with-var (filter (lambda (u)
                                                      (and (pair? u)
                                                           (memq var (free-vars u))))
                                              uses)])
                      (if (> (length uses-with-var) 1)
                          ;; Check that uses are otherwise independent
                          (let* ([fvs-without-var (map (lambda (u)
                                                               (filter (lambda (v) (not (eq? v var)))
                                                                       (free-vars u)))
                                                       uses-with-var)]
                                 [pairwise-independent (find-independent-pairs fvs-without-var)])
                                (if (pair? pairwise-independent)
                                    (list (make-parallel-opportunity
                                           'fanout
                                           uses-with-var
                                           pos
                                           (estimate-speedup (length uses-with-var) uses-with-var)
                                           (make-dep-graph `((source . ,var)
                                                             (branches . ,(length uses-with-var))))))
                                    '()))
                          '()))
                '()))
      '()))

(doc 'section 'speedup-estimation)

(define (estimate-speedup branch-count exprs)
  (doc 'type '(-> Nat (List Expr) Rational))
  (doc 'description "Estimate potential speedup based on branch count and work estimates.
Returns a factor (e.g., 2.0 means ~2x faster).")
  (let* ([base-speedup (min branch-count 8)]  ; Cap at 8 (diminishing returns)
         [overhead 0.9]  ; 10% overhead for parallelization
         [work-factors (map estimate-work exprs)]
         [total-work (apply + work-factors)]
         [max-work (if (null? work-factors) 1 (apply max work-factors))]
         ;; Amdahl's law approximation
         [parallel-fraction (/ (- total-work max-work) total-work)])
        (* overhead
           (/ 1 (+ (- 1 parallel-fraction)
                   (/ parallel-fraction base-speedup))))))

(define (estimate-work expr)
  (doc 'type '(-> Expr Nat))
  (doc 'description "Rough estimate of computational work in an expression.
Simple heuristic: count operations.")
  (cond
   [(not (pair? expr)) 1]
   [(eq? (car expr) 'quote) 1]
   [(memq (car expr) '(+ - * /)) (+ 1 (apply + (map estimate-work (cdr expr))))]
   [(memq (car expr) '(map filter fold foldl foldr))
    (* 10 (apply + (map estimate-work (cdr expr))))]  ; HOFs are expensive
   [else (apply + (map estimate-work expr))]))

(doc 'section 'core-detection-logic)

(define (try-all-detectors expr pos)
  (doc 'type '(-> Expr Position (List ParallelOpportunity)))
  (doc 'description "Run all parallel pattern detectors on an expression.")
  (append
   (detect-independent-let expr pos)
   (detect-independent-maps expr pos)
   (detect-par-eligible expr pos)
   (detect-fanout expr pos)))

(define (detect-at-depth expr pos fuel)
  (doc 'type '(-> Expr Position Nat (List ParallelOpportunity)))
  (doc 'description "Recursively detect parallelization opportunities with depth limit.")
  (if (<= fuel 0)
      '()
      (let* ([here (try-all-detectors expr pos)]
             [children
              (if (pair? expr)
                  (let loop ([items expr] [i 0] [acc '()])
                       (if (or (null? items) (not (pair? items)))
                           acc
                           (let ([child-opps (detect-at-depth
                                              (car items)
                                              (append pos (list i))
                                              (- fuel 1))])
                                (loop (cdr items) (+ i 1) (append acc child-opps)))))
                  '())])
            (append here children))))

(define *detect-fuel* 100)
(doc *detect-fuel* 'type 'Nat)
(doc *detect-fuel* 'description "Default recursion depth for detection.
Can be overridden by passing an optional fuel parameter to detect-parallel-regions.")

(define detect-parallel-regions
  (case-lambda
   [(expr) (detect-at-depth expr '() *detect-fuel*)]
   [(expr fuel) (detect-at-depth expr '() fuel)]))
(doc detect-parallel-regions 'type '(-> Expr [Nat] (List ParallelOpportunity)))
(doc detect-parallel-regions 'description "Main entry point: detect all parallelization opportunities in an expression.
Optional fuel parameter allows handling larger ASTs (default: 100).")

(doc 'section 'convenience-functions)

(define (count-parallel-opportunities opps)
  (doc 'type '(-> (List ParallelOpportunity) Nat))
  (length opps))

(define (total-estimated-speedup opps)
  (doc 'type '(-> (List ParallelOpportunity) Rational))
  (doc 'description "Estimate combined speedup if all opportunities are exploited.
Note: This is optimistic - actual speedup depends on execution patterns.")
  (if (null? opps)
      1.0
      (apply max (map par-estimated-speedup opps))))

(define (opportunities-by-pattern opps)
  (doc 'type '(-> (List ParallelOpportunity) ((Symbol . (List ParallelOpportunity)) ...)))
  (doc 'description "Group opportunities by pattern type.")
  (let ([types '(independent-let independent-map par-eligible fanout)])
       (map (lambda (t)
                    (cons t (filter (lambda (o) (eq? (par-type o) t)) opps)))
            types)))

(define (high-value-opportunities opps)
  (doc 'type '(-> (List ParallelOpportunity) (List ParallelOpportunity)))
  (doc 'description "Filter to opportunities with speedup >= 1.5x.")
  (filter (lambda (o) (>= (par-estimated-speedup o) 1.5)) opps))

(doc 'section 'display-utilities)

(define (format-parallel-opportunity opp)
  (doc 'type '(-> ParallelOpportunity String))
  (format "[~a] at ~a\n  Branches: ~a\n  Estimated speedup: ~ax"
          (par-type opp)
          (par-location opp)
          (length (par-branches opp))
          (par-estimated-speedup opp)))

(define (display-parallel-opportunities opps)
  (doc 'type '(-> (List ParallelOpportunity) Void))
  (if (null? opps)
      (display "No parallelization opportunities detected.\n")
      (begin
       (display (format "Found ~a parallelization opportunities:\n\n" (length opps)))
       (for-each
        (lambda (opp)
                (display (format-parallel-opportunity opp))
                (display "\n\n"))
        opps))))

(doc 'section 'summary-report)

(define (parallel-summary opps)
  (doc 'type '(-> (List ParallelOpportunity) Summary))
  (doc 'description "Generate a summary report of parallelization opportunities.")
  (let* ([by-pattern (opportunities-by-pattern opps)]
         [high-value (high-value-opportunities opps)])
        `(parallel-summary
          (total . ,(length opps))
          (high-value . ,(length high-value))
          (max-speedup . ,(if (null? opps) 1.0 (apply max (map par-estimated-speedup opps))))
          (avg-speedup . ,(if (null? opps)
                              1.0
                              (/ (apply + (map par-estimated-speedup opps)) (length opps))))
          (by-pattern . ,(map (lambda (pair) (cons (car pair) (length (cdr pair)))) by-pattern)))))

(doc 'section 'transformation-suggestions)

(define (suggest-par-transform opp)
  (doc 'type '(-> ParallelOpportunity Expr))
  (doc 'description "Suggest a transformation using par forms.")
  (case (par-type opp)
        [(independent-let)
         ;; Suggest: wrap independent bindings in par
         (let ([branches (par-branches opp)])
              (if (= (length branches) 2)
                  `(par ,(car branches) ,(cadr branches))
                  `(comment "Consider splitting into nested par forms"
                    ,@branches)))]
        [(par-eligible)
         ;; Suggest: wrap arguments in par
         (let ([branches (par-branches opp)])
              `(comment "Arguments can be evaluated in parallel"
                (par ,@branches)))]
        [(fanout)
         ;; Suggest: use par for fanout branches
         `(comment "Fanout branches can run in parallel")]
        [else
         `(comment "See branches" ,@(par-branches opp))]))
