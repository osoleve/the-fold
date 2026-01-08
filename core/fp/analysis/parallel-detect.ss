;;; core/fp/analysis/parallel-detect.ss --- Parallelization Opportunity Detection
;;;
;;; Analyzes S-expressions to detect parallelization opportunities:
;;;   - Independent let bindings: (let ([a (f x)] [b (g y)]) ...) where a, b don't depend on each other
;;;   - Independent map operations: Multiple maps on different data
;;;   - Par-eligible expressions: Expressions that could use (par ...) form
;;;   - Fanout patterns: Single input feeding multiple independent computations
;;;
;;; This is Core code: pure, total, assumes perfect input.
;;;
;;; API:
;;;   detect-parallel-regions : Expr -> (List ParallelOpportunity)
;;;   parallel-opportunity? : Any -> Boolean
;;;   par-type : ParallelOpportunity -> Symbol
;;;   par-branches : ParallelOpportunity -> (List Expr)
;;;   par-location : ParallelOpportunity -> Position
;;;   par-estimated-speedup : ParallelOpportunity -> Rational
;;;   par-dependencies : ParallelOpportunity -> DependencyGraph
;;;   parallel-summary : (List ParallelOpportunity) -> Summary
;;;
;;; Dependencies:
;;;   - core/base/prelude.ss

(load "core/base/prelude.ss")

;;; ============================================================
;;; ParallelOpportunity Data Structure
;;; ============================================================

;;; A ParallelOpportunity captures a detected parallelization pattern:
;;;   (parallel-opportunity
;;;     (type . Symbol)              ; Pattern: independent-let, independent-map, par-eligible, fanout
;;;     (branches . (List Expr))     ; Independent branches that can run in parallel
;;;     (location . Position)        ; Path in expression tree (list of indices)
;;;     (estimated-speedup . Rational) ; Speedup factor (e.g., 2.0 for 2 branches)
;;;     (dependencies . Graph))      ; Dependency info showing why branches are independent

;;; make-parallel-opportunity : Symbol x (List Expr) x Position x Rational x Graph -> ParallelOpportunity
;;; Create a new parallel opportunity record.
(define (make-parallel-opportunity type branches location estimated-speedup dependencies)
  `(parallel-opportunity
    (type . ,type)
    (branches . ,branches)
    (location . ,location)
    (estimated-speedup . ,estimated-speedup)
    (dependencies . ,dependencies)))

;;; parallel-opportunity? : Any -> Boolean
;;; Check if x is a ParallelOpportunity.
(define (parallel-opportunity? x)
  (and (pair? x)
       (eq? (car x) 'parallel-opportunity)
       (pair? (assq 'type (cdr x)))
       (pair? (assq 'branches (cdr x)))
       (pair? (assq 'location (cdr x)))
       #t))

;;; Accessors
(define (par-type opp) (cdr (assq 'type (cdr opp))))
(define (par-branches opp) (cdr (assq 'branches (cdr opp))))
(define (par-location opp) (cdr (assq 'location (cdr opp))))
(define (par-estimated-speedup opp) (cdr (assq 'estimated-speedup (cdr opp))))
(define (par-dependencies opp) (cdr (assq 'dependencies (cdr opp))))

;;; ============================================================
;;; Dependency Graph
;;; ============================================================

;;; A DependencyGraph shows variable dependencies between expressions.
;;; Structure: ((name . (List DependsOn)) ...)
;;; Example: ((a) (b . (x)) (c . (a b)))  ; a has no deps, b depends on x, c depends on a and b

;;; make-dep-graph : (List (Pair Symbol (List Symbol))) -> DependencyGraph
(define (make-dep-graph entries)
  `(dep-graph . ,entries))

;;; dep-graph? : Any -> Boolean
(define (dep-graph? x)
  (and (pair? x) (eq? (car x) 'dep-graph)))

;;; dep-graph-entries : DependencyGraph -> (List (Pair Symbol (List Symbol)))
(define (dep-graph-entries g)
  (cdr g))

;;; dep-graph-lookup : DependencyGraph x Symbol -> (List Symbol)
(define (dep-graph-lookup g sym)
  (let ([entry (assq sym (dep-graph-entries g))])
       (if entry (cdr entry) '())))

;;; ============================================================
;;; Free Variables Analysis
;;; ============================================================

;;; Binders recognized: fn, lambda, let, let*, letrec, fix

;;; free-vars : Expr -> (List Symbol)
;;; Collect free variables in an expression.
(define (free-vars expr)
  (free-vars-env expr '()))

;;; free-vars-env : Expr x (List Symbol) -> (List Symbol)
;;; Collect free variables with bound variable tracking.
(define (free-vars-env expr bound)
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
              (apply append (map (lambda (e) (free-vars-env e new-bound)) body)))
        '())]
   
   ;; (let ((var val) ...) body ...)
   [(eq? (car expr) 'let)
    (if (and (pair? (cdr expr)) (pair? (cadr expr)))
        (let* ([bindings (cadr expr)]
               [body (cddr expr)]
               [vars (map car bindings)]
               [vals (map cadr bindings)]
               ;; For regular let: vals are in outer scope, body in extended scope
               [val-fvs (apply append (map (lambda (v) (free-vars-env v bound)) vals))]
               [body-fvs (apply append (map (lambda (e) (free-vars-env e (append vars bound))) body))])
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
                               (apply append (map (lambda (e) (free-vars-env e cur-bound)) body)))
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
               [val-fvs (apply append (map (lambda (v) (free-vars-env v new-bound)) vals))]
               [body-fvs (apply append (map (lambda (e) (free-vars-env e new-bound)) body))])
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
    (apply append (map (lambda (e) (free-vars-env e bound)) expr))]))

;;; unique-symbols : (List Symbol) -> (List Symbol)
;;; Remove duplicate symbols, preserving order.
(define (unique-symbols lst)
  (let loop ([items lst] [seen '()] [acc '()])
       (cond
        [(null? items) (reverse acc)]
        [(memq (car items) seen) (loop (cdr items) seen acc)]
        [else (loop (cdr items) (cons (car items) seen) (cons (car items) acc))])))

;;; ============================================================
;;; Independence Analysis
;;; ============================================================

;;; Two expressions are independent if:
;;; 1. Neither's free vars include the other's defined vars
;;; 2. Neither writes to shared mutable state (assumed pure in Core)

;;; sets-disjoint? : (List Symbol) x (List Symbol) -> Boolean
;;; Check if two symbol lists have no common elements.
(define (sets-disjoint? xs ys)
  (not (ormap (lambda (x) (memq x ys)) xs)))

;;; bindings-independent? : (List (Pair Symbol Expr)) -> (List (List Nat))
;;; Given let bindings ((var val) ...), return groups of independent binding indices.
;;; Each group can be evaluated in parallel.
(define (bindings-independent? bindings)
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

;;; find-independent-groups : Nat x (List (List Nat)) -> (List (List Nat))
;;; Find groups of mutually independent indices.
(define (find-independent-groups n deps)
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

;;; ============================================================
;;; Pattern Detectors
;;; ============================================================

;;; --- Independent Let Bindings ---

;;; detect-independent-let : Expr x Position -> (List ParallelOpportunity)
;;; Detect let forms with independent bindings that can run in parallel.
(define (detect-independent-let expr pos)
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

;;; --- Independent Map Operations ---

;;; detect-independent-maps : Expr x Position -> (List ParallelOpportunity)
;;; Detect multiple map calls on different data that could run in parallel.
(define (detect-independent-maps expr pos)
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

;;; filter-indexed : (α x Nat -> Boolean) x (List α) -> (List (Pair α Nat))
;;; Filter keeping track of original indices.
(define (filter-indexed pred lst)
  (let loop ([items lst] [i 0] [acc '()])
       (if (null? items)
           (reverse acc)
           (if (pred (car items) i)
               (loop (cdr items) (+ i 1) (cons (cons (car items) i) acc))
               (loop (cdr items) (+ i 1) acc)))))

;;; find-independent-pairs : (List (List Symbol)) -> (List (Pair Nat Nat))
;;; Find pairs of indices whose free variable sets are disjoint.
(define (find-independent-pairs fvs-list)
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

;;; --- Par-Eligible Expressions ---

;;; detect-par-eligible : Expr x Position -> (List ParallelOpportunity)
;;; Detect expressions that could benefit from (par a b) wrapping.
;;; Specifically: function calls with independent argument computations.
(define (detect-par-eligible expr pos)
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

;;; --- Fanout Patterns ---

;;; detect-fanout : Expr x Position -> (List ParallelOpportunity)
;;; Detect patterns where one input feeds multiple independent computations.
;;; Pattern: (let ([x input]) (list (f x) (g x) (h x)))
(define (detect-fanout expr pos)
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

;;; ============================================================
;;; Speedup Estimation
;;; ============================================================

;;; estimate-speedup : Nat x (List Expr) -> Rational
;;; Estimate potential speedup based on branch count and work estimates.
;;; Returns a factor (e.g., 2.0 means ~2x faster).
(define (estimate-speedup branch-count exprs)
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

;;; estimate-work : Expr -> Nat
;;; Rough estimate of computational work in an expression.
;;; Simple heuristic: count operations.
(define (estimate-work expr)
  (cond
   [(not (pair? expr)) 1]
   [(eq? (car expr) 'quote) 1]
   [(memq (car expr) '(+ - * /)) (+ 1 (apply + (map estimate-work (cdr expr))))]
   [(memq (car expr) '(map filter fold foldl foldr))
    (* 10 (apply + (map estimate-work (cdr expr))))]  ; HOFs are expensive
   [else (apply + (map estimate-work expr))]))

;;; ============================================================
;;; Core Detection Logic
;;; ============================================================

;;; try-all-detectors : Expr x Position -> (List ParallelOpportunity)
;;; Run all parallel pattern detectors on an expression.
(define (try-all-detectors expr pos)
  (append
   (detect-independent-let expr pos)
   (detect-independent-maps expr pos)
   (detect-par-eligible expr pos)
   (detect-fanout expr pos)))

;;; detect-at-depth : Expr x Position x Nat -> (List ParallelOpportunity)
;;; Recursively detect parallelization opportunities with depth limit.
(define (detect-at-depth expr pos fuel)
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

;;; *detect-fuel* : Nat
;;; Default recursion depth for detection.
;;; Can be overridden by passing an optional fuel parameter to detect-parallel-regions.
(define *detect-fuel* 100)

;;; detect-parallel-regions : Expr [x Nat] -> (List ParallelOpportunity)
;;; Main entry point: detect all parallelization opportunities in an expression.
;;; Optional fuel parameter allows handling larger ASTs (default: 100).
(define detect-parallel-regions
  (case-lambda
   [(expr) (detect-at-depth expr '() *detect-fuel*)]
   [(expr fuel) (detect-at-depth expr '() fuel)]))

;;; ============================================================
;;; Convenience Functions
;;; ============================================================

;;; count-parallel-opportunities : (List ParallelOpportunity) -> Nat
(define (count-parallel-opportunities opps)
  (length opps))

;;; total-estimated-speedup : (List ParallelOpportunity) -> Rational
;;; Estimate combined speedup if all opportunities are exploited.
;;; Note: This is optimistic - actual speedup depends on execution patterns.
(define (total-estimated-speedup opps)
  (if (null? opps)
      1.0
      (apply max (map par-estimated-speedup opps))))

;;; opportunities-by-pattern : (List ParallelOpportunity) -> ((Symbol . (List ParallelOpportunity)) ...)
;;; Group opportunities by pattern type.
(define (opportunities-by-pattern opps)
  (let ([types '(independent-let independent-map par-eligible fanout)])
       (map (lambda (t)
                    (cons t (filter (lambda (o) (eq? (par-type o) t)) opps)))
            types)))

;;; high-value-opportunities : (List ParallelOpportunity) -> (List ParallelOpportunity)
;;; Filter to opportunities with speedup >= 1.5x.
(define (high-value-opportunities opps)
  (filter (lambda (o) (>= (par-estimated-speedup o) 1.5)) opps))

;;; ============================================================
;;; Display Utilities
;;; ============================================================

;;; format-opportunity : ParallelOpportunity -> String
(define (format-parallel-opportunity opp)
  (format "[~a] at ~a\n  Branches: ~a\n  Estimated speedup: ~ax"
          (par-type opp)
          (par-location opp)
          (length (par-branches opp))
          (par-estimated-speedup opp)))

;;; display-parallel-opportunities : (List ParallelOpportunity) -> Void
(define (display-parallel-opportunities opps)
  (if (null? opps)
      (display "No parallelization opportunities detected.\n")
      (begin
       (display (format "Found ~a parallelization opportunities:\n\n" (length opps)))
       (for-each
        (lambda (opp)
                (display (format-parallel-opportunity opp))
                (display "\n\n"))
        opps))))

;;; ============================================================
;;; Summary Report
;;; ============================================================

;;; parallel-summary : (List ParallelOpportunity) -> Summary
;;; Generate a summary report of parallelization opportunities.
(define (parallel-summary opps)
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

;;; ============================================================
;;; Transformation Suggestions
;;; ============================================================

;;; suggest-par-transform : ParallelOpportunity -> Expr
;;; Suggest a transformation using par forms.
(define (suggest-par-transform opp)
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
