;;; lattice/fp/analysis/cost-analysis.ss — Cost Analysis
;;; @module cost-analysis
;;; @requires prelude sort

(require 'prelude)
(require 'sort)

(doc 'module 'cost-analysis)
(doc 'description "Cost Estimation for Parallelization Heuristics

Provides cost estimation for determining when parallelization is beneficial.
Auto-parallelization only makes sense when the computation cost exceeds
the overhead of spawning/coordinating parallel tasks.")
(doc 'layer 'lattice)
(doc 'purity 'partial)
(doc 'dependencies '(core/base/prelude.ss core/util/cost-model.ss))
(doc 'exports '(estimate-work parallel-beneficial? min-parallel-threshold
                set-parallel-threshold! work-analysis operation-cost
                expr-complexity-class))

(doc 'section 'configuration-parameters)

(define *parallel-overhead* 100)
(doc *parallel-overhead* 'type 'Nat)
(doc *parallel-overhead* 'description "Estimated cost of spawning a parallel task and synchronizing.
This includes: thread creation, context switch, result collection.
Units are abstract work units matching estimate-work output.")

(define *min-parallel-work* 200)
(doc *min-parallel-work* 'type 'Nat)
(doc *min-parallel-work* 'description "Minimum work threshold for parallelization to be worthwhile.
Below this, sequential execution is always preferred.")

(define (min-parallel-threshold)
  (doc 'type '(-> Nat))
  (doc 'description "Return the current minimum parallel work threshold.")
  *min-parallel-work*)

(define (set-parallel-threshold! n)
  (doc 'type '(-> Nat Void))
  (doc 'description "Set the minimum parallel work threshold.")
  (set! *min-parallel-work* n))

(define (get-parallel-overhead)
  (doc 'type '(-> Nat))
  (doc 'description "Return the current parallel overhead estimate.")
  *parallel-overhead*)

(define (set-parallel-overhead! n)
  (doc 'type '(-> Nat Void))
  (doc 'description "Set the parallel overhead estimate.")
  (set! *parallel-overhead* n))

(doc 'section 'operation-cost-tables)
(doc 'note "Costs are in abstract work units:
  1 = trivial (variable lookup)
  2-5 = cheap (basic arithmetic, cons)
  10-20 = moderate (function call overhead)
  50+ = expensive (complex operations)")

(define *primitive-costs*
  '(;; Trivial - pure data access
    (car . 1)
    (cdr . 1)
    (null? . 1)
    (pair? . 1)
    (eq? . 1)
    (symbol? . 1)
    (number? . 1)
    (boolean? . 1)
    
    ;; Cheap - basic arithmetic, allocation
    (+ . 2)
    (- . 2)
    (* . 3)
    (= . 2)
    (< . 2)
    (> . 2)
    (<= . 2)
    (>= . 2)
    (not . 1)
    (cons . 3)
    (list . 5)
    
    ;; Moderate - division, comparison
    (/ . 5)
    (quotient . 5)
    (remainder . 5)
    (modulo . 5)
    (abs . 2)
    (min . 3)
    (max . 3)
    (equal? . 10)
    
    ;; Expensive - transcendental, allocation-heavy
    (sqrt . 15)
    (expt . 20)
    (log . 20)
    (exp . 20)
    (sin . 20)
    (cos . 20)
    (tan . 20)
    (asin . 25)
    (acos . 25)
    (atan . 25)
    
    ;; List operations - variable cost
    (length . 10)          ; O(n) but counted as base
    (append . 20)          ; O(n) allocation
    (reverse . 15)         ; O(n) allocation
    (member . 15)          ; O(n) search
    (memq . 10)            ; O(n) but faster comparison
    (assoc . 15)           ; O(n) search
    (assq . 10)            ; O(n) but faster comparison
    
    ;; String operations
    (string-length . 1)
    (string-ref . 2)
    (string-append . 20)
    (string=? . 10)
    (substring . 10)
    
    ;; Vector operations
    (vector-ref . 2)
    (vector-set! . 3)
    (vector-length . 1)
    (make-vector . 10)))
(doc *primitive-costs* 'type 'Alist)
(doc *primitive-costs* 'description "Base costs for primitive operations.")

(define *hof-costs*
  '((map . 20)
    (filter . 20)
    (fold-left . 15)
    (fold-right . 15)
    (foldl . 15)
    (foldr . 15)
    (for-each . 15)
    (andmap . 15)
    (ormap . 15)
    (apply . 10)
    (stream-map . 25)
    (stream-filter . 25)))
(doc *hof-costs* 'type 'Alist)
(doc *hof-costs* 'description "Costs for higher-order functions (base overhead, not including body).")

(define (operation-cost op)
  (doc 'type '(-> Symbol Nat))
  (doc 'description "Look up the cost of a primitive or HOF.
Returns 10 for unknown operations (conservative estimate).")
  (let ([prim-entry (assq op *primitive-costs*)]
        [hof-entry (assq op *hof-costs*)])
       (cond
        [prim-entry (cdr prim-entry)]
        [hof-entry (cdr hof-entry)]
        [else 10])))

(doc 'section 'expression-form-costs)

(define *form-costs*
  '((lambda . 5)     ; Closure creation
    (fn . 5)         ; Alternative lambda
    (let . 3)        ; Binding setup
    (let* . 4)       ; Sequential binding
    (letrec . 6)     ; Recursive binding
    (if . 2)         ; Branch
    (cond . 3)       ; Multi-branch
    (case . 4)       ; Dispatch
    (and . 1)        ; Short-circuit
    (or . 1)         ; Short-circuit
    (begin . 1)      ; Sequencing
    (quote . 1)      ; Literal
    (quasiquote . 5) ; Template
    (set! . 3)       ; Mutation
    (define . 5)))   ; Definition
(doc *form-costs* 'type 'Alist)
(doc *form-costs* 'description "Base costs for different expression forms.")

(define (form-cost form)
  (doc 'type '(-> Symbol Nat))
  (doc 'description "Look up the base cost of an expression form.")
  (let ([entry (assq form *form-costs*)])
       (if entry (cdr entry) 1)))

(doc 'section 'work-estimation-engine)

(define *estimate-work-fuel* 1000)
(doc *estimate-work-fuel* 'type 'Nat)
(doc *estimate-work-fuel* 'description "Default recursion depth for work estimation.
Can be overridden by passing an optional fuel parameter to estimate-work.")

(define estimate-work
  (case-lambda
   [(expr) (estimate-work-with-fuel expr *estimate-work-fuel*)]
   [(expr fuel) (estimate-work-with-fuel expr fuel)]))
(doc estimate-work 'type '(-> Expr [Nat] Nat))
(doc estimate-work 'description "Estimate the computational cost of an expression.
Walks the expression tree and sums operation costs.
Uses fuel to bound recursion depth.
Optional fuel parameter allows handling larger ASTs (default: 1000).")

(define (estimate-work-with-fuel expr fuel)
  (doc 'type '(-> Expr Nat Nat))
  (doc 'description "Core work estimator with fuel for termination guarantee.")
  (if (<= fuel 0)
      0  ; Ran out of fuel, return conservative 0
      (cond
       ;; Atoms: trivial cost
       [(or (number? expr) (string? expr) (boolean? expr) (char? expr))
        1]
       
       ;; Symbol: variable lookup
       [(symbol? expr) 1]
       
       ;; Empty list: trivial
       [(null? expr) 1]
       
       ;; Pair (application or special form)
       [(pair? expr)
        (let ([head (car expr)]
              [args (if (pair? (cdr expr)) (cdr expr) '())])
             (cond
              ;; Quote: just the literal cost
              [(eq? head 'quote) 1]
              
              ;; Lambda/fn: closure creation + body estimation
              [(memq head '(lambda fn))
               (+ (form-cost head)
                  (estimate-work-with-fuel (last expr) (- fuel 1)))]
              
              ;; Let forms: bindings + body
              [(memq head '(let let* letrec))
               (estimate-let-work expr fuel)]
              
              ;; Conditional: form cost + branches
              [(eq? head 'if)
               (+ (form-cost 'if)
                  (estimate-work-with-fuel (cadr expr) (- fuel 1))
                  (estimate-branch-work (cddr expr) (- fuel 1)))]
              
              ;; Cond: estimate each clause
              [(eq? head 'cond)
               (+ (form-cost 'cond)
                  (estimate-cond-work (cdr expr) (- fuel 1)))]
              
              ;; Begin: sum of expressions
              [(eq? head 'begin)
               (+ (form-cost 'begin)
                  (fold-left + 0
                             (map (lambda (e) (estimate-work-with-fuel e (- fuel 1)))
                                  args)))]
              
              ;; Higher-order functions: special handling for list size
              [(assq head *hof-costs*)
               (estimate-hof-work head args fuel)]
              
              ;; Known primitive: operation cost + argument costs
              [(assq head *primitive-costs*)
               (+ (operation-cost head)
                  (fold-left + 0
                             (map (lambda (e) (estimate-work-with-fuel e (- fuel 1)))
                                  args)))]
              
              ;; General application: function + args + call overhead
              [else
               (+ 10  ; Function call overhead
                  (estimate-work-with-fuel head (- fuel 1))
                  (fold-left + 0
                             (map (lambda (e) (estimate-work-with-fuel e (- fuel 1)))
                                  args)))]))]
       
       ;; Unknown: conservative estimate
       [else 1])))

(define (estimate-let-work expr fuel)
  (doc 'type '(-> Expr Nat Nat))
  (doc 'description "Estimate work for let/let*/letrec forms.")
  (let* ([form (car expr)]
         [bindings-and-body (cdr expr)]
         [bindings (if (and (pair? bindings-and-body)
                            (pair? (car bindings-and-body))
                            (pair? (caar bindings-and-body)))
                       (car bindings-and-body)
                       '())]
         [body (if (pair? bindings-and-body)
                   (cdr bindings-and-body)
                   '())]
         [binding-cost
          (fold-left + 0
                     (map (lambda (b)
                                  (if (and (pair? b) (>= (length b) 2))
                                      (estimate-work-with-fuel (cadr b) (- fuel 1))
                                      0))
                          bindings))]
         [body-cost
          (fold-left + 0
                     (map (lambda (e) (estimate-work-with-fuel e (- fuel 1)))
                          body))])
        (+ (form-cost form) binding-cost body-cost)))

(define (estimate-branch-work branches fuel)
  (doc 'type '(-> (List Expr) Nat Nat))
  (doc 'description "Estimate work for if branches (conservative: sum both).
In practice, only one branch executes, but we estimate conservatively.")
  (if (null? branches)
      0
      (+ (estimate-work-with-fuel (car branches) fuel)
         (if (pair? (cdr branches))
             (estimate-work-with-fuel (cadr branches) fuel)
             0))))

(define (estimate-cond-work clauses fuel)
  (doc 'type '(-> (List Clause) Nat Nat))
  (doc 'description "Estimate work for cond clauses.")
  (if (null? clauses)
      0
      (let* ([clause (car clauses)]
             [test-cost (if (and (pair? clause) (not (eq? (car clause) 'else)))
                            (estimate-work-with-fuel (car clause) fuel)
                            0)]
             [body-cost (if (and (pair? clause) (pair? (cdr clause)))
                            (fold-left + 0
                                       (map (lambda (e) (estimate-work-with-fuel e fuel))
                                            (cdr clause)))
                            0)])
            (+ test-cost body-cost (estimate-cond-work (cdr clauses) fuel)))))

(define (estimate-hof-work hof args fuel)
  (doc 'type '(-> Symbol (List Expr) Nat Nat))
  (doc 'description "Estimate work for higher-order functions.
Considers: base cost + list-expr cost + (estimated iterations * body cost)
Handles both 2-arg HOFs (map, filter) and 3-arg HOFs (fold-left, fold-right).")
  (let* ([base-cost (operation-cost hof)]
         ;; Determine argument positions based on HOF type
         ;; Folds: (fold f z list) - 3 args
         ;; Map/filter: (map f list) - 2 args
         [is-fold (memq hof '(fold-left fold-right foldl foldr))]
         [fn-arg (if (pair? args) (car args) #f)]
         [list-arg (cond
                    ;; Folds have 3 args: (f z list)
                    [is-fold
                     (if (and (pair? args) (pair? (cdr args)) (pair? (cddr args)))
                         (caddr args)
                         #f)]
                    ;; Map/filter have 2 args: (f list)
                    [else
                     (if (and (pair? args) (pair? (cdr args)))
                         (cadr args)
                         #f)])]
         ;; For folds, also estimate the initial value cost
         [init-cost (if (and is-fold (pair? args) (pair? (cdr args)))
                        (estimate-work-with-fuel (cadr args) (- fuel 1))
                        0)]
         [fn-body-cost (if fn-arg
                           (estimate-work-with-fuel fn-arg (- fuel 1))
                           5)]
         ;; IMPORTANT: Also add the cost of producing the list
         ;; This handles cases like (map f (map g xs)) where inner map has cost
         [list-expr-cost (if list-arg
                             (estimate-work-with-fuel list-arg (- fuel 1))
                             0)]
         [list-size-estimate (estimate-list-size list-arg)]
         ;; HOF cost = base + init + list-production + (iterations * body cost)
         [iteration-cost (* list-size-estimate fn-body-cost)])
        (+ base-cost init-cost list-expr-cost iteration-cost)))

(define (estimate-list-size expr)
  (doc 'type '(-> Expr Nat))
  (doc 'description "Heuristically estimate the size of a list expression.
Returns conservative estimates for unknown cases.")
  (cond
   ;; Null: empty
   [(null? expr) 0]
   
   ;; Quote of list: count elements
   [(and (pair? expr) (eq? (car expr) 'quote) (pair? (cdr expr)))
    (let ([quoted (cadr expr)])
         (if (list? quoted)
             (length quoted)
             10))]  ; Conservative for non-list
   
   ;; Literal list construction
   [(and (pair? expr) (eq? (car expr) 'list))
    (length (cdr expr))]
   
   ;; iota: the argument is the size
   [(and (pair? expr) (eq? (car expr) 'iota)
         (pair? (cdr expr)) (number? (cadr expr)))
    (cadr expr)]
   
   ;; range: estimate from bounds
   [(and (pair? expr) (eq? (car expr) 'range)
         (pair? (cdr expr)) (number? (cadr expr))
         (pair? (cddr expr)) (number? (caddr expr)))
    (max 0 (- (caddr expr) (cadr expr)))]
   
   ;; map/filter: preserve source size estimate
   [(and (pair? expr) (memq (car expr) '(map filter)))
    (if (and (pair? (cdr expr)) (pair? (cddr expr)))
        (estimate-list-size (caddr expr))
        10)]
   
   ;; Unknown: conservative estimate
   [else 10]))

(doc 'section 'parallelization-decision)

(define (parallel-beneficial? e1 e2)
  (doc 'type '(-> Expr Expr Bool))
  (doc 'description "Determine if parallelizing e1 and e2 is worthwhile.
Returns #t if combined work exceeds overhead threshold.")
  (let* ([w1 (estimate-work e1)]
         [w2 (estimate-work e2)]
         [combined (+ w1 w2)]
         [overhead *parallel-overhead*]
         [threshold *min-parallel-work*])
        ;; Parallelization beneficial if:
        ;; 1. Combined work exceeds minimum threshold
        ;; 2. Both expressions have substantial work (avoid one-sided parallelism)
        ;; 3. Overhead is justified by work savings
        (and (> combined threshold)
             (> w1 (quotient overhead 2))
             (> w2 (quotient overhead 2)))))

(define (parallel-beneficial-n? exprs)
  (doc 'type '(-> (List Expr) Bool))
  (doc 'description "Determine if parallelizing a list of expressions is worthwhile.")
  (let* ([works (map estimate-work exprs)]
         [total-work (fold-left + 0 works)]
         [n (length exprs)]
         [overhead (* n *parallel-overhead*)]
         [min-per-task (quotient *min-parallel-work* 2)])
        (and (> total-work *min-parallel-work*)
             (>= n 2)
             (andmap (lambda (w) (> w min-per-task)) works))))

(define (suggested-parallelism exprs)
  (doc 'type '(-> (List Expr) Nat))
  (doc 'description "Suggest optimal number of parallel tasks for a workload.
Returns 1 for sequential, or N for N-way parallelism.")
  (let* ([works (map estimate-work exprs)]
         [total-work (fold-left + 0 works)]
         [n (length exprs)])
        (cond
         ;; Too little work: sequential
         [(< total-work *min-parallel-work*) 1]
         ;; Single expression: no parallelism
         [(<= n 1) 1]
         ;; Otherwise, suggest based on work distribution
         [else
          (let* ([avg-work (quotient total-work n)]
                 [viable-tasks
                  (length (filter (lambda (w) (> w (quotient *parallel-overhead* 2)))
                                  works))])
                (max 1 viable-tasks))])))

(doc 'section 'work-analysis-report)
(doc 'note "WorkAnalysis structure:
  (work-analysis
    (total-cost . Nat)
    (complexity-class . Symbol)
    (hotspots . (List Hotspot))
    (parallelizable? . Bool)
    (suggested-splits . Nat))")

(define (work-analysis expr)
  (doc 'type '(-> Expr WorkAnalysis))
  (doc 'description "Generate a detailed work analysis report.")
  (let* ([total (estimate-work expr)]
         [complexity (expr-complexity-class expr)]
         [hotspots (find-hotspots expr 3)]
         [parallelizable (> total *min-parallel-work*)])
        `(work-analysis
          (total-cost . ,total)
          (complexity-class . ,complexity)
          (hotspots . ,hotspots)
          (parallelizable? . ,parallelizable)
          (suggested-splits . ,(if parallelizable
                                   (min 4 (quotient total *min-parallel-work*))
                                   1)))))

(define (work-analysis? x)
  (doc 'type '(-> Any Boolean))
  (and (pair? x) (eq? (car x) 'work-analysis)))

(define (analysis-total-cost wa)
  (doc 'type '(-> WorkAnalysis Nat))
  (cdr (assq 'total-cost (cdr wa))))

(define (analysis-complexity-class wa)
  (doc 'type '(-> WorkAnalysis Symbol))
  (cdr (assq 'complexity-class (cdr wa))))

(define (analysis-hotspots wa)
  (doc 'type '(-> WorkAnalysis (List Hotspot)))
  (cdr (assq 'hotspots (cdr wa))))

(define (analysis-parallelizable? wa)
  (doc 'type '(-> WorkAnalysis Bool))
  (cdr (assq 'parallelizable? (cdr wa))))

(define (analysis-suggested-splits wa)
  (doc 'type '(-> WorkAnalysis Nat))
  (cdr (assq 'suggested-splits (cdr wa))))

(doc 'section 'complexity-classification)

(define (expr-complexity-class expr)
  (doc 'type '(-> Expr Symbol))
  (doc 'description "Classify an expression's complexity class based on structure.
Returns: constant, linear, quadratic, recursive, unknown")
  (classify-complexity expr 100))

(define (classify-complexity expr fuel)
  (doc 'type '(-> Expr Nat Symbol))
  (if (<= fuel 0)
      'unknown
      (cond
       ;; Atoms: constant
       [(or (number? expr) (string? expr) (boolean? expr) (symbol? expr))
        'constant]
       
       ;; Empty list: constant
       [(null? expr) 'constant]
       
       ;; Pair: analyze structure
       [(pair? expr)
        (let ([head (car expr)])
             (cond
              ;; Recursive constructs
              [(memq head '(fix letrec))
               'recursive]
              
              ;; HOFs: at least linear
              [(memq head '(map filter fold-left fold-right foldl foldr for-each))
               (let ([body-class (if (and (pair? (cdr expr)) (pair? (cadr expr)))
                                     (classify-complexity (cadr expr) (- fuel 1))
                                     'constant)])
                    (combine-complexity 'linear body-class))]
              
              ;; Nested HOFs suggest higher complexity
              [(and (pair? (cdr expr))
                    (pair? (cddr expr))
                    (pair? (caddr expr))
                    (memq (car (caddr expr)) '(map filter fold-left fold-right)))
               'quadratic]
              
              ;; Lambda: analyze body
              [(memq head '(lambda fn))
               (classify-complexity (last expr) (- fuel 1))]
              
              ;; Let: max of bindings and body
              [(memq head '(let let* letrec))
               (let* ([parts (cdr expr)]
                      [bindings (if (and (pair? parts) (pair? (car parts)))
                                    (car parts)
                                    '())]
                      [body (if (pair? parts) (cdr parts) '())]
                      [binding-class
                       (fold-left combine-complexity 'constant
                                  (map (lambda (b)
                                               (if (and (pair? b) (>= (length b) 2))
                                                   (classify-complexity (cadr b) (- fuel 1))
                                                   'constant))
                                       bindings))]
                      [body-class
                       (fold-left combine-complexity 'constant
                                  (map (lambda (e) (classify-complexity e (- fuel 1)))
                                       body))])
                     (combine-complexity binding-class body-class))]
              
              ;; Default application: constant overhead
              [else
               (fold-left combine-complexity 'constant
                          (map (lambda (e) (classify-complexity e (- fuel 1)))
                               (cdr expr)))]))]
       
       [else 'unknown])))

(define (combine-complexity c1 c2)
  (doc 'type '(-> Symbol Symbol Symbol))
  (doc 'description "Combine two complexity classes (takes the higher one).")
  (let ([order '((constant . 0) (linear . 1) (quadratic . 2)
                 (recursive . 3) (unknown . 4))])
       (let ([o1 (cdr (assq c1 order))]
             [o2 (cdr (assq c2 order))])
            (if (> o1 o2) c1 c2))))

(doc 'section 'hotspot-detection)
(doc 'note "Hotspot structure: (position cost expression)")

(define (find-hotspots expr n)
  (doc 'type '(-> Expr Nat (List Hotspot)))
  (doc 'description "Find the N most expensive subexpressions.")
  (let* ([all-costs (collect-costs expr '() 100)]
         [sorted (sort-by-cost all-costs)])
        (take n sorted)))

(define (collect-costs expr pos fuel)
  (doc 'type '(-> Expr Position Nat (List (Position Cost Expr))))
  (doc 'description "Collect costs of all subexpressions with their positions.")
  (if (<= fuel 0)
      '()
      (let ([cost (estimate-work expr)])
           (if (pair? expr)
               (let* ([this (list pos cost expr)]
                      [children
                       (let loop ([items (cdr expr)] [i 1] [acc '()])
                            (if (or (null? items) (not (pair? items)))
                                acc
                                (let ([child-costs
                                       (collect-costs (car items)
                                                      (append pos (list i))
                                                      (- fuel 1))])
                                     (loop (cdr items) (+ i 1)
                                           (append acc child-costs)))))])
                     (cons this children))
               (list (list pos cost expr))))))

(define (sort-by-cost entries)
  (doc 'type '(-> (List (Position Cost Expr)) (List (Position Cost Expr))))
  (doc 'description "Sort cost entries by cost, descending.")
  (let ([sorted (sort-by (lambda (a b) (> (cadr a) (cadr b))) entries)])
       sorted))

(doc 'section 'utility-functions)

(define (total-work exprs)
  (doc 'type '(-> (List Expr) Nat))
  (doc 'description "Sum the estimated work of multiple expressions.")
  (fold-left + 0 (map estimate-work exprs)))

(define (work-ratio e1 e2)
  (doc 'type '(-> Expr Expr Real))
  (doc 'description "Compute the work ratio between two expressions.
Returns w1/w2, useful for load balancing.")
  (let ([w1 (estimate-work e1)]
        [w2 (estimate-work e2)])
       (if (= w2 0)
           +inf.0
           (/ w1 w2))))

(define (balanced-split? exprs)
  (doc 'type '(-> (List Expr) Bool))
  (doc 'description "Check if a list of expressions has balanced work distribution.
Balanced = max/min ratio < 2.")
  (let* ([works (map estimate-work exprs)]
         [works-positive (filter (lambda (w) (> w 0)) works)])
        (if (< (length works-positive) 2)
            #t  ; Trivially balanced
            (let ([max-w (fold-left max 0 works-positive)]
                  [min-w (fold-left min +inf.0 works-positive)])
                 (< (/ max-w min-w) 2.0)))))

(doc 'section 'display-utilities)

(define (format-work-analysis wa)
  (doc 'type '(-> WorkAnalysis String))
  (doc 'description "Format a work analysis for display.")
  (format "Work Analysis:\n  Total cost: ~a\n  Complexity: ~a\n  Parallelizable: ~a\n  Suggested splits: ~a"
          (analysis-total-cost wa)
          (analysis-complexity-class wa)
          (analysis-parallelizable? wa)
          (analysis-suggested-splits wa)))

(define (display-work-analysis wa)
  (doc 'type '(-> WorkAnalysis Void))
  (doc 'description "Display a work analysis to current output port.")
  (display (format-work-analysis wa))
  (newline)
  (display "  Hotspots:\n")
  (for-each
   (lambda (hotspot)
           (display (format "    ~a: cost ~a\n"
                            (car hotspot)
                            (cadr hotspot))))
   (analysis-hotspots wa)))
