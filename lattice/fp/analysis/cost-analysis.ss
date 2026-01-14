;;; core/fp/analysis/cost-analysis.ss --- Cost Estimation for Parallelization Heuristics
;;;
;;; Provides cost estimation for determining when parallelization is beneficial.
;;; Auto-parallelization only makes sense when the computation cost exceeds
;;; the overhead of spawning/coordinating parallel tasks.
;;;
;;; This is Core code: pure, total, assumes perfect input.
;;;
;;; API:
;;;   estimate-work : Expr [x Nat] -> Nat
;;;   parallel-beneficial? : Expr x Expr -> Bool
;;;   min-parallel-threshold : -> Nat
;;;   set-parallel-threshold! : Nat -> Void
;;;   work-analysis : Expr -> WorkAnalysis
;;;   operation-cost : Symbol -> Nat
;;;   expr-complexity-class : Expr -> Symbol
;;;
;;; Dependencies:
;;;   - core/base/prelude.ss
;;;   - core/util/cost-model.ss (for reference patterns)

(load "core/base/prelude.ss")

;;; ====
;;; Configuration Parameters
;;; ====

;;; *parallel-overhead* : Nat
;;; Estimated cost of spawning a parallel task and synchronizing.
;;; This includes: thread creation, context switch, result collection.
;;; Units are abstract "work units" matching estimate-work output.
(define *parallel-overhead* 100)

;;; *min-parallel-work* : Nat
;;; Minimum work threshold for parallelization to be worthwhile.
;;; Below this, sequential execution is always preferred.
(define *min-parallel-work* 200)

;;; min-parallel-threshold : -> Nat
;;; Return the current minimum parallel work threshold.
(define (min-parallel-threshold)
  *min-parallel-work*)

;;; set-parallel-threshold! : Nat -> Void
;;; Set the minimum parallel work threshold.
(define (set-parallel-threshold! n)
  (set! *min-parallel-work* n))

;;; get-parallel-overhead : -> Nat
;;; Return the current parallel overhead estimate.
(define (get-parallel-overhead)
  *parallel-overhead*)

;;; set-parallel-overhead! : Nat -> Void
;;; Set the parallel overhead estimate.
(define (set-parallel-overhead! n)
  (set! *parallel-overhead* n))

;;; ====
;;; Operation Cost Tables
;;; ====

;;; Costs are in abstract "work units":
;;;   1 = trivial (variable lookup)
;;;   2-5 = cheap (basic arithmetic, cons)
;;;   10-20 = moderate (function call overhead)
;;;   50+ = expensive (complex operations)

;;; *primitive-costs* : Alist
;;; Base costs for primitive operations.
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

;;; *hof-costs* : Alist
;;; Costs for higher-order functions (base overhead, not including body).
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

;;; operation-cost : Symbol -> Nat
;;; Look up the cost of a primitive or HOF.
;;; Returns 10 for unknown operations (conservative estimate).
(define (operation-cost op)
  (let ([prim-entry (assq op *primitive-costs*)]
        [hof-entry (assq op *hof-costs*)])
       (cond
        [prim-entry (cdr prim-entry)]
        [hof-entry (cdr hof-entry)]
        [else 10])))

;;; ====
;;; Expression Form Costs
;;; ====

;;; *form-costs* : Alist
;;; Base costs for different expression forms.
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

;;; form-cost : Symbol -> Nat
;;; Look up the base cost of an expression form.
(define (form-cost form)
  (let ([entry (assq form *form-costs*)])
       (if entry (cdr entry) 1)))

;;; ====
;;; Work Estimation Engine
;;; ====

;;; *estimate-work-fuel* : Nat
;;; Default recursion depth for work estimation.
;;; Can be overridden by passing an optional fuel parameter to estimate-work.
(define *estimate-work-fuel* 1000)

;;; estimate-work : Expr [x Nat] -> Nat
;;; Estimate the computational cost of an expression.
;;; Walks the expression tree and sums operation costs.
;;; Uses fuel to bound recursion depth.
;;; Optional fuel parameter allows handling larger ASTs (default: 1000).
(define estimate-work
  (case-lambda
   [(expr) (estimate-work-with-fuel expr *estimate-work-fuel*)]
   [(expr fuel) (estimate-work-with-fuel expr fuel)]))

;;; estimate-work-with-fuel : Expr x Nat -> Nat
;;; Core work estimator with fuel for termination guarantee.
(define (estimate-work-with-fuel expr fuel)
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

;;; estimate-let-work : Expr x Nat -> Nat
;;; Estimate work for let/let*/letrec forms.
(define (estimate-let-work expr fuel)
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

;;; estimate-branch-work : (List Expr) x Nat -> Nat
;;; Estimate work for if branches (conservative: sum both).
;;; In practice, only one branch executes, but we estimate conservatively.
(define (estimate-branch-work branches fuel)
  (if (null? branches)
      0
      (+ (estimate-work-with-fuel (car branches) fuel)
         (if (pair? (cdr branches))
             (estimate-work-with-fuel (cadr branches) fuel)
             0))))

;;; estimate-cond-work : (List Clause) x Nat -> Nat
;;; Estimate work for cond clauses.
(define (estimate-cond-work clauses fuel)
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

;;; estimate-hof-work : Symbol x (List Expr) x Nat -> Nat
;;; Estimate work for higher-order functions.
;;; Considers: base cost + list-expr cost + (estimated iterations * body cost)
;;; Handles both 2-arg HOFs (map, filter) and 3-arg HOFs (fold-left, fold-right).
(define (estimate-hof-work hof args fuel)
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

;;; estimate-list-size : Expr -> Nat
;;; Heuristically estimate the size of a list expression.
;;; Returns conservative estimates for unknown cases.
(define (estimate-list-size expr)
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

;;; ====
;;; Parallelization Decision
;;; ====

;;; parallel-beneficial? : Expr x Expr -> Bool
;;; Determine if parallelizing e1 and e2 is worthwhile.
;;; Returns #t if combined work exceeds overhead threshold.
(define (parallel-beneficial? e1 e2)
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

;;; parallel-beneficial-n? : (List Expr) -> Bool
;;; Determine if parallelizing a list of expressions is worthwhile.
(define (parallel-beneficial-n? exprs)
  (let* ([works (map estimate-work exprs)]
         [total-work (fold-left + 0 works)]
         [n (length exprs)]
         [overhead (* n *parallel-overhead*)]
         [min-per-task (quotient *min-parallel-work* 2)])
        (and (> total-work *min-parallel-work*)
             (>= n 2)
             (andmap (lambda (w) (> w min-per-task)) works))))

;;; suggested-parallelism : (List Expr) -> Nat
;;; Suggest optimal number of parallel tasks for a workload.
;;; Returns 1 for sequential, or N for N-way parallelism.
(define (suggested-parallelism exprs)
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

;;; ====
;;; Work Analysis Report
;;; ====

;;; WorkAnalysis structure:
;;;   (work-analysis
;;;     (total-cost . Nat)
;;;     (complexity-class . Symbol)
;;;     (hotspots . (List Hotspot))
;;;     (parallelizable? . Bool)
;;;     (suggested-splits . Nat))

;;; work-analysis : Expr -> WorkAnalysis
;;; Generate a detailed work analysis report.
(define (work-analysis expr)
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

;;; work-analysis? : Any -> Boolean
(define (work-analysis? x)
  (and (pair? x) (eq? (car x) 'work-analysis)))

;;; Accessors for WorkAnalysis
(define (analysis-total-cost wa)
  (cdr (assq 'total-cost (cdr wa))))

(define (analysis-complexity-class wa)
  (cdr (assq 'complexity-class (cdr wa))))

(define (analysis-hotspots wa)
  (cdr (assq 'hotspots (cdr wa))))

(define (analysis-parallelizable? wa)
  (cdr (assq 'parallelizable? (cdr wa))))

(define (analysis-suggested-splits wa)
  (cdr (assq 'suggested-splits (cdr wa))))

;;; ====
;;; Complexity Classification
;;; ====

;;; expr-complexity-class : Expr -> Symbol
;;; Classify an expression's complexity class based on structure.
;;; Returns: 'constant, 'linear, 'quadratic, 'recursive, 'unknown
(define (expr-complexity-class expr)
  (classify-complexity expr 100))

;;; classify-complexity : Expr x Nat -> Symbol
(define (classify-complexity expr fuel)
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

;;; combine-complexity : Symbol x Symbol -> Symbol
;;; Combine two complexity classes (takes the higher one).
(define (combine-complexity c1 c2)
  (let ([order '((constant . 0) (linear . 1) (quadratic . 2)
                 (recursive . 3) (unknown . 4))])
       (let ([o1 (cdr (assq c1 order))]
             [o2 (cdr (assq c2 order))])
            (if (> o1 o2) c1 c2))))

;;; ====
;;; Hotspot Detection
;;; ====

;;; Hotspot structure: (position cost expression)

;;; find-hotspots : Expr x Nat -> (List Hotspot)
;;; Find the N most expensive subexpressions.
(define (find-hotspots expr n)
  (let* ([all-costs (collect-costs expr '() 100)]
         [sorted (sort-by-cost all-costs)])
        (take n sorted)))

;;; collect-costs : Expr x Position x Nat -> (List (Position Cost Expr))
;;; Collect costs of all subexpressions with their positions.
(define (collect-costs expr pos fuel)
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

;;; sort-by-cost : (List (Position Cost Expr)) -> (List (Position Cost Expr))
;;; Sort cost entries by cost, descending.
(define (sort-by-cost entries)
  (let ([sorted (sort (lambda (a b) (> (cadr a) (cadr b))) entries)])
       sorted))

;;; ====
;;; Utility Functions
;;; ====

;;; total-work : (List Expr) -> Nat
;;; Sum the estimated work of multiple expressions.
(define (total-work exprs)
  (fold-left + 0 (map estimate-work exprs)))

;;; work-ratio : Expr x Expr -> Real
;;; Compute the work ratio between two expressions.
;;; Returns w1/w2, useful for load balancing.
(define (work-ratio e1 e2)
  (let ([w1 (estimate-work e1)]
        [w2 (estimate-work e2)])
       (if (= w2 0)
           +inf.0
           (/ w1 w2))))

;;; balanced-split? : (List Expr) -> Bool
;;; Check if a list of expressions has balanced work distribution.
;;; Balanced = max/min ratio < 2.
(define (balanced-split? exprs)
  (let* ([works (map estimate-work exprs)]
         [works-positive (filter (lambda (w) (> w 0)) works)])
        (if (< (length works-positive) 2)
            #t  ; Trivially balanced
            (let ([max-w (fold-left max 0 works-positive)]
                  [min-w (fold-left min +inf.0 works-positive)])
                 (< (/ max-w min-w) 2.0)))))

;;; ====
;;; Display Utilities
;;; ====

;;; format-work-analysis : WorkAnalysis -> String
;;; Format a work analysis for display.
(define (format-work-analysis wa)
  (format "Work Analysis:\n  Total cost: ~a\n  Complexity: ~a\n  Parallelizable: ~a\n  Suggested splits: ~a"
          (analysis-total-cost wa)
          (analysis-complexity-class wa)
          (analysis-parallelizable? wa)
          (analysis-suggested-splits wa)))

;;; display-work-analysis : WorkAnalysis -> Void
;;; Display a work analysis to current output port.
(define (display-work-analysis wa)
  (display (format-work-analysis wa))
  (newline)
  (display "  Hotspots:\n")
  (for-each
   (lambda (hotspot)
           (display (format "    ~a: cost ~a\n"
                            (car hotspot)
                            (cadr hotspot))))
   (analysis-hotspots wa)))
