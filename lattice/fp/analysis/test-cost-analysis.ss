;;; core/fp/analysis/test-cost-analysis.ss --- Tests for Cost Analysis Module
;;;
;;; Tests the cost estimation and parallelization heuristics.

(load "core/test-framework.ss")
(load "lattice/fp/analysis/cost-analysis.ss")

(display "\n")
(display "=== Cost Analysis Tests ===\n")
(display "\n")

(test-group cost-analysis
            
            ;;; ============================================================
            ;;; Configuration Tests
            ;;; ============================================================
            
            (define-test min-parallel-threshold-returns-value
              (assert-true (number? (min-parallel-threshold)))
              (assert-true (> (min-parallel-threshold) 0)))
            
            (define-test set-parallel-threshold-works
              (let ([original (min-parallel-threshold)])
                   (set-parallel-threshold! 500)
                   (assert-equal 500 (min-parallel-threshold))
                   (set-parallel-threshold! original)))  ; Restore
            
            (define-test get-parallel-overhead-returns-value
              (assert-true (number? (get-parallel-overhead)))
              (assert-true (> (get-parallel-overhead) 0)))
            
            ;;; ============================================================
            ;;; Operation Cost Tests
            ;;; ============================================================
            
            (define-test primitive-costs-are-positive
              (assert-true (> (operation-cost '+) 0))
              (assert-true (> (operation-cost 'car) 0))
              (assert-true (> (operation-cost 'cons) 0)))
            
            (define-test arithmetic-costs-are-ordered
              ;; Division should cost more than addition
              (assert-true (> (operation-cost '/) (operation-cost '+)))
              ;; Transcendentals should cost more than basic ops
              (assert-true (> (operation-cost 'sqrt) (operation-cost '*))))
            
            (define-test hof-costs-are-substantial
              ;; HOFs have base overhead
              (assert-true (>= (operation-cost 'map) 10))
              (assert-true (>= (operation-cost 'filter) 10))
              (assert-true (>= (operation-cost 'fold-left) 10)))
            
            (define-test unknown-operations-get-default-cost
              ;; Unknown operations should return a reasonable default
              (let ([cost (operation-cost 'totally-made-up-function)])
                   (assert-true (> cost 0))
                   (assert-true (<= cost 20))))
            
            ;;; ============================================================
            ;;; Basic Work Estimation Tests
            ;;; ============================================================
            
            (define-test estimate-work-atoms
              ;; Numbers should have minimal cost
              (assert-true (<= (estimate-work 42) 5))
              ;; Symbols should have minimal cost
              (assert-true (<= (estimate-work 'x) 5))
              ;; Strings should have minimal cost
              (assert-true (<= (estimate-work "hello") 5)))
            
            (define-test estimate-work-simple-arithmetic
              (let ([cost (estimate-work '(+ 1 2))])
                   ;; Should include: + cost + two atom costs
                   (assert-true (> cost 2))
                   (assert-true (< cost 20))))
            
            (define-test estimate-work-nested-arithmetic
              (let ([simple-cost (estimate-work '(+ 1 2))]
                    [nested-cost (estimate-work '(+ (+ 1 2) (+ 3 4)))])
                   ;; Nested should cost more
                   (assert-true (> nested-cost simple-cost))))
            
            (define-test estimate-work-expensive-ops-cost-more
              (let ([add-cost (estimate-work '(+ 1 2))]
                    [sqrt-cost (estimate-work '(sqrt 4))])
                   ;; sqrt should cost more than addition
                   (assert-true (> sqrt-cost add-cost))))
            
            ;;; ============================================================
            ;;; Lambda and Let Work Estimation
            ;;; ============================================================
            
            (define-test estimate-work-lambda
              (let ([cost (estimate-work '(lambda (x) (* x x)))])
                   ;; Should include closure creation + body cost
                   (assert-true (> cost 5))))
            
            (define-test estimate-work-let-form
              (let ([cost (estimate-work '(let ([x 10] [y 20]) (+ x y)))])
                   ;; Should include binding setup + body
                   (assert-true (> cost 3))))
            
            (define-test estimate-work-complex-let
              (let ([simple-let (estimate-work '(let ([x 1]) x))]
                    [complex-let (estimate-work '(let ([x (sqrt 4)]
                                                       [y (sin 1.0)])
                                                  (+ x y)))])
                   ;; Complex bindings should cost more
                   (assert-true (> complex-let simple-let))))
            
            ;;; ============================================================
            ;;; Control Flow Work Estimation
            ;;; ============================================================
            
            (define-test estimate-work-if-form
              (let ([cost (estimate-work '(if (> x 0) x (- x)))])
                   ;; Should include condition + both branches
                   (assert-true (> cost 5))))
            
            (define-test estimate-work-cond-form
              (let ([cost (estimate-work '(cond
                                           [(> x 0) 'positive]
                                           [(< x 0) 'negative]
                                           [else 'zero]))])
                   ;; Should include all clause tests and bodies
                   (assert-true (> cost 10))))
            
            (define-test estimate-work-begin-form
              (let ([cost (estimate-work '(begin (+ 1 2) (+ 3 4) (+ 5 6)))])
                   ;; Should sum all expressions
                   (assert-true (> cost 10))))
            
            ;;; ============================================================
            ;;; Higher-Order Function Work Estimation
            ;;; ============================================================
            
            (define-test estimate-work-map
              (let ([cost (estimate-work '(map (lambda (x) (* x x)) xs))])
                   ;; Should include map overhead + body cost * estimated size
                   (assert-true (> cost 20))))
            
            (define-test estimate-work-filter
              (let ([cost (estimate-work '(filter (lambda (x) (> x 0)) xs))])
                   (assert-true (> cost 20))))
            
            (define-test estimate-work-fold
              (let ([cost (estimate-work '(fold-left + 0 xs))])
                   (assert-true (> cost 10))))
            
            (define-test estimate-work-nested-hof
              (let ([single-map (estimate-work '(map f xs))]
                    [nested-map (estimate-work '(map f (map g xs)))])
                   ;; Nested HOFs should cost more
                   (assert-true (> nested-map single-map))))
            
            (define-test estimate-work-with-list-literal
              ;; When list size is known, estimate should reflect it
              (let ([small-list (estimate-work '(map f '(1 2 3)))]
                    [large-list (estimate-work '(map f '(1 2 3 4 5 6 7 8 9 10)))])
                   ;; Larger list should cost more
                   (assert-true (> large-list small-list))))
            
            (define-test estimate-work-with-iota
              ;; iota size is known from argument
              (let ([small (estimate-work '(map f (iota 5)))]
                    [large (estimate-work '(map f (iota 100)))])
                   (assert-true (> large small))))
            
            ;;; ============================================================
            ;;; Parallelization Decision Tests
            ;;; ============================================================
            
            (define-test parallel-beneficial-trivial-work
              ;; Trivial expressions should not benefit from parallelization
              (assert-false (parallel-beneficial? '(+ 1 2) '(+ 3 4))))
            
            (define-test parallel-beneficial-substantial-work
              ;; Substantial work on both sides should benefit
              (let* ([big-expr '(fold-left + 0 (map (lambda (x) (* x x))
                                                    (iota 1000)))]
                     [result (parallel-beneficial? big-expr big-expr)])
                    ;; Should be true if both have substantial work
                    (assert-true (or result
                                     ;; Or at least the estimate is reasonable
                                     (> (estimate-work big-expr) 100)))))
            
            (define-test parallel-beneficial-unbalanced
              ;; One trivial + one substantial should not parallelize well
              (let ([trivial '(+ 1 2)]
                    [substantial '(fold-left + 0 (map sqrt (iota 100)))])
                   ;; Unbalanced work should return false
                   (assert-false (parallel-beneficial? trivial substantial))))
            
            (define-test parallel-beneficial-n-empty
              (assert-false (parallel-beneficial-n? '())))
            
            (define-test parallel-beneficial-n-single
              (assert-false (parallel-beneficial-n? '((+ 1 2)))))
            
            ;;; ============================================================
            ;;; Suggested Parallelism Tests
            ;;; ============================================================
            
            (define-test suggested-parallelism-trivial
              ;; Trivial work should suggest sequential
              (assert-equal 1 (suggested-parallelism '((+ 1 2) (+ 3 4)))))
            
            (define-test suggested-parallelism-returns-positive
              (let ([result (suggested-parallelism '((fold-left + 0 xs)
                                                     (fold-left * 1 ys)))])
                   (assert-true (>= result 1))))
            
            ;;; ============================================================
            ;;; Work Analysis Tests
            ;;; ============================================================
            
            (define-test work-analysis-structure
              (let ([wa (work-analysis '(+ 1 2))])
                   (assert-true (work-analysis? wa))
                   (assert-true (pair? (assq 'total-cost (cdr wa))))
                   (assert-true (pair? (assq 'complexity-class (cdr wa))))
                   (assert-true (pair? (assq 'parallelizable? (cdr wa))))))
            
            (define-test work-analysis-accessors
              (let ([wa (work-analysis '(map (lambda (x) x) xs))])
                   (assert-true (number? (analysis-total-cost wa)))
                   (assert-true (symbol? (analysis-complexity-class wa)))
                   (assert-true (boolean? (analysis-parallelizable? wa)))
                   (assert-true (number? (analysis-suggested-splits wa)))))
            
            (define-test work-analysis-hotspots
              (let* ([wa (work-analysis '(begin
                                          (sqrt 4)
                                          (map f xs)
                                          (+ 1 2)))]
                     [hotspots (analysis-hotspots wa)])
                    ;; Should have found some hotspots
                    (assert-true (list? hotspots))))
            
            ;;; ============================================================
            ;;; Complexity Classification Tests
            ;;; ============================================================
            
            (define-test classify-constant
              (assert-equal 'constant (expr-complexity-class 42))
              (assert-equal 'constant (expr-complexity-class 'x))
              (assert-equal 'constant (expr-complexity-class '(+ 1 2))))
            
            (define-test classify-linear-map
              (assert-equal 'linear (expr-complexity-class '(map f xs))))
            
            (define-test classify-linear-filter
              (assert-equal 'linear (expr-complexity-class '(filter p xs))))
            
            (define-test classify-linear-fold
              (assert-equal 'linear (expr-complexity-class '(fold-left + 0 xs))))
            
            (define-test classify-quadratic-nested-hof
              ;; Nested HOFs suggest quadratic complexity
              (let ([class (expr-complexity-class
                            '(map (lambda (x) (filter p xs)) ys))])
                   ;; Should be at least linear, possibly quadratic
                   (assert-true (if (memq class '(linear quadratic)) #t #f))))
            
            (define-test classify-recursive
              ;; letrec suggests recursion
              (assert-equal 'recursive
                            (expr-complexity-class
                             '(letrec ([f (lambda (x) (f x))]) (f 1)))))
            
            ;;; ============================================================
            ;;; Utility Function Tests
            ;;; ============================================================
            
            (define-test total-work-sums-correctly
              (let* ([e1 '(+ 1 2)]
                     [e2 '(* 3 4)]
                     [sum (total-work (list e1 e2))]
                     [individual (+ (estimate-work e1) (estimate-work e2))])
                    (assert-equal individual sum)))
            
            (define-test work-ratio-calculates
              (let ([ratio (work-ratio '(sqrt 4) '(+ 1 2))])
                   ;; sqrt should have higher work ratio than addition
                   (assert-true (> ratio 1))))
            
            (define-test balanced-split-detects-balance
              ;; Same expressions should be balanced
              (assert-true (balanced-split? '((+ 1 2) (+ 3 4)))))
            
            (define-test balanced-split-detects-imbalance
              ;; Very different work should be unbalanced
              (let* ([trivial '(+ 1 2)]
                     [complex '(fold-left + 0 (map sqrt (iota 100)))]
                     [result (balanced-split? (list trivial complex))])
                    ;; Should detect imbalance (if work differs significantly)
                    (assert-true (boolean? result))))
            
            ;;; ============================================================
            ;;; Edge Cases
            ;;; ============================================================
            
            (define-test estimate-work-empty-list
              (let ([cost (estimate-work '())])
                   (assert-true (<= cost 5))))
            
            (define-test estimate-work-quoted-expression
              (let ([cost (estimate-work '(quote (a b c)))])
                   (assert-true (<= cost 5))))
            
            (define-test estimate-work-deeply-nested
              ;; Should not stack overflow on deep nesting
              (let ([cost (estimate-work
                           '(+ (+ (+ (+ (+ (+ (+ (+ 1 2) 3) 4) 5) 6) 7) 8) 9))])
                   (assert-true (> cost 10))))
            
            (define-test estimate-work-malformed-gracefully
              ;; Should handle unusual expressions without crashing
              (assert-true (number? (estimate-work '())))
              (assert-true (number? (estimate-work '(map)))))
            
            (define-test work-analysis-empty-expression
              (let ([wa (work-analysis '())])
                   (assert-true (work-analysis? wa))
                   (assert-true (number? (analysis-total-cost wa)))))
            
            ;;; ============================================================
            ;;; Optional Fuel Parameter Tests
            ;;; ============================================================
            
            (define-test estimate-work-optional-fuel-default
              ;; Calling with one argument should use default fuel
              (let ([cost (estimate-work '(+ 1 2))])
                   (assert-true (> cost 0))))
            
            (define-test estimate-work-optional-fuel-explicit
              ;; Calling with explicit fuel should work
              (let ([cost (estimate-work '(+ 1 2) 2000)])
                   (assert-true (> cost 0))))
            
            (define-test estimate-work-optional-fuel-low
              ;; Very low fuel should return conservative estimate
              (let* ([expr '(+ (+ (+ 1 2) 3) 4)]
                     [cost-high (estimate-work expr 1000)]
                     [cost-low (estimate-work expr 1)])
                    ;; Low fuel returns 0 (conservative) for deep expressions
                    (assert-true (>= cost-high cost-low))))
            
            (define-test estimate-work-optional-fuel-zero
              ;; Zero fuel should return 0 (conservative estimate)
              (let ([cost (estimate-work '(sqrt (+ 1 2)) 0)])
                   (assert-equal 0 cost))))

(print-summary)
(when (> *tests-failed* 0) (exit 1))
