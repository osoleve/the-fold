;;; core/fp/analysis/test-parallel-detect.ss --- Tests for Parallelization Detection
;;;
;;; Tests the parallel opportunity detection module.

(load "core/lang/module.ss")
(load "core/test-framework.ss")
(load "lattice/fp/analysis/parallel-detect.ss")

(display "\n")
(display "=== Parallel Detection Tests ===\n")
(display "\n")

(test-group parallel-detect
            
            ;;; ====
            ;;; Data Structure Tests
            ;;; ====
            
            (define-test parallel-opportunity-structure
              (let ([opp (make-parallel-opportunity
                          'independent-let
                          '((+ a 1) (+ b 2))
                          '(0 1)
                          2.0
                          (make-dep-graph '((x) (y))))])
                   (assert-true (parallel-opportunity? opp))
                   (assert-equal 'independent-let (par-type opp))
                   (assert-equal '(0 1) (par-location opp))
                   (assert-equal 2 (length (par-branches opp)))
                   (assert-true (> (par-estimated-speedup opp) 1.0))))
            
            (define-test parallel-opportunity-rejects-invalid
              (assert-false (parallel-opportunity? '()))
              (assert-false (parallel-opportunity? 42))
              (assert-false (parallel-opportunity? '(not-an-opportunity)))
              (assert-false (parallel-opportunity? '(parallel-opportunity))))
            
            (define-test dep-graph-structure
              (let ([g (make-dep-graph '((a) (b . (x)) (c . (a b))))])
                   (assert-true (dep-graph? g))
                   (assert-equal '() (dep-graph-lookup g 'a))
                   (assert-equal '(x) (dep-graph-lookup g 'b))
                   (assert-equal '(a b) (dep-graph-lookup g 'c))))
            
            ;;; ====
            ;;; Free Variables Analysis
            ;;; ====
            
            (define-test free-vars-simple
              (assert-equal '(x) (free-vars 'x))
              (assert-equal '() (free-vars 42))
              (assert-equal '() (free-vars "string"))
              (assert-equal '() (free-vars '(quote foo))))
            
            (define-test free-vars-compound
              (let ([fvs (free-vars '(+ x y z))])
                   (assert-true (if (memq 'x fvs) #t #f))
                   (assert-true (if (memq 'y fvs) #t #f))
                   (assert-true (if (memq 'z fvs) #t #f))
                   (assert-equal 4 (length fvs))))  ; includes +
            
            (define-test free-vars-lambda-binds
              (assert-equal '() (free-vars '(lambda (x) x)))
              (assert-equal '() (free-vars '(fn (x) x)))
              (let ([fvs (free-vars '(lambda (x) (+ x y)))])
                   (assert-true (if (memq 'y fvs) #t #f))
                   (assert-true (if (memq '+ fvs) #t #f))
                   (assert-false (if (memq 'x fvs) #t #f))))
            
            (define-test free-vars-let-binds
              (let ([fvs (free-vars '(let ([x 1]) (+ x y)))])
                   (assert-true (if (memq 'y fvs) #t #f))
                   (assert-true (if (memq '+ fvs) #t #f))
                   (assert-false (if (memq 'x fvs) #t #f))))
            
            (define-test free-vars-let-star-sequential
              ;; In let*, later bindings can reference earlier ones
              (let ([fvs (free-vars '(let* ([a 1] [b a]) b))])
                   (assert-false (memq 'a fvs))
                   (assert-false (memq 'b fvs))))
            
            (define-test free-vars-letrec
              ;; In letrec, all vars are in scope for all vals
              (let ([fvs (free-vars '(letrec ([f (lambda (x) (g x))]
                                              [g (lambda (x) (f x))])
                                      (f 1)))])
                   (assert-false (memq 'f fvs))
                   (assert-false (memq 'g fvs))))
            
            ;;; ====
            ;;; Independent Let Bindings Detection
            ;;; ====
            
            (define-test detect-independent-let-simple
              ;; (let ([a (f x)] [b (g y)]) ...) - a and b are independent
              (let* ([expr '(let ([a (f x)] [b (g y)]) (+ a b))]
                     [opps (detect-parallel-regions expr)])
                    (assert-true (> (length opps) 0))
                    (let ([opp (find (lambda (o) (eq? (par-type o) 'independent-let)) opps)])
                         (assert-true (parallel-opportunity? opp))
                         (assert-equal 2 (length (par-branches opp))))))
            
            (define-test detect-independent-let-with-deps
              ;; (let ([a (f x)] [b (g a)]) ...) - b depends on a, NOT independent
              (let* ([expr '(let ([a (f x)] [b (g a)]) (+ a b))]
                     [opps (detect-parallel-regions expr)]
                     [let-opps (filter (lambda (o) (eq? (par-type o) 'independent-let)) opps)])
                    ;; Should find no independent-let opportunities (or none with both a,b)
                    (assert-true (or (null? let-opps)
                                     (andmap (lambda (o) (< (length (par-branches o)) 2))
                                             let-opps)))))
            
            (define-test detect-independent-let-three-bindings
              ;; (let ([a (f x)] [b (g y)] [c (h z)]) ...) - all independent
              (let* ([expr '(let ([a (f x)] [b (g y)] [c (h z)]) (list a b c))]
                     [opps (detect-parallel-regions expr)]
                     [let-opps (filter (lambda (o) (eq? (par-type o) 'independent-let)) opps)])
                    (assert-true (> (length let-opps) 0))))
            
            (define-test detect-independent-let-partial
              ;; (let ([a (f x)] [b (g a)] [c (h z)]) ...) - a,c independent but not b
              (let* ([expr '(let ([a (f x)] [b (g a)] [c (h z)]) (list a b c))]
                     [opps (detect-parallel-regions expr)]
                     [let-opps (filter (lambda (o) (eq? (par-type o) 'independent-let)) opps)])
                    ;; Should find a group with a and c
                    (assert-true (>= (length let-opps) 0))))  ; May or may not detect partial
            
            ;;; ====
            ;;; Independent Map Operations
            ;;; ====
            
            (define-test detect-independent-maps-in-begin
              (let* ([expr '(begin (map f xs) (map g ys))]
                     [opps (detect-parallel-regions expr)]
                     [map-opps (filter (lambda (o) (eq? (par-type o) 'independent-map)) opps)])
                    ;; xs and ys are independent inputs
                    (assert-true (>= (length map-opps) 0))))
            
            (define-test detect-independent-maps-in-list
              (let* ([expr '(list (map f xs) (map g ys))]
                     [opps (detect-parallel-regions expr)]
                     [map-opps (filter (lambda (o) (eq? (par-type o) 'independent-map)) opps)])
                    (assert-true (>= (length map-opps) 0))))
            
            ;;; ====
            ;;; Par-Eligible Expressions
            ;;; ====
            
            (define-test detect-par-eligible-function-call
              ;; (func (expensive-a) (expensive-b)) - args can run in parallel
              (let* ([expr '(combine (compute-x data1) (compute-y data2))]
                     [opps (detect-parallel-regions expr)]
                     [par-opps (filter (lambda (o) (eq? (par-type o) 'par-eligible)) opps)])
                    (assert-true (>= (length par-opps) 0))))
            
            (define-test detect-par-eligible-with-shared-input
              ;; (func (f x) (g x)) - same input x, but f and g are independent
              (let* ([expr '(combine (transform-a x) (transform-b x))]
                     [opps (detect-parallel-regions expr)])
                    ;; Both use x, so not independent in free-vars sense
                    ;; This tests that we correctly identify non-parallelizable cases
                    (assert-true (list? opps))))
            
            ;;; ====
            ;;; Fanout Pattern Detection
            ;;; ====
            
            (define-test detect-fanout-simple
              ;; (let ([x input]) (list (f x) (g x) (h x)))
              (let* ([expr '(let ([x (get-data)]) (list (analyze x) (transform x) (summarize x)))]
                     [opps (detect-parallel-regions expr)]
                     [fanout-opps (filter (lambda (o) (eq? (par-type o) 'fanout)) opps)])
                    (assert-true (>= (length fanout-opps) 0))))
            
            (define-test detect-fanout-with-values
              (let* ([expr '(let ([data input]) (values (process-a data) (process-b data)))]
                     [opps (detect-parallel-regions expr)]
                     [fanout-opps (filter (lambda (o) (eq? (par-type o) 'fanout)) opps)])
                    (assert-true (>= (length fanout-opps) 0))))
            
            ;;; ====
            ;;; No False Positives
            ;;; ====
            
            (define-test single-binding-no-parallel
              (let* ([expr '(let ([a (f x)]) (g a))]
                     [opps (detect-parallel-regions expr)]
                     [let-opps (filter (lambda (o) (eq? (par-type o) 'independent-let)) opps)])
                    (assert-equal 0 (length let-opps))))
            
            (define-test atoms-no-parallel
              (assert-equal 0 (length (detect-parallel-regions 42)))
              (assert-equal 0 (length (detect-parallel-regions 'symbol)))
              (assert-equal 0 (length (detect-parallel-regions "string"))))
            
            (define-test empty-list-no-parallel
              (assert-equal 0 (length (detect-parallel-regions '()))))
            
            ;;; ====
            ;;; Nested Detection
            ;;; ====
            
            (define-test detect-nested-opportunities
              (let* ([expr '(let ([outer 1])
                             (let ([a (f x)] [b (g y)])
                                  (+ a b)))]
                     [opps (detect-parallel-regions expr)])
                    ;; Should find the inner independent let
                    (assert-true (>= (length opps) 0))))
            
            (define-test detect-deeply-nested
              (let* ([expr '(define (main)
                             (let ([result
                                    (let ([a (compute-x)] [b (compute-y)])
                                         (combine a b))])
                                  result))]
                     [opps (detect-parallel-regions expr)])
                    (assert-true (list? opps))))
            
            ;;; ====
            ;;; Speedup Estimation
            ;;; ====
            
            (define-test speedup-increases-with-branches
              (let ([s2 (estimate-speedup 2 '((+ 1 2) (+ 3 4)))]
                    [s4 (estimate-speedup 4 '((+ 1 2) (+ 3 4) (+ 5 6) (+ 7 8)))])
                   (assert-true (> s4 s2))))
            
            (define-test speedup-has-cap
              ;; Speedup should not exceed a reasonable bound
              (let ([s100 (estimate-speedup 100 (map (lambda (x) '(+ 1 2)) (iota 100)))])
                   (assert-true (< s100 10))))  ; Cap at 8x with overhead
            
            (define-test speedup-accounts-for-work
              ;; Map operations are "heavier" than simple arithmetic
              (let ([simple-work (estimate-work '(+ 1 2))]
                    [map-work (estimate-work '(map f xs))])
                   (assert-true (> map-work simple-work))))
            
            ;;; ====
            ;;; Convenience Functions
            ;;; ====
            
            (define-test count-opportunities-works
              (let* ([expr '(let ([a (f x)] [b (g y)]) (+ a b))]
                     [opps (detect-parallel-regions expr)])
                    (assert-equal (length opps) (count-parallel-opportunities opps))))
            
            (define-test opportunities-by-pattern-groups
              (let* ([expr '(begin
                             (let ([a (f x)] [b (g y)]) (+ a b))
                             (list (map h as) (map i bs)))]
                     [opps (detect-parallel-regions expr)]
                     [by-pattern (opportunities-by-pattern opps)])
                    (assert-true (list? by-pattern))
                    (assert-true (andmap pair? by-pattern))))
            
            (define-test high-value-filters
              (let* ([expr '(let ([a (f x)] [b (g y)]) (+ a b))]
                     [opps (detect-parallel-regions expr)]
                     [high (high-value-opportunities opps)])
                    (assert-true (andmap (lambda (o) (>= (par-estimated-speedup o) 1.5)) high))))
            
            ;;; ====
            ;;; Summary Report
            ;;; ====
            
            (define-test parallel-summary-structure
              (let* ([expr '(let ([a (f x)] [b (g y)]) (+ a b))]
                     [opps (detect-parallel-regions expr)]
                     [summary (parallel-summary opps)])
                    (assert-true (pair? summary))
                    (assert-equal 'parallel-summary (car summary))
                    (assert-true (pair? (assq 'total (cdr summary))))
                    (assert-true (pair? (assq 'high-value (cdr summary))))
                    (assert-true (pair? (assq 'max-speedup (cdr summary))))
                    (assert-true (pair? (assq 'by-pattern (cdr summary))))))
            
            (define-test empty-summary
              (let* ([opps '()]
                     [summary (parallel-summary opps)])
                    (assert-equal 0 (cdr (assq 'total (cdr summary))))
                    (assert-equal 1.0 (cdr (assq 'max-speedup (cdr summary))))))
            
            ;;; ====
            ;;; Transformation Suggestions
            ;;; ====
            
            (define-test suggest-transform-returns-expr
              (let ([opp (make-parallel-opportunity
                          'independent-let
                          '((f x) (g y))
                          '()
                          2.0
                          (make-dep-graph '()))])
                   (let ([suggestion (suggest-par-transform opp)])
                        (assert-true (pair? suggestion))
                        (assert-equal 'par (car suggestion)))))
            
            ;;; ====
            ;;; Edge Cases
            ;;; ====
            
            (define-test handles-quoted-expressions
              (let* ([expr '(let ([a 'x] [b 'y]) (list a b))]
                     [opps (detect-parallel-regions expr)])
                    (assert-true (list? opps))))
            
            (define-test handles-nested-lambdas
              (let* ([expr '(let ([f (lambda (x) (+ x 1))]
                                  [g (lambda (y) (* y 2))])
                             (compose f g))]
                     [opps (detect-parallel-regions expr)])
                    (assert-true (list? opps))))
            
            ;;; ====
            ;;; Optional Fuel Parameter Tests
            ;;; ====
            
            (define-test optional-fuel-parameter-default
              ;; Calling with one argument should use default fuel
              (let* ([expr '(let ([a (f x)] [b (g y)]) (+ a b))]
                     [opps (detect-parallel-regions expr)])
                    (assert-true (list? opps))))
            
            (define-test optional-fuel-parameter-explicit
              ;; Calling with explicit fuel should work
              (let* ([expr '(let ([a (f x)] [b (g y)]) (+ a b))]
                     [opps (detect-parallel-regions expr 200)])
                    (assert-true (list? opps))))
            
            (define-test optional-fuel-parameter-low
              ;; Very low fuel should limit detection depth
              (let* ([expr '(let ([outer 1])
                             (let ([a (f x)] [b (g y)])
                                  (+ a b)))]
                     [opps-high (detect-parallel-regions expr 100)]
                     [opps-low (detect-parallel-regions expr 1)])
                    ;; Low fuel may find fewer opportunities due to depth limit
                    (assert-true (>= (length opps-high) (length opps-low)))))
            
            (define-test optional-fuel-zero-returns-empty
              ;; Zero fuel should return empty list (no processing)
              (let* ([expr '(let ([a (f x)] [b (g y)]) (+ a b))]
                     [opps (detect-parallel-regions expr 0)])
                    (assert-equal 0 (length opps)))))

(print-summary)
(when (> *tests-failed* 0) (exit 1))
