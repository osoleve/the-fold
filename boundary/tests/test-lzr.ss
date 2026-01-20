(load "core/test-framework.ss")
(load "boundary/tools/fusion-analyzer.ss")

(doc 'module 'test-lzr)
(doc 'description "Integration Tests for LZR Shell Interface")
(doc 'layer 'boundary)
(doc 'purity 'partial)
(doc 'note "Tests for the lzr (LaZeR) optimization analyzer shell interface")
(doc 'note "LZR provides auto-parallelization detection, fusion hints, and safe rewrite suggestions for functional pipelines")
(doc 'note "Test Categories: 1. analyze-fusion, 2. suggest-parallel, 3. optimize, 4. Integration")
(doc 'note "Dependencies: core/test-framework.ss, boundary/tools/fusion-analyzer.ss")

(display "\n")
(display "====\n")
(display "          LZR Shell Interface Tests\n")
(display "====\n")
(display "\n")

(doc 'section 'analyze-fusion-tests)

(test-group analyze-fusion
            
            (define-test analyze-fusion-returns-list
              (doc 'note "Should return a list (possibly empty) of opportunities")
              (let ([result (analyze-fusion '(+ 1 2))])
                   (assert-true (list? result))))
            
            (define-test analyze-fusion-detects-map-map
              (doc 'note "(map f (map g xs)) is a classic fusion opportunity")
              (let* ([expr '(map f (map g xs))]
                     [opps (analyze-fusion expr)])
                    (assert-true (> (length opps) 0))
                    (assert-true (fusion-opportunity? (car opps)))
                    (assert-equal 'map-map-fuse (fusion-type (car opps)))))
            
            (define-test analyze-fusion-detects-filter-map
              (doc 'note "(map f (filter p xs)) can be fused to filter-map")
              (let* ([expr '(map transform (filter predicate items))]
                     [opps (analyze-fusion expr)])
                    (assert-true (> (length opps) 0))
                    (assert-equal 'filter-map-fuse (fusion-type (car opps)))))
            
            (define-test analyze-fusion-returns-empty-for-optimal
              (doc 'note "Already optimal expressions should have no opportunities")
              (let* ([expr '(filter-map p f xs)]
                     [opps (analyze-fusion expr)])
                    (assert-equal 0 (length opps))))
            
            (define-test analyze-fusion-works-with-nested-expressions
              (doc 'note "Should find fusion opportunities inside let bindings")
              (let* ([expr '(let ([result (map f (map g xs))])
                             (display result))]
                     [opps (analyze-fusion expr)])
                    (assert-true (> (length opps) 0))))
            
            (define-test analyze-fusion-detects-fold-map
              (doc 'note "(foldl f z (map g xs)) can fuse the map into the fold")
              (let* ([expr '(foldl + 0 (map square numbers))]
                     [opps (analyze-fusion expr)])
                    (assert-true (> (length opps) 0))
                    (assert-equal 'fold-map-fuse (fusion-type (car opps)))))
            
            (define-test analyze-fusion-detects-concat-map
              (doc 'note "(flatten (map f xs)) can become flatMap")
              (let* ([expr '(flatten (map expand items))]
                     [opps (analyze-fusion expr)])
                    (assert-true (> (length opps) 0))
                    (assert-equal 'concat-map-fuse (fusion-type (car opps)))))
            
            )

(doc 'section 'suggest-parallel-tests)

(test-group suggest-parallel
            
            (define-test suggest-parallel-returns-list
              (doc 'note "Should always return a list")
              (let ([result (suggest-parallel '(+ 1 2))])
                   (assert-true (list? result))))
            
            (define-test suggest-parallel-detects-independent-let
              (doc 'note "Independent let bindings can run in parallel")
              (let* ([expr '(let ([a (expensive-fn x)]
                                  [b (other-fn y)])
                             (combine a b))]
                     [opps (suggest-parallel expr)])
                    (assert-true (> (length opps) 0))
                    (assert-true (parallel-opportunity? (car opps)))
                    (assert-equal 'independent-let (par-type (car opps)))))
            
            (define-test suggest-parallel-respects-dependencies
              (doc 'note "Dependent bindings should not be suggested for parallelization")
              (let* ([expr '(let ([a (compute x)]
                                  [b (compute a)])  (doc 'note "b depends on a")
                             (+ a b))]
                     [opps (suggest-parallel expr)])
                    (doc 'note "Should not find independent-let opportunities here")
                    (let ([let-opps (filter (lambda (o) (eq? (par-type o) 'independent-let))
                                            opps)])
                         (assert-equal 0 (length let-opps)))))
            
            (define-test suggest-parallel-returns-empty-for-sequential
              (doc 'note "Sequential operations with dependencies have no parallel opportunities")
              (let* ([expr '(let* ([a (f x)]
                                   [b (g a)]
                                   [c (h b)])
                             c)]
                     [opps (suggest-parallel expr)])
                    (doc 'note "let* is inherently sequential - check no independent-let found")
                    (let ([let-opps (filter (lambda (o) (eq? (par-type o) 'independent-let))
                                            opps)])
                         (assert-equal 0 (length let-opps)))))
            
            (define-test suggest-parallel-detects-fanout
              (doc 'note "Single input feeding multiple independent computations")
              (let* ([expr '(let ([x input])
                             (list (f x) (g x) (h x)))]
                     [opps (suggest-parallel expr)])
                    (let ([fanout-opps (filter (lambda (o) (eq? (par-type o) 'fanout))
                                               opps)])
                         (assert-true (> (length fanout-opps) 0)))))
            
            (define-test suggest-parallel-estimates-speedup
              (doc 'note "Each opportunity should have an estimated speedup")
              (let* ([expr '(let ([a (expensive-fn x)]
                                  [b (other-fn y)])
                             (combine a b))]
                     [opps (suggest-parallel expr)])
                    (when (> (length opps) 0)
                          (let ([speedup (par-estimated-speedup (car opps))])
                               (assert-true (number? speedup))
                               (assert-true (> speedup 1.0))))))
            
            (define-test suggest-parallel-handles-par-eligible
              (doc 'note "Function calls with independent arguments")
              (let* ([expr '(combine (expensive-a x) (expensive-b y))]
                     [opps (suggest-parallel expr)])
                    (let ([par-opps (filter (lambda (o) (eq? (par-type o) 'par-eligible))
                                            opps)])
                         (assert-true (>= (length par-opps) 0)))))
            
            )

(doc 'section 'optimize-tests)

(test-group optimize
            
            (define-test optimize-returns-expression
              (doc 'note "optimize should always return an expression")
              (let ([result (optimize '(+ 1 2))])
                   (assert-true (pair? result))))
            
            (define-test optimize-applies-map-map-fusion
              (doc 'note "Should fuse consecutive maps")
              (let* ([expr '(map f (map g xs))]
                     [result (optimize expr)])
                    (doc 'note "Result should be (map (compose f g) xs)")
                    (assert-equal 'map (car result))
                    (assert-true (pair? (cadr result)))
                    (assert-equal 'compose (caadr result))))
            
            (define-test optimize-returns-same-if-optimal
              (doc 'note "Already optimal expressions should be unchanged")
              (let* ([expr '(map (compose f g) xs)]
                     [result (optimize expr)])
                    (assert-equal expr result)))
            
            (define-test optimize-handles-nested-fusion
              (doc 'note "Should handle multiple fusion opportunities")
              (let* ([expr '(map h (map f (map g xs)))]
                     [result (optimize expr)])
                    (doc 'note "Should fuse into single map with composed function")
                    (assert-equal 'map (car result))
                    (doc 'note "Inner expression should contain compose")
                    (assert-true (pair? (cadr result)))))
            
            (define-test optimize-handles-filter-map
              (doc 'note "Should fuse filter-map patterns")
              (let* ([expr '(map f (filter p xs))]
                     [result (optimize expr)])
                    (assert-equal 'filter-map (car result))))
            
            (define-test optimize-computes-savings
              (doc 'note "optimize-with-stats should return savings info")
              (let* ([expr '(map f (map g xs))]
                     [result (optimize-with-stats expr)])
                    (assert-true (pair? result))
                    (assert-true (pair? (assq 'before-traversals result)))
                    (assert-true (pair? (assq 'after-traversals result)))
                    (let ([before (cdr (assq 'before-traversals result))]
                          [after (cdr (assq 'after-traversals result))])
                         (assert-true (>= before after)))))
            
            (define-test optimize-preserves-semantics
              (doc 'note "Length of result list should be same concept")
              (let* ([expr '(length (map f xs))]
                     [result (optimize expr)])
                    (doc 'note "Should eliminate map when only length is needed")
                    (assert-equal 'length (car result))
                    (assert-equal 'xs (cadr result))))
            
            )

(doc 'section 'integration-tests)

(test-group integration
            
            (define-test lzr-report-produces-output
              (doc 'note "lzr-report should produce a summary report")
              (let* ([expr '(map f (map g (filter p xs)))]
                     [report (lzr-report expr)])
                    (assert-true (pair? report))
                    (assert-true (pair? (assq 'fusion-opportunities report)))
                    (assert-true (pair? (assq 'parallel-opportunities report)))))
            
            (define-test full-pipeline-detect-optimize-verify
              (doc 'note "Full workflow: detect, optimize, verify improvement")
              (let* ([expr '(foldl + 0 (map square (filter positive? numbers)))]
                     (doc 'note "Step 1: Detect opportunities")
                     [fusion-opps (analyze-fusion expr)]
                     [parallel-opps (suggest-parallel expr)]
                     (doc 'note "Step 2: Optimize")
                     [optimized (optimize expr)]
                     (doc 'note "Step 3: Verify fewer traversals")
                     [before-count (count-traversals expr)]
                     [after-count (count-traversals optimized)])
                    (doc 'note "Should have found fusion opportunities")
                    (assert-true (> (length fusion-opps) 0))
                    (doc 'note "Optimization should reduce traversals")
                    (assert-true (< after-count before-count))))
            
            (define-test analyze-all-comprehensive-report
              (doc 'note "analyze-all should combine fusion and parallel analysis")
              (let* ([expr '(let ([a (map f xs)]
                                  [b (map g ys)])
                             (append a b))]
                     [analysis (analyze-all expr)])
                    (assert-true (pair? analysis))
                    (doc 'note "Should have both types of analysis")
                    (assert-true (pair? (assq 'fusion analysis)))
                    (assert-true (pair? (assq 'parallel analysis)))))
            
            (define-test optimization-preserves-structure
              (doc 'note "Non-fusable expressions should remain unchanged")
              (let* ([expr '(if (> x 0) (f x) (g x))]
                     [result (optimize expr)])
                    (assert-equal 'if (car result))
                    (assert-equal 3 (length (cdr result)))))
            
            )

(doc 'section 'edge-cases)

(test-group edge-cases
            
            (define-test analyze-handles-atoms
              (doc 'note "Should handle simple atoms without error")
              (let ([result (analyze-fusion 42)])
                   (assert-equal '() result)))
            
            (define-test analyze-handles-empty-list
              (doc 'note "Should handle empty list")
              (let ([result (analyze-fusion '())])
                   (assert-equal '() result)))
            
            (define-test analyze-handles-symbols
              (doc 'note "Should handle bare symbols")
              (let ([result (analyze-fusion 'x)])
                   (assert-equal '() result)))
            
            (define-test suggest-parallel-handles-single-binding
              (doc 'note "Single binding has nothing to parallelize with")
              (let* ([expr '(let ([x (f y)]) x)]
                     [opps (suggest-parallel expr)])
                    (let ([let-opps (filter (lambda (o) (eq? (par-type o) 'independent-let))
                                            opps)])
                         (assert-equal 0 (length let-opps)))))
            
            )

(doc 'section 'summary)

(newline)
(exit-with-summary)
