(load "boundary/lens/call-graph.ss")
(load "boundary/lens/dep-slice.ss")

(doc 'module 'test-dep-slice)
(doc 'description "Tests for Dependency Slicing")
(doc 'layer 'boundary)
(doc 'purity 'partial)
(doc 'note "Comprehensive test suite for boundary/lens/dep-slice.ss")
(doc 'note "Tests transitive dependency computation and impact analysis")

(define test-count 0)
(define pass-count 0)
(define fail-count 0)

(define (assert-equal name actual expected)
(doc assert-equal 'description "Test assertion helper")
  (set! test-count (+ test-count 1))
  (if (equal? actual expected)
      (begin
       (set! pass-count (+ pass-count 1))
       (printf "  ✓ ~a\n" name))
      (begin
       (set! fail-count (+ fail-count 1))
       (printf "  ✗ ~a\n" name)
       (printf "    Expected: ~s\n" expected)
       (printf "    Actual:   ~s\n" actual))))

(define (assert-true name actual)
  (assert-equal name actual #t))

(define (assert-false name actual)
  (assert-equal name actual #f))

(doc 'section 'setup)

(printf "\n=== Setting up call graph for dep-slice tests ===\n")
(unless (call-graph-built?)
        (call-graph-refresh!))
(printf "Call graph ready.\n")

(doc 'section 'compute-closure-tests)

(printf "\n=== Testing compute-closure ===\n")

(assert-true "closure returns a list"
             (list? (compute-closure 'foo call-graph-callers 5)))

(assert-true "closure respects max-depth"
             (let* ([deep-result (compute-closure 'call-graph-refresh! call-graph-callees 10)]
                    [shallow-result (compute-closure 'call-graph-refresh! call-graph-callees 1)])
                   ;; Shallow should be same or smaller
                   (<= (length shallow-result) (length deep-result))))

(assert-true "closure with no neighbors is empty"
             (let ([result (compute-closure 'nonexistent-symbol-xyz call-graph-callers 5)])
                  (null? result)))

(doc 'section 'dep-slice-up-tests)

(printf "\n=== Testing dep-slice-up ===\n")

(assert-true "slice-up returns a list"
             (list? (dep-slice-up 'foo)))

(assert-true "slice-up respects depth param"
             (let ([deep (dep-slice-up 'call-graph-callers 10)]
                   [shallow (dep-slice-up 'call-graph-callers 2)])
                  (<= (length shallow) (length deep))))

(assert-true "unknown symbol has empty slice-up"
             (null? (dep-slice-up 'nonexistent-symbol-xyz)))

(doc 'section 'dep-slice-down-tests)

(printf "\n=== Testing dep-slice-down ===\n")

(assert-true "slice-down returns a list"
             (list? (dep-slice-down 'foo)))

(assert-true "slice-down respects depth param"
             (let ([deep (dep-slice-down 'call-graph-refresh! 10)]
                   [shallow (dep-slice-down 'call-graph-refresh! 2)])
                  (<= (length shallow) (length deep))))

(assert-true "slice-down includes direct callees"
             (let* ([direct (call-graph-callees 'call-graph-refresh!)]
                    [slice (dep-slice-down 'call-graph-refresh!)])
                   ;; All direct should be in slice
                   (andmap (lambda (d) (member d slice)) direct)))

(doc 'section 'dep-slice-both-tests)

(printf "\n=== Testing dep-slice-both ===\n")

(assert-true "slice-both returns alist"
             (let ([result (dep-slice-both 'foo)])
                  (and (list? result)
                       (assq 'up result)
                       (assq 'down result))))

(assert-true "slice-both up matches slice-up"
             (let* ([both (dep-slice-both 'call-graph-callers 5)]
                    [up-from-both (cdr (assq 'up both))]
                    [up-direct (dep-slice-up 'call-graph-callers 5)])
                   (equal? up-from-both up-direct)))

(assert-true "slice-both down matches slice-down"
             (let* ([both (dep-slice-both 'call-graph-callers 5)]
                    [down-from-both (cdr (assq 'down both))]
                    [down-direct (dep-slice-down 'call-graph-callers 5)])
                   (equal? down-from-both down-direct)))

(doc 'section 'dep-slice-layers-up-tests)

(printf "\n=== Testing dep-slice-layers-up ===\n")

(assert-true "layers-up returns list"
             (list? (dep-slice-layers-up 'foo 5)))

(assert-true "layers are numbered"
             (let ([layers (dep-slice-layers-up 'call-graph-callers 5)])
                  (or (null? layers)
                      (andmap (lambda (l) (and (pair? l) (number? (car l)))) layers))))

(assert-true "layer numbers are positive"
             (let ([layers (dep-slice-layers-up 'call-graph-callers 5)])
                  (or (null? layers)
                      (andmap (lambda (l) (> (car l) 0)) layers))))

(doc 'section 'dep-slice-layers-down-tests)

(printf "\n=== Testing dep-slice-layers-down ===\n")

(assert-true "layers-down returns list"
             (list? (dep-slice-layers-down 'foo 5)))

(assert-true "layers-down organized by depth"
             (let ([layers (dep-slice-layers-down 'call-graph-refresh! 5)])
                  (or (null? layers)
                      (andmap (lambda (l)
                                      (and (pair? l)
                                           (number? (car l))
                                           (list? (cdr l))))
                              layers))))

(doc 'section 'dep-impact-tests)

(printf "\n=== Testing dep-impact ===\n")

(assert-true "impact returns alist"
             (let ([result (dep-impact 'foo)])
                  (and (list? result)
                       (assq 'symbol result)
                       (assq 'impact-score result))))

(assert-true "impact symbol matches input"
             (let ([result (dep-impact 'call-graph-callers)])
                  (eq? (cdr (assq 'symbol result)) 'call-graph-callers)))

(assert-true "impact score is non-negative"
             (let ([result (dep-impact 'foo)])
                  (>= (cdr (assq 'impact-score result)) 0)))

(assert-true "impact has direct-dependents"
             (let ([result (dep-impact 'foo)])
                  (number? (cdr (assq 'direct-dependents result)))))

(assert-true "impact has transitive-dependents"
             (let ([result (dep-impact 'foo)])
                  (number? (cdr (assq 'transitive-dependents result)))))

(doc 'section 'dep-complexity-tests)

(printf "\n=== Testing dep-complexity ===\n")

(assert-true "complexity returns alist"
             (let ([result (dep-complexity 'foo)])
                  (and (list? result)
                       (assq 'symbol result)
                       (assq 'complexity-score result))))

(assert-true "complexity symbol matches input"
             (let ([result (dep-complexity 'call-graph-refresh!)])
                  (eq? (cdr (assq 'symbol result)) 'call-graph-refresh!)))

(assert-true "complexity score is non-negative"
             (let ([result (dep-complexity 'foo)])
                  (>= (cdr (assq 'complexity-score result)) 0)))

(assert-true "complexity has direct-dependencies"
             (let ([result (dep-complexity 'foo)])
                  (number? (cdr (assq 'direct-dependencies result)))))

(assert-true "complexity has transitive-dependencies"
             (let ([result (dep-complexity 'foo)])
                  (number? (cdr (assq 'transitive-dependencies result)))))

(doc 'section 'edge-cases)

(printf "\n=== Testing Edge Cases ===\n")

(assert-true "empty depth gives empty slice"
             (null? (dep-slice-up 'foo 0)))

(assert-true "nonexistent symbol has zero impact"
             (let ([result (dep-impact 'nonexistent-symbol-xyz)])
                  (= (cdr (assq 'impact-score result)) 0)))

(assert-true "nonexistent symbol has zero complexity"
             (let ([result (dep-complexity 'nonexistent-symbol-xyz)])
                  (= (cdr (assq 'complexity-score result)) 0)))

(doc 'section 'test-summary)

(printf "\n====\n")
(printf "Test Results (dep-slice):\n")
(printf "  Total:  ~a\n" test-count)
(printf "  Passed: ~a\n" pass-count)
(printf "  Failed: ~a\n" fail-count)

(if (= fail-count 0)
    (printf "\n✓ All tests passed!\n\n")
    (printf "\n✗ Some tests failed\n\n"))

(= fail-count 0)
