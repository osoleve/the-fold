(load "core/testing/test-framework.ss")
(when (not (top-level-bound? 'meta-printf))
  (eval '(define meta-printf printf)))
(load "lattice/meta/lattice-walk.ss")
(load "lattice/meta/analyzer-exports.ss")
(load "lattice/meta/analyzer-effects.ss")
(load "lattice/meta/analyzer-unused.ss")
(load "lattice/meta/analyzer-patterns.ss")
(load "lattice/meta/analyzer-coverage.ss")

;;; ============================================================================
;;; Helpers
;;; ============================================================================

(define (mr name deps . sexps)
  (make-module-record name (string-append (symbol->string name) ".ss")
                      deps (if (null? sexps) '() (car sexps)) ""))

(define (list-index lst item)
  (let loop ([l lst] [i 0])
    (cond
      [(null? l) -1]
      [(eq? (car l) item) i]
      [else (loop (cdr l) (+ i 1))])))

;;; ============================================================================
;;; Module Record Tests
;;; ============================================================================

(test-group "module-record"

  (define-test "construction and accessors"
    (let ([m (make-module-record 'vec "lattice/linalg/vec.ss"
                                 '(prelude) '((define x 1)) "abc123")])
      (assert-true (module-record? m))
      (assert-equal 'vec (module-record-name m))
      (assert-equal "lattice/linalg/vec.ss" (module-record-path m))
      (assert-equal '(prelude) (module-record-deps m))
      (assert-equal '((define x 1)) (module-record-sexps m))
      (assert-equal "abc123" (module-record-fingerprint m))))

  (define-test "predicate rejects non-records"
    (assert-false (module-record? '(not a record)))
    (assert-false (module-record? 42))
    (assert-false (module-record? (vector 'wrong 1 2 3 4 5)))))

;;; ============================================================================
;;; Analyzer Protocol Tests
;;; ============================================================================

(test-group "analyzer-protocol"

  (define-test "construction and accessors"
    (let ([a (make-analyzer 'test (lambda () 0) (lambda (mr acc st) st))])
      (assert-true (analyzer? a))
      (assert-equal 'test (analyzer-key a))
      (assert-true (procedure? (analyzer-init a)))
      (assert-true (procedure? (analyzer-step a)))))

  (define-test "predicate rejects non-analyzers"
    (assert-false (analyzer? '(not an analyzer)))
    (assert-false (analyzer? (vector 'analyzer 1 2)))))

;;; ============================================================================
;;; Topological Sort Tests
;;; ============================================================================

(test-group "topo-sort-modules"

  (define-test "empty input"
    (let ([result (topo-sort-modules '())])
      (assert-equal 'ok (car result))
      (assert-equal '() (cadr result))))

  (define-test "single module no deps"
    (let* ([a (mr 'a '())]
           [result (topo-sort-modules (list a))])
      (assert-equal 'ok (car result))
      (assert-equal 1 (length (cadr result)))
      (assert-equal 'a (module-record-name (car (cadr result))))))

  (define-test "linear chain: a -> b -> c"
    (let* ([a (mr 'a '())]
           [b (mr 'b '(a))]
           [c (mr 'c '(b))]
           ;; Pass in shuffled order
           [result (topo-sort-modules (list c a b))]
           [sorted (cadr result)]
           [names (map module-record-name sorted)])
      (assert-equal 'ok (car result))
      (assert-equal 3 (length sorted))
      ;; a must come before b, b before c
      (assert-true (< (list-index names 'a) (list-index names 'b)))
      (assert-true (< (list-index names 'b) (list-index names 'c)))))

  (define-test "diamond: d <- b <- a, d <- c <- a"
    (let* ([d (mr 'd '())]
           [b (mr 'b '(d))]
           [c (mr 'c '(d))]
           [a (mr 'a '(b c))]
           [result (topo-sort-modules (list a c b d))]
           [sorted (cadr result)]
           [names (map module-record-name sorted)])
      (assert-equal 'ok (car result))
      (assert-equal 4 (length sorted))
      ;; d must come before b and c; b and c before a
      (assert-true (< (list-index names 'd) (list-index names 'b)))
      (assert-true (< (list-index names 'd) (list-index names 'c)))
      (assert-true (< (list-index names 'b) (list-index names 'a)))
      (assert-true (< (list-index names 'c) (list-index names 'a)))))

  (define-test "external deps are ignored"
    ;; Module depends on 'prelude which isn't in the input set
    (let* ([a (mr 'a '(prelude))]
           [b (mr 'b '(a prelude))]
           [result (topo-sort-modules (list b a))]
           [sorted (cadr result)]
           [names (map module-record-name sorted)])
      (assert-equal 'ok (car result))
      (assert-equal 2 (length sorted))
      (assert-true (< (list-index names 'a) (list-index names 'b)))))

  (define-test "cycle detection"
    (let* ([a (mr 'a '(b))]
           [b (mr 'b '(a))]
           [result (topo-sort-modules (list a b))])
      (assert-equal 'err (car result))
      ;; Both modules should be in the cycle list
      (assert-true (pair? (memq 'a (cadr result))))
      (assert-true (pair? (memq 'b (cadr result))))))

  (define-test "independent modules sort without constraint"
    (let* ([a (mr 'a '())]
           [b (mr 'b '())]
           [c (mr 'c '())]
           [result (topo-sort-modules (list c b a))]
           [sorted (cadr result)])
      (assert-equal 'ok (car result))
      (assert-equal 3 (length sorted)))))

;;; ============================================================================
;;; Walker Tests
;;; ============================================================================

(test-group "lattice-walk"

  (define-test "counting analyzer threads state"
    (let* ([counter (make-analyzer 'count
                      (lambda () 0)
                      (lambda (mr acc state) (+ state 1)))]
           [a (mr 'a '())]
           [b (mr 'b '(a))]
           [result (lattice-walk (list b a) (list counter))])
      (assert-true (walk-ok? result))
      (assert-equal 2 (cdr (assq 'modules-processed result)))
      (assert-equal 2 (acc-ref (walk-state result) 'count))))

  (define-test "name-collecting analyzer"
    ;; Collects module names in processing order
    (let* ([collector (make-analyzer 'names
                        (lambda () '())
                        (lambda (mr acc state)
                          (append state (list (module-record-name mr)))))]
           [a (mr 'a '())]
           [b (mr 'b '(a))]
           [c (mr 'c '(b))]
           [result (lattice-walk (list c a b) (list collector))]
           [names (acc-ref (walk-state result) 'names)])
      (assert-true (walk-ok? result))
      ;; Must be in dependency order: a, b, c
      (assert-equal '(a b c) names)))

  (define-test "multiple analyzers compose sequentially"
    ;; First analyzer counts, second collects names
    ;; The name collector reads the count from acc to verify cross-read works
    (let* ([counter (make-analyzer 'count
                      (lambda () 0)
                      (lambda (mr acc state) (+ state 1)))]
           [reader (make-analyzer 'snapshots
                     (lambda () '())
                     (lambda (mr acc state)
                       ;; Read the count from the accumulator (already updated for this module)
                       (let ([current-count (acc-ref acc 'count)])
                         (cons (cons (module-record-name mr) current-count) state))))]
           [a (mr 'a '())]
           [b (mr 'b '(a))]
           [result (lattice-walk (list b a) (list counter reader))]
           [snapshots (acc-ref (walk-state result) 'snapshots)])
      (assert-true (walk-ok? result))
      ;; counter runs first, so by the time reader runs for 'a, count is 1
      ;; By the time reader runs for 'b, count is 2
      (assert-true (pair? (assq 'a snapshots)))
      (assert-equal 1 (cdr (assq 'a snapshots)))
      (assert-true (pair? (assq 'b snapshots)))
      (assert-equal 2 (cdr (assq 'b snapshots)))))

  (define-test "cycle returns error"
    (let* ([counter (make-analyzer 'count
                      (lambda () 0)
                      (lambda (mr acc state) (+ state 1)))]
           [a (mr 'a '(b))]
           [b (mr 'b '(a))]
           [result (lattice-walk (list a b) (list counter))])
      (assert-false (walk-ok? result))
      (assert-equal "cycle detected" (cdr (assq 'error result)))))

  (define-test "empty module list"
    (let* ([counter (make-analyzer 'count
                      (lambda () 0)
                      (lambda (mr acc state) (+ state 1)))]
           [result (lattice-walk '() (list counter))])
      (assert-true (walk-ok? result))
      (assert-equal 0 (cdr (assq 'modules-processed result)))
      (assert-equal 0 (acc-ref (walk-state result) 'count))))

  (define-test "sexps are passed through to analyzer"
    (let* ([def-counter (make-analyzer 'defs
                          (lambda () 0)
                          (lambda (mr acc state)
                            (+ state
                               (length (filter (lambda (s)
                                                 (and (pair? s) (eq? (car s) 'define)))
                                               (module-record-sexps mr))))))]
           [a (mr 'a '() '((define x 1) (define y 2)))]
           [b (mr 'b '(a) '((define z 3)))]
           [result (lattice-walk (list b a) (list def-counter))])
      (assert-true (walk-ok? result))
      (assert-equal 3 (acc-ref (walk-state result) 'defs)))))

;;; ============================================================================
;;; Accumulator Helper Tests
;;; ============================================================================

(test-group "accumulator-helpers"

  (define-test "acc-ref finds entry"
    (let ([acc '((a . 1) (b . 2))])
      (assert-equal 1 (acc-ref acc 'a))
      (assert-equal 2 (acc-ref acc 'b))))

  (define-test "acc-ref returns #f for missing key"
    (assert-false (acc-ref '((a . 1)) 'z))))

;;; ============================================================================
;;; Export Analyzer Tests
;;; ============================================================================

(test-group "export-analyzer"

  (define-test "collects annotated exports"
    (let* ([analyzer (make-export-analyzer)]
           [a (mr 'a '() '((define (foo x) (doc 'export #t) x)
                            (define (bar y) y)))]
           [result (lattice-walk (list a) (list analyzer))]
           [exports (acc-ref (walk-state result) 'exports)])
      (assert-true (walk-ok? result))
      ;; Should find 'foo (annotated), not 'bar (not annotated)
      (let ([a-exports (export-db-lookup exports 'a)])
        (assert-true (pair? a-exports))
        (assert-equal 'known (car a-exports))
        (assert-true (pair? (memq 'foo (cadr a-exports)))))))

  (define-test "collects targeted doc exports"
    (let* ([analyzer (make-export-analyzer)]
           [a (mr 'a '() '((doc baz 'export #t)
                            (define (baz x) x)))]
           [result (lattice-walk (list a) (list analyzer))]
           [exports (acc-ref (walk-state result) 'exports)])
      (assert-true (walk-ok? result))
      (let ([a-exports (export-db-lookup exports 'a)])
        (assert-equal 'known (car a-exports))
        (assert-true (pair? (memq 'baz (cadr a-exports)))))))

  (define-test "falls back to all defines when no annotations"
    (let* ([analyzer (make-export-analyzer)]
           [a (mr 'a '() '((define (foo x) x)
                            (define bar 42)))]
           [result (lattice-walk (list a) (list analyzer))]
           [exports (acc-ref (walk-state result) 'exports)])
      (assert-true (walk-ok? result))
      (let ([syms (export-db-symbols exports 'a)])
        (assert-true (pair? (memq 'foo syms)))
        (assert-true (pair? (memq 'bar syms))))))

  (define-test "accumulates across modules in dependency order"
    (let* ([analyzer (make-export-analyzer)]
           [a (mr 'a '() '((define (x) 1)
                            (define (y) 2)))]
           [b (mr 'b '(a) '((define (z) 3)))]
           [result (lattice-walk (list b a) (list analyzer))]
           [exports (acc-ref (walk-state result) 'exports)])
      (assert-true (walk-ok? result))
      ;; Both modules should have entries
      (assert-true (pair? (export-db-symbols exports 'a)))
      (assert-true (pair? (export-db-symbols exports 'b)))
      ;; Unknown module returns 'unknown
      (assert-equal 'unknown (export-db-lookup exports 'nonexistent))))

  (define-test "tri-state: unknown vs known-empty"
    (let* ([analyzer (make-export-analyzer)]
           ;; Module with no defines at all
           [a (mr 'a '() '((doc 'module 'a)))]
           [result (lattice-walk (list a) (list analyzer))]
           [exports (acc-ref (walk-state result) 'exports)])
      ;; Module 'a was analyzed — should be (known ()), not unknown
      (let ([a-entry (export-db-lookup exports 'a)])
        (assert-equal 'known (car a-entry))
        (assert-equal '() (cadr a-entry)))
      ;; Module 'z was never analyzed — should be 'unknown
      (assert-equal 'unknown (export-db-lookup exports 'z)))))

;;; ============================================================================
;;; Unused-Requires Analyzer Tests
;;; ============================================================================

(test-group "unused-requires-analyzer"

  (define-test "detects unused require with known exports"
    ;; Module A exports foo. Module B requires A but only uses bar (not from A).
    (let* ([export-a (make-export-analyzer)]
           [unused-a (make-unused-requires-analyzer)]
           [a (mr 'a '() '((doc my-fn 'export #t)
                            (define (my-fn x) (+ x 1))))]
           [b (mr 'b '(a) '((require 'a)
                             (define (bar x) (+ x 2))))]
           [result (lattice-walk (list b a) (list export-a unused-a))]
           [unused-db (acc-ref (walk-state result) 'unused)]
           [b-result (cdr (assq 'b unused-db))])
      (assert-true (walk-ok? result))
      ;; A should be unused in B (B doesn't call my-fn)
      (assert-true (pair? (memq 'a (analysis-unused b-result))))))

  (define-test "detects used require with known exports"
    ;; Module A exports my-fn. Module B requires A and uses my-fn.
    (let* ([export-a (make-export-analyzer)]
           [unused-a (make-unused-requires-analyzer)]
           [a (mr 'a '() '((doc my-fn 'export #t)
                            (define (my-fn x) (+ x 1))))]
           [b (mr 'b '(a) '((require 'a)
                             (define (bar x) (my-fn x))))]
           [result (lattice-walk (list b a) (list export-a unused-a))]
           [unused-db (acc-ref (walk-state result) 'unused)]
           [b-result (cdr (assq 'b unused-db))])
      (assert-true (walk-ok? result))
      (assert-true (pair? (memq 'a (analysis-used b-result))))))

  (define-test "all three analyzers compose"
    ;; Run exports + effects + unused all together
    (let* ([ea (make-export-analyzer)]
           [eff (make-effect-analyzer)]
           [ua (make-unused-requires-analyzer)]
           [a (mr 'a '() '((define (io-fn x) (display x))))]
           [b (mr 'b '(a) '((require 'a)
                             (define (wrapper x) (io-fn x))))]
           [result (lattice-walk (list b a) (list ea eff ua))]
           [state (walk-state result)])
      (assert-true (walk-ok? result))
      ;; Exports collected
      (assert-true (pair? (export-db-symbols (acc-ref state 'exports) 'a)))
      ;; Effects propagated
      (assert-equal 'io (effect-db-lookup (acc-ref state 'effects) 'wrapper))
      ;; A is used in B (B calls io-fn which is from A)
      (let* ([unused-db (acc-ref state 'unused)]
             [b-result (cdr (assq 'b unused-db))])
        (assert-true (pair? (memq 'a (analysis-used b-result))))))))

;;; ============================================================================
;;; Pattern Analyzer Tests
;;; ============================================================================

(test-group "pattern-analyzer"

  (define-test "clean module — case with else"
    (let* ([analyzer (make-pattern-analyzer)]
           [a (mr 'a '() '((define (foo x) (case x [(a) 1] [else 0]))))]
           [result (lattice-walk (list a) (list analyzer))]
           [patterns (acc-ref (walk-state result) 'patterns)])
      (assert-true (walk-ok? result))
      (assert-equal '() patterns)))

  (define-test "case without else flagged"
    (let* ([analyzer (make-pattern-analyzer)]
           [a (mr 'a '() '((define (foo x) (case x [(a) 1] [(b) 2]))))]
           [result (lattice-walk (list a) (list analyzer))]
           [patterns (acc-ref (walk-state result) 'patterns)])
      (assert-true (walk-ok? result))
      (assert-true (pair? patterns))
      (let ([entry (cdr (assq 'a patterns))])
        (assert-equal 1 (cdr (assq 'cases-without-else entry))))))

  (define-test "duplicate datums flagged"
    (let* ([analyzer (make-pattern-analyzer)]
           [a (mr 'a '() '((define (foo x)
                              (case x [(a) 1] [(b) 2] [(a) 3] [else 0]))))]
           [result (lattice-walk (list a) (list analyzer))]
           [patterns (acc-ref (walk-state result) 'patterns)])
      (assert-true (walk-ok? result))
      (assert-true (pair? patterns))
      (let ([entry (cdr (assq 'a patterns))])
        (assert-equal 1 (cdr (assq 'duplicate-datums entry))))))

  (define-test "no case expressions — no entry"
    (let* ([analyzer (make-pattern-analyzer)]
           [a (mr 'a '() '((define (foo x) (+ x 1))))]
           [result (lattice-walk (list a) (list analyzer))]
           [patterns (acc-ref (walk-state result) 'patterns)])
      (assert-true (walk-ok? result))
      (assert-equal '() patterns)))

  (define-test "nested case found"
    (let* ([analyzer (make-pattern-analyzer)]
           [a (mr 'a '() '((define (foo x y)
                              (case x
                                [(a) (case y [(1) "a1"] [(2) "a2"])]
                                [else "other"]))))]
           [result (lattice-walk (list a) (list analyzer))]
           [patterns (acc-ref (walk-state result) 'patterns)])
      (assert-true (walk-ok? result))
      ;; Inner case has no else → flagged
      (assert-true (pair? patterns))
      (let ([entry (cdr (assq 'a patterns))])
        (assert-equal 1 (cdr (assq 'cases-without-else entry)))))))

;;; ============================================================================
;;; Coverage (Export Utilization) Analyzer Tests
;;; ============================================================================

(test-group "coverage-analyzer"

  (define-test "downstream usage tracked"
    (let* ([ea (make-export-analyzer)]
           [ca (make-coverage-analyzer)]
           [a (mr 'a '() '((define (foo x) (+ x 1))
                            (define (bar y) (+ y 2))))]
           [b (mr 'b '(a) '((require 'a)
                             (define (baz x) (foo x))))]
           [result (lattice-walk (list b a) (list ea ca))]
           [state (walk-state result)]
           [coverage-db (acc-ref state 'coverage)]
           [exports (acc-ref state 'exports)])
      (assert-true (walk-ok? result))
      ;; foo should be used (referenced in b), bar should not
      (let ([unused (coverage-unused-exports coverage-db exports 'a)])
        (assert-true (pair? (memq 'bar unused)))
        (assert-false (memq 'foo unused)))))

  (define-test "no downstream usage — all exports dead"
    (let* ([ea (make-export-analyzer)]
           [ca (make-coverage-analyzer)]
           [a (mr 'a '() '((define (foo x) (+ x 1))))]
           [result (lattice-walk (list a) (list ea ca))]
           [state (walk-state result)]
           [coverage-db (acc-ref state 'coverage)]
           [exports (acc-ref state 'exports)])
      (assert-true (walk-ok? result))
      (let ([unused (coverage-unused-exports coverage-db exports 'a)])
        (assert-true (pair? (memq 'foo unused))))))

  (define-test "multiple consumers accumulate"
    (let* ([ea (make-export-analyzer)]
           [ca (make-coverage-analyzer)]
           [a (mr 'a '() '((define (foo x) x)
                            (define (bar y) y)
                            (define (baz z) z)))]
           [b (mr 'b '(a) '((require 'a)
                             (define (use-foo) (foo 1))))]
           [c (mr 'c '(a) '((require 'a)
                             (define (use-bar) (bar 2))))]
           [result (lattice-walk (list c b a) (list ea ca))]
           [state (walk-state result)]
           [coverage-db (acc-ref state 'coverage)]
           [exports (acc-ref state 'exports)])
      (assert-true (walk-ok? result))
      ;; foo used by b, bar used by c, baz used by nobody
      (let ([stats (coverage-module-stats coverage-db exports 'a)])
        (assert-equal 2 (car stats))    ; 2 used
        (assert-equal 3 (cdr stats)))   ; 3 total
      (let ([unused (coverage-unused-exports coverage-db exports 'a)])
        (assert-true (pair? (memq 'baz unused)))
        (assert-false (memq 'foo unused))
        (assert-false (memq 'bar unused)))))

  (define-test "all five analyzers compose"
    ;; Run all analyzers together
    (let* ([ea (make-export-analyzer)]
           [eff (make-effect-analyzer)]
           [ua (make-unused-requires-analyzer)]
           [pa (make-pattern-analyzer)]
           [ca (make-coverage-analyzer)]
           [a (mr 'a '() '((define (io-fn x) (display x))
                            (define (dead-fn) 42)))]
           [b (mr 'b '(a) '((require 'a)
                             (define (wrapper x)
                               (case x
                                 [(go) (io-fn x)]))))]
           [result (lattice-walk (list b a) (list ea eff ua pa ca))]
           [state (walk-state result)])
      (assert-true (walk-ok? result))
      ;; Effects: wrapper → io
      (assert-equal 'io (effect-db-lookup (acc-ref state 'effects) 'wrapper))
      ;; Patterns: b has case without else
      (assert-true (pair? (acc-ref state 'patterns)))
      ;; Coverage: dead-fn is unused
      (let ([unused (coverage-unused-exports
                     (acc-ref state 'coverage)
                     (acc-ref state 'exports)
                     'a)])
        (assert-true (pair? (memq 'dead-fn unused)))))))

(run-all-tests)
