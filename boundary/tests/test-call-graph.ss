;;; boundary/tests/test-call-graph.ss — Tests for Call Graph Construction
;;;
;;; Comprehensive test suite for boundary/lens/call-graph.ss
;;; Tests call graph building, caller/callee extraction, and path finding.

(load "boundary/lens/call-graph.ss")

(define test-count 0)
(define pass-count 0)
(define fail-count 0)

;;; Test assertion helper
(define (assert-equal name actual expected)
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

;;; ====
;;; Helper Function Tests
;;; ====

(printf "\n=== Testing remove-duplicates-eq ===\n")

(assert-equal "remove dups from empty list"
              (remove-duplicates-eq '())
              '())

(assert-equal "remove dups with single element"
              (remove-duplicates-eq '(a))
              '(a))

(assert-equal "remove dups with no duplicates"
              (remove-duplicates-eq '(a b c))
              '(a b c))

(assert-equal "remove dups with duplicates"
              (remove-duplicates-eq '(a b a c b d))
              '(a b c d))

(assert-equal "remove dups with all same"
              (remove-duplicates-eq '(x x x x))
              '(x))

;;; ====
;;; extract-calls-from-body Tests
;;; ====

(printf "\n=== Testing extract-calls-from-body ===\n")

(assert-equal "extract from simple call"
              (extract-calls-from-body '(foo x) 'test)
              '(foo x))

(assert-equal "extract from nested calls"
              (sort (lambda (a b) (string<? (symbol->string a) (symbol->string b)))
                    (extract-calls-from-body '(+ (foo x) (bar y)) 'test))
              '(+ bar foo x y))

(assert-equal "extract ignores definer"
              (member 'test (extract-calls-from-body '(test x) 'test))
              #f)

(assert-equal "extract from lambda body"
              (let ([result (extract-calls-from-body '(lambda (x) (foo x)) 'test)])
                   ;; x is a local param, foo should be extracted
                   (member 'foo result))
              #t)

(assert-equal "lambda params are excluded"
              (let ([result (extract-calls-from-body '(lambda (param) param) 'test)])
                   (member 'param result))
              #f)

(assert-equal "extract from let bindings"
              (let ([result (extract-calls-from-body
                             '(let ([x 1]) (foo x))
                             'test)])
                   (member 'foo result))
              #t)

(assert-equal "let-bound vars excluded"
              (let ([result (extract-calls-from-body
                             '(let ([local 1]) local)
                             'test)])
                   (member 'local result))
              #f)

;;; ====
;;; extract-definition-info Tests
;;; ====

(printf "\n=== Testing extract-definition-info ===\n")

(assert-equal "extract simple define"
              (let ([info (extract-definition-info '(define foo 42))])
                   (and info (car info)))
              'foo)

(assert-equal "extract function define"
              (let ([info (extract-definition-info '(define (bar x) (+ x 1)))])
                   (and info (car info)))
              'bar)

(assert-equal "extract define-syntax"
              (let ([info (extract-definition-info '(define-syntax mac (syntax-rules () ())))])
                   (and info (car info)))
              'mac)

(assert-equal "non-definition returns #f"
              (extract-definition-info '(if a b c))
              #f)

(assert-equal "non-pair returns #f"
              (extract-definition-info 'symbol)
              #f)

;;; ====
;;; find-scheme-files Tests
;;; ====

(printf "\n=== Testing find-scheme-files ===\n")

(assert-true "finds .ss files in core"
             (let ([files (find-scheme-files "core")])
                  (and (list? files)
                       (> (length files) 0))))

(assert-true "all results are .ss files"
             (let ([files (find-scheme-files "core")])
                  (andmap (lambda (f) (string-suffix? ".ss" f)) files)))

(assert-true "skips .git directory"
             (let ([files (find-scheme-files ".")])
                  (not (ormap (lambda (f) (string-contains? f ".git/")) files))))

(assert-equal "non-existent dir returns empty"
              (find-scheme-files "/nonexistent/path")
              '())

;;; ====
;;; Call Graph Operations Tests
;;; ====

(printf "\n=== Testing call-graph-built? ===\n")

;; Clear state before testing
(hashtable-clear! *forward-calls*)
(hashtable-clear! *reverse-calls*)
(set! *call-graph-built?* #f)

(assert-false "graph not built initially"
              (call-graph-built?))

;;; Build graph for testing
(printf "\n=== Building call graph for tests ===\n")
(call-graph-refresh!)

(assert-true "graph built after refresh"
             (call-graph-built?))

;;; ====
;;; call-graph-callers/callees Tests
;;; ====

(printf "\n=== Testing call-graph-callers/callees ===\n")

(assert-true "callers returns a list"
             (list? (call-graph-callers 'foo)))

(assert-true "callees returns a list"
             (list? (call-graph-callees 'foo)))

(assert-true "known function has callees"
             ;; call-graph-refresh! should have callees like display, for-each, etc.
             (let ([callees (call-graph-callees 'call-graph-refresh!)])
                  (> (length callees) 0)))

;;; ====
;;; call-graph-path Tests
;;; ====

(printf "\n=== Testing call-graph-path ===\n")

(assert-true "path to self is nil or trivial"
             (let ([paths (call-graph-path 'call-graph-refresh! 'call-graph-refresh! 5)])
                  (or (not paths)
                      (null? paths)
                      (and (list? paths) (list? (car paths))))))

(assert-true "path with max-depth 0 is limited"
             (let ([paths (call-graph-path 'call-graph-refresh! 'display 0)])
                  ;; Either no paths or they're length 1 max
                  (or (not paths)
                      (null? paths)
                      (andmap (lambda (p) (<= (length p) 1)) paths))))

(assert-true "path result is list of lists when found"
             (let ([paths (call-graph-path 'call-graph-refresh! 'display 5)])
                  (or (not paths)
                      (and (list? paths)
                           (andmap list? paths)))))

;;; ====
;;; filter-map Tests
;;; ====

(printf "\n=== Testing filter-map ===\n")

(assert-equal "filter-map with identity-ish"
              (filter-map (lambda (x) (and (even? x) x)) '(1 2 3 4 5 6))
              '(2 4 6))

(assert-equal "filter-map with all #f"
              (filter-map (lambda (x) #f) '(1 2 3))
              '())

(assert-equal "filter-map with all passing"
              (filter-map (lambda (x) (* x 2)) '(1 2 3))
              '(2 4 6))

(assert-equal "filter-map empty list"
              (filter-map (lambda (x) x) '())
              '())

;;; ====
;;; Test Summary
;;; ====

(printf "\n====\n")
(printf "Test Results (call-graph):\n")
(printf "  Total:  ~a\n" test-count)
(printf "  Passed: ~a\n" pass-count)
(printf "  Failed: ~a\n" fail-count)

(if (= fail-count 0)
    (printf "\n✓ All tests passed!\n\n")
    (printf "\n✗ Some tests failed\n\n"))

;;; Return success/failure
(= fail-count 0)
