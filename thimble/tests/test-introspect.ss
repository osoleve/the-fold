;;; thimble/introspect/test-introspect.ss — Tests for Introspection Tools
;;;
;;; Tests timing, memory, and complexity analysis modules.

(load "thimble/introspect/timing.ss")
(load "thimble/introspect/memory.ss")
(load "thimble/introspect/complexity.ss")

;;; ============================================================
;;; Test Harness
;;; ============================================================

(define test-count 0)
(define pass-count 0)
(define fail-count 0)

(define (test name expected actual)
  (set! test-count (+ test-count 1))
  (display "  ")
  (display name)
  (display ": ")
  (if (equal? expected actual)
      (begin
       (set! pass-count (+ pass-count 1))
       (display "✓"))
      (begin
       (set! fail-count (+ fail-count 1))
       (display "✗\n    expected: ")
       (display expected)
       (display "\n    got: ")
       (display actual)))
  (newline))

(define (test-pred name pred actual)
  (set! test-count (+ test-count 1))
  (display "  ")
  (display name)
  (display ": ")
  (if (pred actual)
      (begin
       (set! pass-count (+ pass-count 1))
       (display "✓"))
      (begin
       (set! fail-count (+ fail-count 1))
       (display "✗\n    failed predicate on: ")
       (display actual)))
  (newline))

(define (test-section name)
  (newline)
  (display "=== ")
  (display name)
  (display " ===")
  (newline))

(define (test-summary)
  (newline)
  (display "---")
  (newline)
  (display (format "Total: ~a  Passed: ~a  Failed: ~a\n"
                   test-count pass-count fail-count))
  (if (= fail-count 0)
      (display "All tests passed!\n")
      (display "SOME TESTS FAILED\n")))

;;; ============================================================
;;; Timing Tests
;;; ============================================================

(test-section "Timing Module")

;; Test current-nanoseconds returns positive number
(test-pred "current-nanoseconds returns positive"
           (lambda (x) (> x 0))
           (current-nanoseconds))

;; Test current-cpu-nanoseconds returns non-negative
(test-pred "current-cpu-nanoseconds returns non-negative"
           (lambda (x) (>= x 0))
           (current-cpu-nanoseconds))

;; Test time-thunk produces timing-result
(let ([result (time-thunk (lambda () (+ 1 2)))])
     (test "time-thunk returns timing-result"
           #t
           (timing-result? result))
     (test "time-thunk captures correct value"
           3
           (timing-result-value result))
     (test-pred "time-thunk elapsed-ns >= 0"
                (lambda (x) (>= x 0))
                (timing-result-elapsed-ns result)))

;; Test compute-stats
(let ([stats (compute-stats '(100 200 300 400 500))])
     (test "stats min-ns"
           100
           (timing-stats-min-ns stats))
     (test "stats max-ns"
           500
           (timing-stats-max-ns stats))
     (test-pred "stats mean-ns close to 300"
                (lambda (x) (< (abs (- x 300)) 0.01))
                (timing-stats-mean-ns stats))
     (test "stats median-ns"
           300.0
           (timing-stats-median-ns stats)))

;; Test benchmark runs correct iterations
(let ([counter 0]
      [result (benchmark "counter-test" 5
                         (lambda ()
                                 (set! counter (+ counter 1))
                                 counter))])
     (test "benchmark runs correct iterations"
           5
           counter)
     (test "benchmark-result iterations field"
           5
           (benchmark-result-iterations result))
     (test "benchmark-result has stats"
           #t
           (timing-stats? (benchmark-result-stats result))))

;; Test format-ns
(test "format-ns nanoseconds"
      "500ns"
      (format-ns 500))
(test "format-ns microseconds"
      "1.50μs"
      (format-ns 1500))
(test "format-ns milliseconds"
      "1.50ms"
      (format-ns 1500000))
(test "format-ns seconds"
      "1.50s"
      (format-ns 1500000000))

;;; ============================================================
;;; Memory Tests
;;; ============================================================

(test-section "Memory Module")

;; Test current-memory-snapshot
(let ([snap (current-memory-snapshot)])
     (test "current-memory-snapshot returns snapshot"
           #t
           (memory-snapshot? snap))
     (test-pred "snapshot bytes-allocated > 0"
                (lambda (x) (> x 0))
                (memory-snapshot-bytes-allocated snap))
     (test-pred "snapshot bytes-in-use > 0"
                (lambda (x) (> x 0))
                (memory-snapshot-bytes-in-use snap)))

;; Test memory-thunk
(let ([result (memory-thunk (lambda () (make-bytevector 1000 0)))])
     (test "memory-thunk returns memory-result"
           #t
           (memory-result? result))
     (test "memory-thunk captures result"
           1000
           (bytevector-length (memory-result-value result)))
     (test-pred "memory-thunk delta allocated >= 1000"
                (lambda (x) (>= x 1000))
                (memory-delta-bytes-allocated (memory-result-delta result))))

;; Test gc-and-measure
(let ([snap (gc-and-measure)])
     (test "gc-and-measure returns snapshot"
           #t
           (memory-snapshot? snap)))

;; Test format-bytes
(test "format-bytes bytes"
      "500B"
      (format-bytes 500))
(test "format-bytes KB"
      "1.50KB"
      (format-bytes 1536))
(test "format-bytes MB"
      "1.50MB"
      (format-bytes (* 1536 1024)))
(test "format-bytes GB"
      "1.50GB"
      (format-bytes (* 1536 1024 1024)))

;;; ============================================================
;;; Complexity Tests
;;; ============================================================

(test-section "Complexity Module")

;; Test line-type classification
(test "line-type blank"
      'blank
      (line-type "   "))
(test "line-type comment"
      'comment
      (line-type "  ;;; a comment"))
(test "line-type code"
      'code
      (line-type "(define x 42)"))

;; Test string-trim
(test "string-trim removes whitespace"
      "hello"
      (string-trim "  hello  "))
(test "string-trim empty string"
      ""
      (string-trim "   "))

;; Test sexp-size
(test "sexp-size atom"
      1
      (sexp-size 'foo))
(test "sexp-size simple list"
      4  ; (+ x 1) = cons, +, cons, x, cons, 1, nil -> 4 nodes really
      (sexp-size '(+ x 1)))

;; Test sexp-depth
(test "sexp-depth atom"
      0
      (sexp-depth 'foo))
(test "sexp-depth flat list"
      2
      (sexp-depth '(a b c)))
(test "sexp-depth nested"
      3
      (sexp-depth '(a (b c))))

;; Test definition detection
(test "definition? recognizes define"
      #t
      (definition? '(define x 42)))
(test "definition? recognizes define with args"
      #t
      (definition? '(define (foo x) x)))
(test "definition? rejects non-definition"
      #f
      (definition? '(+ 1 2)))

;; Test extract-definition-name
(test "extract-definition-name simple"
      'x
      (extract-definition-name '(define x 42)))
(test "extract-definition-name function"
      'foo
      (extract-definition-name '(define (foo x) x)))
(test "extract-definition-name record"
      'point
      (extract-definition-name '(define-record-type point (fields x y))))

;; Test extract-references
(test "extract-references finds symbols"
      '(+ x y)
      (extract-references '(+ x y)))

;; Test unique
(test "unique removes duplicates"
      '(a b c)
      (unique '(a b a c b c)))

;; Test complexity-score
(let ([def (make-definition-info 'test 'define 1 10 3 '(a b c))])
     (test-pred "complexity-score returns positive"
                (lambda (x) (> x 0))
                (complexity-score def)))

;;; ============================================================
;;; Integration Test: Analyze this file
;;; ============================================================

(test-section "Integration: Self-Analysis")

;; Analyze this test file
(let ([metrics (analyze-file "shell/introspect/test-introspect.ss")])
     (test "analyze-file returns file-metrics"
           #t
           (file-metrics? metrics))
     (test-pred "file has > 100 lines"
                (lambda (x) (> x 100))
                (file-metrics-total-lines metrics))
     (test-pred "file has some code lines"
                (lambda (x) (> x 0))
                (file-metrics-code-lines metrics))
     (test-pred "file has some definitions"
                (lambda (x) (> x 0))
                (length (file-metrics-definitions metrics)))
     (test-pred "file has load dependencies"
                (lambda (x) (>= x 3))  ; We load 3 files
                (length (file-metrics-load-deps metrics))))

;;; ============================================================
;;; Summary
;;; ============================================================

(test-summary)
