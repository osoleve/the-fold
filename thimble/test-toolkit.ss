;;; thimble/test-toolkit.ss — Basic Tests for Development Toolkit
;;;
;;; Smoke tests to verify toolkit components load and work.
;;;
;;; Usage: scheme --script shell/test-toolkit.ss

;;; Set up paths
(source-directories (cons "shell" (source-directories)))
(source-directories (cons "fabric/stitches" (source-directories)))

(display "═══════════════════════════════════════════════════════════\n")
(display "  TOOLKIT SMOKE TESTS\n")
(display "═══════════════════════════════════════════════════════════\n\n")

(define test-count 0)
(define pass-count 0)
(define fail-count 0)

(define (test-case name thunk)
  (set! test-count (+ test-count 1))
  (display (format "Test ~a: ~a... " test-count name))
  (guard (e [else
             (set! fail-count (+ fail-count 1))
             (display "FAIL\n")
             (display (format "  Error: ~a\n" (if (condition? e)
                                                 (condition-message e)
                                                 e)))])
    (thunk)
    (set! pass-count (+ pass-count 1))
    (display "PASS\n")))

;;; ============================================================
;;; Test Module Dependency Analyzer
;;; ============================================================

(display "\n─────────────────────────────────────────────────────────\n")
(display "Module Dependency Analyzer\n")
(display "─────────────────────────────────────────────────────────\n")

(test-case "Load module-deps.ss"
  (lambda ()
    (load "module-deps.ss")))

(test-case "Extract loads from string"
  (lambda ()
    (let ([code "(load \"foo.ss\") (load \"bar.ss\")"])
      (let ([loads (extract-loads code)])
        (unless (= (length loads) 2)
          (error 'test "Expected 2 loads"))))))

(test-case "Normalize path"
  (lambda ()
    (let ([result (normalize-path "shell/repl.ss" "fs.ss")])
      (unless (string=? result "shell/fs.ss")
        (error 'test (format "Expected shell/fs.ss, got ~a" result))))))

;;; ============================================================
;;; Test Cross-Referencer
;;; ============================================================

(display "\n─────────────────────────────────────────────────────────\n")
(display "Cross-Referencer\n")
(display "─────────────────────────────────────────────────────────\n")

(test-case "Load xref.ss"
  (lambda ()
    (load "xref.ss")))

(test-case "Find symbol in expression"
  (lambda ()
    (let ([result (find-symbol-in-expr 'foo '(lambda (x) (+ x foo)))])
      (unless result
        (error 'test "Should find 'foo in expression")))))

(test-case "Extract defined name from define"
  (lambda ()
    (let ([name (extract-defined-name '(define (foo x) x))])
      (unless (eq? name 'foo)
        (error 'test (format "Expected 'foo, got ~a" name))))))

;;; ============================================================
;;; Test Block Diff
;;; ============================================================

(display "\n─────────────────────────────────────────────────────────\n")
(display "Block Diff Utility\n")
(display "─────────────────────────────────────────────────────────\n")

(test-case "Load block-diff.ss"
  (lambda ()
    (load "block-diff.ss")))

(test-case "Blocks equal comparison"
  (lambda ()
    (load "block.ss")
    (let ([b1 (make-block 'test #vu8(1 2 3) '#())]
          [b2 (make-block 'test #vu8(1 2 3) '#())])
      (unless (block-equal? b1 b2)
        (error 'test "Identical blocks should be equal")))))

(test-case "Compute block similarity"
  (lambda ()
    (let ([b1 (make-block 'test #vu8(1 2 3 4) '#())]
          [b2 (make-block 'test #vu8(1 2 3 4) '#())])
      (let ([sim (compute-block-similarity b1 b2)])
        (unless (= sim 1.0)
          (error 'test "Identical blocks should have similarity 1.0"))))))

;;; ============================================================
;;; Test Type Inspector
;;; ============================================================

(display "\n─────────────────────────────────────────────────────────\n")
(display "Type Inspector\n")
(display "─────────────────────────────────────────────────────────\n")

(test-case "Load type-inspect.ss"
  (lambda ()
    (load "type-inspect.ss")))

(test-case "Type to string conversion"
  (lambda ()
    (let ([type-str (type->string 'Int)])
      (unless (string=? type-str "Int")
        (error 'test "Type->string failed")))))

(test-case "Function type to string"
  (lambda ()
    (let ([type-str (type->string '(→ (Int Int) Bool))])
      (unless (string-contains? type-str "→")
        (error 'test "Function type should contain arrow")))))

;;; ============================================================
;;; Test Fuel Profiler
;;; ============================================================

(display "\n─────────────────────────────────────────────────────────\n")
(display "Fuel Profiler\n")
(display "─────────────────────────────────────────────────────────\n")

(test-case "Load fuel-profile.ss"
  (lambda ()
    (load "fuel-profile.ss")))

(test-case "Find minimum fuel for simple expression"
  (lambda ()
    (load "eval.ss")
    (let ([min-fuel (find-min-fuel '(+ 1 2) 1000)])
      (unless (and (> min-fuel 0) (< min-fuel 1000))
        (error 'test "Min fuel should be reasonable")))))

;;; ============================================================
;;; Test Store Analyzer
;;; ============================================================

(display "\n─────────────────────────────────────────────────────────\n")
(display "Store Analyzer\n")
(display "─────────────────────────────────────────────────────────\n")

(test-case "Load store-analyze.ss"
  (lambda ()
    (load "store-analyze.ss")))

(test-case "Create hash set"
  (lambda ()
    (let ([hs (make-hash-set (list #vu8(1 2 3) #vu8(4 5 6)))])
      (unless (hash-set-contains? hs #vu8(1 2 3))
        (error 'test "Hash set should contain added hash")))))

(test-case "Make histogram buckets"
  (lambda ()
    (let ([buckets (make-buckets '(50 150 600 3000))])
      (unless (= (length buckets) 6)
        (error 'test "Should create 6 buckets")))))

;;; ============================================================
;;; Test Toolkit Index
;;; ============================================================

(display "\n─────────────────────────────────────────────────────────\n")
(display "Toolkit Index\n")
(display "─────────────────────────────────────────────────────────\n")

(test-case "Load toolkit.ss"
  (lambda ()
    (load "toolkit.ss")))

;;; ============================================================
;;; Summary
;;; ============================================================

(display "\n═══════════════════════════════════════════════════════════\n")
(display "  TEST SUMMARY\n")
(display "═══════════════════════════════════════════════════════════\n\n")
(display (format "Total tests:  ~a\n" test-count))
(display (format "Passed:       ~a ✓\n" pass-count))
(display (format "Failed:       ~a ✗\n" fail-count))
(display "\n")

(if (= fail-count 0)
    (begin
      (display "All tests passed! ✓\n")
      (exit 0))
    (begin
      (display "Some tests failed. ✗\n")
      (exit 1)))
