;;; core/lang/test-depgraph.ss — Tests for depgraph.ss
;;;
;;; Dependencies:
;;;   - depgraph.ss

(load "core/lang/depgraph.ss")

(define tests-passed 0)
(define tests-failed 0)

(define (test name expected actual)
  (if (equal? expected actual)
      (begin
       (set! tests-passed (+ tests-passed 1))
       (display "  ✓ ") (display name) (newline))
      (begin
       (set! tests-failed (+ tests-failed 1))
       (display "  ✗ ") (display name)
       (display " — expected ") (write expected)
       (display ", got ") (write actual)
       (newline))))

(display "\nTesting depgraph.ss\n")
(display "===================\n\n")

;;; Build the graph first
(display "Building graph...\n")
(depgraph-refresh!)
(display "\n")

;;; Test path-basename
(display "path-basename:\n")
(test "simple path" "prelude.ss" (path-basename "core/base/prelude.ss"))
(test "no slash" "file.ss" (path-basename "file.ss"))
(test "deep path" "deep.ss" (path-basename "a/b/c/d/deep.ss"))

;;; Test graph building
(display "\ngraph building:\n")
(test "graph not empty" #t (> (hashtable-size *depgraph*) 0))
(test "reverse graph not empty" #t (> (hashtable-size *depgraph-reverse*) 0))
(test "initialized flag" #t *depgraph-initialized*)

;;; Test queries
(display "\nquery functions:\n")
(let ([prelude-deps (depgraph-deps "core/base/prelude.ss")])
     (test "prelude has no deps" '() prelude-deps))

(let ([prelude-dependents (depgraph-dependents "core/base/prelude.ss")])
     (test "prelude has dependents" #t (> (length prelude-dependents) 0)))

;;; Test cycle detection (should be empty for clean codebase)
(display "\ncycle detection:\n")
(test "no cycles" '() *depgraph-cycles*)

;;; Test cycle-edges helper
(display "\ncycle-edges:\n")
(test "empty cycle" '() (cycle-edges '()))
(test "two-node cycle" '(("a" . "b") ("b" . "a")) (cycle-edges '("a" "b")))
(test "three-node cycle" '(("a" . "b") ("b" . "c") ("c" . "a")) (cycle-edges '("a" "b" "c")))

;;; Test transitive dependencies
(display "\ntransitive queries:\n")
(let ([all-deps (depgraph-all-deps "core/types/infer.ss")])
     (test "infer.ss has transitive deps" #t (> (length all-deps) 0))
     (test "prelude in transitive deps" #t
           (not (not (member "core/base/prelude.ss" all-deps)))))

;;; Summary
(newline)
(display "===================\n")
(display "Tests passed: ") (display tests-passed) (newline)
(display "Tests failed: ") (display tests-failed) (newline)
(newline)

(when (> tests-failed 0)
      (error 'test-depgraph "Some tests failed"))
