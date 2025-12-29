;;; tests/integration/test-cas-roundtrip.ss — CAS Round-Trip Integration Tests
;;;
;;; Tests the complete workflow:
;;;   1. Create expression → Normalize → Store → Fetch → Expand
;;;   2. Type inference → Annotation → Typed evaluation
;;;
;;; Run from ccverse root: scheme --script tests/integration/test-cas-roundtrip.ss

(source-directories (cons "fabric/stitches" (source-directories)))

(load "prelude.ss")
(load "block.ss")
(load "cas.ss")
(load "normalize.ss")
(load "expand.ss")
(load "types.ss")
(load "kinds.ss")
(load "infer.ss")
(load "annotate.ss")
(load "eval.ss")
(load "typed-eval.ss")

;;; ============================================================
;;; Test Infrastructure
;;; ============================================================

(define test-count 0)
(define pass-count 0)

(define (test name expected actual)
  (set! test-count (+ test-count 1))
  (display "  ")
  (display name)
  (display ": ")
  (if (equal? expected actual)
      (begin
       (set! pass-count (+ pass-count 1))
       (display "✓\n"))
      (begin
       (display "✗\n")
       (display "    expected: ")
       (display expected)
       (display "\n    got: ")
       (display actual)
       (newline))))

;;; ============================================================
;;; Test: Expression Storage Round-Trip
;;; ============================================================

(display "\n╔══════════════════════════════════════════════════════════╗\n")
(display "║     INTEGRATION TEST: CAS Round-Trip                      ║\n")
(display "╚══════════════════════════════════════════════════════════╝\n\n")

(display "Expression Storage Round-Trip\n")
(display "─────────────────────────────\n")

;;; Store a simple value block
(let* ([blk (make-block 'data (string->utf8 "hello world") (vector))]
       [hash (store! blk)]
       [fetched (fetch hash)])
      (test "block stored and fetched" #t (block? fetched))
      (test "tag preserved" 'data (block-tag fetched))
      (test "payload preserved" "hello world" (utf8->string (block-payload fetched))))

;;; Store a block with refs
(let* ([child (make-block 'child (string->utf8 "child data") (vector))]
       [child-hash (store! child)]
       [parent (make-block 'parent (string->utf8 "parent data") (vector child-hash))]
       [parent-hash (store! parent)]
       [fetched-parent (fetch parent-hash)])
      (test "parent has ref" 1 (vector-length (block-refs fetched-parent)))
      (test "ref points to child" child-hash (vector-ref (block-refs fetched-parent) 0))
      (let ([fetched-child (fetch (vector-ref (block-refs fetched-parent) 0))])
           (test "child fetchable via ref" "child data" (utf8->string (block-payload fetched-child)))))

;;; ============================================================
;;; Test: Type Inference Round-Trip
;;; ============================================================

(display "\nType Inference Round-Trip\n")
(display "─────────────────────────\n")

;;; Type-check and evaluate simple expressions
(let ([result (typed-run 42 100)])
     (test "literal type-check" 'ok (car result))
     (test "literal is Int" 'Int (typed-type (cadr result))))

(let ([result (typed-run '(fn (x) x) 100)])
     (test "identity type-check" 'ok (car result)))

(let ([result (typed-run '((fn (x) x) 5) 100)])
     (test "application type-check" 'ok (car result))
     (test "application evaluates" 5 (typed-value (cadr result))))

;;; ============================================================
;;; Test: Evaluation Pipeline
;;; ============================================================

(display "\nEvaluation Pipeline\n")
(display "───────────────────\n")

;;; Direct evaluation of expressions
(let ([result (run '((fn (x) (prim 'add x 1)) 41) 100)])
     (test "simple function eval" 'ok (car result))
     (test "42 = 41 + 1" 42 (cadr result)))

;;; Recursive evaluation with fix
(let ([result (run '((fix fact (fn (n) (if (prim 'eq? n 0) 1 (prim 'mul n (fact (prim 'sub n 1)))))) 5) 1000)])
     (test "factorial eval" 'ok (car result))
     (test "factorial(5) = 120" 120 (cadr result)))

;;; ============================================================
;;; Summary
;;; ============================================================

(newline)
(display "╔══════════════════════════════════════════════════════════╗\n")
(display "║                    SUMMARY                                ║\n")
(display "╚══════════════════════════════════════════════════════════╝\n")
(display (string-append "  Total: " (number->string test-count) "\n"))
(display (string-append "  Passed: " (number->string pass-count) "\n"))
(display (string-append "  Failed: " (number->string (- test-count pass-count)) "\n"))

(if (= pass-count test-count)
    (display "\n✓ All integration tests passed!\n")
    (begin
     (display "\n✗ Some tests failed!\n")
     (exit 1)))
