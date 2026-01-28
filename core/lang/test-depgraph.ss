;;; core/lang/test-depgraph.ss — Tests for depgraph.ss
;;; Standardized to use test-framework.ss

(load "core/testing/test-framework.ss")
(load "core/lang/depgraph.ss")

;;; ============================================================================
;;; Tests
;;; ============================================================================

;; Build the graph first
(depgraph-refresh!)

(test-group path-basename
  (define-test "simple path"
    (assert-equal "prelude.ss" (path-basename "core/base/prelude.ss")))

  (define-test "no slash"
    (assert-equal "file.ss" (path-basename "file.ss")))

  (define-test "deep path"
    (assert-equal "deep.ss" (path-basename "a/b/c/d/deep.ss"))))

(test-group graph-building
  (define-test "graph not empty"
    (assert-true (> (hashtable-size *depgraph*) 0)))

  (define-test "reverse graph not empty"
    (assert-true (> (hashtable-size *depgraph-reverse*) 0)))

  (define-test "initialized flag"
    (assert-true *depgraph-initialized*)))

(test-group query-functions
  ;; Note: prelude now loads string.ss, so it has deps
  (define-test "prelude deps include string.ss"
    (let ([prelude-deps (depgraph-deps "core/base/prelude.ss")])
      (assert-true (if (member "core/base/string/string.ss" prelude-deps) #t #f))))

  (define-test "prelude has dependents"
    (let ([prelude-dependents (depgraph-dependents "core/base/prelude.ss")])
      (assert-true (> (length prelude-dependents) 0)))))

(test-group cycle-detection
  (define-test "no cycles"
    (assert-equal '() *depgraph-cycles*)))

(test-group cycle-edges
  (define-test "empty cycle"
    (assert-equal '() (cycle-edges '())))

  (define-test "two-node cycle"
    (assert-equal '(("a" . "b") ("b" . "a")) (cycle-edges '("a" "b"))))

  (define-test "three-node cycle"
    (assert-equal '(("a" . "b") ("b" . "c") ("c" . "a")) (cycle-edges '("a" "b" "c")))))

(test-group transitive-queries
  (define-test "infer.ss has transitive deps"
    (let ([all-deps (depgraph-all-deps "core/types/infer.ss")])
      (assert-true (> (length all-deps) 0))))

  (define-test "prelude in transitive deps"
    (let ([all-deps (depgraph-all-deps "core/types/infer.ss")])
      (assert-true (not (not (member "core/base/prelude.ss" all-deps)))))))

;;; ============================================================================
;;; Run all tests
;;; ============================================================================

(run-all-tests-and-exit)
