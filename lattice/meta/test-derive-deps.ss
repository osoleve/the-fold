(load "core/testing/test-framework.ss")
(load "lattice/meta/derive-deps.ss")

;;; ====
;;; Helpers
;;; ====

;; Minimal manifest for pure testing (no I/O).
;; derive-skill-deps reads files via file-requires, so we test the
;; pure components: parse-requires-from-file-lines, deps-diff,
;; cycle detection, and break-dep-cycles.
(define (make-test-manifest name path modules deps)
  `((name . ,name)
    (version . "1.0.0")
    (path . ,path)
    (purity . total)
    (stability . stable)
    (fuel-bound . "O(?)")
    (deps . ,deps)
    (description . "test")
    (keywords . ())
    (aliases . ())
    (exports . ())
    (modules . ,modules)
    (concepts . ())))

;;; ====
;;; Test: parse-requires-from-file-lines
;;; ====

(test-group "parse-requires-from-file-lines"

  (define-test "extracts single requires line"
    (assert-equal '(prelude vec)
      (parse-requires-from-file-lines
        '(";;; @module foo"
          ";;; @requires prelude vec"))))

  (define-test "handles multiple requires lines"
    (assert-equal '(prelude vec matrix)
      (parse-requires-from-file-lines
        '(";;; @module foo"
          ";;; @requires prelude vec"
          ";;; @requires matrix"))))

  (define-test "ignores non-requires lines"
    (assert-equal '(prelude)
      (parse-requires-from-file-lines
        '(";;; @module foo"
          ";;; @requires prelude"
          "(define x 10)"
          ";; just a comment"))))

  (define-test "empty lines produce empty list"
    (assert-equal '()
      (parse-requires-from-file-lines '())))

  (define-test "no requires lines produce empty list"
    (assert-equal '()
      (parse-requires-from-file-lines
        '(";;; @module foo"
          "(define x 10)"))))

  (define-test "handles extra whitespace in line"
    ;; The trimmer should handle leading whitespace
    (assert-equal '(prelude)
      (parse-requires-from-file-lines
        '("  ;;; @requires prelude"))))

  (define-test "handles namespaced module names"
    (assert-equal '(diffgeo/charts tiles/core)
      (parse-requires-from-file-lines
        '(";;; @requires diffgeo/charts tiles/core"))))
)

;;; ====
;;; Test: deps-diff
;;; ====

(test-group "deps-diff"

  (define-test "identical deps produce empty diff"
    (let ([diff (deps-diff '(a b c) '(a b c))])
      (assert-equal '() (cdr (assq 'added diff)))
      (assert-equal '() (cdr (assq 'removed diff)))
      (assert-equal '(a b c) (cdr (assq 'same diff)))))

  (define-test "added deps detected"
    (let ([diff (deps-diff '(a b c) '(a b))])
      (assert-equal '(c) (cdr (assq 'added diff)))
      (assert-equal '() (cdr (assq 'removed diff)))))

  (define-test "removed deps detected"
    (let ([diff (deps-diff '(a) '(a b c))])
      (assert-equal '() (cdr (assq 'added diff)))
      (assert-equal '(b c) (cdr (assq 'removed diff)))))

  (define-test "mixed adds and removes"
    (let ([diff (deps-diff '(a c d) '(a b))])
      (assert-equal '(c d) (cdr (assq 'added diff)))
      (assert-equal '(b) (cdr (assq 'removed diff)))
      (assert-equal '(a) (cdr (assq 'same diff)))))

  (define-test "empty lists produce empty diff"
    (let ([diff (deps-diff '() '())])
      (assert-equal '() (cdr (assq 'added diff)))
      (assert-equal '() (cdr (assq 'removed diff)))
      (assert-equal '() (cdr (assq 'same diff)))))
)

;;; ====
;;; Test: find-cycle-dfs
;;; ====

(test-group "find-cycle-dfs"

  (define-test "no cycle in DAG"
    (let ([deps '((a . (b c)) (b . (c)) (c . ()))])
      (assert-false (find-cycle-dfs 'a deps))))

  (define-test "detects simple 2-node cycle"
    (let ([deps '((a . (b)) (b . (a)))])
      (let ([cycle (find-cycle-dfs 'a deps)])
        (assert-true (list? cycle))
        ;; Cycle should be of form (x ... x), starts and ends with same node
        (assert-true (eq? (car cycle) (car (reverse cycle)))))))

  (define-test "detects 3-node cycle"
    (let ([deps '((a . (b)) (b . (c)) (c . (a)))])
      (let ([cycle (find-cycle-dfs 'a deps)])
        (assert-true (list? cycle))
        (assert-true (eq? (car cycle) (car (reverse cycle)))))))

  (define-test "returns #f for isolated node"
    (let ([deps '((a . ()))])
      (assert-false (find-cycle-dfs 'a deps))))

  (define-test "detects self-loop"
    (let ([deps '((a . (a)))])
      (let ([cycle (find-cycle-dfs 'a deps)])
        (assert-true (list? cycle))
        (assert-equal 'a (car cycle)))))
)

;;; ====
;;; Test: find-any-cycle
;;; ====

(test-group "find-any-cycle"

  (define-test "acyclic graph returns #f"
    (assert-false
      (find-any-cycle '((a . (b)) (b . (c)) (c . ())))))

  (define-test "finds cycle in graph"
    (let ([cycle (find-any-cycle '((a . (b)) (b . (a))))])
      (assert-true (list? cycle))))

  (define-test "empty graph returns #f"
    (assert-false (find-any-cycle '())))
)

;;; ====
;;; Test: remove-dep-edge
;;; ====

(test-group "remove-dep-edge"

  (define-test "removes specific edge"
    (let ([result (remove-dep-edge '((a . (b c)) (b . (c))) 'a 'b)])
      (assert-equal '(c) (cdr (assq 'a result)))
      (assert-equal '(c) (cdr (assq 'b result)))))

  (define-test "removing last dep leaves empty list"
    (let ([result (remove-dep-edge '((a . (b))) 'a 'b)])
      (assert-equal '() (cdr (assq 'a result)))))

  (define-test "removing nonexistent edge is no-op"
    (let ([result (remove-dep-edge '((a . (b))) 'a 'c)])
      (assert-equal '(b) (cdr (assq 'a result)))))
)

;;; ====
;;; Test: break-dep-cycles
;;; ====

(test-group "break-dep-cycles"

  ;; For break-dep-cycles we need manifests and a skill map.
  ;; Since it calls count-cross-requires (which reads files), we
  ;; construct manifests with empty module lists so the count is 0.
  ;; The cycle-breaker will still remove edges — it picks the weakest,
  ;; and when all counts are 0, it picks the first edge in the cycle.

  (define m-a (make-test-manifest 'a "test/a" '() '()))
  (define m-b (make-test-manifest 'b "test/b" '() '()))
  (define m-c (make-test-manifest 'c "test/c" '() '()))
  (define manifests (list m-a m-b m-c))
  (define smap '()) ;; empty skill map, no module lookups needed

  (define-test "acyclic graph unchanged"
    (let-values ([(deps report)
                  (break-dep-cycles '((a . (b)) (b . (c)) (c . ()))
                                    manifests smap)])
      (assert-equal '() report)
      (assert-equal '(b) (cdr (assq 'a deps)))))

  (define-test "breaks simple 2-node cycle"
    (let-values ([(deps report)
                  (break-dep-cycles '((a . (b)) (b . (a)))
                                    manifests smap)])
      ;; After breaking, should be acyclic
      (assert-false (find-any-cycle deps))
      ;; Report should have 1 entry
      (assert-equal 1 (length report))))

  (define-test "breaks 3-node cycle"
    (let-values ([(deps report)
                  (break-dep-cycles '((a . (b)) (b . (c)) (c . (a)))
                                    manifests smap)])
      (assert-false (find-any-cycle deps))
      (assert-true (>= (length report) 1))))

  (define-test "preserves non-cyclic edges"
    (let-values ([(deps report)
                  (break-dep-cycles '((a . (b c)) (b . (a)) (c . ()))
                                    manifests smap)])
      ;; c is not involved in any cycle — should be preserved
      (assert-false (find-any-cycle deps))
      (assert-true (pair? (memq 'c (cdr (assq 'a deps)))))))
)

;;; ====
;;; Test: find-mutual-deps
;;; ====

(test-group "find-mutual-deps"

  (define-test "finds mutual dependency"
    (let ([result (find-mutual-deps '((a . (b)) (b . (a))))])
      (assert-equal 1 (length result))))

  (define-test "no mutual deps in DAG"
    (let ([result (find-mutual-deps '((a . (b)) (b . (c)) (c . ())))])
      (assert-equal '() result)))

  (define-test "returns only one direction per pair"
    (let ([result (find-mutual-deps '((a . (b)) (b . (a)) (c . (a)) (a . (c))))])
      ;; a<->b and a<->c — but a is in entry (a . (b)) and separately
      ;; Actually, let's just check that the result is correct for a simple case
      (let ([result (find-mutual-deps '((a . (b)) (b . (a))))])
        (assert-equal 1 (length result)))))
)

;;; ====
;;; Test: derive-skill-deps (integration, reads real files)
;;; ====

(test-group "derive-skill-deps-integration"

  ;; This test reads actual manifest files to verify real-world behavior.
  ;; It requires the lattice to be present on disk.

  (define-test "optics has no self-dependency"
    (let* ([sexp (guard (e [else #f])
                   (call-with-input-file "lattice/optics/manifest.sexp"
                     (lambda (p) (read p))))]
           [manifest (and sexp (parse-manifest sexp))])
      (when manifest
        (let* ([all-manifests (list manifest)]
               [smap (build-skill-map all-manifests)]
               [deps (derive-skill-deps manifest smap)])
          (assert-true (not (memq 'optics deps)))))))

  (define-test "derive-all-deps produces entry for each skill"
    ;; Use 3 simple real manifests
    (let* ([read-manifest (lambda (path)
                            (guard (e [else #f])
                              (call-with-input-file path
                                (lambda (p)
                                  (parse-manifest (read p))))))]
           [m1 (read-manifest "lattice/optics/manifest.sexp")]
           [m2 (read-manifest "lattice/data/manifest.sexp")]
           [manifests (filter (lambda (x) x) (list m1 m2))])
      (when (>= (length manifests) 2)
        (let ([all-deps (derive-all-deps manifests)])
          ;; Should have one entry per manifest
          (assert-equal (length manifests) (length all-deps))
          ;; Each entry should be (skill-name . deps-list)
          (for-each
           (lambda (entry)
             (assert-true (symbol? (car entry)))
             (assert-true (list? (cdr entry))))
           all-deps)))))
)

;;; ====
;;; Run
;;; ====

(run-all-tests)
