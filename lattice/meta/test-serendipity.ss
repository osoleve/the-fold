(load "core/testing/test-framework.ss")
(load "lattice/meta/meta.ss")
(load "lattice/meta/serendipity.ss")

;; Initialize lattice tooling (uses cache if available)
(lattice-ensure!)

;;; ====
;;; Co-Module Exports
;;; ====

(test-group "co-module-exports"

  (define-test "vec2-add has vec2 siblings"
    (let ([siblings (co-module-exports 'vec2-add)])
      ;; vec2-add is in the vec2 module alongside vec2-sub, vec2-dot, etc.
      (assert-true (> (length siblings) 5))
      ;; Should not include the input symbol
      (assert-false (memq 'vec2-add siblings))))

  (define-test "unknown symbol returns empty"
    (assert-equal '() (co-module-exports 'nonexistent-symbol-xyz)))

  (define-test "export-module finds module"
    (let ([mod (export-module 'vec2-add)])
      ;; Should resolve to vec2 module
      (assert-true (symbol? mod))))

  (define-test "export-skill finds skill"
    (let ([skill (export-skill 'vec2-add)])
      (assert-equal 'linalg skill)))
)

;;; ====
;;; Co-Skill Exports
;;; ====

(test-group "co-skill-exports"

  (define-test "vec2-add has co-skill exports from other linalg modules"
    (let ([peers (co-skill-exports 'vec2-add 10)])
      ;; Should find exports from matrix, vec, etc. (same skill, different module)
      (assert-true (> (length peers) 0))
      ;; Each entry is (export-name . module-name)
      (assert-true (pair? (car peers)))))

  (define-test "unknown symbol returns empty"
    (assert-equal '() (co-skill-exports 'nonexistent-symbol-xyz 10)))
)

;;; ====
;;; Export Neighbors
;;; ====

(test-group "export-neighbors"

  (define-test "vec2-add has similar exports"
    (let ([neighbors (export-neighbors 'vec2-add 5)])
      ;; Should find vec3-add, vec-add, etc.
      (assert-true (> (length neighbors) 0))
      ;; Each entry is (name . score)
      (assert-true (number? (cdar neighbors)))))

  (define-test "does not include self"
    (let ([neighbors (export-neighbors 'vec2-add 20)])
      (assert-false (assq 'vec2-add neighbors))))
)

;;; ====
;;; Related Skills
;;; ====

(test-group "related-skills"

  (define-test "linalg has dependents"
    (let* ([related (related-skills 'linalg)]
           [dependents (cadr (assq 'dependents (cdr related)))])
      ;; linalg is tier 0 — many skills depend on it
      (assert-true (> (length dependents) 0))))

  (define-test "find-siblings works"
    ;; A skill with dependencies should have siblings (other skills sharing deps)
    (let* ([related (related-skills 'autodiff)]
           [siblings (cadr (assq 'siblings (cdr related)))])
      ;; autodiff depends on linalg/data — other skills also depend on these
      (assert-true (list? siblings))))

  (define-test "structure is correct"
    (let ([related (related-skills 'linalg)])
      (assert-true (pair? (assq 'skill (cdr related))))
      (assert-true (pair? (assq 'deps (cdr related))))
      (assert-true (pair? (assq 'dependents (cdr related))))
      (assert-true (pair? (assq 'siblings (cdr related))))
      (assert-true (pair? (assq 'nearby (cdr related))))))
)

;;; ====
;;; Browse
;;; ====

(test-group "browse-from"

  (define-test "browse export returns full context"
    (let ([result (browse-from 'vec2-add)])
      (assert-true (pair? (assq 'found? (cdr result))))
      (assert-true (cadr (assq 'found? (cdr result))))
      (assert-equal 'export (cadr (assq 'type (cdr result))))
      ;; Should have module and skill info
      (assert-true (pair? (assq 'module (cdr result))))
      (assert-true (pair? (assq 'skill (cdr result))))
      ;; Should have related data
      (assert-true (pair? (assq 'same-module (cdr result))))
      (assert-true (pair? (assq 'similar (cdr result))))))

  (define-test "browse skill returns DAG context"
    (let ([result (browse-from 'linalg)])
      (assert-true (cadr (assq 'found? (cdr result))))
      (assert-equal 'skill (cadr (assq 'type (cdr result))))
      (assert-true (pair? (assq 'deps (cdr result))))
      (assert-true (pair? (assq 'dependents (cdr result))))))

  (define-test "browse unknown returns suggestions"
    (let ([result (browse-from 'nonexistent-symbol-xyz)])
      (assert-false (cadr (assq 'found? (cdr result))))
      (assert-true (pair? (assq 'suggestions (cdr result))))))
)

;;; ====
;;; Pretty Printing (smoke test)
;;; ====

(test-group "pretty-printing"

  (define-test "print-browse export runs without error"
    (let ([out (with-output-to-string (lambda () (print-browse 'vec2-add)))])
      (assert-true (> (string-length out) 10))))

  (define-test "print-browse skill runs without error"
    (let ([out (with-output-to-string (lambda () (print-browse 'linalg)))])
      (assert-true (> (string-length out) 10))))

  (define-test "print-browse unknown runs without error"
    (let ([out (with-output-to-string (lambda () (print-browse 'zzz-fake-symbol)))])
      (assert-true (> (string-length out) 5))))

  (define-test "lr is an alias for print-browse"
    (let ([out (with-output-to-string (lambda () (lr 'vec2-add)))])
      (assert-true (> (string-length out) 10))))
)

(run-all-tests)
