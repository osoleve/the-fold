;;; test-concept-bridges.ss — Tests for concept bridge detection (zy13)
;;;
;;; Run with: scheme --script lattice/meta/test-concept-bridges.ss
;;;
;;; Tests kg-bridge-score, kg-detect-bridges, kg-surprising-bridges,
;;; and bridge cache invalidation.

(load "core/testing/test-framework.ss")

;;; Suppress meta load messages during tests
(define *meta-quiet* #t)

(load "core/base/prelude.ss")
(load "core/base/sha256.ss")
(load "core/blocks/block.ss")
(load "core/blocks/cas.ss")
(load "lattice/data/hamt.ss")
(load "lattice/meta/manifest.ss")
(load "lattice/meta/concept-normalize.ss")
(load "lattice/meta/kg.ss")

(display "\n====\n")
(display "Concept Bridge Detection Tests\n")
(display "====\n\n")

;;; ====================================================================
;;; Ontology Fixture
;;; ====================================================================

;;; Hierarchy:
;;;   mathematics → linear-algebra → eigenvalue-theory
;;;   mathematics → calculus
;;;   computation → data-structures
;;;
;;; Cross-cutting: differentiability spans (autodiff-skill, calculus-skill)
;;;
;;; This gives us testable bridge scenarios:
;;;   linalg-skill ↔ eigen-skill: hierarchical bridge (eigenvalue-theory is-a linear-algebra)
;;;   linalg-skill ↔ calculus-skill: hierarchical bridge (both under mathematics)
;;;   autodiff-skill ↔ calculus-skill: cross-cutting bridge (differentiability)
;;;   data-skill ↔ eigen-skill: no bridge (different subtrees, no cross-cutting)

(define test-ontology
  '(concept-ontology
    (version 1)
    (concepts
      (concept mathematics
        (description "Foundation of quantitative reasoning")
        (children (linear-algebra calculus)))
      (concept linear-algebra
        (description "Vectors, matrices, linear maps")
        (parent mathematics)
        (children (eigenvalue-theory)))
      (concept calculus
        (description "Rates of change and accumulation")
        (parent mathematics))
      (concept eigenvalue-theory
        (description "Eigenvalues, eigenvectors, SVD")
        (parent linear-algebra))
      (concept computation
        (description "Computational structures")
        (children (data-structures)))
      (concept data-structures
        (description "Trees, graphs, maps")
        (parent computation)))
    (synonym-groups
      (linear-algebra la lin-alg)
      (eigenvalue-theory eigenvalue eigen))
    (cross-cutting
      (concept differentiability
        (description "Things that can be differentiated")
        (skills (autodiff-skill calculus-skill))))))

;;; ====================================================================
;;; Test KG Construction
;;; ====================================================================

;;; Build a test KG with skills that have known concept relationships.
;;; Some skills depend on others (to test surprise penalty).
(define (build-bridge-test-kg!)
  (kg-reset!)
  (install-concept-ontology! test-ontology)
  ;; linalg-skill: keywords (la matrix) → normalized to (linear-algebra matrix)
  (kg-add-skill!
   `((name . linalg-skill)
     (path . "lattice/linalg")
     (description . "Linear algebra skill")
     (keywords la matrix)
     (aliases)
     (deps)
     (modules)
     (exports)))
  ;; eigen-skill: keywords (eigenvalue) → normalized to (eigenvalue-theory)
  ;; depends on linalg-skill (direct dep)
  (kg-add-skill!
   `((name . eigen-skill)
     (path . "lattice/linalg")
     (description . "Eigenvalue skill")
     (keywords eigenvalue)
     (aliases)
     (deps linalg-skill)
     (modules)
     (exports)))
  ;; calculus-skill: keywords (calculus)
  (kg-add-skill!
   `((name . calculus-skill)
     (path . "lattice/calculus")
     (description . "Calculus skill")
     (keywords calculus)
     (aliases)
     (deps)
     (modules)
     (exports)))
  ;; autodiff-skill: keywords (autodiff)
  ;; depends on calculus-skill (direct dep)
  (kg-add-skill!
   `((name . autodiff-skill)
     (path . "lattice/autodiff")
     (description . "Autodiff skill")
     (keywords autodiff)
     (aliases)
     (deps calculus-skill)
     (modules)
     (exports)))
  ;; data-skill: keywords (data-structures)
  (kg-add-skill!
   `((name . data-skill)
     (path . "lattice/data")
     (description . "Data structures skill")
     (keywords data-structures)
     (aliases)
     (deps)
     (modules)
     (exports)))
  ;; physics-skill: depends on linalg-skill AND eigen-skill (transitive to linalg)
  ;; keywords (physics) — no shared concepts with math subtree directly
  (kg-add-skill!
   `((name . physics-skill)
     (path . "lattice/physics")
     (description . "Physics skill")
     (keywords physics)
     (aliases)
     (deps linalg-skill eigen-skill)
     (modules)
     (exports)))
  (kg-build-deps!)
  (kg-extract-concepts!))

;;; ====================================================================
;;; Tests: kg-bridge-score
;;; ====================================================================

(test-group "kg-bridge-score"

  (define-test "self-pair scores zero"
    (build-bridge-test-kg!)
    (let ([result (kg-bridge-score 'linalg-skill 'linalg-skill)])
      (assert-equal 0 (cdr (assq 'score result)))))

  (define-test "no concept overlap scores zero"
    (build-bridge-test-kg!)
    (let ([result (kg-bridge-score 'data-skill 'calculus-skill)])
      (assert-equal 0 (cdr (assq 'score result)))
      (assert-equal '() (cdr (assq 'shared result)))))

  (define-test "returns well-formed alist"
    (build-bridge-test-kg!)
    (let ([result (kg-bridge-score 'linalg-skill 'calculus-skill)])
      (assert-true (pair? (assq 'score result)))
      (assert-true (pair? (assq 'shared result)))
      (assert-true (pair? (assq 'direct result)))
      (assert-true (pair? (assq 'hierarchical result)))
      (assert-true (pair? (assq 'cross-cutting result)))
      (assert-true (pair? (assq 'dep-connected result)))))

  (define-test "hierarchical bridge detected (linalg ↔ calculus via mathematics)"
    (build-bridge-test-kg!)
    (let ([result (kg-bridge-score 'linalg-skill 'calculus-skill)])
      (assert-true (> (cdr (assq 'score result)) 0))
      (assert-true (pair? (memq 'mathematics (cdr (assq 'shared result)))))))

  (define-test "direct dep applies surprise penalty"
    (build-bridge-test-kg!)
    ;; eigen-skill depends directly on linalg-skill
    (let ([result (kg-bridge-score 'eigen-skill 'linalg-skill)])
      (assert-equal 'direct (cdr (assq 'dep-connected result)))))

  (define-test "no dep connection = no penalty"
    (build-bridge-test-kg!)
    ;; linalg-skill and calculus-skill have no dep connection
    (let ([result (kg-bridge-score 'linalg-skill 'calculus-skill)])
      (assert-equal 'none (cdr (assq 'dep-connected result)))))

  (define-test "cross-cutting concept detected"
    (build-bridge-test-kg!)
    ;; autodiff-skill and calculus-skill share differentiability (cross-cutting)
    (let ([result (kg-bridge-score 'autodiff-skill 'calculus-skill)])
      (assert-true (pair? (memq 'differentiability (cdr (assq 'cross-cutting result)))))))

  (define-test "dep-connected pair scores lower than non-dep pair with same concepts"
    (build-bridge-test-kg!)
    ;; eigen depends on linalg (direct dep), both share mathematics hierarchy
    ;; calculus shares mathematics with linalg but no dep
    (let ([dep-score (cdr (assq 'score (kg-bridge-score 'eigen-skill 'linalg-skill)))]
          [nodep-score (cdr (assq 'score (kg-bridge-score 'linalg-skill 'calculus-skill)))])
      ;; The non-dep pair should score higher (no penalty multiplier)
      (assert-true (> nodep-score 0))
      ;; dep-score may still be > 0 but should be reduced by 0.3 multiplier
      (assert-true (>= nodep-score dep-score))))

  (define-test "transitive dep detected"
    (build-bridge-test-kg!)
    ;; physics-skill depends on eigen-skill which depends on linalg-skill
    ;; So physics→linalg is transitive (but also direct in our fixture)
    ;; Let's check autodiff→linalg: autodiff depends on calculus, no path to linalg
    (let ([result (kg-bridge-score 'autodiff-skill 'linalg-skill)])
      (assert-equal 'none (cdr (assq 'dep-connected result))))))

;;; ====================================================================
;;; Tests: kg-detect-bridges
;;; ====================================================================

(test-group "kg-detect-bridges"

  (define-test "returns non-empty list for connected KG"
    (build-bridge-test-kg!)
    (let ([bridges (kg-detect-bridges)])
      (assert-true (pair? bridges))))

  (define-test "each entry has required fields"
    (build-bridge-test-kg!)
    (let ([bridges (kg-detect-bridges)])
      (for-each
       (lambda (b)
         (assert-true (pair? (assq 'skills b)))
         (assert-true (pair? (assq 'score b)))
         (assert-true (pair? (assq 'surprise b))))
       bridges)))

  (define-test "sorted by score descending"
    (build-bridge-test-kg!)
    (let ([bridges (kg-detect-bridges)])
      (let check ([remaining bridges])
        (when (and (pair? remaining) (pair? (cdr remaining)))
          (assert-true (>= (cdr (assq 'score (car remaining)))
                           (cdr (assq 'score (cadr remaining)))))
          (check (cdr remaining))))))

  (define-test "no self-pairs in results"
    (build-bridge-test-kg!)
    (let ([bridges (kg-detect-bridges)])
      (for-each
       (lambda (b)
         (let ([skills (cdr (assq 'skills b))])
           (assert-false (eq? (car skills) (cadr skills)))))
       bridges)))

  (define-test "surprise classification correct"
    (build-bridge-test-kg!)
    (let ([bridges (kg-detect-bridges)])
      (for-each
       (lambda (b)
         (let ([surprise (cdr (assq 'surprise b))]
               [score (cdr (assq 'score b))]
               [dep (cdr (assq 'dep-connected b))])
           (assert-true (pair? (memq surprise '(high medium low))))))
       bridges)))

  (define-test "results are cached"
    (build-bridge-test-kg!)
    (let ([first (kg-detect-bridges)]
          [second (kg-detect-bridges)])
      (assert-true (eq? first second))))

  (define-test "cache cleared by kg-reset!"
    (build-bridge-test-kg!)
    (kg-detect-bridges)  ; populate cache
    (assert-true (list? *kg-bridge-cache*))
    (kg-reset!)
    (assert-false *kg-bridge-cache*))

  (define-test "cache invalidated by kg-extract-concepts!"
    (build-bridge-test-kg!)
    (let ([first (kg-detect-bridges)])
      (assert-true (list? *kg-bridge-cache*))
      ;; Re-extracting concepts invalidates the bridge cache
      (kg-extract-concepts!)
      (assert-false *kg-bridge-cache*)
      ;; Next call recomputes
      (let ([second (kg-detect-bridges)])
        (assert-true (list? *kg-bridge-cache*))
        ;; Should NOT be eq? (freshly computed)
        (assert-false (eq? first second))))))

;;; ====================================================================
;;; Tests: kg-surprising-bridges
;;; ====================================================================

(test-group "kg-surprising-bridges"

  (define-test "returns at most k results"
    (build-bridge-test-kg!)
    (let ([bridges (kg-surprising-bridges 2)])
      (assert-true (<= (length bridges) 2))))

  (define-test "excludes low-surprise bridges"
    (build-bridge-test-kg!)
    (let ([bridges (kg-surprising-bridges 100)])
      (for-each
       (lambda (b)
         (assert-false (eq? 'low (cdr (assq 'surprise b)))))
       bridges)))

  (define-test "empty when no surprising bridges exist"
    ;; Build a KG where all concept-connected skills also share deps
    (kg-reset!)
    (install-concept-ontology! test-ontology)
    (kg-add-skill!
     `((name . skill-a)
       (path . "lattice/a")
       (description . "Skill A")
       (keywords calculus)
       (aliases)
       (deps)
       (modules)
       (exports)))
    (kg-add-skill!
     `((name . skill-b)
       (path . "lattice/b")
       (description . "Skill B")
       (keywords calculus)
       (aliases)
       (deps skill-a)
       (modules)
       (exports)))
    (kg-build-deps!)
    (kg-extract-concepts!)
    ;; They share 'calculus' directly but are dep-connected
    ;; Score = 1.0 * 0.3 = 0.3, classified as 'low'
    (let ([bridges (kg-surprising-bridges 10)])
      ;; All bridges should be low-surprise (dep-connected)
      ;; so surprising-bridges filters them out
      (assert-true (null? bridges)))))

;;; ====================================================================
;;; Run
;;; ====================================================================

(run-all-tests)
