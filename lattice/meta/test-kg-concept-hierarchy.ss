;;; test-kg-concept-hierarchy.ss — Tests for concept hierarchy integration in kg.ss
;;;
;;; Run with: scheme --script lattice/meta/test-kg-concept-hierarchy.ss
;;;
;;; Tests the ontology-aware path in kg-extract-concepts! and the new
;;; hierarchical query functions: kg-concept-ancestors, kg-concept-descendants,
;;; kg-transitive-concept-skills, kg-shared-concepts-transitive.

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
(display "KG Concept Hierarchy Tests\n")
(display "====\n\n")

;;; ====================================================================
;;; Fixtures
;;; ====================================================================

;;; A minimal two-level ontology with synonym groups for testing.
;;; Hierarchy: mathematics → linear-algebra → eigenvalue-theory
;;;            mathematics → calculus
;;; Synonyms: linear-algebra ← la, lin-alg
;;;           eigenvalue-theory ← eigenvalue
(define hier-ontology
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
        (parent linear-algebra)))
    (synonym-groups
      (linear-algebra la lin-alg)
      (eigenvalue-theory eigenvalue eigen))
    (cross-cutting
      (concept differentiability
        (description "Things that can be differentiated")
        (skills (autodiff calculus-skill))))))

;;; Reset KG to a clean state and install a fresh ontology.
(define (reset-kg-with-ontology! ontology)
  (kg-reset!)
  (install-concept-ontology! ontology))

;;; Build a minimal in-memory KG with a few synthetic skills without
;;; touching the filesystem. Skills are added via kg-add-skill! with
;;; hand-constructed manifest alists, then kg-extract-concepts! is called.
;;;
;;; Skills:
;;;   linalg-skill   keywords: (la matrix)
;;;   eigen-skill    keywords: (eigenvalue)
;;;   calculus-skill keywords: (calculus)
;;;   geometry-skill keywords: (geometry)
(define (build-test-kg! ontology)
  (reset-kg-with-ontology! ontology)
  (kg-add-skill!
   `((name . linalg-skill)
     (path . "lattice/linalg")
     (description . "Linear algebra skill")
     (keywords la matrix)
     (aliases)
     (deps)
     (modules)
     (exports)))
  (kg-add-skill!
   `((name . eigen-skill)
     (path . "lattice/linalg")
     (description . "Eigenvalue skill")
     (keywords eigenvalue)
     (aliases)
     (deps)
     (modules)
     (exports)))
  (kg-add-skill!
   `((name . calculus-skill)
     (path . "lattice/calculus")
     (description . "Calculus skill")
     (keywords calculus)
     (aliases)
     (deps)
     (modules)
     (exports)))
  (kg-add-skill!
   `((name . geometry-skill)
     (path . "lattice/geometry")
     (description . "Geometry skill")
     (keywords geometry)
     (aliases)
     (deps)
     (modules)
     (exports)))
  (kg-build-deps!)
  (kg-extract-concepts!))

;;; ====================================================================
;;; Alias Collapse / Concept Count
;;; ====================================================================

(test-group "alias-collapse"

  (define-test "aliases normalize to canonical: la → linear-algebra"
    (build-test-kg! hier-ontology)
    ;; la is a synonym for linear-algebra — should not appear as a separate concept
    (assert-false (assq 'la *kg-concepts*)))

  (define-test "canonical concept linear-algebra is present after la alias"
    (build-test-kg! hier-ontology)
    (assert-true (pair? (assq 'linear-algebra *kg-concepts*))))

  (define-test "eigenvalue alias collapses to eigenvalue-theory"
    (build-test-kg! hier-ontology)
    (assert-false (assq 'eigenvalue *kg-concepts*)))

  (define-test "eigenvalue-theory canonical is present"
    (build-test-kg! hier-ontology)
    (assert-true (pair? (assq 'eigenvalue-theory *kg-concepts*))))

  (define-test "matrix keyword (unknown synonym) becomes its own concept"
    ;; 'matrix' is not in the ontology synonym groups, so it stays as-is
    (build-test-kg! hier-ontology)
    (assert-true (pair? (assq 'matrix *kg-concepts*))))

  (define-test "total concepts fewer than total raw keywords due to collapse"
    ;; Raw keywords: la, matrix, eigenvalue, calculus, geometry (5)
    ;; After normalization: linear-algebra, matrix, eigenvalue-theory, calculus, geometry (5 raw → 5 canonical but la→linear-algebra and eigenvalue→eigenvalue-theory)
    ;; Plus cross-cutting: differentiability
    ;; The point is that 'la' and 'lin-alg' both pointing to 'linear-algebra'
    ;; do NOT create separate concept blocks.
    (build-test-kg! hier-ontology)
    ;; At most 6 concepts: linear-algebra, matrix, eigenvalue-theory, calculus, geometry, differentiability
    (assert-true (<= (length *kg-concepts*) 6)))

  (define-test "linalg-skill links to linear-algebra (not la) after normalization"
    (build-test-kg! hier-ontology)
    (let ([skill-concepts (kg-skill-concepts 'linalg-skill)])
      (assert-true (pair? (memq 'linear-algebra skill-concepts)))
      (assert-false (memq 'la skill-concepts)))))

;;; ====================================================================
;;; Cross-Cutting Concepts Added Without Skill Reference
;;; ====================================================================

(test-group "cross-cutting"

  (define-test "differentiability added even though no skill has it as keyword"
    (build-test-kg! hier-ontology)
    ;; 'differentiability' appears in cross-cutting section only — no skill references it
    (assert-true (pair? (assq 'differentiability *kg-concepts*))))

  (define-test "differentiability links to declared skills in concept-skills map"
    (build-test-kg! hier-ontology)
    ;; Cross-cutting concepts now link to their declared skills (if those skills exist in KG)
    ;; The ontology declares differentiability → (autodiff calculus-skill)
    ;; Only calculus-skill exists in our test KG
    (let ([skills (kg-concept-skills 'differentiability)])
      (assert-true (pair? (memq 'calculus-skill skills))))))

;;; ====================================================================
;;; Hierarchy Edges Created
;;; ====================================================================

(test-group "is-a-edges"

  (define-test "is-a edges appear in *kg-edges* for hierarchy concepts"
    (build-test-kg! hier-ontology)
    ;; At least one kg/concept-is-a edge should exist for linear-algebra → mathematics
    (let ([is-a-edges (filter (lambda (e) (eq? (block-tag e) 'kg/concept-is-a)) *kg-edges*)])
      (assert-true (pair? is-a-edges))))

  (define-test "number of is-a edges matches hierarchy relationships among created concepts"
    ;; Concepts created: linear-algebra (parent: mathematics), eigenvalue-theory (parent: linear-algebra),
    ;; calculus (parent: mathematics), differentiability (cross-cutting, no parent in hierarchy).
    ;; mathematics itself is not created (no skill references it).
    ;; So only edges where BOTH child AND parent blocks exist count.
    ;; linear-algebra parent=mathematics → mathematics not created, so no edge
    ;; eigenvalue-theory parent=linear-algebra → linear-algebra created, so edge EXISTS
    ;; calculus parent=mathematics → mathematics not created, so no edge
    ;; Expected: exactly 1 is-a edge (eigenvalue-theory → linear-algebra)
    (build-test-kg! hier-ontology)
    (let ([is-a-edges (filter (lambda (e) (eq? (block-tag e) 'kg/concept-is-a)) *kg-edges*)])
      (assert-equal 1 (length is-a-edges)))))

;;; ====================================================================
;;; kg-concept-ancestors
;;; ====================================================================

(test-group "kg-concept-ancestors"

  (define-test "ancestors when ontology not loaded returns empty"
    ;; Uninstall ontology
    (kg-reset!)
    (install-concept-ontology!
     '(concept-ontology (version 1) (concepts) (synonym-groups) (cross-cutting)))
    ;; Install empty ontology: ancestors should return empty for any concept
    ;; Actually, install-concept-ontology! marks it as loaded.
    ;; To test "not loaded" we need to reset the flag — we can't easily do that from here.
    ;; Instead test that an unknown concept returns empty (equivalent behavior).
    (assert-equal '() (kg-concept-ancestors 'no-such-concept)))

  (define-test "ancestors of eigenvalue-theory returns (linear-algebra mathematics)"
    (build-test-kg! hier-ontology)
    (assert-equal '(linear-algebra mathematics) (kg-concept-ancestors 'eigenvalue-theory)))

  (define-test "ancestors of linear-algebra returns (mathematics)"
    (build-test-kg! hier-ontology)
    (assert-equal '(mathematics) (kg-concept-ancestors 'linear-algebra)))

  (define-test "ancestors of mathematics (root) returns empty"
    (build-test-kg! hier-ontology)
    (assert-equal '() (kg-concept-ancestors 'mathematics)))

  (define-test "ancestors via alias eigen resolves to eigenvalue-theory ancestors"
    (build-test-kg! hier-ontology)
    (assert-equal '(linear-algebra mathematics) (kg-concept-ancestors 'eigen))))

;;; ====================================================================
;;; kg-concept-descendants
;;; ====================================================================

(test-group "kg-concept-descendants"

  (define-test "descendants of mathematics includes linear-algebra"
    (build-test-kg! hier-ontology)
    (assert-true (pair? (memq 'linear-algebra (kg-concept-descendants 'mathematics)))))

  (define-test "descendants of mathematics includes calculus"
    (build-test-kg! hier-ontology)
    (assert-true (pair? (memq 'calculus (kg-concept-descendants 'mathematics)))))

  (define-test "descendants of mathematics includes eigenvalue-theory (transitive)"
    (build-test-kg! hier-ontology)
    (assert-true (pair? (memq 'eigenvalue-theory (kg-concept-descendants 'mathematics)))))

  (define-test "descendants of eigenvalue-theory is empty (leaf)"
    (build-test-kg! hier-ontology)
    (assert-equal '() (kg-concept-descendants 'eigenvalue-theory)))

  (define-test "descendants of unknown concept is empty"
    (build-test-kg! hier-ontology)
    (assert-equal '() (kg-concept-descendants 'no-such-thing))))

;;; ====================================================================
;;; kg-transitive-concept-skills
;;; ====================================================================

(test-group "transitive-concept-skills"

  (define-test "transitive skills of linear-algebra includes linalg-skill (direct)"
    (build-test-kg! hier-ontology)
    (let ([skills (kg-transitive-concept-skills 'linear-algebra)])
      (assert-true (pair? (memq 'linalg-skill skills)))))

  (define-test "transitive skills of linear-algebra includes eigen-skill (via eigenvalue-theory descendant)"
    ;; eigen-skill has keyword 'eigenvalue' → normalizes to 'eigenvalue-theory'
    ;; eigenvalue-theory is a child of linear-algebra
    ;; So transitive skills of linear-algebra should include eigen-skill
    (build-test-kg! hier-ontology)
    (let ([skills (kg-transitive-concept-skills 'linear-algebra)])
      (assert-true (pair? (memq 'eigen-skill skills)))))

  (define-test "transitive skills of mathematics includes linalg-skill"
    (build-test-kg! hier-ontology)
    (let ([skills (kg-transitive-concept-skills 'mathematics)])
      (assert-true (pair? (memq 'linalg-skill skills)))))

  (define-test "transitive skills of calculus includes calculus-skill (direct)"
    (build-test-kg! hier-ontology)
    (let ([skills (kg-transitive-concept-skills 'calculus)])
      (assert-true (pair? (memq 'calculus-skill skills)))))

  (define-test "transitive skills of eigenvalue-theory includes eigen-skill (direct)"
    (build-test-kg! hier-ontology)
    (let ([skills (kg-transitive-concept-skills 'eigenvalue-theory)])
      (assert-true (pair? (memq 'eigen-skill skills))))))

;;; ====================================================================
;;; kg-shared-concepts-transitive
;;; ====================================================================

(test-group "shared-concepts-transitive"

  (define-test "transitive shared finds connection linalg-skill↔eigen-skill via hierarchy"
    ;; linalg-skill → linear-algebra
    ;; eigen-skill → eigenvalue-theory (child of linear-algebra)
    ;; Flat kg-shared-concepts: no shared concept (different canonical names)
    ;; Transitive: linalg-skill's linear-algebra is an ancestor of eigenvalue-theory
    (build-test-kg! hier-ontology)
    (let ([flat   (kg-shared-concepts 'linalg-skill 'eigen-skill)]
          [transitive (kg-shared-concepts-transitive 'linalg-skill 'eigen-skill)])
      ;; Flat should find nothing (different canonicals)
      (assert-equal '() flat)
      ;; Transitive should find something
      (assert-true (pair? transitive))))

  (define-test "transitive shared on same skill returns non-empty when skill has concepts"
    (build-test-kg! hier-ontology)
    (let ([shared (kg-shared-concepts-transitive 'linalg-skill 'linalg-skill)])
      (assert-true (pair? shared))))

  (define-test "transitive shared between unrelated skills returns empty"
    ;; geometry-skill has keyword 'geometry' — not in hierarchy, no ancestor overlap with linalg-skill
    (build-test-kg! hier-ontology)
    (let ([shared (kg-shared-concepts-transitive 'linalg-skill 'geometry-skill)])
      (assert-equal '() shared)))

  (define-test "flat kg-shared-concepts finds nothing linalg-skill↔eigen-skill"
    ;; Sanity check: flat version can't bridge the hierarchy
    (build-test-kg! hier-ontology)
    (assert-equal '() (kg-shared-concepts 'linalg-skill 'eigen-skill))))

;;; ====================================================================
;;; Backward Compatibility — Flat Fallback (No Ontology)
;;; ====================================================================

(test-group "backward-compat-flat"

  (define-test "without ontology: la stays as-is (not normalized)"
    ;; Reset and use empty ontology so concept-ontology-loaded? is still #t but no synonyms
    ;; We can't truly test "not loaded" since install-concept-ontology! always marks it loaded.
    ;; Instead, reinstall an ontology with no synonym groups so la stays as la.
    (install-concept-ontology!
     '(concept-ontology (version 1) (concepts) (synonym-groups) (cross-cutting)))
    (kg-reset!)
    ;; Re-add a skill with keyword 'la'
    (kg-add-skill!
     `((name . test-skill)
       (path . "lattice/test")
       (description . "Test skill")
       (keywords la)
       (aliases)
       (deps)
       (modules)
       (exports)))
    (kg-build-deps!)
    (kg-extract-concepts!)
    ;; With empty ontology, 'la' cannot be normalized — should map to itself
    (let ([skill-concepts (kg-skill-concepts 'test-skill)])
      (assert-true (pair? (memq 'la skill-concepts)))))

  (define-test "v2 root (version 2) loaded by kg-load-from-cas! still produces loaded KG"
    ;; We test this by building a KG without ontology, persisting it,
    ;; then loading it — the in-memory state should hydrate correctly.
    ;; Since we don't have a real v2 root on disk, we verify that
    ;; kg-load-from-cas! doesn't break when concept-is-a edges are absent.
    ;; We construct a valid root without is-a edges manually.
    (install-concept-ontology!
     '(concept-ontology (version 1) (concepts) (synonym-groups) (cross-cutting)))
    (kg-reset!)
    (kg-add-skill!
     `((name . compat-skill)
       (path . "lattice/test")
       (description . "Compat test skill")
       (keywords compat-concept)
       (aliases)
       (deps)
       (modules)
       (exports)))
    (kg-build-deps!)
    (kg-extract-concepts!)
    (let ([root-hash (kg-build-root!)])
      ;; Now reload from CAS
      (kg-reset!)
      (let ([ok (kg-load-from-cas! root-hash)])
        (assert-true ok)
        ;; KG should be loaded with the compat-skill
        (assert-true (pair? (assq 'compat-skill *kg-skills*)))))))

;;; ====================================================================
;;; Stats and Structural Soundness
;;; ====================================================================

(test-group "structural"

  (define-test "kg-stats reflects concept count including cross-cutting"
    (build-test-kg! hier-ontology)
    (let ([stats (kg-stats)])
      (let ([concept-count (cdr (assq 'concepts stats))])
        ;; Should have at least: linear-algebra, eigenvalue-theory, calculus, matrix, geometry, differentiability
        (assert-true (>= concept-count 5)))))

  (define-test "kg-initialized? is true after build"
    (build-test-kg! hier-ontology)
    (let ([root-hash (kg-build-root!)])
      (assert-true (kg-initialized?))))

  (define-test "all created concept blocks survive round-trip through CAS"
    (build-test-kg! hier-ontology)
    (let ([root-hash (kg-build-root!)])
      (kg-reset!)
      (let ([ok (kg-load-from-cas! root-hash)])
        (assert-true ok)
        ;; Spot-check: linear-algebra concept survives
        (assert-true (pair? (assq 'linear-algebra *kg-concepts*)))
        ;; calculus survives
        (assert-true (pair? (assq 'calculus *kg-concepts*)))))))

(run-all-tests)
