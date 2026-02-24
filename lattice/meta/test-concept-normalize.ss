;;; test-concept-normalize.ss — Tests for lattice/meta/concept-normalize.ss
;;;
;;; Uses a self-contained mock ontology so the test has no external dependencies.
;;;
;;; Design note: define-test both runs immediately and registers for run-all-tests.
;;; Every test is therefore self-contained — it installs the state it needs before
;;; asserting, so it produces the same result regardless of when it runs.

(load "core/testing/test-framework.ss")
(load "lattice/data/hamt.ss")
(load "core/base/prelude.ss")
(load "lattice/meta/concept-normalize.ss")

;;; ====================================================================
;;; Shared fixtures
;;; ====================================================================

(define test-ontology
  '(concept-ontology
    (version 2)
    (concepts
      (concept mathematics
        (description "Foundation of quantitative reasoning")
        (children (linear-algebra calculus)))
      (concept linear-algebra
        (description "Vectors, matrices, linear maps")
        (parent mathematics)
        (children (eigenvalue svd))
        (synonyms la lin-alg))
      (concept calculus
        (description "Rates of change")
        (parent mathematics))
      (concept eigenvalue
        (description "Scalar factor of eigenvector")
        (parent linear-algebra))
      (concept svd
        (description "Singular value decomposition")
        (parent linear-algebra)
        (synonyms singular-value-decomposition)))
    (cross-cutting
      (concept composability
        (description "Combining smaller pieces")
        (skills (fp optics))))))

(define empty-ontology
  '(concept-ontology
    (version 2)
    (concepts)
    (cross-cutting)))

;;; with-empty-ontology : thunk -> result
;;; Install an empty ontology, call thunk, return its result.
;;; Used inline within test bodies.
(define (with-empty-ontology thunk)
  (install-concept-ontology! empty-ontology)
  (thunk))

;;; with-test-ontology : thunk -> result
;;; Install the test ontology, call thunk, return its result.
(define (with-test-ontology thunk)
  (install-concept-ontology! test-ontology)
  (thunk))

;;; ====================================================================
;;; Empty-state tests
;;; ====================================================================

(test-group "empty-state"

  (define-test "ontology-loaded? is true after empty install"
    (with-empty-ontology (lambda ()
      (assert-true (concept-ontology-loaded?)))))

  (define-test "normalize returns sym unchanged with empty ontology"
    (with-empty-ontology (lambda ()
      (assert-equal 'linear-algebra (concept-normalize 'linear-algebra)))))

  (define-test "normalize unknown sym with empty ontology"
    (with-empty-ontology (lambda ()
      (assert-equal 'no-such-thing (concept-normalize 'no-such-thing)))))

  (define-test "canonical? returns false with empty ontology"
    (with-empty-ontology (lambda ()
      (assert-false (concept-canonical? 'mathematics)))))

  (define-test "parent returns #f with empty ontology"
    (with-empty-ontology (lambda ()
      (assert-false (concept-parent 'eigenvalue)))))

  (define-test "ancestors returns empty with empty ontology"
    (with-empty-ontology (lambda ()
      (assert-equal '() (concept-ancestors 'eigenvalue)))))

  (define-test "children returns empty with empty ontology"
    (with-empty-ontology (lambda ()
      (assert-equal '() (concept-children 'mathematics)))))

  (define-test "root? returns false with empty ontology"
    (with-empty-ontology (lambda ()
      (assert-false (concept-root? 'mathematics)))))

  (define-test "synonyms returns empty with empty ontology"
    (with-empty-ontology (lambda ()
      (assert-equal '() (concept-synonyms 'linear-algebra)))))

  (define-test "description returns empty string with empty ontology"
    (with-empty-ontology (lambda ()
      (assert-equal "" (concept-description 'eigenvalue)))))

  (define-test "cross-cutting? returns false with empty ontology"
    (with-empty-ontology (lambda ()
      (assert-false (concept-cross-cutting? 'composability)))))

  (define-test "cross-cutting-skills returns empty with empty ontology"
    (with-empty-ontology (lambda ()
      (assert-equal '() (concept-cross-cutting-skills 'composability)))))

  (define-test "concept-roots returns empty with empty ontology"
    (with-empty-ontology (lambda ()
      (assert-equal '() (concept-roots)))))

  (define-test "concept-all returns empty with empty ontology"
    (with-empty-ontology (lambda ()
      (assert-equal '() (concept-all))))))

;;; ====================================================================
;;; Installation
;;; ====================================================================

(test-group "installation"

  (define-test "ontology-loaded? is true after install"
    (with-test-ontology (lambda ()
      (assert-true (concept-ontology-loaded?))))))

;;; ====================================================================
;;; Normalization
;;; ====================================================================

(test-group "normalization"

  (define-test "alias la normalizes to linear-algebra"
    (with-test-ontology (lambda ()
      (assert-equal 'linear-algebra (concept-normalize 'la)))))

  (define-test "alias lin-alg normalizes to linear-algebra"
    (with-test-ontology (lambda ()
      (assert-equal 'linear-algebra (concept-normalize 'lin-alg)))))

  (define-test "canonical eigenvalue normalizes to itself"
    (with-test-ontology (lambda ()
      (assert-equal 'eigenvalue (concept-normalize 'eigenvalue)))))

  (define-test "unknown symbol normalizes to itself"
    (with-test-ontology (lambda ()
      (assert-equal 'unknown-thing (concept-normalize 'unknown-thing)))))

  (define-test "singular-value-decomposition alias normalizes to svd"
    (with-test-ontology (lambda ()
      ;; singular-value-decomposition is a synonym of concept svd
      (assert-equal 'svd (concept-normalize 'singular-value-decomposition)))))

  (define-test "canonical? true for linear-algebra"
    (with-test-ontology (lambda ()
      (assert-true (concept-canonical? 'linear-algebra)))))

  (define-test "canonical? false for alias la"
    (with-test-ontology (lambda ()
      (assert-false (concept-canonical? 'la)))))

  (define-test "canonical? false for unknown symbol"
    (with-test-ontology (lambda ()
      (assert-false (concept-canonical? 'no-such-concept))))))

;;; ====================================================================
;;; Hierarchy — parent
;;; ====================================================================

(test-group "hierarchy-parent"

  (define-test "parent of eigenvalue is linear-algebra"
    (with-test-ontology (lambda ()
      (assert-equal 'linear-algebra (concept-parent 'eigenvalue)))))

  (define-test "parent of linear-algebra is mathematics"
    (with-test-ontology (lambda ()
      (assert-equal 'mathematics (concept-parent 'linear-algebra)))))

  (define-test "parent of mathematics is #f (root)"
    (with-test-ontology (lambda ()
      (assert-false (concept-parent 'mathematics)))))

  (define-test "parent of alias la resolves via normalization"
    (with-test-ontology (lambda ()
      (assert-equal 'mathematics (concept-parent 'la)))))

  (define-test "parent of unknown concept is #f"
    (with-test-ontology (lambda ()
      (assert-false (concept-parent 'no-such-concept))))))

;;; ====================================================================
;;; Hierarchy — ancestors
;;; ====================================================================

(test-group "hierarchy-ancestors"

  (define-test "ancestors of eigenvalue are (linear-algebra mathematics)"
    (with-test-ontology (lambda ()
      (assert-equal '(linear-algebra mathematics) (concept-ancestors 'eigenvalue)))))

  (define-test "ancestors of linear-algebra are (mathematics)"
    (with-test-ontology (lambda ()
      (assert-equal '(mathematics) (concept-ancestors 'linear-algebra)))))

  (define-test "ancestors of mathematics (root) are empty"
    (with-test-ontology (lambda ()
      (assert-equal '() (concept-ancestors 'mathematics)))))

  (define-test "ancestors via alias la match ancestors via canonical"
    (with-test-ontology (lambda ()
      (assert-equal (concept-ancestors 'linear-algebra)
                    (concept-ancestors 'la)))))

  (define-test "ancestors of unknown concept are empty"
    (with-test-ontology (lambda ()
      (assert-equal '() (concept-ancestors 'no-such-concept))))))

;;; ====================================================================
;;; Hierarchy — children
;;; ====================================================================

(test-group "hierarchy-children"

  (define-test "children of mathematics include linear-algebra"
    (with-test-ontology (lambda ()
      (assert-true (pair? (memq 'linear-algebra (concept-children 'mathematics)))))))

  (define-test "children of mathematics include calculus"
    (with-test-ontology (lambda ()
      (assert-true (pair? (memq 'calculus (concept-children 'mathematics)))))))

  (define-test "children of eigenvalue are empty (leaf)"
    (with-test-ontology (lambda ()
      (assert-equal '() (concept-children 'eigenvalue)))))

  (define-test "children of linear-algebra include eigenvalue"
    (with-test-ontology (lambda ()
      (assert-true (pair? (memq 'eigenvalue (concept-children 'linear-algebra)))))))

  (define-test "children of linear-algebra include svd"
    (with-test-ontology (lambda ()
      (assert-true (pair? (memq 'svd (concept-children 'linear-algebra)))))))

  (define-test "children via alias la match children via canonical"
    (with-test-ontology (lambda ()
      (assert-equal (concept-children 'linear-algebra)
                    (concept-children 'la)))))

  (define-test "children of unknown concept are empty"
    (with-test-ontology (lambda ()
      (assert-equal '() (concept-children 'no-such-concept))))))

;;; ====================================================================
;;; Roots
;;; ====================================================================

(test-group "roots"

  (define-test "mathematics is a root"
    (with-test-ontology (lambda ()
      (assert-true (concept-root? 'mathematics)))))

  (define-test "eigenvalue is not a root"
    (with-test-ontology (lambda ()
      (assert-false (concept-root? 'eigenvalue)))))

  (define-test "linear-algebra is not a root"
    (with-test-ontology (lambda ()
      (assert-false (concept-root? 'linear-algebra)))))

  (define-test "unknown concept is not a root"
    (with-test-ontology (lambda ()
      (assert-false (concept-root? 'no-such-concept)))))

  (define-test "concept-roots list contains mathematics"
    (with-test-ontology (lambda ()
      (assert-true (pair? (memq 'mathematics (concept-roots)))))))

  (define-test "concept-roots list does not contain eigenvalue"
    (with-test-ontology (lambda ()
      (assert-false (memq 'eigenvalue (concept-roots)))))))

;;; ====================================================================
;;; Synonyms
;;; ====================================================================

(test-group "synonyms"

  (define-test "synonyms of linear-algebra is non-empty"
    (with-test-ontology (lambda ()
      (assert-true (pair? (concept-synonyms 'linear-algebra))))))

  (define-test "synonyms of linear-algebra include la"
    (with-test-ontology (lambda ()
      (assert-true (pair? (memq 'la (concept-synonyms 'linear-algebra)))))))

  (define-test "synonyms of linear-algebra include lin-alg"
    (with-test-ontology (lambda ()
      (assert-true (pair? (memq 'lin-alg (concept-synonyms 'linear-algebra)))))))

  (define-test "synonyms of linear-algebra has linear-algebra as first element"
    (with-test-ontology (lambda ()
      (assert-equal 'linear-algebra (car (concept-synonyms 'linear-algebra))))))

  (define-test "synonyms via alias la same as via canonical"
    (with-test-ontology (lambda ()
      (assert-equal (concept-synonyms 'linear-algebra)
                    (concept-synonyms 'la)))))

  (define-test "synonyms of unknown concept is empty"
    (with-test-ontology (lambda ()
      (assert-equal '() (concept-synonyms 'no-such-concept)))))

  (define-test "eigenvalue has no aliases — synonyms is empty"
    ;; eigenvalue has no synonym-group entry, so synonyms returns '()
    (with-test-ontology (lambda ()
      (assert-equal '() (concept-synonyms 'eigenvalue))))))

;;; ====================================================================
;;; Descriptions
;;; ====================================================================

(test-group "descriptions"

  (define-test "description of eigenvalue matches manifest"
    (with-test-ontology (lambda ()
      (assert-equal "Scalar factor of eigenvector" (concept-description 'eigenvalue)))))

  (define-test "description of mathematics is non-empty"
    (with-test-ontology (lambda ()
      (assert-true (not (string=? "" (concept-description 'mathematics)))))))

  (define-test "description of unknown concept is empty string"
    (with-test-ontology (lambda ()
      (assert-equal "" (concept-description 'no-such-concept)))))

  (define-test "description via alias la same as via canonical"
    (with-test-ontology (lambda ()
      (assert-equal (concept-description 'linear-algebra)
                    (concept-description 'la))))))

;;; ====================================================================
;;; Cross-cutting
;;; ====================================================================

(test-group "cross-cutting"

  (define-test "composability is cross-cutting"
    (with-test-ontology (lambda ()
      (assert-true (concept-cross-cutting? 'composability)))))

  (define-test "eigenvalue is not cross-cutting"
    (with-test-ontology (lambda ()
      (assert-false (concept-cross-cutting? 'eigenvalue)))))

  (define-test "unknown concept is not cross-cutting"
    (with-test-ontology (lambda ()
      (assert-false (concept-cross-cutting? 'no-such-concept)))))

  (define-test "cross-cutting-skills of composability include fp"
    (with-test-ontology (lambda ()
      (assert-true (pair? (memq 'fp (concept-cross-cutting-skills 'composability)))))))

  (define-test "cross-cutting-skills of composability include optics"
    (with-test-ontology (lambda ()
      (assert-true (pair? (memq 'optics (concept-cross-cutting-skills 'composability)))))))

  (define-test "cross-cutting-skills of non-cross-cutting concept is empty"
    (with-test-ontology (lambda ()
      (assert-equal '() (concept-cross-cutting-skills 'eigenvalue)))))

  (define-test "cross-cutting-skills of unknown concept is empty"
    (with-test-ontology (lambda ()
      (assert-equal '() (concept-cross-cutting-skills 'no-such-concept))))))

;;; ====================================================================
;;; concept-all
;;; ====================================================================

(test-group "concept-all"

  (define-test "concept-all is a non-empty list"
    (with-test-ontology (lambda ()
      (assert-true (pair? (concept-all))))))

  (define-test "concept-all contains mathematics"
    (with-test-ontology (lambda ()
      (assert-true (pair? (memq 'mathematics (concept-all)))))))

  (define-test "concept-all contains eigenvalue"
    (with-test-ontology (lambda ()
      (assert-true (pair? (memq 'eigenvalue (concept-all)))))))

  (define-test "concept-all contains composability (cross-cutting)"
    (with-test-ontology (lambda ()
      (assert-true (pair? (memq 'composability (concept-all))))))))

;;; ====================================================================
;;; Idempotency — reinstall clears old state
;;; ====================================================================

(test-group "idempotency"

  (define-test "reinstall with minimal ontology clears prior state"
    ;; Start with real ontology, then overwrite with minimal one
    (install-concept-ontology! test-ontology)
    (install-concept-ontology!
      '(concept-ontology
        (version 2)
        (concepts
          (concept alpha (description "Just alpha")))
        (cross-cutting)))
    ;; Old concept gone
    (assert-equal "" (concept-description 'eigenvalue))
    ;; New concept present
    (assert-equal "Just alpha" (concept-description 'alpha))
    ;; Still installed
    (assert-true (concept-ontology-loaded?)))

  (define-test "reinstalling test ontology restores full state"
    (install-concept-ontology! test-ontology)
    (assert-equal "Scalar factor of eigenvector"
                  (concept-description 'eigenvalue))))

;;; ====================================================================
;;; build-ontology-from-manifests
;;; ====================================================================

;;; Helper: make a minimal manifest alist with a concepts field
(define (make-test-manifest name concepts)
  `((name . ,name)
    (version . "0.1.0")
    (tier . 0)
    (path . ,(string-append "lattice/" (symbol->string name)))
    (purity . total)
    (stability . stable)
    (fuel-bound . "O(?)")
    (deps . ())
    (description . "test")
    (keywords . ())
    (aliases . ())
    (exports . ())
    (modules . ())
    (concepts . ,concepts)))

(test-group "build-ontology-single"

  (define-test "single manifest produces valid ontology sexp"
    (let* ([m (make-test-manifest 'linalg
               '((concept linear-algebra
                   (description "Vectors and matrices")
                   (parent mathematics)
                   (synonyms la))))]
           [ontology (build-ontology-from-manifests (list m))])
      (install-concept-ontology! ontology)
      (assert-true (concept-ontology-loaded?))
      (assert-equal "Vectors and matrices" (concept-description 'linear-algebra))
      (assert-equal 'linear-algebra (concept-normalize 'la))))

  (define-test "concept with no parent is a root"
    (let* ([m (make-test-manifest 'algebra
               '((concept mathematics
                   (description "Foundation"))))]
           [ontology (build-ontology-from-manifests (list m))])
      (install-concept-ontology! ontology)
      (assert-true (concept-root? 'mathematics)))))

(test-group "build-ontology-children-derived"

  (define-test "children derived from parent declarations"
    (let* ([m1 (make-test-manifest 'meta
                '((concept mathematics
                    (description "Foundation"))))]
           [m2 (make-test-manifest 'linalg
                '((concept linear-algebra
                    (description "Vectors")
                    (parent mathematics))))]
           [m3 (make-test-manifest 'algebra
                '((concept abstract-algebra
                    (description "Groups and rings")
                    (parent mathematics))))]
           [ontology (build-ontology-from-manifests (list m1 m2 m3))])
      (install-concept-ontology! ontology)
      (assert-true (pair? (memq 'linear-algebra (concept-children 'mathematics))))
      (assert-true (pair? (memq 'abstract-algebra (concept-children 'mathematics))))))

  (define-test "multi-level children derived correctly"
    (let* ([m (make-test-manifest 'linalg
               '((concept mathematics
                   (description "Root"))
                 (concept linear-algebra
                   (description "LA")
                   (parent mathematics))
                 (concept eigenvalue
                   (description "Eigen")
                   (parent linear-algebra))))]
           [ontology (build-ontology-from-manifests (list m))])
      (install-concept-ontology! ontology)
      (assert-true (pair? (memq 'linear-algebra (concept-children 'mathematics))))
      (assert-true (pair? (memq 'eigenvalue (concept-children 'linear-algebra))))
      (assert-equal '() (concept-children 'eigenvalue))
      ;; Ancestor chain works
      (assert-equal '(linear-algebra mathematics) (concept-ancestors 'eigenvalue)))))

(test-group "build-ontology-auto-roots"

  (define-test "undeclared parent auto-created as root"
    (let* ([m (make-test-manifest 'linalg
               '((concept linear-algebra
                   (description "LA")
                   (parent mathematics))))]
           [ontology (build-ontology-from-manifests (list m))])
      (install-concept-ontology! ontology)
      ;; mathematics was auto-created
      (assert-true (concept-root? 'mathematics))
      (assert-equal 'mathematics (concept-parent 'linear-algebra))
      (assert-true (pair? (memq 'linear-algebra (concept-children 'mathematics)))))))

(test-group "build-ontology-cross-cutting"

  (define-test "cross-cutting concepts collected"
    (let* ([m (make-test-manifest 'meta
               '((concept composability
                   (description "Combining pieces")
                   (cross-cutting #t)
                   (skills (fp optics dsl)))))]
           [ontology (build-ontology-from-manifests (list m))])
      (install-concept-ontology! ontology)
      (assert-true (concept-cross-cutting? 'composability))
      (assert-true (pair? (memq 'fp (concept-cross-cutting-skills 'composability))))))

  (define-test "cross-cutting not in regular concepts"
    (let* ([m (make-test-manifest 'meta
               '((concept composability
                   (description "Combining pieces")
                   (cross-cutting #t)
                   (skills (fp optics)))))]
           [ontology (build-ontology-from-manifests (list m))])
      (install-concept-ontology! ontology)
      ;; Cross-cutting concepts should not have parents or be children
      (assert-false (concept-parent 'composability)))))

(test-group "build-ontology-conflict"

  (define-test "first-seen-wins on duplicate concept"
    (let* ([m1 (make-test-manifest 'linalg
                '((concept linear-algebra
                    (description "First"))))]
           [m2 (make-test-manifest 'algebra
                '((concept linear-algebra
                    (description "Second"))))]
           [ontology (build-ontology-from-manifests (list m1 m2))])
      (install-concept-ontology! ontology)
      (assert-equal "First" (concept-description 'linear-algebra)))))

(test-group "build-ontology-round-trip"

  (define-test "build → install → full hierarchy queries"
    (let* ([m1 (make-test-manifest 'meta
                '((concept mathematics
                    (description "Foundation"))
                  (concept computation
                    (description "Algorithms and structures"))
                  (concept composability
                    (description "Combining pieces")
                    (cross-cutting #t)
                    (skills (fp optics)))))]
           [m2 (make-test-manifest 'linalg
                '((concept linear-algebra
                    (description "Vectors, matrices")
                    (parent mathematics)
                    (synonyms la lin-alg))
                  (concept eigenvalue-theory
                    (description "Eigenvalues and SVD")
                    (parent linear-algebra)
                    (synonyms eigenvalue svd))))]
           [m3 (make-test-manifest 'data
                '((concept data-structures
                    (description "Trees, graphs, heaps")
                    (parent computation)
                    (synonyms ds))))]
           [ontology (build-ontology-from-manifests (list m1 m2 m3))])
      (install-concept-ontology! ontology)
      ;; Roots
      (assert-true (concept-root? 'mathematics))
      (assert-true (concept-root? 'computation))
      (assert-false (concept-root? 'linear-algebra))
      ;; Children derived
      (assert-true (pair? (memq 'linear-algebra (concept-children 'mathematics))))
      (assert-true (pair? (memq 'data-structures (concept-children 'computation))))
      (assert-true (pair? (memq 'eigenvalue-theory (concept-children 'linear-algebra))))
      ;; Synonyms
      (assert-equal 'linear-algebra (concept-normalize 'la))
      (assert-equal 'linear-algebra (concept-normalize 'lin-alg))
      (assert-equal 'eigenvalue-theory (concept-normalize 'svd))
      ;; Cross-cutting
      (assert-true (concept-cross-cutting? 'composability))
      ;; Validation clean
      (assert-equal '() (validate-ontology))))

  (define-test "empty manifests produce empty ontology"
    (let ([ontology (build-ontology-from-manifests '())])
      (install-concept-ontology! ontology)
      (assert-equal '() (concept-all))
      (assert-equal '() (concept-roots)))))

(run-all-tests)
