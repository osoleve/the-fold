;;; lattice/meta/test-meta.ss — Tests for Lattice Meta-Tooling
;;;
;;; Run with: scheme --script lattice/meta/test-meta.ss
;;;
;;; Performance notes:
;;; - Tests use kg-ensure! and lattice-ensure! instead of kg-build!/lattice-init!
;;;   to avoid rebuilding the knowledge graph and search indices for each test.
;;; - The exception is test-kg-build, which tests the actual build process.
;;; - This reduces test runtime from ~20s to ~2s (9x speedup).

(load "core/testing/test-framework.ss")

;;; Suppress module load messages during tests
(define *meta-quiet* #t)

(load "lattice/meta/meta.ss")

(display "\n====\n")
(display "Lattice Meta-Tooling Tests\n")
(display "====\n\n")

;;; ====
;;; BM25 Tests
;;; ====

(test-group bm25-tests
            
            (define-test test-bm25-create
              (let ([idx (bm25-create)])
                   (assert-equal 0 (cdr (assq 'documents (bm25-stats idx))))))
            
            (define-test test-bm25-add-doc
              (let* ([idx (bm25-create)]
                     [idx (bm25-add-doc idx 'doc1 '(hello world test) 'data1)]
                     [idx (bm25-add-doc idx 'doc2 '(hello scheme) 'data2)])
                    (assert-equal 2 (cdr (assq 'documents (bm25-stats idx))))))
            
            (define-test test-bm25-search
              (let* ([idx (bm25-create)]
                     [idx (bm25-add-doc idx 'doc1 '(linear algebra matrix) 'data1)]
                     [idx (bm25-add-doc idx 'doc2 '(physics simulation) 'data2)]
                     [idx (bm25-add-doc idx 'doc3 '(matrix operations linear) 'data3)]
                     [results (bm25-search idx '(matrix) 10)])
                    (assert-true (> (length results) 0))))
            
            (define-test test-bm25-search-string
              (let* ([idx (bm25-create)]
                     [idx (bm25-add-doc idx 'linalg '(vector matrix algebra) 'data)]
                     [results (bm25-search-string idx "vector algebra" 5)])
                    (assert-true (> (length results) 0))))

            ;; Coverage boost tests (from f2d66418 fix)
            (define-test test-bm25-coverage-single-term
              "Single-term query: no coverage penalty"
              (let* ([idx (bm25-create)]
                     [idx (bm25-add-doc idx 'doc1 '(alpha beta gamma) 'data)])
                    ;; Single term should get a score (no penalty)
                    (assert-true (> (bm25-score idx 'doc1 '(alpha)) 0))))

            (define-test test-bm25-coverage-full-match
              "Multi-term query with full match: no penalty"
              (let* ([idx (bm25-create)]
                     [idx (bm25-add-doc idx 'doc1 '(alpha beta gamma) 'data)]
                     [full-score (bm25-score idx 'doc1 '(alpha beta))])
                    ;; Full match should have positive score
                    (assert-true (> full-score 0))))

            (define-test test-bm25-coverage-partial-match
              "Multi-term query with partial match: penalized"
              (let* ([idx (bm25-create)]
                     [idx (bm25-add-doc idx 'doc1 '(alpha gamma) 'data)]  ; missing beta
                     [idx (bm25-add-doc idx 'doc2 '(alpha beta) 'data)]   ; has both
                     [partial-score (bm25-score idx 'doc1 '(alpha beta))]
                     [full-score (bm25-score idx 'doc2 '(alpha beta))])
                    ;; Partial match should score lower than full match
                    (assert-true (> full-score partial-score))))

            (define-test test-bm25-coverage-ranking
              "Coverage boost improves ranking of complete matches"
              (let* ([idx (bm25-create)]
                     ;; doc1: has 'matrix' twice but not 'vector'
                     [idx (bm25-add-doc idx 'doc1 '(matrix matrix algebra ops) 'data1)]
                     ;; doc2: has both terms once
                     [idx (bm25-add-doc idx 'doc2 '(matrix vector linear) 'data2)]
                     [results (bm25-search idx '(matrix vector) 10)])
                    ;; doc2 should rank higher despite doc1 having more 'matrix' occurrences
                    (assert-equal 'doc2 (caar results))))

            )

;;; ====
;;; Knowledge Graph Tests
;;; ====

(test-group kg-tests
            
            (define-test test-kg-build
              (kg-reset!)
              (kg-build!)
              (assert-true (> (length (kg-skills)) 0)))
            
            (define-test test-kg-skill-lookup
              (kg-ensure!)
              (assert-true (if (kg-skill 'linalg) #t #f)))

            (define-test test-kg-deps
              (kg-ensure!)
              ;; With derived deps, optimization depends on data/linalg/sat
              ;; (autodiff was a manifest phantom — not in @requires)
              (let ([deps (kg-deps 'optimization)])
                   (assert-true (if (memq 'linalg deps) #t #f))
                   (assert-true (if (memq 'data deps) #t #f))))

            (define-test test-kg-uses
              (kg-ensure!)
              (let ([uses (kg-uses 'linalg)])
                   (assert-true (> (length uses) 0))))
            
            )

;;; ====
;;; DAG Navigation Tests
;;; ====

(test-group dag-tests

            (define-test test-lattice-roots
              (kg-ensure!)
              ;; With derived deps + cycle breaking, roots are skills with
              ;; zero derived deps. These shift as module parsing improves.
              ;; After fp split (fold-zy24): algebra has derived deps (data linalg number-theory)
              (let ([roots (lattice-roots)])
                   (assert-true (if (memq 'template roots) #t #f))
                   (assert-true (if (memq 'fp roots) #t #f))))

            (define-test test-lattice-deps-transitive
              (kg-ensure!)
              (let ([deps (lattice-deps-transitive 'physics/diff)])
                   (assert-true (if (memq 'linalg deps) #t #f))))

            (define-test test-lattice-path
              (kg-ensure!)
              (let ([path (lattice-path 'physics/diff 'linalg)])
                   (assert-true (list? path))))

            (define-test test-lattice-tiers
              (kg-ensure!)
              (let ([tiers (lattice-tiers)])
                   (assert-true (if (assq 0 tiers) #t #f))
                   (assert-true (if (assq 1 tiers) #t #f))
                   (assert-true (if (assq 2 tiers) #t #f))))

            )

;;; ====
;;; Cycle Detection Tests
;;; ====

(test-group cycle-detection-tests

            ;; Test that self-loops are detected
            (define-test test-self-loop-detection
              (kg-ensure!)
              (assert-true (lattice-would-cycle? 'linalg 'linalg)))

            ;; Test that safe dependencies are correctly identified
            (define-test test-safe-dependency
              (kg-ensure!)
              ;; physics/diff -> linalg exists and is safe (linalg is tier 0)
              (assert-false (lattice-would-cycle? 'physics/diff 'linalg)))

            ;; Test that reverse dependencies would create cycles
            (define-test test-reverse-would-cycle
              (kg-ensure!)
              ;; linalg -> physics/diff would create cycle since physics/diff -> linalg
              (assert-true (lattice-would-cycle? 'linalg 'physics/diff)))

            ;; Test cycle path finding for self-loop
            (define-test test-find-cycle-path-self
              (kg-ensure!)
              (let ([path (lattice-find-cycle-path 'linalg 'linalg)])
                   (assert-true (list? path))
                   (assert-equal '(linalg linalg) path)))

            ;; Test cycle path finding for valid cycle
            (define-test test-find-cycle-path-real
              (kg-ensure!)
              (let ([path (lattice-find-cycle-path 'linalg 'physics/diff)])
                   (assert-true (list? path))
                   (assert-true (> (length path) 2))))

            ;; Test no cycle path for safe dep
            (define-test test-no-cycle-path-safe
              (kg-ensure!)
              (assert-false (lattice-find-cycle-path 'physics/diff 'linalg)))

            ;; Test lattice-check-deps returns ok for valid skill
            (define-test test-check-deps-valid
              (kg-ensure!)
              (let ([result (lattice-check-deps 'physics/diff)])
                   (assert-true (pair? result))
                   (assert-equal 'ok (car result))))

            ;; Test lattice-check-deps returns ok for tier 0 skill (no deps)
            (define-test test-check-deps-tier0
              (kg-ensure!)
              (let ([result (lattice-check-deps 'linalg)])
                   (assert-true (pair? result))
                   (assert-equal 'ok (car result))))

            ;; Test lattice-validate-all returns empty on healthy lattice
            (define-test test-validate-all-healthy
              (kg-ensure!)
              (let ([errors (lattice-validate-all)])
                   (assert-true (null? errors))))

            ;; Test format-cycle-path helper
            (define-test test-format-cycle-path
              (assert-equal "a → b → c" (format-cycle-path '(a b c)))
              (assert-equal "x" (format-cycle-path '(x)))
              (assert-equal "" (format-cycle-path '())))

            )

;;; ====
;;; Search API Tests
;;; ====

(test-group search-tests
            
            (define-test test-search-init
              (kg-ensure!)
              (lattice-index!)
              (assert-true *search-ready*))
            
            (define-test test-lattice-find
              (lattice-ensure!)
              (let ([results (lattice-find "matrix")])
                   (assert-true (> (length results) 0))))
            
            (define-test test-lattice-find-exact
              (lattice-ensure!)
              (let ([result (lattice-find-exact 'linalg)])
                   (assert-true (list? result))))
            
            (define-test test-lattice-complete
              (lattice-ensure!)
              (let ([results (lattice-complete "mat")])
                   (assert-true (> (length results) 0))))
            
            )

;;; ====
;;; Analytics Tests
;;; ====

(test-group analytics-tests
            
            (define-test test-lattice-stats
              (lattice-ensure!)
              (let ([stats (lattice-stats)])
                   (assert-true (> (cdr (assq 'skills stats)) 0))
                   (assert-true (> (cdr (assq 'modules stats)) 0))
                   (assert-true (> (cdr (assq 'exports stats)) 0))))
            
            (define-test test-lattice-health
              (lattice-ensure!)
              (let ([report (lattice-health)])
                   (assert-true (list? report))
                   (assert-true (if (assq 'healthy report) #t #f))))
            
            (define-test test-lattice-coverage
              (lattice-ensure!)
              (let ([cov (lattice-coverage)])
                   (assert-true (if (assq 'total cov) #t #f))
                   (assert-true (>= (cdr (assq 'total cov)) 0))))
            
            )

;;; ====
;;; Inspection Tests
;;; ====

(test-group inspect-tests
            
            (define-test test-lattice-info
              (lattice-ensure!)
              (let ([info (lattice-info 'linalg)])
                   (assert-true (list? info))
                   (assert-equal 'linalg (cdr (assq 'name info)))
                   (assert-true (if (assq 'tier info) #t #f))
                   (assert-true (if (assq 'purity info) #t #f))))
            
            (define-test test-lattice-skill-exports
              (lattice-ensure!)
              (let ([exports (lattice-skill-exports 'linalg)])
                   (assert-true (> (length exports) 0))))
            
            )

;;; ====
;;; Test Discovery Tests
;;; ====

(test-group test-discovery-tests

            (define-test test-lattice-tests-finds-files
              (lattice-ensure!)
              (let ([tests (lattice-tests 'linalg)])
                   (assert-true (> (length tests) 0))
                   ;; Should find known test files
                   (assert-true (if (find (lambda (f) (string-contains? f "test-vec.ss")) tests) #t #f))))

            (define-test test-lattice-tests-returns-paths
              (lattice-ensure!)
              (let ([tests (lattice-tests 'meta)])
                   (assert-true (> (length tests) 0))
                   ;; Paths should include skill directory
                   (assert-true (if (find (lambda (f) (string-starts-with? f "lattice/meta/")) tests) #t #f))))

            (define-test test-lattice-tests-unknown-skill
              (lattice-ensure!)
              (let ([tests (lattice-tests 'nonexistent-skill-xyz)])
                   (assert-equal '() tests)))

            (define-test test-find-test-files-helper
              ;; Test the low-level helper
              (let ([files (find-test-files "lattice/linalg")])
                   (assert-true (> (length files) 0))
                   ;; Should return just filenames, not paths
                   (assert-true (if (find (lambda (f) (string=? f "test-vec.ss")) files) #t #f))))

            (define-test test-lattice-all-tests-structure
              (lattice-ensure!)
              (let ([all (lattice-all-tests)])
                   (assert-true (> (length all) 0))
                   ;; Each entry should be (skill . tests-list)
                   (let ([first (car all)])
                        (assert-true (symbol? (car first)))
                        (assert-true (list? (cdr first))))))

            )

;;; ====
;;; Concept Tests (KG-first architecture)
;;; ====

(test-group concept-tests

            (define-test test-concepts-populated
              (kg-ensure!)
              (assert-true (> (length (kg-concepts)) 0)))

            (define-test test-concept-skills-lookup
              (kg-ensure!)
              ;; 'matrix' should be a concept (keyword in linalg manifest)
              (let ([skills (kg-concept-skills 'matrix)])
                (assert-true (list? skills))
                (assert-true (> (length skills) 0))))

            (define-test test-skill-concepts-lookup
              (kg-ensure!)
              ;; linalg should have concepts (from its keywords)
              (let ([concepts (kg-skill-concepts 'linalg)])
                (assert-true (list? concepts))
                (assert-true (> (length concepts) 0))))

            (define-test test-shared-concepts
              (kg-ensure!)
              ;; linalg and geometry should share at least some concepts
              (let ([shared (kg-shared-concepts 'linalg 'geometry)])
                (assert-true (list? shared))))

            (define-test test-related-by-concept
              (kg-ensure!)
              (let ([related (kg-related-by-concept 'linalg 5)])
                (assert-true (list? related))
                ;; linalg should be related to something
                (assert-true (> (length related) 0))
                ;; Each entry should be (skill-name . count)
                (let ([first (car related)])
                  (assert-true (symbol? (car first)))
                  (assert-true (number? (cdr first))))))

            (define-test test-concept-bridge
              (kg-ensure!)
              ;; concept-bridge should return a list (possibly empty)
              (let ([bridge (kg-concept-bridge 'linalg 'geometry)])
                (assert-true (list? bridge))))

            (define-test test-kg-stats-includes-concepts
              (kg-ensure!)
              (let ([stats (kg-stats)])
                (assert-true (if (assq 'concepts stats) #t #f))
                (assert-true (> (cdr (assq 'concepts stats)) 0))
                (assert-true (if (assq 'edges stats) #t #f))
                (assert-true (> (cdr (assq 'edges stats)) 0))))

            (define-test test-cas-stores-blocks
              (kg-ensure!)
              ;; After build, skill blocks should be in the CAS
              (let ([skill-block (kg-skill 'linalg)])
                (when skill-block
                  (let* ([hash (hash-block skill-block)]
                         [fetched (fetch hash)])
                    (assert-true (if fetched #t #f))))))

            )

;;; ====
;;; Concept-Aware Search Tests (fold-zy0y)
;;; ====

(test-group concept-search-tests

            (define-test test-concept-boost-for-query-empty
              "No boost when query terms don't match any concept"
              (lattice-ensure!)
              (let ([boost (concept-boost-for-query '(xyznonexistent))])
                (assert-equal 0 (hamt-size boost))))

            (define-test test-concept-boost-for-query-match
              "Query term matching a concept produces boost entries"
              (lattice-ensure!)
              ;; 'matrix' is a concept (keyword in linalg manifest)
              (let ([boost (concept-boost-for-query '(matrix))])
                (assert-true (> (hamt-size boost) 0))
                ;; linalg should be in the boost set
                (assert-true (if (hamt-lookup 'linalg boost) #t #f))))

            (define-test test-concept-boost-multi-term
              "Multiple concept-matching terms accumulate counts"
              (lattice-ensure!)
              ;; 'collision' and 'simulation' both link to physics skills
              (let ([boost (concept-boost-for-query '(collision simulation))])
                (assert-true (> (hamt-size boost) 0))
                ;; physics/diff should have count 2 (both concepts)
                (let ([count (hamt-lookup 'physics/diff boost)])
                  (assert-true (if count #t #f))
                  (assert-true (>= count 2)))))

            (define-test test-result-skill-name-skill
              "result->skill-name extracts id from skill results"
              (lattice-ensure!)
              (let ([result (list 'linalg 1.0 'skill '())])
                (assert-equal 'linalg (result->skill-name result))))

            (define-test test-result-skill-name-module
              "result->skill-name extracts skill from module results"
              (lattice-ensure!)
              (let ([result (list 'linalg/vec 0.5 'module '((name . linalg/vec) (skill . linalg)))])
                (assert-equal 'linalg (result->skill-name result))))

            (define-test test-apply-concept-boosts-no-match
              "Results unchanged when boost map is empty"
              (let ([results (list (list 'foo 1.0 'skill '()))]
                    [boost hamt-empty])
                (let ([boosted (apply-concept-boosts results boost)])
                  (assert-equal 1.0 (cadr (car boosted))))))

            (define-test test-apply-concept-boosts-with-match
              "Results boosted when skill matches concept"
              (let* ([results (list (list 'linalg 1.0 'skill '()))]
                     [boost (hamt-assoc 'linalg 1 hamt-empty)]
                     [boosted (apply-concept-boosts results boost)])
                ;; Should be 1.0 * (1 + 0.25 * 1) = 1.25
                (assert-equal 1.25 (cadr (car boosted)))))

            (define-test test-apply-concept-boosts-cap
              "Concept boost is capped at CONCEPT-BOOST-CAP"
              (let* ([results (list (list 'linalg 1.0 'skill '()))]
                     ;; 10 concept matches would give 1 + 0.25*10 = 3.5, capped to 2.0
                     [boost (hamt-assoc 'linalg 10 hamt-empty)]
                     [boosted (apply-concept-boosts results boost)])
                (assert-equal 2.0 (cadr (car boosted)))))

            (define-test test-concept-boost-changes-ranking
              "Concept boost can reorder search results"
              (lattice-ensure!)
              ;; Search for 'gradient descent' — optimization skill should rank high
              ;; because 'gradient-descent' is a concept linking to optimization
              (let ([results (lattice-find "gradient descent" 10 'skill)])
                (assert-true (> (length results) 0))
                ;; optimization should be in results (linked by concept)
                (assert-true (if (find (lambda (r) (eq? 'optimization (car r))) results)
                                 #t #f))))

            (define-test test-module-skill-map-populated
              "Module→skill reverse map is populated during indexing"
              (lattice-ensure!)
              ;; 'vec' module should map to 'linalg' skill
              (let ([skill (hamt-lookup 'vec *module-skill-map*)])
                (assert-equal 'linalg skill)))

            )

;;; ====
;;; Type Signature Tests (fold-zy0z)
;;; ====

(test-group type-sig-tests

            (define-test test-type-sigs-populated
              "Type signatures are extracted during build"
              (kg-ensure!)
              (assert-true (> (kg-type-count) 0)))

            (define-test test-type-sig-lookup
              "Can look up type for a known export"
              (kg-ensure!)
              ;; foldr has a type annotation: (-> (-> α β β) β (List α) β)
              (let ([type (kg-export-type 'foldr)])
                (assert-true (if type #t #f))))

            (define-test test-type-sig-unknown
              "Unknown exports return #f for type"
              (kg-ensure!)
              (assert-false (kg-export-type 'xyznonexistent-symbol-123)))

            (define-test test-typed-exports-list
              "kg-typed-exports returns non-empty list"
              (kg-ensure!)
              (let ([typed (kg-typed-exports)])
                (assert-true (> (length typed) 0))
                ;; Each entry is (symbol . type-expr)
                (let ([first (car typed)])
                  (assert-true (symbol? (car first))))))

            (define-test test-type-sig-in-stats
              "kg-stats includes type-sigs count"
              (kg-ensure!)
              (let ([stats (kg-stats)])
                (assert-true (if (assq 'type-sigs stats) #t #f))
                (assert-true (> (cdr (assq 'type-sigs stats)) 0))))

            (define-test test-type-sig-terms
              "type-sig->terms tokenizes type expressions"
              (lattice-ensure!)
              ;; (-> Matrix Vector) should produce tokens including matrix, vector
              (let ([terms (type-sig->terms '(-> Matrix Vector))])
                (assert-true (> (length terms) 0))
                (assert-true (if (memq 'matrix terms) #t #f))
                (assert-true (if (memq 'vector terms) #t #f))
                (assert-true (if (memq 'arrow terms) #t #f))))

            (define-test test-type-sig-in-search-results
              "Search results include type data for typed exports"
              (lattice-ensure!)
              ;; Search for foldr which has a type annotation
              (let ([results (lattice-find "foldr" 5 'export)])
                (when (> (length results) 0)
                  (let* ([first (car results)]
                         [data (cadddr first)]
                         [typ (and data (assq 'type data))])
                    ;; foldr should have type data
                    (when (eq? (car first) 'foldr)
                      (assert-true (if typ #t #f)))))))

            (define-test test-type-search-finds-typed-exports
              "Searching for a type name surfaces typed exports"
              (lattice-ensure!)
              ;; Searching "Matrix" should find functions with Matrix in their type
              (let ([results (lattice-find "matrix" 20 'export)])
                (assert-true (> (length results) 0))))

            )

;;; ====
;;; CAS Round-Trip Tests (fold-zy11)
;;; ====

(test-group cas-round-trip-tests

            ;; Build KG, save root, reset, load from CAS, verify all state
            (define-test test-cas-round-trip-skills
              (kg-ensure!)
              (let ([original-skills (kg-skills)]
                    [root-hash *kg-index-root*])
                ;; Reset and reload from CAS
                (kg-reset!)
                (assert-true (null? (kg-skills)))
                (assert-true (if (kg-load-from-cas! root-hash) #t #f))
                ;; Skills should be restored
                (assert-equal (length original-skills) (length (kg-skills)))
                ;; Specific skill should exist
                (assert-true (if (memq 'linalg (kg-skills)) #t #f))))

            (define-test test-cas-round-trip-skill-data
              ;; After CAS load, skill-data should have full manifest info
              (assert-true (kg-initialized?))
              (let ([data (kg-skill-data 'linalg)])
                (assert-true (if data #t #f))
                ;; Should have key manifest fields
                (assert-true (if (assq 'name data) #t #f))
                (assert-true (if (assq 'path data) #t #f))
                (assert-true (if (assq 'modules data) #t #f))
                (assert-true (if (assq 'exports data) #t #f))))

            (define-test test-cas-round-trip-modules
              ;; Modules should be reconstructed from manifest data
              (let ([mods (kg-modules 'linalg)])
                (assert-true (> (length mods) 0))))

            (define-test test-cas-round-trip-exports
              ;; Exports should be reconstructed from manifest data
              (let ([exports (kg-exports)])
                (assert-true (> (length exports) 0))))

            (define-test test-cas-round-trip-deps
              ;; Dependencies should work after CAS load
              ;; Uses derived deps (from require graph, acyclic)
              (let ([deps (kg-deps 'optimization)])
                (assert-true (if (memq 'linalg deps) #t #f))
                (assert-true (if (memq 'data deps) #t #f))))

            (define-test test-cas-round-trip-concepts
              ;; Concepts should be restored
              (assert-true (> (length (kg-concepts)) 0))
              ;; Concept reverse maps should be rebuilt
              (let ([skills (kg-concept-skills 'matrix)])
                (assert-true (list? skills))
                (assert-true (> (length skills) 0)))
              (let ([concepts (kg-skill-concepts 'linalg)])
                (assert-true (list? concepts))
                (assert-true (> (length concepts) 0))))

            (define-test test-cas-round-trip-search-ready
              ;; After CAS load + index, search should work
              (lattice-index!)
              (let ([results (lattice-find "matrix")])
                (assert-true (> (length results) 0))))

            (define-test test-cas-round-trip-type-sigs
              ;; Type sigs should survive CAS round-trip
              (assert-true (> (kg-type-count) 0))
              ;; A specific known type should exist
              (let ([type (kg-export-type 'foldr)])
                (assert-true (if type #t #f))))

            )

;;; ====
;;; Summary
;;; ====

(display "\n====\n")
(display (format "Tests: ~a run, ~a passed, ~a failed\n"
                 *tests-run* *tests-passed* *tests-failed*))
(display "====\n")

(when (> *tests-failed* 0)
      (exit 1))
