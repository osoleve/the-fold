;;; lattice/meta/test-meta.ss — Tests for Lattice Meta-Tooling
;;;
;;; Run with: scheme --script lattice/meta/test-meta.ss

(load "core/testing/test-framework.ss")
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
              (let ([deps (kg-deps 'optimization)])
                   (assert-true (if (memq 'autodiff deps) #t #f))
                   (assert-true (if (memq 'linalg deps) #t #f))))

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
              (let ([roots (lattice-roots)])
                   (assert-true (if (memq 'linalg roots) #t #f))
                   (assert-true (if (memq 'data roots) #t #f))))

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
;;; Summary
;;; ====

(display "\n====\n")
(display (format "Tests: ~a run, ~a passed, ~a failed\n"
                 *tests-run* *tests-passed* *tests-failed*))
(display "====\n")

(when (> *tests-failed* 0)
      (exit 1))
