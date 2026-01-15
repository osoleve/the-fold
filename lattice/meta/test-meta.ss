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
              (kg-build!)
              (assert-true (if (kg-skill 'linalg) #t #f)))
            
            (define-test test-kg-deps
              (kg-build!)
              (let ([deps (kg-deps 'optimization)])
                   (assert-true (if (memq 'autodiff deps) #t #f))
                   (assert-true (if (memq 'linalg deps) #t #f))))
            
            (define-test test-kg-uses
              (kg-build!)
              (let ([uses (kg-uses 'linalg)])
                   (assert-true (> (length uses) 0))))
            
            )

;;; ====
;;; DAG Navigation Tests
;;; ====

(test-group dag-tests

            (define-test test-lattice-roots
              (kg-build!)
              (let ([roots (lattice-roots)])
                   (assert-true (if (memq 'linalg roots) #t #f))
                   (assert-true (if (memq 'data roots) #t #f))))

            (define-test test-lattice-deps-transitive
              (kg-build!)
              (let ([deps (lattice-deps-transitive 'physics/diff)])
                   (assert-true (if (memq 'linalg deps) #t #f))))

            (define-test test-lattice-path
              (kg-build!)
              (let ([path (lattice-path 'physics/diff 'linalg)])
                   (assert-true (list? path))))

            (define-test test-lattice-tiers
              (kg-build!)
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
              (kg-build!)
              (assert-true (lattice-would-cycle? 'linalg 'linalg)))

            ;; Test that safe dependencies are correctly identified
            (define-test test-safe-dependency
              (kg-build!)
              ;; physics/diff -> linalg exists and is safe (linalg is tier 0)
              (assert-false (lattice-would-cycle? 'physics/diff 'linalg)))

            ;; Test that reverse dependencies would create cycles
            (define-test test-reverse-would-cycle
              (kg-build!)
              ;; linalg -> physics/diff would create cycle since physics/diff -> linalg
              (assert-true (lattice-would-cycle? 'linalg 'physics/diff)))

            ;; Test cycle path finding for self-loop
            (define-test test-find-cycle-path-self
              (kg-build!)
              (let ([path (lattice-find-cycle-path 'linalg 'linalg)])
                   (assert-true (list? path))
                   (assert-equal '(linalg linalg) path)))

            ;; Test cycle path finding for valid cycle
            (define-test test-find-cycle-path-real
              (kg-build!)
              (let ([path (lattice-find-cycle-path 'linalg 'physics/diff)])
                   (assert-true (list? path))
                   (assert-true (> (length path) 2))))

            ;; Test no cycle path for safe dep
            (define-test test-no-cycle-path-safe
              (kg-build!)
              (assert-false (lattice-find-cycle-path 'physics/diff 'linalg)))

            ;; Test lattice-check-deps returns ok for valid skill
            (define-test test-check-deps-valid
              (kg-build!)
              (let ([result (lattice-check-deps 'physics/diff)])
                   (assert-true (pair? result))
                   (assert-equal 'ok (car result))))

            ;; Test lattice-check-deps returns ok for tier 0 skill (no deps)
            (define-test test-check-deps-tier0
              (kg-build!)
              (let ([result (lattice-check-deps 'linalg)])
                   (assert-true (pair? result))
                   (assert-equal 'ok (car result))))

            ;; Test lattice-validate-all returns empty on healthy lattice
            (define-test test-validate-all-healthy
              (kg-build!)
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
              (kg-build!)
              (lattice-index!)
              (assert-true *search-ready*))
            
            (define-test test-lattice-find
              (lattice-init!)
              (let ([results (lattice-find "matrix")])
                   (assert-true (> (length results) 0))))
            
            (define-test test-lattice-find-exact
              (lattice-init!)
              (let ([result (lattice-find-exact 'linalg)])
                   (assert-true (list? result))))
            
            (define-test test-lattice-complete
              (lattice-init!)
              (let ([results (lattice-complete "mat")])
                   (assert-true (> (length results) 0))))
            
            )

;;; ====
;;; Analytics Tests
;;; ====

(test-group analytics-tests
            
            (define-test test-lattice-stats
              (lattice-init!)
              (let ([stats (lattice-stats)])
                   (assert-true (> (cdr (assq 'skills stats)) 0))
                   (assert-true (> (cdr (assq 'modules stats)) 0))
                   (assert-true (> (cdr (assq 'exports stats)) 0))))
            
            (define-test test-lattice-health
              (lattice-init!)
              (let ([report (lattice-health)])
                   (assert-true (list? report))
                   (assert-true (if (assq 'healthy report) #t #f))))
            
            (define-test test-lattice-coverage
              (lattice-init!)
              (let ([cov (lattice-coverage)])
                   (assert-true (if (assq 'total cov) #t #f))
                   (assert-true (>= (cdr (assq 'total cov)) 0))))
            
            )

;;; ====
;;; Inspection Tests
;;; ====

(test-group inspect-tests
            
            (define-test test-lattice-info
              (lattice-init!)
              (let ([info (lattice-info 'linalg)])
                   (assert-true (list? info))
                   (assert-equal 'linalg (cdr (assq 'name info)))
                   (assert-true (if (assq 'tier info) #t #f))
                   (assert-true (if (assq 'purity info) #t #f))))
            
            (define-test test-lattice-skill-exports
              (lattice-init!)
              (let ([exports (lattice-skill-exports 'linalg)])
                   (assert-true (> (length exports) 0))))
            
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
