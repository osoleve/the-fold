;;; lattice/meta/test-bridge-discovery.ss -- Tests for bridge discovery module
;;;
;;; Run with: scheme --script lattice/meta/test-bridge-discovery.ss

(load "core/testing/test-framework.ss")
(load "lattice/meta/bridge-discovery.ss")

(display "\n====\n")
(display "Bridge Discovery Tests\n")
(display "====\n\n")

;;; has? : Symbol (List Symbol) -> Boolean
;;; Boolean wrapper for memq (assert-true needs #t, not a list tail).
(define (has? sym lst) (if (memq sym lst) #t #f))

;;; ====================================================================
;;; Registry Tests
;;; ====================================================================

(test-group bridge-registry-tests

  (define-test test-bridge-count
    "Registry has 23 known bridge modules"
    (assert-true (>= (bridge-count) 23)))

  (define-test test-bridge-predicate-positive
    "bridge? returns true for registered bridges"
    (assert-true (bridge? 'traced-optics))
    (assert-true (bridge? 'graph-laplacian))
    (assert-true (bridge? 'info-stat/mi-bridge)))

  (define-test test-bridge-predicate-negative
    "bridge? returns false for non-bridge modules"
    (assert-false (bridge? 'prelude))
    (assert-false (bridge? 'vec))
    (assert-false (bridge? 'this-module-does-not-exist)))

  (define-test test-bridge-domains-lookup
    "bridge-domains returns correct domain list"
    (let ([domains (bridge-domains 'traced-optics)])
      (assert-true (pair? domains))
      (assert-true (has? 'optics domains))
      (assert-true (has? 'autodiff domains))))

  (define-test test-bridge-domains-missing
    "bridge-domains returns #f for non-bridge"
    (assert-false (bridge-domains 'prelude)))

  (define-test test-register-bridge
    "register-bridge! adds new entries"
    (register-bridge! '__test-bridge '(alpha beta))
    (assert-true (bridge? '__test-bridge))
    (let ([domains (bridge-domains '__test-bridge)])
      (assert-true (has? 'alpha domains))
      (assert-true (has? 'beta domains)))
    ;; Clean up
    (hashtable-delete! *bridge-registry* '__test-bridge))
)

;;; ====================================================================
;;; Query Tests
;;; ====================================================================

(test-group bridge-query-tests

  (define-test test-lattice-bridges-returns-list
    "lattice-bridges returns a non-empty list"
    (let ([bridges (lattice-bridges)])
      (assert-true (pair? bridges))
      (assert-true (>= (length bridges) 23))))

  (define-test test-lattice-bridges-sorted
    "lattice-bridges returns entries sorted by module name"
    (let* ([bridges (lattice-bridges)]
           [names (map car bridges)]
           [name-strs (map symbol->string names)])
      ;; Check each consecutive pair is in order
      (let loop ([prev (car name-strs)]
                 [rest (cdr name-strs)])
        (unless (null? rest)
          (assert-true (string<=? prev (car rest)))
          (loop (car rest) (cdr rest))))))

  (define-test test-lattice-bridges-entry-format
    "Each entry is (symbol . (list-of-symbols))"
    (let ([bridges (lattice-bridges)])
      (for-each
        (lambda (entry)
          (assert-true (symbol? (car entry)))
          (assert-true (pair? (cdr entry)))
          (assert-true (>= (length (cdr entry)) 2))
          (for-each
            (lambda (d) (assert-true (symbol? d)))
            (cdr entry)))
        bridges)))

  (define-test test-lattice-bridges-for-optics
    "lattice-bridges-for 'optics finds known optics bridges"
    (let* ([bridges (lattice-bridges-for 'optics)]
           [names (map car bridges)])
      (assert-true (pair? bridges))
      (assert-true (has? 'traced-optics names))
      (assert-true (has? 'matrix-optics names))
      (assert-true (has? 'geometry-optics names))
      (assert-true (has? 'optics-integration names))))

  (define-test test-lattice-bridges-for-autodiff
    "lattice-bridges-for 'autodiff finds autodiff bridges"
    (let* ([bridges (lattice-bridges-for 'autodiff)]
           [names (map car bridges)])
      (assert-true (pair? bridges))
      (assert-true (has? 'traced-optics names))
      (assert-true (has? 'first-order names))
      (assert-true (has? 'differentiable-signal names))))

  (define-test test-lattice-bridges-for-nonexistent
    "lattice-bridges-for returns empty for unknown skill"
    (assert-true (null? (lattice-bridges-for 'nonexistent-skill))))

  (define-test test-lattice-bridges-for-data
    "lattice-bridges-for 'data finds graph-matrix etc"
    (let* ([bridges (lattice-bridges-for 'data)]
           [names (map car bridges)])
      (assert-true (has? 'graph-matrix names))
      (assert-true (has? 'graph-laplacian names))
      (assert-true (has? 'graph-homology names))
      (assert-true (has? 'tropical-graph names))))
)

;;; ====================================================================
;;; Path Parsing Tests
;;; ====================================================================

(test-group path-parsing-tests

  (define-test test-path-to-skill-domain-lattice
    "path->skill-domain extracts domain from lattice paths"
    (assert-equal 'linalg (path->skill-domain "lattice/linalg/vec.ss"))
    (assert-equal 'data (path->skill-domain "lattice/data/graph/pagerank.ss"))
    (assert-equal 'fp (path->skill-domain "lattice/fp/sat/solver.ss"))
    (assert-equal 'optics (path->skill-domain "lattice/optics/optics.ss"))
    (assert-equal 'physics (path->skill-domain "lattice/physics/diff/traced-vec2.ss")))

  (define-test test-path-to-skill-domain-core-autodiff
    "path->skill-domain handles core/autodiff specially"
    (assert-equal 'autodiff (path->skill-domain "core/autodiff/comp-graph.ss")))

  (define-test test-path-to-skill-domain-non-lattice
    "path->skill-domain returns #f for non-lattice paths"
    (assert-false (path->skill-domain "boundary/repl/repl.ss"))
    (assert-false (path->skill-domain "core/base/prelude.ss")))

  (define-test test-module-skill-domain
    "module-skill-domain works for registered modules"
    (assert-equal 'linalg (module-skill-domain 'vec))
    (assert-equal 'data (module-skill-domain 'graph-matrix))
    (assert-equal 'autodiff (module-skill-domain 'reverse-diff)))
)

;;; ====================================================================
;;; Potential Bridge Detection Tests
;;; ====================================================================

(test-group potential-bridge-tests

  (define-test test-find-potential-bridges-returns-list
    "find-potential-bridges returns a list"
    (let ([candidates (find-potential-bridges)])
      (assert-true (list? candidates))))

  (define-test test-potential-bridges-exclude-annotated
    "Annotated bridges do not appear in potential bridges"
    (let* ([candidates (find-potential-bridges)]
           [candidate-names (map car candidates)])
      (assert-false (has? 'traced-optics candidate-names))
      (assert-false (has? 'graph-laplacian candidate-names))
      (assert-false (has? 'info-stat/mi-bridge candidate-names))))

  (define-test test-potential-bridges-entry-format
    "Each potential bridge entry has symbol and 2+ domains"
    (let ([candidates (find-potential-bridges)])
      (for-each
        (lambda (entry)
          (assert-true (symbol? (car entry)))
          (assert-true (>= (length (cdr entry)) 2)))
        candidates)))

  (define-test test-potential-bridges-sorted
    "Potential bridges are sorted by module name"
    (let* ([candidates (find-potential-bridges)]
           [names (map car candidates)]
           [name-strs (map symbol->string names)])
      (unless (null? name-strs)
        (let loop ([prev (car name-strs)]
                   [rest (cdr name-strs)])
          (unless (null? rest)
            (assert-true (string<=? prev (car rest)))
            (loop (car rest) (cdr rest)))))))
)

;;; ====================================================================
;;; Alias Tests
;;; ====================================================================

(test-group alias-tests

  (define-test test-lb-alias
    "lb is the same as lattice-bridges"
    (assert-equal (lattice-bridges) (lb)))

  (define-test test-lbf-alias
    "lbf is the same as lattice-bridges-for"
    (assert-equal (lattice-bridges-for 'optics) (lbf 'optics)))

  (define-test test-lbp-alias
    "lbp is the same as find-potential-bridges"
    (assert-equal (find-potential-bridges) (lbp)))
)

(run-all-tests)
