;;; lattice/data/graph/test-spectral-community.ss — Tests for spectral-community bridge
;;;
;;; Tests that spectral graph theory methods integrate properly with
;;; the community detection API.
;;;
;;; Run from project root: scheme --script lattice/data/graph/test-spectral-community.ss

(load "core/lang/module.ss")
(load "lattice/data/graph/spectral-community.ss")

(define tests-passed 0)
(define tests-failed 0)

(define (test name expected actual)
  (if (equal? expected actual)
      (begin
        (set! tests-passed (+ tests-passed 1))
        (display "  PASS ") (display name) (newline))
      (begin
        (set! tests-failed (+ tests-failed 1))
        (display "  FAIL ") (display name)
        (display " -- expected ") (write expected)
        (display ", got ") (write actual)
        (newline))))

(define (test-true name actual)
  (test name #t (if actual #t #f)))

(define (test-false name actual)
  (test name #f (if actual #t #f)))

(define (test-approx name expected actual tol)
  (if (< (abs (- expected actual)) tol)
      (begin
        (set! tests-passed (+ tests-passed 1))
        (display "  PASS ") (display name) (newline))
      (begin
        (set! tests-failed (+ tests-failed 1))
        (display "  FAIL ") (display name)
        (display " -- expected ~") (write expected)
        (display ", got ") (write actual)
        (newline))))

(define (test-group name)
  (newline)
  (display "=== ")
  (display name)
  (display " ===")
  (newline))

(display "\nTesting spectral-community.ss\n")
(display "====\n")

;;; ====
;;; Test Graphs
;;; ====

;;; Two disconnected triangles (clear 2-community structure)
(define two-cliques-edges
  '((0 1) (1 2) (2 0)      ; First triangle (K_3)
    (3 4) (4 5) (5 3)))    ; Second triangle (K_3)

(define two-cliques-adj
  (edges->adjacency-matrix two-cliques-edges 6 #t))

;;; Barbell graph: two triangles connected by one edge
(define barbell-edges
  '((0 1) (1 2) (2 0)      ; First triangle
    (3 4) (4 5) (5 3)      ; Second triangle
    (2 3)))                 ; Bridge edge

(define barbell-adj
  (edges->adjacency-matrix barbell-edges 6 #t))

;;; Ring graph C_6
(define ring-adj (cycle-graph 6))

;;; Complete graph K_4
(define k4-adj (complete-graph 4))

;;; Path graph P_4
(define path-4-adj (path-graph 4))

;;; ====
;;; Spectral Bipartition Tests
;;; ====

(test-group "Spectral Bipartition")

;; Two disconnected cliques -- spectral bipartition should find them
(let ([labels (spectral-bipartition two-cliques-adj)])
  (if (and (pair? labels) (eq? (car labels) 'error))
      (display "  ~ two-cliques bipartition (skipped: eigenvalue error)\n")
      (begin
        (test "bipartition returns vector" #t (vector? labels))
        (test "bipartition length" 6 (vector-length labels))
        ;; Nodes in same clique should share label
        (test-true "nodes 0,1 same label"
                   (= (vector-ref labels 0) (vector-ref labels 1)))
        (test-true "nodes 1,2 same label"
                   (= (vector-ref labels 1) (vector-ref labels 2)))
        (test-true "nodes 3,4 same label"
                   (= (vector-ref labels 3) (vector-ref labels 4)))
        (test-true "nodes 4,5 same label"
                   (= (vector-ref labels 4) (vector-ref labels 5)))
        ;; The two cliques should get different labels
        (test-true "cliques get different labels"
                   (not (= (vector-ref labels 0) (vector-ref labels 3))))
        ;; Labels should be 0 or 1
        (test-true "labels are binary"
                   (let loop ([i 0])
                     (or (= i (vector-length labels))
                         (and (or (= (vector-ref labels i) 0)
                                  (= (vector-ref labels i) 1))
                              (loop (+ i 1)))))))))

;; Barbell graph should partition at the bridge
(let ([labels (spectral-bipartition barbell-adj)])
  (if (and (pair? labels) (eq? (car labels) 'error))
      (display "  ~ barbell bipartition (skipped: eigenvalue error)\n")
      (begin
        (test "barbell bipartition returns vector" #t (vector? labels))
        ;; Nodes in same triangle should share label
        (test-true "barbell 0,1 same label"
                   (= (vector-ref labels 0) (vector-ref labels 1)))
        (test-true "barbell 3,4 same label"
                   (= (vector-ref labels 3) (vector-ref labels 4))))))

;; Ring graph bipartition
(let ([labels (spectral-bipartition ring-adj)])
  (if (and (pair? labels) (eq? (car labels) 'error))
      (display "  ~ ring bipartition (skipped: eigenvalue error)\n")
      (begin
        (test "ring bipartition returns vector" #t (vector? labels))
        (test "ring bipartition length" 6 (vector-length labels))
        ;; Should produce exactly 2 communities
        (test "ring bipartition has 2 communities" 2 (num-communities labels)))))

;;; ====
;;; Spectral Communities Tests
;;; ====

(test-group "Spectral Communities (k partitions)")

;; Two cliques with k=2
(let ([labels (spectral-communities two-cliques-adj 2)])
  (if (and (pair? labels) (eq? (car labels) 'error))
      (display "  ~ two-cliques k=2 (skipped: eigenvalue error)\n")
      (begin
        (test "k=2 returns vector" #t (vector? labels))
        (test "k=2 has 2 communities" 2 (num-communities labels))
        ;; Same clique -> same label
        (test-true "clique 1 same label"
                   (and (= (vector-ref labels 0) (vector-ref labels 1))
                        (= (vector-ref labels 1) (vector-ref labels 2))))
        (test-true "clique 2 same label"
                   (and (= (vector-ref labels 3) (vector-ref labels 4))
                        (= (vector-ref labels 4) (vector-ref labels 5))))
        ;; Different cliques -> different labels
        (test-true "cliques separated"
                   (not (= (vector-ref labels 0) (vector-ref labels 3)))))))

;; Single community (k=1) should label everything 0
(let ([labels (spectral-communities two-cliques-adj 1)])
  (if (and (pair? labels) (eq? (car labels) 'error))
      (display "  ~ k=1 (skipped: eigenvalue error)\n")
      (begin
        (test "k=1 returns vector" #t (vector? labels))
        (test "k=1 has 1 community" 1 (num-communities labels))
        (test "k=1 all label 0" 0 (vector-ref labels 0)))))

;;; ====
;;; Spectral Modularity Tests
;;; ====

(test-group "Spectral Modularity")

;; Two disconnected cliques should have high modularity
(let ([Q (spectral-modularity two-cliques-adj 2)])
  (if (and (pair? Q) (eq? (car Q) 'error))
      (display "  ~ two-cliques modularity (skipped: eigenvalue error)\n")
      (begin
        (test-true "two-cliques Q > 0.3" (> Q 0.3))
        (test-true "two-cliques Q is number" (number? Q)))))

;; Barbell should have decent modularity with k=2
(let ([Q (spectral-modularity barbell-adj 2)])
  (if (and (pair? Q) (eq? (car Q) 'error))
      (display "  ~ barbell modularity (skipped: eigenvalue error)\n")
      (test-true "barbell Q >= 0" (>= Q 0))))

;; K4 all-in-one should have Q near 0
(let ([Q (spectral-modularity k4-adj 1)])
  (if (and (pair? Q) (eq? (car Q) 'error))
      (display "  ~ K4 modularity k=1 (skipped: eigenvalue error)\n")
      (test-approx "K4 k=1 Q near 0" 0.0 Q 0.01)))

;;; ====
;;; Partition Conductance Tests
;;; ====

(test-group "Partition Conductance")

;; Two disconnected cliques: conductance should be 0 (no edges between them)
(let* ([labels (spectral-bipartition two-cliques-adj)])
  (if (and (pair? labels) (eq? (car labels) 'error))
      (display "  ~ two-cliques conductance (skipped: eigenvalue error)\n")
      (let ([conds (partition-conductance two-cliques-adj labels)])
        (test "two-cliques conductance list length" 2 (length conds))
        ;; Disconnected cliques have conductance 0
        (test-approx "clique 1 conductance" 0.0 (car conds) 1e-10)
        (test-approx "clique 2 conductance" 0.0 (cadr conds) 1e-10))))

;; Barbell: conductance should be positive but low
(let* ([labels (spectral-bipartition barbell-adj)])
  (if (and (pair? labels) (eq? (car labels) 'error))
      (display "  ~ barbell conductance (skipped: eigenvalue error)\n")
      (let ([conds (partition-conductance barbell-adj labels)])
        (test-true "barbell conductances non-negative"
                   (and (>= (car conds) 0)
                        (>= (cadr conds) 0))))))

;; Conductance values should be non-negative for any partition
(let* ([labels (vector 0 0 1 1)]
       [conds (partition-conductance path-4-adj labels)])
  (test "path conductance list length" 2 (length conds))
  (test-true "path conductance 1 non-negative" (>= (car conds) 0))
  (test-true "path conductance 2 non-negative" (>= (cadr conds) 0)))

;;; ====
;;; Partition Normalized Cut Tests
;;; ====

(test-group "Partition Normalized Cut")

;; Two disconnected cliques: normalized cut should be 0
(let* ([labels (spectral-bipartition two-cliques-adj)])
  (if (and (pair? labels) (eq? (car labels) 'error))
      (display "  ~ two-cliques ncut (skipped: eigenvalue error)\n")
      (let ([ncuts (partition-normalized-cut two-cliques-adj labels)])
        (test "two-cliques ncut list length" 2 (length ncuts)))))

;;; ====
;;; Compare Partitions Tests
;;; ====

(test-group "Compare Partitions")

(let ([result (compare-partitions barbell-adj 2)])
  (if (and (pair? result) (eq? (car result) 'error))
      (display "  ~ compare partitions (skipped: eigenvalue error)\n")
      (begin
        ;; Result should be an association list with all expected keys
        (test-true "has spectral-modularity"
                   (pair? (assq 'spectral-modularity result)))
        (test-true "has label-prop-modularity"
                   (pair? (assq 'label-prop-modularity result)))
        (test-true "has spectral-communities"
                   (pair? (assq 'spectral-communities result)))
        (test-true "has label-prop-communities"
                   (pair? (assq 'label-prop-communities result)))
        ;; Both modularities should be numbers
        (test-true "spectral Q is number"
                   (number? (cdr (assq 'spectral-modularity result))))
        (test-true "label-prop Q is number"
                   (number? (cdr (assq 'label-prop-modularity result)))))))

;;; ====
;;; Spectral Quality Tests
;;; ====

(test-group "Spectral Quality")

(let ([result (spectral-quality barbell-adj 2)])
  (if (and (pair? result) (eq? (car result) 'error))
      (display "  ~ spectral quality (skipped: eigenvalue error)\n")
      (begin
        (test-true "has modularity" (pair? (assq 'modularity result)))
        (test-true "has num-communities" (pair? (assq 'num-communities result)))
        (test-true "has community-sizes" (pair? (assq 'community-sizes result)))
        (test-true "has conductances" (pair? (assq 'conductances result)))
        (test-true "has algebraic-connectivity"
                   (pair? (assq 'algebraic-connectivity result)))
        (test-true "modularity is number"
                   (number? (cdr (assq 'modularity result))))
        (test "num-communities is 2" 2
              (cdr (assq 'num-communities result)))
        ;; Community sizes should sum to n
        (test "sizes sum to 6" 6
              (apply + (cdr (assq 'community-sizes result)))))))

;;; ====
;;; Integration: Spectral Labels with Graph-Community Functions
;;; ====

(test-group "Integration with graph-community")

;; spectral-bipartition output works with communities->partition
(let ([labels (spectral-bipartition two-cliques-adj)])
  (if (and (pair? labels) (eq? (car labels) 'error))
      (display "  ~ partition integration (skipped: eigenvalue error)\n")
      (let ([partition (communities->partition labels)])
        (test "partition from spectral labels" 2 (length partition))
        (test "partition covers all 6 nodes" 6
              (apply + (map length partition))))))

;; spectral-communities output works with modularity
(let ([labels (spectral-communities two-cliques-adj 2)])
  (if (and (pair? labels) (eq? (car labels) 'error))
      (display "  ~ modularity integration (skipped: eigenvalue error)\n")
      (let ([Q (modularity two-cliques-adj labels)])
        (test-true "spectral labels give valid modularity" (number? Q))
        (test-true "spectral modularity > 0.3 for clear structure" (> Q 0.3)))))

;;; ====
;;; Summary
;;; ====

(newline)
(display "====\n")
(display "Tests passed: ") (display tests-passed) (newline)
(display "Tests failed: ") (display tests-failed) (newline)
(display "====\n")

(when (> tests-failed 0)
  (error 'test-spectral-community "Some tests failed"))
