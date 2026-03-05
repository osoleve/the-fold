;;; test-graph-matching.ss — Tests for Edmonds' Blossom maximum matching
;;;
;;; Run with: scheme --script lattice/data/graph/test-graph-matching.ss

(load "core/lang/module.ss")
(load "lattice/data/graph/graph-matching.ss")
(load "core/testing/test-framework.ss")

(printf "\n=== Maximum Matching (Blossom) Tests ===\n\n")

;;; ====
;;; Helpers
;;; ====

;;; Verify matching validity: every vertex appears at most once,
;;; every pair is an actual edge.
(define (valid-matching? edges-or-adj n result)
  (let ([size (car result)]
        [pairs (cadr result)]
        [seen (make-vector n #f)])
    (and (= size (length pairs))
         (let check ([ps pairs])
           (if (null? ps)
               #t
               (let ([u (caar ps)]
                     [v (cadar ps)])
                 (and (< u v)               ; canonical order
                      (not (vector-ref seen u))
                      (not (vector-ref seen v))
                      (begin
                        (vector-set! seen u #t)
                        (vector-set! seen v #t)
                        (check (cdr ps))))))))))

;;; ====
;;; 1. Trivial Cases
;;; ====

(test-group "trivial"
  (define-test "empty edge list"
    (assert-equal (list 0 '()) (max-matching '())))

  (define-test "single edge"
    (let ([r (max-matching '((0 1)))])
      (assert-equal 1 (car r))
      (assert-equal '((0 1)) (cadr r))))

  (define-test "two disjoint edges"
    (let ([r (max-matching '((0 1) (2 3)))])
      (assert-equal 2 (car r))))

  (define-test "path P3 (3 vertices, 2 edges)"
    (let ([r (max-matching '((0 1) (1 2)))])
      (assert-equal 1 (car r))
      (assert-true (valid-matching? '((0 1) (1 2)) 3 r)))))

;;; ====
;;; 2. Odd Cycles — The Raison d'Etre
;;; ====

(test-group "odd-cycles"
  (define-test "triangle C3 — matching size 1"
    (let ([r (max-matching '((0 1) (1 2) (2 0)))])
      (assert-equal 1 (car r))
      (assert-true (valid-matching? '((0 1) (1 2) (2 0)) 3 r))))

  (define-test "pentagon C5 — matching size 2"
    (let ([r (max-matching (cycle-graph 5))])
      (assert-equal 2 (car r))))

  (define-test "heptagon C7 — matching size 3"
    (let ([r (max-matching (cycle-graph 7))])
      (assert-equal 3 (car r))))

  (define-test "C9 — matching size 4"
    (let ([r (max-matching (cycle-graph 9))])
      (assert-equal 4 (car r)))))

;;; ====
;;; 3. Blossom Contraction Required
;;; ====

(test-group "blossom-contraction"
  (define-test "triangle with pendant — needs contraction to find size 2"
    ;; 0-1, 1-2, 2-0, 2-3
    ;; Without blossom contraction: might find only {1,2} missing the pendant
    ;; With contraction: finds {0,1} + {2,3} = size 2
    (let ([r (max-matching '((0 1) (1 2) (2 0) (2 3)))])
      (assert-equal 2 (car r))))

  (define-test "bowtie — two triangles sharing vertex 2"
    ;; Triangle 0-1-2 + triangle 2-3-4
    (let ([r (max-matching '((0 1) (1 2) (2 0) (2 3) (3 4) (4 2)))])
      (assert-equal 2 (car r))))

  (define-test "K4 — complete graph on 4 vertices"
    (let ([r (max-matching (complete-graph 4))])
      (assert-equal 2 (car r))
      (assert-true (perfect-matching? 4 r)))))

;;; ====
;;; 4. Even Cycles (Bipartite) — Should Match floor(n/2)
;;; ====

(test-group "even-cycles"
  (define-test "square C4 — perfect matching"
    (let ([r (max-matching (cycle-graph 4))])
      (assert-equal 2 (car r))
      (assert-true (perfect-matching? 4 r))))

  (define-test "hexagon C6 — perfect matching"
    (let ([r (max-matching (cycle-graph 6))])
      (assert-equal 3 (car r))
      (assert-true (perfect-matching? 6 r)))))

;;; ====
;;; 5. Structured Graphs
;;; ====

(test-group "structured"
  (define-test "complete-graph K5 — floor(5/2) = 2"
    (let ([r (max-matching (complete-graph 5))])
      (assert-equal 2 (car r))))

  (define-test "complete-graph K6 — perfect matching, size 3"
    (let ([r (max-matching (complete-graph 6))])
      (assert-equal 3 (car r))
      (assert-true (perfect-matching? 6 r))))

  (define-test "path P5 — matching size 2"
    (let ([r (max-matching (path-graph 5))])
      (assert-equal 2 (car r))))

  (define-test "path P6 — perfect matching, size 3"
    (let ([r (max-matching (path-graph 6))])
      (assert-equal 3 (car r))
      (assert-true (perfect-matching? 6 r))))

  (define-test "star S5 — matching size 1"
    ;; Star: center 0 connected to 1,2,3,4. Only one edge can match.
    (let ([r (max-matching (star-graph 5))])
      (assert-equal 1 (car r)))))

;;; ====
;;; 6. Petersen Graph — Classic Non-Bipartite Perfect Matching
;;; ====

(test-group "petersen"
  (define-test "Petersen graph — perfect matching, size 5"
    ;; 10 vertices, 15 edges, 3-regular
    ;; Outer cycle: 0-1-2-3-4-0
    ;; Inner pentagram: 5-7-9-6-8-5
    ;; Spokes: 0-5, 1-6, 2-7, 3-8, 4-9
    (let ([r (max-matching '((0 1) (1 2) (2 3) (3 4) (4 0)
                              (5 7) (7 9) (9 6) (6 8) (8 5)
                              (0 5) (1 6) (2 7) (3 8) (4 9)))])
      (assert-equal 5 (car r))
      (assert-true (perfect-matching? 10 r)))))

;;; ====
;;; 7. Cross-Validation with Bipartite Max-Matching
;;; ====

(test-group "bipartite"
  (define-test "K3,3 bipartite — perfect matching size 3"
    (let ([r (max-matching '((0 3) (0 4) (0 5)
                              (1 3) (1 4) (1 5)
                              (2 3) (2 4) (2 5)))])
      (assert-equal 3 (car r))
      (assert-true (valid-matching? #f 6 r))))

  (define-test "sparse bipartite — matching size 3"
    (let ([r (max-matching '((0 3) (1 4) (2 5) (0 4)))])
      (assert-equal 3 (car r))
      (assert-true (valid-matching? #f 6 r)))))

;;; ====
;;; 8. Edge Cases
;;; ====

(test-group "edge-cases"
  (define-test "disconnected components"
    ;; Two triangles: {0,1,2} and {3,4,5}
    (let ([r (max-matching '((0 1) (1 2) (2 0) (3 4) (4 5) (5 3)))])
      (assert-equal 2 (car r))))

  (define-test "isolated vertices mixed with edges"
    ;; Vertices 0,1 are connected; 2,3 are isolated
    (let ([r (max-matching '((0 1)) 4)])
      (assert-equal 1 (car r))))

  (define-test "single vertex, no edges"
    (let ([r (max-matching '() 1)])
      (assert-equal 0 (car r))))

  (define-test "adjacency matrix input"
    (let ([r (max-matching (cycle-graph 5))])
      (assert-equal 2 (car r))
      (assert-true (valid-matching? #f 5 r)))))

;;; ====
;;; 9. Matching Predicates
;;; ====

(test-group "predicates"
  (define-test "matching-size extracts size"
    (assert-equal 3 (matching-size (list 3 '((0 1) (2 3) (4 5))))))

  (define-test "perfect-matching? true for K4"
    (assert-true (perfect-matching? 4 (max-matching (complete-graph 4)))))

  (define-test "perfect-matching? false for odd n"
    (assert-false (perfect-matching? 5 (max-matching (complete-graph 5)))))

  (define-test "perfect-matching? false for star"
    (assert-false (perfect-matching? 5 (max-matching (star-graph 5))))))

(run-all-tests)
