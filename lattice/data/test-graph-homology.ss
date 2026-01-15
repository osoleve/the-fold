;;; test-graph-homology.ss — Tests for homology-based cycle analysis
;;;
;;; Tests graph->simplicial-complex, graph-betti-numbers, cycle-basis-homology
;;; on various simple graphs.

(source-directories (cons "core" (source-directories)))
(load "lattice/data/graph-algorithms.ss")

(define tests-passed 0)
(define tests-failed 0)

(define (test name expected actual)
  (if (equal? expected actual)
      (begin
       (set! tests-passed (+ tests-passed 1))
       (printf "  [PASS] ~a~n" name))
      (begin
       (set! tests-failed (+ tests-failed 1))
       (printf "  [FAIL] ~a~n" name)
       (printf "      Expected: ~s~n" expected)
       (printf "      Got:      ~s~n" actual))))

(define (test-true name actual)
  (test name #t actual))

(define (test-false name actual)
  (test name #f actual))

(printf "~n")
(printf "====~n")
(printf "         HOMOLOGY-BASED CYCLE ANALYSIS TESTS~n")
(printf "====~n~n")

;;; ====
;;; Test 1: Triangle (3 vertices, 3 edges)
;;; ====
;;; A triangle has:
;;;   - 1 connected component (beta_0 = 1)
;;;   - 1 independent cycle (beta_1 = 1)

(printf "Triangle graph (K_3):~n")
(printf "----~n")

(define triangle-edges '((0 . 1) (1 . 2) (2 . 0)))
(define triangle-vertices '(0 1 2))

(define triangle-betti (graph-betti-numbers triangle-edges triangle-vertices))
(test "triangle beta_0 (components)" 1 (car triangle-betti))
(test "triangle beta_1 (cycles)" 1 (cdr triangle-betti))

(test-true "triangle is NOT a tree" (not (graph-is-tree? triangle-edges triangle-vertices)))
(test-false "triangle is NOT a forest" (graph-is-forest? triangle-edges triangle-vertices))

(define triangle-euler (graph-euler-characteristic triangle-edges triangle-vertices))
(test "triangle Euler characteristic V-E" 0 triangle-euler)
;; chi = V - E = 3 - 3 = 0

(printf "~n")

;;; ====
;;; Test 2: Square (4 vertices, 4 edges)
;;; ====
;;; A square has:
;;;   - 1 connected component (beta_0 = 1)
;;;   - 1 independent cycle (beta_1 = 1)

(printf "Square graph (C_4):~n")
(printf "----~n")

(define square-edges '((0 . 1) (1 . 2) (2 . 3) (3 . 0)))
(define square-vertices '(0 1 2 3))

(define square-betti (graph-betti-numbers square-edges square-vertices))
(test "square beta_0 (components)" 1 (car square-betti))
(test "square beta_1 (cycles)" 1 (cdr square-betti))

(printf "~n")

;;; ====
;;; Test 3: Figure-8 (theta graph: 2 vertices, 3 edges)
;;; ====
;;; A figure-8 (theta graph with vertices a,b and 3 parallel edges) has:
;;;   - 1 connected component (beta_0 = 1)
;;;   - 2 independent cycles (beta_1 = 2)
;;;
;;; We use the "house" shape variant: 5 vertices, 6 edges forming two triangles
;;; sharing an edge:
;;;     0 --- 1
;;;     |\   /|
;;;     | \ / |
;;;     |  2  |
;;;     | / \ |
;;;     |/   \|
;;;     3 --- 4
;;;
;;; Actually, let's use a simpler figure-8: two triangles sharing a vertex

(printf "Figure-8 graph (two triangles sharing a vertex):~n")
(printf "----~n")

;; Two triangles: (0,1,2) and (2,3,4) sharing vertex 2
(define figure8-edges '((0 . 1) (1 . 2) (2 . 0) (2 . 3) (3 . 4) (4 . 2)))
(define figure8-vertices '(0 1 2 3 4))

(define figure8-betti (graph-betti-numbers figure8-edges figure8-vertices))
(test "figure-8 beta_0 (components)" 1 (car figure8-betti))
(test "figure-8 beta_1 (cycles)" 2 (cdr figure8-betti))

(printf "~n")

;;; ====
;;; Test 4: Tree (4 vertices, 3 edges - linear path)
;;; ====
;;; A path graph has:
;;;   - 1 connected component (beta_0 = 1)
;;;   - 0 independent cycles (beta_1 = 0) - it's a tree!

(printf "Path graph (P_4, a tree):~n")
(printf "----~n")

(define path-edges '((0 . 1) (1 . 2) (2 . 3)))
(define path-vertices '(0 1 2 3))

(define path-betti (graph-betti-numbers path-edges path-vertices))
(test "path beta_0 (components)" 1 (car path-betti))
(test "path beta_1 (cycles)" 0 (cdr path-betti))

(test-true "path IS a tree" (graph-is-tree? path-edges path-vertices))
(test-true "path IS a forest" (graph-is-forest? path-edges path-vertices))

(printf "~n")

;;; ====
;;; Test 5: Forest (6 vertices, 4 edges - two trees)
;;; ====
;;; A forest with 2 trees:
;;;   - 2 connected components (beta_0 = 2)
;;;   - 0 independent cycles (beta_1 = 0)

(printf "Forest (two disjoint trees):~n")
(printf "----~n")

;; Tree 1: 0-1-2, Tree 2: 3-4-5
(define forest-edges '((0 . 1) (1 . 2) (3 . 4) (4 . 5)))
(define forest-vertices '(0 1 2 3 4 5))

(define forest-betti (graph-betti-numbers forest-edges forest-vertices))
(test "forest beta_0 (components)" 2 (car forest-betti))
(test "forest beta_1 (cycles)" 0 (cdr forest-betti))

(test-false "forest is NOT a tree (2 components)" (graph-is-tree? forest-edges forest-vertices))
(test-true "forest IS a forest" (graph-is-forest? forest-edges forest-vertices))

(printf "~n")

;;; ====
;;; Test 6: Isolated vertices (3 vertices, 0 edges)
;;; ====
;;; No edges means:
;;;   - Each vertex is its own component (beta_0 = 3)
;;;   - No cycles possible (beta_1 = 0)

(printf "Isolated vertices (no edges):~n")
(printf "----~n")

(define isolated-edges '())
(define isolated-vertices '(0 1 2))

(define isolated-betti (graph-betti-numbers isolated-edges isolated-vertices))
(test "isolated beta_0 (components)" 3 (car isolated-betti))
(test "isolated beta_1 (cycles)" 0 (cdr isolated-betti))

(printf "~n")

;;; ====
;;; Test 7: Complete graph K_4 (4 vertices, 6 edges)
;;; ====
;;; K_4 has:
;;;   - 1 connected component (beta_0 = 1)
;;;   - beta_1 = E - V + 1 = 6 - 4 + 1 = 3 independent cycles

(printf "Complete graph K_4:~n")
(printf "----~n")

(define k4-edges '((0 . 1) (0 . 2) (0 . 3) (1 . 2) (1 . 3) (2 . 3)))
(define k4-vertices '(0 1 2 3))

(define k4-betti (graph-betti-numbers k4-edges k4-vertices))
(test "K_4 beta_0 (components)" 1 (car k4-betti))
(test "K_4 beta_1 (cycles)" 3 (cdr k4-betti))

(printf "~n")

;;; ====
;;; Test 8: Cycle basis for triangle
;;; ====
;;; The triangle should have exactly 1 fundamental cycle containing all 3 edges

(printf "Cycle basis (triangle):~n")
(printf "----~n")

(define triangle-cycles (cycle-basis-homology triangle-edges triangle-vertices))
(test "triangle has 1 fundamental cycle" 1 (length triangle-cycles))

;; The cycle should contain 3 edges
(when (not (null? triangle-cycles))
  (test "triangle cycle has 3 edges" 3 (length (car triangle-cycles))))

(printf "~n")

;;; ====
;;; Test 9: Cycle basis for figure-8
;;; ====
;;; Should have exactly 2 fundamental cycles

(printf "Cycle basis (figure-8):~n")
(printf "----~n")

(define figure8-cycles (cycle-basis-homology figure8-edges figure8-vertices))
(test "figure-8 has 2 fundamental cycles" 2 (length figure8-cycles))

(printf "~n")

;;; ====
;;; Test 10: Cycle basis for tree
;;; ====
;;; Tree should have 0 fundamental cycles

(printf "Cycle basis (tree):~n")
(printf "----~n")

(define tree-cycles (cycle-basis-homology path-edges path-vertices))
(test "tree has 0 fundamental cycles" 0 (length tree-cycles))

(printf "~n")

;;; ====
;;; Test 11: Graph to simplicial complex conversion
;;; ====

(printf "Simplicial complex conversion:~n")
(printf "----~n")

(define tri-sc (graph->simplicial-complex triangle-edges triangle-vertices))
(test-true "triangle SC is valid" (sc? tri-sc))
(test "triangle SC has 3 vertices" 3 (length (sc-vertices tri-sc)))
(test "triangle SC has 3 edges" 3 (length (sc-edges tri-sc)))
(test "triangle SC max dimension is 1" 1 (sc-max-dim tri-sc))

(printf "~n")

;;; ====
;;; Summary
;;; ====

(printf "====~n")
(printf "                    TEST RESULTS~n")
(printf "====~n~n")

(printf "Tests passed: ~a~n" tests-passed)
(printf "Tests failed: ~a~n" tests-failed)
(printf "Total tests:  ~a~n" (+ tests-passed tests-failed))

(if (= tests-failed 0)
    (printf "~n[SUCCESS] All homology tests passed.~n~n")
    (printf "~n[FAILURE] Some homology tests failed.~n~n"))
