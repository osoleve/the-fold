(load "core/testing/test-framework.ss")
(load "lattice/tiles/topology-analysis.ss")
(load "lattice/tiles/square.ss")

(doc 'module 'test-topology-analysis)
(doc 'description "Tests for board topological analysis")

;;; ====
;;; Test Utilities
;;; ====

;; Use ortho (4-way) neighbors for tests
(define (test-neighbors c)
  (square-neighbors-ortho c))

(define (make-test-board width height walkable-coords)
  "Create a test board with specified walkable tiles"
  (let ([board (make-board 'square '())])
    ;; Fill with walls
    (let fill-loop ([b board] [x 0] [y 0])
      (if (>= y height)
          ;; Now mark walkable tiles
          (let walk-loop ([b2 b] [coords walkable-coords])
            (if (null? coords)
                b2
                (walk-loop (board-set b2 (car coords) (make-tile 'floor '()))
                           (cdr coords))))
          (if (>= x width)
              (fill-loop b 0 (+ y 1))
              (fill-loop (board-set b (coord x y) (make-tile 'wall '()))
                         (+ x 1) y))))))

;;; ====
;;; Test Cases
;;; ====

(test-group "board->simplicial-complex"

  (define-test "single tile produces 1 vertex, 0 edges"
    (let* ([board (make-test-board 3 3 (list (coord 1 1)))]
           [sc (board->simplicial-complex board test-neighbors)])
      (assert-equal 1 (length (sc-vertices sc)))
      (assert-equal 0 (length (sc-edges sc)))))

  (define-test "two adjacent tiles produce 2 vertices, 1 edge"
    (let* ([board (make-test-board 3 3 (list (coord 1 1) (coord 2 1)))]
           [sc (board->simplicial-complex board test-neighbors)])
      (assert-equal 2 (length (sc-vertices sc)))
      (assert-equal 1 (length (sc-edges sc)))))

  ;; A 2x2 walkable area has 4 vertices and 4 grid edges.
  ;; With triangulation, the diagonal edge is added (5 total 1-simplices).
  ;; Also 2 triangular faces (2-simplices) to fill the square.
  (define-test "2x2 walkable area produces 4 vertices, 5 edges (incl diagonal), 2 faces"
    (let* ([board (make-test-board 4 4 (list (coord 1 1) (coord 2 1)
                                              (coord 1 2) (coord 2 2)))]
           [sc (board->simplicial-complex board test-neighbors)])
      (assert-equal 4 (length (sc-vertices sc)))
      (assert-equal 5 (length (sc-edges sc)))  ; 4 grid edges + 1 diagonal
      (assert-equal 2 (length (sc-faces sc)))))  ; 2 triangles

  (define-test "diagonal tiles are not adjacent in square grid"
    (let* ([board (make-test-board 3 3 (list (coord 0 0) (coord 2 2)))]
           [sc (board->simplicial-complex board test-neighbors)])
      (assert-equal 2 (length (sc-vertices sc)))
      (assert-equal 0 (length (sc-edges sc))))))

(test-group "betti numbers"

  (define-test "single connected region has β₀ = 1"
    (let* ([board (make-test-board 5 5 (list (coord 1 1) (coord 2 1) (coord 3 1)
                                              (coord 1 2) (coord 2 2) (coord 3 2)))]
           [regions (board-connected-regions board test-neighbors)])
      (assert-equal 1 regions)))

  (define-test "two disconnected regions have β₀ = 2"
    (let* ([board (make-test-board 7 3 (list (coord 1 1) (coord 2 1)   ; region 1
                                              (coord 5 1) (coord 6 1)))] ; region 2
           [regions (board-connected-regions board test-neighbors)])
      (assert-equal 2 regions)))

  (define-test "three disconnected tiles have β₀ = 3"
    (let* ([board (make-test-board 5 5 (list (coord 0 0)
                                              (coord 2 2)
                                              (coord 4 4)))]
           [regions (board-connected-regions board test-neighbors)])
      (assert-equal 3 regions)))

  (define-test "ring of tiles has β₁ = 1 (one hole)"
    ;; Create a ring:
    ;;   ###
    ;;   # #
    ;;   ###
    ;; where # is walkable and space is wall
    (let* ([board (make-test-board 5 5
                    (list (coord 1 1) (coord 2 1) (coord 3 1)
                          (coord 1 2)             (coord 3 2)
                          (coord 1 3) (coord 2 3) (coord 3 3)))]
           [holes (board-terrain-holes board test-neighbors)])
      (assert-equal 1 holes)))

  ;; With 2-simplices filling solid 2×2 squares, β₁ now correctly
  ;; counts terrain holes (impassable islands), not graph cycles.
  (define-test "solid rectangle has β₁ = 0 (no terrain holes)"
    (let* ([board (make-test-board 5 5
                    (list (coord 1 1) (coord 2 1) (coord 3 1)
                          (coord 1 2) (coord 2 2) (coord 3 2)
                          (coord 1 3) (coord 2 3) (coord 3 3)))]
           [holes (board-terrain-holes board test-neighbors)])
      (assert-equal 0 holes)))

  (define-test "figure-8 has β₁ = 2 (two holes)"
    ;; Two adjacent rings sharing a vertex
    ;;   ### ###
    ;;   # # # #
    ;;   ### ###
    (let* ([board (make-test-board 9 5
                    (list ;; Left ring
                          (coord 1 1) (coord 2 1) (coord 3 1)
                          (coord 1 2)             (coord 3 2)
                          (coord 1 3) (coord 2 3) (coord 3 3)
                          ;; Right ring (shares edge at x=3)
                                      (coord 4 1) (coord 5 1)
                                                  (coord 5 2)
                                      (coord 4 3) (coord 5 3)))]
           [holes (board-terrain-holes board test-neighbors)])
      (assert-equal 2 holes))))

(test-group "critical edges"

  (define-test "line of tiles has critical edges (all are bridges)"
    (let* ([board (make-test-board 5 3 (list (coord 1 1) (coord 2 1) (coord 3 1)))]
           [critical (board-critical-edges board test-neighbors)])
      ;; A line has 2 edges, both are critical (bridges)
      (assert-equal 2 (length critical))))

  (define-test "2x2 square has no critical edges"
    (let* ([board (make-test-board 4 4 (list (coord 1 1) (coord 2 1)
                                              (coord 1 2) (coord 2 2)))]
           [critical (board-critical-edges board test-neighbors)])
      (assert-equal 0 (length critical))))

  (define-test "barbell shape has critical bridge"
    ;; Two 2x2 squares connected by single tile corridor
    ;;   ## # ##
    ;;   ##   ##
    ;; The bridge at (3,1) connects left to right via (2,1)-(3,1)-(4,1)
    (let* ([board (make-test-board 7 4
                    (list ;; Left square
                          (coord 1 1) (coord 2 1)
                          (coord 1 2) (coord 2 2)
                          ;; Bridge (connects (2,1) to (4,1))
                          (coord 3 1)
                          ;; Right square
                          (coord 4 1) (coord 5 1)
                          (coord 4 2) (coord 5 2)))]
           [critical (board-critical-edges board test-neighbors)])
      ;; The bridge tile creates 2 critical edges: (2,1)-(3,1) and (3,1)-(4,1)
      (assert-equal 2 (length critical)))))

(test-group "strategic predicates"

  (define-test "connected board reports connected"
    (let* ([board (make-test-board 5 5 (list (coord 1 1) (coord 2 1) (coord 3 1)
                                              (coord 1 2) (coord 2 2) (coord 3 2)))])
      (assert-true (board-is-connected? board test-neighbors))))

  (define-test "disconnected board reports not connected"
    (let* ([board (make-test-board 5 3 (list (coord 0 1) (coord 4 1)))])
      (assert-false (board-is-connected? board test-neighbors))))

  (define-test "line has chokepoints"
    (let* ([board (make-test-board 5 3 (list (coord 1 1) (coord 2 1) (coord 3 1)))])
      (assert-true (board-has-chokepoints? board test-neighbors))))

  (define-test "solid area has no chokepoints"
    (let* ([board (make-test-board 5 5 (list (coord 1 1) (coord 2 1) (coord 3 1)
                                              (coord 1 2) (coord 2 2) (coord 3 2)
                                              (coord 1 3) (coord 2 3) (coord 3 3)))])
      (assert-false (board-has-chokepoints? board test-neighbors)))))

(test-group "bottleneck scoring"

  (define-test "tile in bridge has positive bottleneck score"
    (let* ([board (make-test-board 7 3
                    (list (coord 1 1) (coord 2 1)  ; left
                          (coord 3 1)              ; bridge
                          (coord 4 1) (coord 5 1)))] ; right
           [score (board-bottleneck-score board test-neighbors (coord 3 1))])
      (assert-true (> score 0))))

  (define-test "tile not in bridge has zero bottleneck score"
    (let* ([board (make-test-board 5 5 (list (coord 1 1) (coord 2 1) (coord 3 1)
                                              (coord 1 2) (coord 2 2) (coord 3 2)
                                              (coord 1 3) (coord 2 3) (coord 3 3)))]
           [score (board-bottleneck-score board test-neighbors (coord 2 2))])
      (assert-equal 0 score))))

(test-group "weighted bridges"

  (define-test "weighted bridges include component sizes"
    ;; Line of 5 tiles: 1-2-3-4-5
    ;; Bridge 2-3 splits into {1,2} and {3,4,5} = sizes 2 and 3
    ;; Bridge 3-4 splits into {1,2,3} and {4,5} = sizes 3 and 2
    (let* ([board (make-test-board 7 3
                    (list (coord 1 1) (coord 2 1) (coord 3 1) (coord 4 1) (coord 5 1)))]
           [weighted (board-critical-edges-weighted board test-neighbors)])
      ;; Should have 4 bridges in a line of 5
      (assert-equal 4 (length weighted))
      ;; Each bridge should have component sizes that sum to 5
      (assert-true (for-all (lambda (wb)
                              (= 5 (+ (weighted-bridge-size1 wb)
                                      (weighted-bridge-size2 wb))))
                            weighted))))

  (define-test "bridge weight is product of component sizes"
    ;; Barbell: two 4-tile squares connected by single tile
    ;; Bridge connects 4 tiles to 5 tiles (4 + bridge + 4 = 9 total, but bridge is 1)
    ;; Actually: left square (4) + bridge (1) + right square (4) = 9 tiles
    ;; The bridge edges (left-to-bridge, bridge-to-right) each split:
    ;;   - left-to-bridge: 4 vs 5 (weight = 20)
    ;;   - bridge-to-right: 5 vs 4 (weight = 20)
    (let* ([board (make-test-board 7 4
                    (list ;; Left square
                          (coord 1 1) (coord 2 1)
                          (coord 1 2) (coord 2 2)
                          ;; Bridge
                          (coord 3 1)
                          ;; Right square
                          (coord 4 1) (coord 5 1)
                          (coord 4 2) (coord 5 2)))]
           [weighted (board-critical-edges-weighted board test-neighbors)])
      ;; Should have 2 critical bridges
      (assert-equal 2 (length weighted))
      ;; Each should have weight = 4 * 5 = 20
      (assert-true (for-all (lambda (wb)
                              (= 20 (weighted-bridge-weight wb)))
                            weighted))))

  (define-test "most-critical-bridges returns top k sorted by weight"
    ;; Line of 7 tiles: bridges have varying weights
    ;; Middle bridge (3-4) has highest weight: 3 * 4 = 12
    ;; Edge bridges (1-2, 6-7) have lowest: 1 * 6 = 6
    (let* ([board (make-test-board 9 3
                    (list (coord 1 1) (coord 2 1) (coord 3 1) (coord 4 1)
                          (coord 5 1) (coord 6 1) (coord 7 1)))]
           [top3 (board-most-critical-bridges board test-neighbors 3)])
      ;; Should return 3 bridges
      (assert-equal 3 (length top3))
      ;; First should have highest weight
      (assert-true (>= (weighted-bridge-weight (car top3))
                       (weighted-bridge-weight (cadr top3))))
      (assert-true (>= (weighted-bridge-weight (cadr top3))
                       (weighted-bridge-weight (caddr top3))))))

  (define-test "bottleneck score sums weights for multi-bridge tiles"
    ;; Line of 5: middle tile (coord 3 1) participates in 2 bridges
    ;; Bridge 2-3: sizes 2, 3, weight 6
    ;; Bridge 3-4: sizes 3, 2, weight 6
    ;; Total score for tile 3 = 6 + 6 = 12
    (let* ([board (make-test-board 7 3
                    (list (coord 1 1) (coord 2 1) (coord 3 1) (coord 4 1) (coord 5 1)))]
           [score (board-bottleneck-score board test-neighbors (coord 3 1))])
      ;; Middle tile participates in 2 bridges, each with weight 6
      (assert-equal 12 score))))

(test-group "performance"

  ;; Helper to make a large walkable board with a bridge in the middle
  (define (make-large-board-with-bridge width height bridge-x)
    "Create a board where two large regions are connected by a single bridge"
    (let ([board (make-board 'square '())])
      (let loop ([b board] [x 0] [y 0])
        (if (>= y height)
            b
            (if (>= x width)
                (loop b 0 (+ y 1))
                (let* ([is-bridge-col (= x bridge-x)]
                       [is-middle-row (= y (quotient height 2))]
                       ;; Only the middle row at bridge-x is walkable in that column
                       [walkable (or (not is-bridge-col)
                                     (and is-bridge-col is-middle-row))])
                  (loop (board-set b (coord x y)
                                   (make-tile (if walkable 'floor 'wall) '()))
                        (+ x 1) y)))))))

  ;; Test that Tarjan's algorithm handles larger boards efficiently
  ;; A 20x20 board has 400 tiles - old algorithm would be very slow
  (define-test "large board (20x20) bridge detection completes quickly"
    (let* ([board (make-large-board-with-bridge 20 20 10)]
           [critical (board-critical-edges board test-neighbors)])
      ;; Should find bridges at the narrow corridor
      (assert-true (> (length critical) 0)))))

(run-all-tests)
