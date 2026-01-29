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

  (define-test "2x2 walkable area produces 4 vertices, 4 edges"
    (let* ([board (make-test-board 4 4 (list (coord 1 1) (coord 2 1)
                                              (coord 1 2) (coord 2 2)))]
           [sc (board->simplicial-complex board test-neighbors)])
      (assert-equal 4 (length (sc-vertices sc)))
      (assert-equal 4 (length (sc-edges sc)))))

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

  ;; Note: β₁ counts cycles in the graph, not terrain holes.
  ;; A 3x3 grid has 4 independent cycles (the "faces" of the grid).
  (define-test "solid rectangle has β₁ = 4 (four cycles in grid)"
    (let* ([board (make-test-board 5 5
                    (list (coord 1 1) (coord 2 1) (coord 3 1)
                          (coord 1 2) (coord 2 2) (coord 3 2)
                          (coord 1 3) (coord 2 3) (coord 3 3)))]
           [holes (board-terrain-holes board test-neighbors)])
      (assert-equal 4 holes)))

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

(run-all-tests)
