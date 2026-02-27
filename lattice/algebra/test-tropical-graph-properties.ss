;;; lattice/algebra/test-tropical-graph-properties.ss — QuickCheck properties for tropical graph bridge

(load "core/testing/test-framework.ss")
(load "core/lang/module.ss")
(require 'quickcheck)
(require 'tropical-graph)

;;; ============================================================================
;;; Generators
;;; ============================================================================

(define gen-w
  (gen-int-range 1 20))

;;; ============================================================================
;;; Properties
;;; ============================================================================

(test-group tropical-graph-properties

  (define-property "triangle shortest path matches min(direct, via-middle)"
    (gen-bind gen-w
      (lambda (w01)
        (gen-bind gen-w
          (lambda (w12)
            (gen-map (lambda (w02) (list w01 w12 w02)) gen-w)))))
    (lambda (weights)
      (let* ([w01 (car weights)]
             [w12 (cadr weights)]
             [w02 (caddr weights)]
             [adj (edges->adjacency-matrix
                   (list (list 0 1 w01)
                         (list 1 2 w12)
                         (list 0 2 w02))
                   3)]
             [dist (tropical-shortest-paths adj)]
             [expected (min w02 (+ w01 w12))])
        (= (tropical-distance dist 0 2) expected)))
    'tests 240)

  (define-property "transitive closure on a chain reaches forward but not backward"
    (gen-int-range 0 1)
    (lambda (_)
      (let* ([adj (edges->adjacency-matrix '((0 1) (1 2)) 3)]
             [tc (tropical-transitive-closure adj)])
        (and (tropical-reachable? tc 0 2)
             (not (tropical-reachable? tc 2 0)))))
    'tests 120)

  (define-property "shortest-path diagonal is always zero"
    (gen-bind gen-w
      (lambda (w01)
        (gen-bind gen-w
          (lambda (w12)
            (gen-map (lambda (w23) (list w01 w12 w23)) gen-w)))))
    (lambda (weights)
      (let* ([w01 (car weights)]
             [w12 (cadr weights)]
             [w23 (caddr weights)]
             [adj (edges->adjacency-matrix
                   (list (list 0 1 w01)
                         (list 1 2 w12)
                         (list 2 3 w23))
                   4)]
             [dist (tropical-shortest-paths adj)])
        (and (= (tropical-distance dist 0 0) 0)
             (= (tropical-distance dist 1 1) 0)
             (= (tropical-distance dist 2 2) 0)
             (= (tropical-distance dist 3 3) 0))))
    'tests 200)

  (define-property "graph/tropical bridge conversions and agreement checks are coherent"
    (gen-bind gen-w
      (lambda (w01)
        (gen-bind gen-w
          (lambda (w12)
            (gen-map (lambda (w02) (list w01 w12 w02)) gen-w)))))
    (lambda (weights)
      (let* ([w01 (car weights)]
             [w12 (cadr weights)]
             [w02 (caddr weights)]
             [adj (edges->adjacency-matrix
                   (list (list 0 1 w01)
                         (list 1 2 w12)
                         (list 0 2 w02))
                   3)]
             [tc-adj (edges->adjacency-matrix '((0 1) (1 2)) 3)]
             [tm (graph->tropical-matrix adj)]
             [bm (graph->boolean-matrix adj)]
             [mm (graph->max-plus-matrix adj)]
             [bkm (graph->bottleneck-matrix adj)]
             [sp (tropical-shortest-paths adj)]
             [lp (tropical-longest-paths adj)]
             [bp (tropical-bottleneck-paths adj)]
             [adj* (tropical-distance-to-graph sp)])
        (and (semiring? boolean-semiring)
             (semiring? min-max-semiring)
             (matrix? tm)
             (matrix? bm)
             (matrix? mm)
             (matrix? bkm)
             (= (matrix-ref bm 0 0) 1)
             (= (matrix-ref mm 0 2) w02)
             (= (matrix-ref bkm 0 0) +inf.0)
             (number? (tropical-distance lp 0 2))
             (number? (tropical-distance bp 0 2))
             (matrix? adj*)
             (tropical-agrees-with-floyd-warshall? adj)
             (tropical-agrees-with-transitive-closure? tc-adj))))
    'tests 120)
)

;;; ============================================================================
;;; Run
;;; ============================================================================

(run-all-tests-and-exit)
