;;; user/physics/test-raycasting.ss — Tests for 2D Raycasting

(load "core/lang/module.ss")
(load "core/test-framework.ss")
(load "lattice/physics/classical/raycasting.ss")

;;; Helper for numeric comparison
(define (assert-= actual expected tolerance)
  (unless (<= (abs (- actual expected)) tolerance)
          (set! *tests-failed* (+ *tests-failed* 1))
          (display "    X ")
          (display *current-test-name*)
          (newline)
          (display "      Expected: ")
          (display expected)
          (display " +/- ")
          (display tolerance)
          (newline)
          (display "      Got:      ")
          (display actual)
          (newline)))

;;; Helper for vec2 comparison
(define (assert-vec2-= actual expected tolerance)
  (assert-= (vec2-x actual) (vec2-x expected) tolerance)
  (assert-= (vec2-y actual) (vec2-y expected) tolerance))

(display "
====
              2D RAYCASTING TESTS
====
")

;;; ====
;;; Ray Construction Tests
;;; ====

(test-group ray-construction-tests
            (define-test make-ray2-normalizes-direction
              (let ([ray (make-ray2 (vec2 0 0) (vec2 3 4) 100)])
                   (assert-= (vec2-x (ray2-direction ray)) 0.6 0.0001)
                   (assert-= (vec2-y (ray2-direction ray)) 0.8 0.0001)))
            
            (define-test ray2-accessors
              (let ([ray (make-ray2 (vec2 1 2) (vec2 1 0) 50)])
                   (assert-vec2-= (ray2-origin ray) (vec2 1 2) 0.0001)
                   (assert-vec2-= (ray2-direction ray) (vec2 1 0) 0.0001)
                   (assert-= (ray2-max-dist ray) 50 0.0001)))
            
            (define-test ray2-point-at-test
              (let* ([ray (make-ray2 (vec2 0 0) (vec2 1 0) 100)]
                     [point (ray2-point-at ray 5)])
                    (assert-vec2-= point (vec2 5 0) 0.0001)))
            
            (define-test ray2-point-at-diagonal
              (let* ([ray (make-ray2 (vec2 0 0) (vec2 1 1) 100)]
                     [point (ray2-point-at ray 10)])
                    ;; Direction normalized to ~(0.707, 0.707)
                    (assert-= (vec2-x point) (* 10 (/ 1 (sqrt 2))) 0.0001)
                    (assert-= (vec2-y point) (* 10 (/ 1 (sqrt 2))) 0.0001)))
            
            (define-test ray2-between-test
              (let ([ray (make-ray2-between (vec2 0 0) (vec2 10 0))])
                   (assert-vec2-= (ray2-origin ray) (vec2 0 0) 0.0001)
                   (assert-vec2-= (ray2-direction ray) (vec2 1 0) 0.0001)
                   (assert-= (ray2-max-dist ray) 10 0.0001))))

;;; ====
;;; HitInfo Tests
;;; ====

(test-group hit-info-tests
            (define-test make-hit-info-test
              (let* ([shape (make-circle (vec2 0 0) 1)]
                     [hit (make-hit-info 5.0 (vec2 5 0) (vec2 -1 0) shape)])
                    (assert-true (hit-info? hit))
                    (assert-= (hit-info-distance hit) 5.0 0.0001)
                    (assert-vec2-= (hit-info-point hit) (vec2 5 0) 0.0001)
                    (assert-vec2-= (hit-info-normal hit) (vec2 -1 0) 0.0001)
                    (assert-true (circle? (hit-info-shape hit))))))

;;; ====
;;; Ray-AABB Tests
;;; ====

(test-group ray-aabb-tests
            (define-test ray-aabb-direct-hit
              (let* ([ray (make-ray2 (vec2 0 5) (vec2 1 0) 100)]
                     [box (make-aabb (vec2 5 0) (vec2 15 10))]
                     [hit (ray2-aabb ray box)])
                    (assert-true (hit-info? hit))
                    (assert-= (hit-info-distance hit) 5 0.0001)
                    (assert-vec2-= (hit-info-point hit) (vec2 5 5) 0.0001)
                    (assert-vec2-= (hit-info-normal hit) (vec2 -1 0) 0.0001)))
            
            (define-test ray-aabb-hit-from-right
              (let* ([ray (make-ray2 (vec2 20 5) (vec2 -1 0) 100)]
                     [box (make-aabb (vec2 5 0) (vec2 15 10))]
                     [hit (ray2-aabb ray box)])
                    (assert-true (hit-info? hit))
                    (assert-= (hit-info-distance hit) 5 0.0001)
                    (assert-vec2-= (hit-info-point hit) (vec2 15 5) 0.0001)
                    (assert-vec2-= (hit-info-normal hit) (vec2 1 0) 0.0001)))
            
            (define-test ray-aabb-hit-from-bottom
              (let* ([ray (make-ray2 (vec2 10 -5) (vec2 0 1) 100)]
                     [box (make-aabb (vec2 5 0) (vec2 15 10))]
                     [hit (ray2-aabb ray box)])
                    (assert-true (hit-info? hit))
                    (assert-= (hit-info-distance hit) 5 0.0001)
                    (assert-vec2-= (hit-info-point hit) (vec2 10 0) 0.0001)
                    (assert-vec2-= (hit-info-normal hit) (vec2 0 -1) 0.0001)))
            
            (define-test ray-aabb-miss-above
              (let* ([ray (make-ray2 (vec2 0 15) (vec2 1 0) 100)]
                     [box (make-aabb (vec2 5 0) (vec2 15 10))]
                     [hit (ray2-aabb ray box)])
                    (assert-false hit)))
            
            (define-test ray-aabb-miss-behind
              (let* ([ray (make-ray2 (vec2 20 5) (vec2 1 0) 100)]
                     [box (make-aabb (vec2 5 0) (vec2 15 10))]
                     [hit (ray2-aabb ray box)])
                    (assert-false hit)))
            
            (define-test ray-aabb-origin-inside
              (let* ([ray (make-ray2 (vec2 10 5) (vec2 1 0) 100)]
                     [box (make-aabb (vec2 5 0) (vec2 15 10))]
                     [hit (ray2-aabb ray box)])
                    (assert-true (hit-info? hit))
                    ;; Should hit exit face at x=15
                    (assert-= (hit-info-distance hit) 5 0.0001)
                    (assert-vec2-= (hit-info-point hit) (vec2 15 5) 0.0001)))
            
            (define-test ray-aabb-beyond-max-dist
              (let* ([ray (make-ray2 (vec2 0 5) (vec2 1 0) 3)]
                     [box (make-aabb (vec2 5 0) (vec2 15 10))]
                     [hit (ray2-aabb ray box)])
                    (assert-false hit)))
            
            (define-test ray-aabb-diagonal-hit
              (let* ([ray (make-ray2 (vec2 0 0) (vec2 1 1) 100)]
                     [box (make-aabb (vec2 5 5) (vec2 15 15))]
                     [hit (ray2-aabb ray box)])
                    (assert-true (hit-info? hit)))))

;;; ====
;;; Ray-Circle Tests
;;; ====

(test-group ray-circle-tests
            (define-test ray-circle-through-center
              (let* ([ray (make-ray2 (vec2 0 0) (vec2 1 0) 100)]
                     [circ (make-circle (vec2 10 0) 3)]
                     [hit (ray2-circle ray circ)])
                    (assert-true (hit-info? hit))
                    (assert-= (hit-info-distance hit) 7 0.0001)
                    (assert-vec2-= (hit-info-point hit) (vec2 7 0) 0.0001)
                    (assert-vec2-= (hit-info-normal hit) (vec2 -1 0) 0.0001)))
            
            (define-test ray-circle-off-center
              (let* ([ray (make-ray2 (vec2 0 2) (vec2 1 0) 100)]
                     [circ (make-circle (vec2 10 0) 3)]
                     [hit (ray2-circle ray circ)])
                    (assert-true (hit-info? hit))
                    ;; Hit point should be on circle, at y=2
                    (assert-= (vec2-y (hit-info-point hit)) 2 0.0001)))
            
            (define-test ray-circle-tangent
              (let* ([ray (make-ray2 (vec2 0 3) (vec2 1 0) 100)]
                     [circ (make-circle (vec2 10 0) 3)]
                     [hit (ray2-circle ray circ)])
                    ;; Tangent - should hit exactly at the edge
                    (assert-true (hit-info? hit))))
            
            (define-test ray-circle-miss
              (let* ([ray (make-ray2 (vec2 0 10) (vec2 1 0) 100)]
                     [circ (make-circle (vec2 10 0) 3)]
                     [hit (ray2-circle ray circ)])
                    (assert-false hit)))
            
            (define-test ray-circle-miss-behind
              (let* ([ray (make-ray2 (vec2 20 0) (vec2 1 0) 100)]
                     [circ (make-circle (vec2 10 0) 3)]
                     [hit (ray2-circle ray circ)])
                    (assert-false hit)))
            
            (define-test ray-circle-origin-inside
              (let* ([ray (make-ray2 (vec2 10 0) (vec2 1 0) 100)]
                     [circ (make-circle (vec2 10 0) 3)]
                     [hit (ray2-circle ray circ)])
                    (assert-true (hit-info? hit))
                    ;; Should hit exit at x=13
                    (assert-= (hit-info-distance hit) 3 0.0001)
                    (assert-vec2-= (hit-info-point hit) (vec2 13 0) 0.0001)))
            
            (define-test ray-circle-beyond-max-dist
              (let* ([ray (make-ray2 (vec2 0 0) (vec2 1 0) 5)]
                     [circ (make-circle (vec2 10 0) 3)]
                     [hit (ray2-circle ray circ)])
                    (assert-false hit)))
            
            (define-test ray-circle-zero-radius
              (let* ([ray (make-ray2 (vec2 0 0) (vec2 1 0) 100)]
                     [circ (make-circle (vec2 10 0) 0)]
                     [hit (ray2-circle ray circ)])
                    (assert-false hit))))

;;; ====
;;; Ray-Polygon Tests
;;; ====

(test-group ray-polygon-tests
            (define-test ray-polygon-box-hit
              (let* ([ray (make-ray2 (vec2 0 5) (vec2 1 0) 100)]
                     [poly (make-box (vec2 10 5) (vec2 3 3))]
                     [hit (ray2-polygon ray poly)])
                    (assert-true (hit-info? hit))
                    (assert-= (hit-info-distance hit) 7 0.0001)))
            
            (define-test ray-polygon-triangle-hit
              (let* ([ray (make-ray2 (vec2 0 2) (vec2 1 0) 100)]
                     [tri (make-polygon (list (vec2 5 0) (vec2 10 5) (vec2 5 5)))]
                     [hit (ray2-polygon ray tri)])
                    (assert-true (hit-info? hit))))
            
            (define-test ray-polygon-miss
              (let* ([ray (make-ray2 (vec2 0 20) (vec2 1 0) 100)]
                     [poly (make-box (vec2 10 5) (vec2 3 3))]
                     [hit (ray2-polygon ray poly)])
                    (assert-false hit)))
            
            (define-test ray-polygon-origin-inside
              (let* ([ray (make-ray2 (vec2 10 5) (vec2 1 0) 100)]
                     [poly (make-box (vec2 10 5) (vec2 5 5))]
                     [hit (ray2-polygon ray poly)])
                    (assert-true (hit-info? hit))
                    ;; Should exit at x=15
                    (assert-= (hit-info-distance hit) 5 0.0001)))
            
            (define-test ray-polygon-hexagon
              (let* ([ray (make-ray2 (vec2 0 0) (vec2 1 0) 100)]
                     [hex (make-regular-polygon (vec2 10 0) 3 6)]
                     [hit (ray2-polygon ray hex)])
                    (assert-true (hit-info? hit)))))

;;; ====
;;; Generic Shape Tests
;;; ====

(test-group ray-shape-tests
            (define-test ray2-shape-aabb
              (let* ([ray (make-ray2 (vec2 0 5) (vec2 1 0) 100)]
                     [shape (make-aabb (vec2 5 0) (vec2 15 10))]
                     [hit (ray2-shape ray shape)])
                    (assert-true (hit-info? hit))))
            
            (define-test ray2-shape-circle
              (let* ([ray (make-ray2 (vec2 0 0) (vec2 1 0) 100)]
                     [shape (make-circle (vec2 10 0) 3)]
                     [hit (ray2-shape ray shape)])
                    (assert-true (hit-info? hit))))
            
            (define-test ray2-shape-polygon
              (let* ([ray (make-ray2 (vec2 0 5) (vec2 1 0) 100)]
                     [shape (make-box (vec2 10 5) (vec2 3 3))]
                     [hit (ray2-shape ray shape)])
                    (assert-true (hit-info? hit))))
            
            (define-test ray2-shape-unknown
              (let* ([ray (make-ray2 (vec2 0 0) (vec2 1 0) 100)]
                     [hit (ray2-shape ray '(unknown-shape))])
                    (assert-false hit))))

;;; ====
;;; Edge Cases
;;; ====

(test-group edge-case-tests
            (define-test zero-length-ray-inside
              (let* ([ray (make-ray2 (vec2 10 5) (vec2 1 0) 0)]
                     [box (make-aabb (vec2 5 0) (vec2 15 10))]
                     [hit (ray2-aabb ray box)])
                    ;; Zero max-dist should not hit even if inside
                    (assert-false hit)))
            
            (define-test ray-parallel-to-aabb-edge
              (let* ([ray (make-ray2 (vec2 0 10) (vec2 1 0) 100)]
                     [box (make-aabb (vec2 5 0) (vec2 15 10))]
                     [hit (ray2-aabb ray box)])
                    ;; Ray along top edge - grazing
                    (assert-true (hit-info? hit))))
            
            (define-test negative-direction-ray
              (let* ([ray (make-ray2 (vec2 20 5) (vec2 -1 0) 100)]
                     [box (make-aabb (vec2 5 0) (vec2 15 10))]
                     [hit (ray2-aabb ray box)])
                    (assert-true (hit-info? hit))
                    (assert-= (hit-info-distance hit) 5 0.0001)))
            
            (define-test very-small-shape
              (let* ([ray (make-ray2 (vec2 0 0) (vec2 1 0) 100)]
                     [tiny (make-circle (vec2 10 0) 0.001)]
                     [hit (ray2-circle ray tiny)])
                    (assert-true (hit-info? hit)))))

;;; ====
;;; Run Tests
;;; ====

(run-all-tests)
