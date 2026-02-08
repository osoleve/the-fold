;;; lattice/tiles/test-triangle.ss — Tests for Triangular Tile Geometry
;;; Run with: scheme --script lattice/tiles/test-triangle.ss

(load "core/testing/test-framework.ss")
(load "lattice/tiles/core.ss")
(load "lattice/tiles/triangle.ss")

;;; ============================================================
;;; Coordinate Construction
;;; ============================================================

(test-group "triangle-coord-construction"
  (define-test "create up triangle"
    (let ([t (triangle-coord 0 0 'up)])
      (assert-equal 0 (triangle-row t))
      (assert-equal 0 (triangle-col t))
      (assert-equal 'up (triangle-orientation t))))

  (define-test "create down triangle"
    (let ([t (triangle-coord 2 3 'down)])
      (assert-equal 2 (triangle-row t))
      (assert-equal 3 (triangle-col t))
      (assert-equal 'down (triangle-orientation t))))

  (define-test "invalid orientation errors"
    (assert-error (lambda () (triangle-coord 0 0 'left)))))

;;; ============================================================
;;; Equality and Hashing
;;; ============================================================

(test-group "triangle-coord-equality"
  (define-test "equal coordinates"
    (assert-true (triangle-coord-equal?
                   (triangle-coord 1 2 'up)
                   (triangle-coord 1 2 'up))))

  (define-test "different row"
    (assert-false (triangle-coord-equal?
                    (triangle-coord 1 2 'up)
                    (triangle-coord 0 2 'up))))

  (define-test "different col"
    (assert-false (triangle-coord-equal?
                    (triangle-coord 1 2 'up)
                    (triangle-coord 1 3 'up))))

  (define-test "different orientation"
    (assert-false (triangle-coord-equal?
                    (triangle-coord 1 2 'up)
                    (triangle-coord 1 2 'down))))

  (define-test "hash consistent for equal coords"
    (assert-equal (triangle-coord-hash (triangle-coord 3 4 'up))
                  (triangle-coord-hash (triangle-coord 3 4 'up))))

  (define-test "hash differs for different orientation"
    (assert-false (= (triangle-coord-hash (triangle-coord 3 4 'up))
                     (triangle-coord-hash (triangle-coord 3 4 'down))))))

;;; ============================================================
;;; Orientation
;;; ============================================================

(test-group "triangle-orientation"
  (define-test "default orientation: even sum is up"
    (assert-equal 'up (triangle-default-orientation 0 0))
    (assert-equal 'up (triangle-default-orientation 1 1))
    (assert-equal 'up (triangle-default-orientation 2 4)))

  (define-test "default orientation: odd sum is down"
    (assert-equal 'down (triangle-default-orientation 0 1))
    (assert-equal 'down (triangle-default-orientation 1 0))
    (assert-equal 'down (triangle-default-orientation 3 2)))

  (define-test "flip toggles orientation"
    (let* ([up (triangle-coord 0 0 'up)]
           [flipped (triangle-flip up)])
      (assert-equal 'down (triangle-orientation flipped))
      (assert-equal 0 (triangle-row flipped))
      (assert-equal 0 (triangle-col flipped))))

  (define-test "double flip is identity"
    (let* ([t (triangle-coord 3 4 'up)]
           [t2 (triangle-flip (triangle-flip t))])
      (assert-true (triangle-coord-equal? t t2)))))

;;; ============================================================
;;; Edge Neighbors
;;; ============================================================

(test-group "triangle-neighbors-edge"
  (define-test "up triangle has 3 edge neighbors"
    (let ([ns (triangle-neighbors-edge (triangle-coord 1 1 'up))])
      (assert-equal 3 (length ns))))

  (define-test "up triangle neighbor orientations"
    ;; Up triangle at (1,1): left-down, right-down, below-up
    (let ([ns (triangle-neighbors-edge (triangle-coord 1 1 'up))])
      ;; Left: (1, 0, down)
      (assert-true (triangle-coord-equal? (car ns) (triangle-coord 1 0 'down)))
      ;; Right: (1, 2, down)
      (assert-true (triangle-coord-equal? (cadr ns) (triangle-coord 1 2 'down)))
      ;; Below: (2, 1, up)
      (assert-true (triangle-coord-equal? (caddr ns) (triangle-coord 2 1 'up)))))

  (define-test "down triangle has 3 edge neighbors"
    (let ([ns (triangle-neighbors-edge (triangle-coord 1 1 'down))])
      (assert-equal 3 (length ns))))

  (define-test "down triangle neighbor orientations"
    ;; Down triangle at (1,1): left-up, right-up, above-down
    (let ([ns (triangle-neighbors-edge (triangle-coord 1 1 'down))])
      ;; Left: (1, 0, up)
      (assert-true (triangle-coord-equal? (car ns) (triangle-coord 1 0 'up)))
      ;; Right: (1, 2, up)
      (assert-true (triangle-coord-equal? (cadr ns) (triangle-coord 1 2 'up)))
      ;; Above: (0, 1, down)
      (assert-true (triangle-coord-equal? (caddr ns) (triangle-coord 0 1 'down))))))

;;; ============================================================
;;; Vertex Neighbors
;;; ============================================================

(test-group "triangle-neighbors-vertex"
  (define-test "up triangle has 3 vertex neighbors"
    (let ([ns (triangle-neighbors-vertex (triangle-coord 2 2 'up))])
      (assert-equal 3 (length ns))))

  (define-test "down triangle has 3 vertex neighbors"
    (let ([ns (triangle-neighbors-vertex (triangle-coord 2 2 'down))])
      (assert-equal 3 (length ns)))))

;;; ============================================================
;;; All Neighbors
;;; ============================================================

(test-group "triangle-neighbors-all"
  (define-test "all neighbors gives 7 total"
    ;; 3 edge + 3 vertex + 1 flip = 7
    (let ([ns (triangle-neighbors-all (triangle-coord 2 2 'up))])
      (assert-equal 7 (length ns))))

  (define-test "all neighbors includes flip"
    (let* ([t (triangle-coord 2 2 'up)]
           [ns (triangle-neighbors-all t)]
           [flip (triangle-flip t)]
           [has-flip (ormap (lambda (n) (triangle-coord-equal? n flip)) ns)])
      (assert-true has-flip)))

  (define-test "generic neighbor function: edge mode"
    (let ([ns (triangle-neighbors (triangle-coord 1 1 'up) 'edge)])
      (assert-equal 3 (length ns))))

  (define-test "generic neighbor function: all mode"
    (let ([ns (triangle-neighbors (triangle-coord 1 1 'up) 'all)])
      (assert-equal 7 (length ns))))

  (define-test "generic neighbor function: invalid mode errors"
    (assert-error (lambda () (triangle-neighbors (triangle-coord 0 0 'up) 'invalid)))))

;;; ============================================================
;;; Distance
;;; ============================================================

(test-group "triangle-distance"
  (define-test "distance to self is 0"
    (let ([t (triangle-coord 2 3 'up)])
      (assert-equal 0 (triangle-distance-manhattan t t))))

  (define-test "distance to flip is 1"
    (let ([t (triangle-coord 2 3 'up)])
      (assert-equal 1 (triangle-distance-manhattan t (triangle-flip t)))))

  (define-test "distance to adjacent"
    (let ([t1 (triangle-coord 1 1 'up)]
          [t2 (triangle-coord 1 2 'down)])
      ;; |1-1| + |2-1| + orientation-diff = 0 + 1 + 1 = 2
      (assert-equal 2 (triangle-distance-manhattan t1 t2))))

  (define-test "distance symmetric"
    (let ([t1 (triangle-coord 0 0 'up)]
          [t2 (triangle-coord 3 4 'down)])
      (assert-equal (triangle-distance-manhattan t1 t2)
                    (triangle-distance-manhattan t2 t1)))))

;;; ============================================================
;;; Line Drawing
;;; ============================================================

(test-group "triangle-line"
  (define-test "line from point to itself"
    (let* ([t (triangle-coord 2 2 'up)]
           [line (triangle-line t t)])
      (assert-equal 1 (length line))))

  (define-test "line has correct endpoints"
    (let* ([t1 (triangle-coord 0 0 'up)]
           [t2 (triangle-coord 3 3 'down)]
           [line (triangle-line t1 t2)])
      ;; First element should be at (0,0)
      (assert-equal 0 (triangle-row (car line)))
      (assert-equal 0 (triangle-col (car line)))
      ;; Last element should be at (3,3)
      (let ([last-el (list-ref line (- (length line) 1))])
        (assert-equal 3 (triangle-row last-el))
        (assert-equal 3 (triangle-col last-el)))))

  (define-test "line length is reasonable"
    (let* ([t1 (triangle-coord 0 0 'up)]
           [t2 (triangle-coord 4 4 'up)]
           [line (triangle-line t1 t2)])
      ;; Should have ~5 points (steps = max(|4|, |4|) = 4, plus 1)
      (assert-equal 5 (length line)))))

;;; ============================================================
;;; Range
;;; ============================================================

(test-group "triangle-range"
  (define-test "range 0 returns just the center orientations"
    (let* ([center (triangle-coord 2 2 'up)]
           [r (triangle-range center 0)])
      ;; Only coordinates with distance 0 = center itself
      (assert-equal 1 (length r))))

  (define-test "range 1 returns small neighborhood"
    (let* ([center (triangle-coord 2 2 'up)]
           [r (triangle-range center 1)])
      ;; Should include center and some neighbors
      (assert-true (> (length r) 1))))

  (define-test "range monotonically increases"
    (let* ([center (triangle-coord 2 2 'up)]
           [r1 (triangle-range center 1)]
           [r2 (triangle-range center 2)])
      (assert-true (>= (length r2) (length r1))))))

;;; ============================================================
;;; Board Creation
;;; ============================================================

(test-group "triangle-board"
  (define-test "make-triangle-board creates board"
    (let ([b (make-triangle-board 3 4)])
      (assert-true (board%? b))
      (assert-equal 'triangle (board-shape b))))

  (define-test "board metadata has rows and cols"
    (let ([b (make-triangle-board 3 4)])
      (assert-equal 3 (triangle-board-rows b))
      (assert-equal 4 (triangle-board-cols b))))

  (define-test "board with default tile fills all cells"
    (let ([b (make-triangle-board 2 2 (make-tile 'floor '()))])
      ;; 2 rows × 2 cols × 2 orientations = 8 tiles
      (assert-true (board%? b))
      ;; Check a specific cell
      (let ([t (board-get b (triangle-coord 0 0 'up))])
        (assert-true (pair? t))
        (assert-equal 'floor (tile-type t)))))

  (define-test "triangle-in-bounds? checks bounds"
    (let ([b (make-triangle-board 3 4)])
      (assert-true (triangle-in-bounds? b (triangle-coord 0 0 'up)))
      (assert-true (triangle-in-bounds? b (triangle-coord 2 3 'down)))
      (assert-false (triangle-in-bounds? b (triangle-coord 3 0 'up)))
      (assert-false (triangle-in-bounds? b (triangle-coord 0 4 'up)))
      (assert-false (triangle-in-bounds? b (triangle-coord -1 0 'up))))))

;;; ============================================================
;;; Transformations
;;; ============================================================

(test-group "triangle-transformations"
  (define-test "rotate-180 flips orientation"
    (let* ([t (triangle-coord 1 1 'up)]
           [rotated (triangle-rotate-180 t 1 1)])
      ;; Rotating (1,1) around center (1,1) should stay at (1,1) but flip
      (assert-equal 1 (triangle-row rotated))
      (assert-equal 1 (triangle-col rotated))
      (assert-equal 'down (triangle-orientation rotated))))

  (define-test "rotate-180 of origin around (1,1)"
    (let* ([t (triangle-coord 0 0 'up)]
           [rotated (triangle-rotate-180 t 1 1)])
      ;; (2*1 - 0, 2*1 - 0) = (2, 2), orientation flipped
      (assert-equal 2 (triangle-row rotated))
      (assert-equal 2 (triangle-col rotated))
      (assert-equal 'down (triangle-orientation rotated))))

  (define-test "reflect-horizontal preserves row"
    (let* ([t (triangle-coord 2 1 'up)]
           [reflected (triangle-reflect-horizontal t 3)])
      ;; row stays, col = 2*3 - 1 = 5
      (assert-equal 2 (triangle-row reflected))
      (assert-equal 5 (triangle-col reflected))
      (assert-equal 'up (triangle-orientation reflected))))

  (define-test "reflect-vertical flips orientation"
    (let* ([t (triangle-coord 1 2 'up)]
           [reflected (triangle-reflect-vertical t 3)])
      ;; row = 2*3 - 1 = 5, col stays, orientation flips
      (assert-equal 5 (triangle-row reflected))
      (assert-equal 2 (triangle-col reflected))
      (assert-equal 'down (triangle-orientation reflected))))

  (define-test "double horizontal reflection is identity"
    (let* ([t (triangle-coord 2 3 'up)]
           [t2 (triangle-reflect-horizontal
                 (triangle-reflect-horizontal t 5) 5)])
      (assert-true (triangle-coord-equal? t t2))))

  (define-test "double vertical reflection is identity"
    (let* ([t (triangle-coord 2 3 'up)]
           [t2 (triangle-reflect-vertical
                 (triangle-reflect-vertical t 5) 5)])
      (assert-true (triangle-coord-equal? t t2)))))

;;; ============================================================
;;; Run Tests
;;; ============================================================

(run-all-tests-and-exit)
