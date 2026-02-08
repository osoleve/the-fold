;;; lattice/tiles/test-visibility.ss — Tests for Line of Sight & Field of View
;;; Run with: scheme --script lattice/tiles/test-visibility.ss

(load "core/testing/test-framework.ss")
(load "lattice/tiles/core.ss")
(load "lattice/tiles/square.ss")
(load "lattice/tiles/pathfinding.ss")
(load "lattice/tiles/visibility.ss")

;;; ============================================================
;;; Helpers
;;; ============================================================

;;; Create a square board with all floor tiles
(define (make-floor-board w h)
  (make-square-board w h (make-tile 'floor '())))

;;; Create a board with a wall at given coordinates
(define (set-wall board coord)
  (board-set board coord (make-tile 'wall '((blocks-vision . #t)))))

;;; Line function for square grids
(define (sq-line c1 c2)
  (square-line c1 c2))

;;; Neighbor function for square grids (orthogonal only)
(define (sq-neighbors coord)
  (square-neighbors coord 'ortho))

;;; Vision blocker using board tile property
(define (blocks-vision-on board)
  (lambda (coord)
    (let ([tile (board-get board coord)])
      (and tile (tile-get-prop tile 'blocks-vision #f)))))

;;; ============================================================
;;; Line of Sight (Core)
;;; ============================================================

(test-group "has-line-of-sight"
  (define board (make-floor-board 5 5))

  (define-test "clear path returns #t"
    (assert-true
     (has-line-of-sight? board
                         (square-coord 0 0)
                         (square-coord 4 4)
                         sq-line
                         (lambda (c) #f))))  ; nothing blocks

  (define-test "blocked path returns #f"
    (assert-false
     (has-line-of-sight? board
                         (square-coord 0 0)
                         (square-coord 4 0)
                         sq-line
                         (lambda (c) (equal? c (square-coord 2 0))))))

  (define-test "adjacent tiles always visible"
    (assert-true
     (has-line-of-sight? board
                         (square-coord 2 2)
                         (square-coord 2 3)
                         sq-line
                         (lambda (c) #f))))

  (define-test "same tile always visible"
    (assert-true
     (has-line-of-sight? board
                         (square-coord 2 2)
                         (square-coord 2 2)
                         sq-line
                         (lambda (c) #f)))))

;;; ============================================================
;;; Board LOS
;;; ============================================================

(test-group "board-has-los"
  (define-test "open board has LOS everywhere"
    (let ([board (make-floor-board 5 5)])
      (assert-true
       (board-has-los? board
                       (square-coord 0 0)
                       (square-coord 4 4)
                       sq-line))))

  (define-test "wall blocks LOS"
    (let* ([board (make-floor-board 5 5)]
           [board (set-wall board (square-coord 2 0))])
      (assert-false
       (board-has-los? board
                       (square-coord 0 0)
                       (square-coord 4 0)
                       sq-line))))

  (define-test "wall adjacent to target still blocks"
    (let* ([board (make-floor-board 5 5)]
           [board (set-wall board (square-coord 3 0))])
      (assert-false
       (board-has-los? board
                       (square-coord 0 0)
                       (square-coord 4 0)
                       sq-line))))

  (define-test "wall not on line doesn't block"
    (let* ([board (make-floor-board 5 5)]
           [board (set-wall board (square-coord 2 4))])
      ;; Wall at (2,4) shouldn't block line from (0,0) to (4,0)
      (assert-true
       (board-has-los? board
                       (square-coord 0 0)
                       (square-coord 4 0)
                       sq-line)))))

;;; ============================================================
;;; Field of View
;;; ============================================================

(test-group "calculate-fov"
  (define board (make-floor-board 7 7))

  (define-test "origin always visible"
    (let ([fov (calculate-fov board
                              (square-coord 3 3)
                              2
                              sq-neighbors
                              sq-line
                              (lambda (c) #f))])
      (assert-true (pair? (member (square-coord 3 3) fov)))))

  (define-test "adjacent tiles visible"
    (let ([fov (calculate-fov board
                              (square-coord 3 3)
                              1
                              sq-neighbors
                              sq-line
                              (lambda (c) #f))])
      (assert-true (pair? (member (square-coord 3 4) fov)))
      (assert-true (pair? (member (square-coord 3 2) fov)))
      (assert-true (pair? (member (square-coord 2 3) fov)))
      (assert-true (pair? (member (square-coord 4 3) fov)))))

  (define-test "radius 0 shows only origin"
    (let ([fov (calculate-fov board
                              (square-coord 3 3)
                              0
                              sq-neighbors
                              sq-line
                              (lambda (c) #f))])
      (assert-equal 1 (length fov))
      (assert-true (pair? (member (square-coord 3 3) fov)))))

  (define-test "larger radius sees more"
    (let ([fov1 (calculate-fov board
                               (square-coord 3 3)
                               1
                               sq-neighbors
                               sq-line
                               (lambda (c) #f))]
          [fov2 (calculate-fov board
                               (square-coord 3 3)
                               2
                               sq-neighbors
                               sq-line
                               (lambda (c) #f))])
      (assert-true (>= (length fov2) (length fov1)))))

  (define-test "wall blocks vision behind it"
    (let* ([board-w (set-wall board (square-coord 3 4))]
           [fov (calculate-fov board-w
                               (square-coord 3 3)
                               3
                               sq-neighbors
                               sq-line
                               (blocks-vision-on board-w))])
      ;; (3,5) should NOT be visible (wall at (3,4) blocks)
      (assert-false (pair? (member (square-coord 3 5) fov))))))

;;; ============================================================
;;; Board FOV
;;; ============================================================

(test-group "board-fov"
  (define-test "open board sees everything in range"
    (let* ([board (make-floor-board 5 5)]
           [fov (board-fov board
                           (square-coord 2 2)
                           2
                           sq-neighbors
                           sq-line)])
      ;; Center and some neighbors should be visible
      (assert-true (pair? (member (square-coord 2 2) fov)))
      (assert-true (> (length fov) 5))))

  (define-test "wall creates shadow"
    (let* ([board (make-floor-board 7 7)]
           [board (set-wall board (square-coord 3 4))]
           [fov (board-fov board
                           (square-coord 3 3)
                           3
                           sq-neighbors
                           sq-line)])
      ;; Origin visible
      (assert-true (pair? (member (square-coord 3 3) fov)))
      ;; Behind wall should be hidden
      (assert-false (pair? (member (square-coord 3 5) fov))))))

;;; ============================================================
;;; Visible Enemies
;;; ============================================================

(test-group "visible-enemies"
  (define-test "finds visible enemy"
    (let* ([board (make-floor-board 5 5)]
           [enemy-coord (square-coord 2 2)]
           [found (visible-enemies board
                                   (square-coord 0 0)
                                   5
                                   sq-neighbors
                                   sq-line
                                   (lambda (c) (equal? c enemy-coord)))])
      (assert-true (pair? (member enemy-coord found)))))

  (define-test "doesn't find enemy behind wall"
    (let* ([board (make-floor-board 5 5)]
           [board (set-wall board (square-coord 1 0))]
           [enemy-coord (square-coord 3 0)]
           [found (visible-enemies board
                                   (square-coord 0 0)
                                   5
                                   sq-neighbors
                                   sq-line
                                   (lambda (c) (equal? c enemy-coord)))])
      (assert-false (pair? (member enemy-coord found)))))

  (define-test "returns empty for no enemies"
    (let* ([board (make-floor-board 5 5)]
           [found (visible-enemies board
                                   (square-coord 0 0)
                                   5
                                   sq-neighbors
                                   sq-line
                                   (lambda (c) #f))])
      (assert-equal '() found))))

;;; ============================================================
;;; Tiles In Sight
;;; ============================================================

(test-group "tiles-in-sight"
  (define board (make-floor-board 5 5))

  (define-test "includes origin"
    (let ([tiles (tiles-in-sight board
                                 (square-coord 0 0)
                                 (square-coord 3 0)
                                 sq-line)])
      (assert-equal (square-coord 0 0) (car tiles))))

  (define-test "includes target"
    (let ([tiles (tiles-in-sight board
                                 (square-coord 0 0)
                                 (square-coord 3 0)
                                 sq-line)])
      (assert-true (pair? (member (square-coord 3 0) tiles)))))

  (define-test "returns intermediate tiles"
    (let ([tiles (tiles-in-sight board
                                 (square-coord 0 0)
                                 (square-coord 4 0)
                                 sq-line)])
      ;; Should have origin + line points
      (assert-true (> (length tiles) 2)))))

;;; ============================================================
;;; Shadowcast FOV (placeholder)
;;; ============================================================

(test-group "shadowcast-fov"
  (define-test "works as fallback to calculate-fov"
    (let* ([board (make-floor-board 5 5)]
           [fov (shadowcast-fov board
                                (square-coord 2 2)
                                2
                                sq-neighbors
                                sq-line
                                (lambda (c) #f)
                                (lambda (c1 c2) (square-distance c1 c2 'manhattan)))])
      (assert-true (pair? (member (square-coord 2 2) fov)))
      (assert-true (> (length fov) 1)))))

;;; ============================================================
;;; Run Tests
;;; ============================================================

(run-all-tests-and-exit)
