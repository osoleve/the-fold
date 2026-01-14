;;; playpen/boardcraft/visibility.ss — Line of Sight and Visibility
;;;
;;; Visibility and line-of-sight algorithms for tile-based board games.
;;;
;;; Implements:
;;;   • Line-of-sight (LOS) checking between two coordinates
;;;   • Field of view (FOV) calculation from a position
;;;   • Raycast visibility with blocking tiles
;;;   • Generic algorithms that work with any tile shape
;;;
;;; Dependencies:
;;;   - playpen/boardcraft/core.ss
;;;   - playpen/boardcraft/pathfinding.ss (for priority queue)

;;; ====
;;; Line of Sight
;;; ====

;;; has-line-of-sight? : Board × Coord × Coord × (Coord × Coord → List Coord) × (Coord → Bool) → Boolean
;;; Check if there's a clear line of sight from origin to target
;;;
;;; Parameters:
;;;   board - the game board
;;;   origin - starting coordinate
;;;   target - target coordinate
;;;   line-fn - function that returns list of coordinates along line from A to B
;;;   blocks-vision-fn - predicate to check if coordinate blocks vision
;;;
;;; Returns: #t if line of sight is clear, #f if blocked
;;;
;;; The line-fn should return coordinates in order from origin to target (exclusive of origin, inclusive of target).
;;; A tile blocks vision if blocks-vision-fn returns #t.
(define (has-line-of-sight? board origin target line-fn blocks-vision-fn)
  (let ([line (line-fn origin target)])
       (let loop ([coords line])
            (cond
             [(null? coords) #t] ; Reached target without blocking
             [(equal? (car coords) target) #t] ; Reached target
             [(blocks-vision-fn (car coords)) #f] ; Blocked by this tile
             [else (loop (cdr coords))])))) ; Continue along line

;;; board-has-los? : Board × Coord × Coord × (Coord × Coord → List Coord) → Boolean
;;; Check line of sight using board's vision-blocking property
(define (board-has-los? board origin target line-fn)
  (has-line-of-sight?
   board origin target line-fn
   (lambda (coord)
           (let ([tile (board-get board coord)])
                (and tile (tile-get-prop tile 'blocks-vision #f))))))

;;; ====
;;; Field of View
;;; ====

;;; calculate-fov : Board × Coord × Integer × (Coord → List Coord) × (Coord × Coord → List Coord) × (Coord → Bool) → (List Coord)
;;; Calculate field of view from origin within radius
;;;
;;; Parameters:
;;;   board - the game board
;;;   origin - viewing position
;;;   max-radius - maximum vision distance
;;;   neighbor-fn - function to get neighbors
;;;   line-fn - function to draw line between two coordinates
;;;   blocks-vision-fn - predicate to check if coordinate blocks vision
;;;
;;; Returns: List of visible coordinates
;;;
;;; Uses simple raycast FOV: for each tile in range, cast a ray and check LOS.
;;; More sophisticated algorithms (shadowcasting) could be added later.
(define (calculate-fov board origin max-radius neighbor-fn line-fn blocks-vision-fn)
  (let ([visible (make-hashtable equal-hash equal?)])
       ;; Origin is always visible
       (hashtable-set! visible origin #t)
       ;; Use BFS to explore tiles in range
       (let loop ([queue (list origin)]
                  [visited (make-hashtable equal-hash equal?)]
                  [distance (make-hashtable equal-hash equal?)])
            (hashtable-set! visited origin #t)
            (hashtable-set! distance origin 0)
            (if (null? queue)
                (vector->list (hashtable-keys visible))
                (let* ([current (car queue)]
                       [rest-q (cdr queue)]
                       [current-dist (hashtable-ref distance current 0)])
                      (if (>= current-dist max-radius)
                          (loop rest-q visited distance)
                          (let* ([neighbors (neighbor-fn current)]
                                 [valid-neighbors
                                  (filter (lambda (n)
                                                  (and (board-get board n) ; Tile exists
                                                       (not (hashtable-ref visited n #f))))
                                          neighbors)])
                                ;; Check LOS for each neighbor
                                (for-each
                                 (lambda (n)
                                         (hashtable-set! visited n #t)
                                         (hashtable-set! distance n (+ current-dist 1))
                                         ;; Check if we have LOS to this neighbor
                                         (when (has-line-of-sight? board origin n line-fn blocks-vision-fn)
                                               (hashtable-set! visible n #t)))
                                 valid-neighbors)
                                (loop (append rest-q valid-neighbors) visited distance))))))))

;;; board-fov : Board × Coord × Integer × (Coord → List Coord) × (Coord × Coord → List Coord) → (List Coord)
;;; Calculate field of view using board's vision-blocking property
(define (board-fov board origin max-radius neighbor-fn line-fn)
  (calculate-fov
   board origin max-radius
   neighbor-fn
   line-fn
   (lambda (coord)
           (let ([tile (board-get board coord)])
                (and tile (tile-get-prop tile 'blocks-vision #f))))))

;;; ====
;;; Shadowcast Field of View (Recursive Shadowcasting)
;;; ====

;;; shadowcast-fov : Board × Coord × Integer × (Coord → List Coord) × (Coord → Bool) × (Coord → Bool) → (List Coord)
;;; Calculate field of view using recursive shadowcasting algorithm
;;;
;;; Parameters:
;;;   board - the game board
;;;   origin - viewing position
;;;   max-radius - maximum vision distance
;;;   neighbor-fn - function to get neighbors
;;;   blocks-vision-fn - predicate to check if coordinate blocks vision
;;;   distance-fn - function to compute distance between two coordinates
;;;
;;; Returns: List of visible coordinates
;;;
;;; This is a placeholder for a more sophisticated shadowcasting implementation.
;;; For now, we use the simple raycast FOV.
(define (shadowcast-fov board origin max-radius neighbor-fn line-fn blocks-vision-fn distance-fn)
  ;; TODO: Implement proper shadowcasting
  ;; For now, use simple raycast FOV
  (calculate-fov board origin max-radius neighbor-fn line-fn blocks-vision-fn))

;;; ====
;;; Visibility Utilities
;;; ====

;;; visible-enemies : Board × Coord × Integer × (Coord → List Coord) × (Coord × Coord → List Coord) × (Coord → Bool) → (List Coord)
;;; Get all enemy positions visible from origin
;;;
;;; Parameters:
;;;   board - the game board
;;;   origin - viewing position
;;;   max-radius - maximum vision distance
;;;   neighbor-fn - function to get neighbors
;;;   line-fn - function to draw line
;;;   is-enemy-fn - predicate to check if coordinate has enemy
;;;
;;; Returns: List of visible enemy coordinates
(define (visible-enemies board origin max-radius neighbor-fn line-fn is-enemy-fn)
  (let ([visible-tiles (board-fov board origin max-radius neighbor-fn line-fn)])
       (filter is-enemy-fn visible-tiles)))

;;; tiles-in-sight : Board × Coord × Coord × (Coord × Coord → List Coord) → (List Coord)
;;; Get all tiles along line of sight from origin to target
;;;
;;; Returns: List of coordinates along the line (including origin and target)
(define (tiles-in-sight board origin target line-fn)
  (cons origin (line-fn origin target)))

;;; ====
;;; Exports Summary
;;; ====

;;; This module provides:
;;;   Core Visibility:
;;;     • has-line-of-sight? — Check if path between two points is clear
;;;     • calculate-fov — Calculate field of view from position
;;;     • shadowcast-fov — Shadowcasting FOV (placeholder)
;;;
;;;   Board Integration:
;;;     • board-has-los? — LOS check using board's vision-blocking tiles
;;;     • board-fov — FOV calculation using board's vision-blocking tiles
;;;
;;;   Utilities:
;;;     • visible-enemies — Find visible enemy positions
;;;     • tiles-in-sight — Get all tiles along line of sight
;;;
;;; All algorithms work with any tile shape through the neighbor and line protocols.
