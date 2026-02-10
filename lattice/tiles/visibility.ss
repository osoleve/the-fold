(unless (top-level-bound? 'require) (load "core/lang/module.ss"))
(require 'tiles/core)

(doc 'module 'tiles/visibility)
(doc 'description "Line of sight and field of view algorithms for tile-based games")
(doc 'layer 'lattice)
(doc 'purity 'total)

(doc 'section 'line-of-sight)

(define (has-line-of-sight? board origin target line-fn blocks-vision-fn)
  (doc 'export #t)
  (doc 'description "Check if there's clear line of sight from origin to target")
  (doc 'type '(-> Board Coord Coord (-> Coord Coord (List Coord)) (-> Coord Bool) Bool))
  (doc 'param 'line-fn "Returns coordinates along line (exclusive of origin, inclusive of target)")
  (doc 'param 'blocks-vision-fn "Predicate checking if coordinate blocks vision")
  (doc 'returns "#t if line of sight is clear, #f if blocked")
  (let ([line (line-fn origin target)])
       (let loop ([coords line])
            (cond
             [(null? coords) #t] ; Reached target without blocking
             [(equal? (car coords) target) #t] ; Reached target
             [(blocks-vision-fn (car coords)) #f] ; Blocked by this tile
             [else (loop (cdr coords))])))) ; Continue along line

(define (board-has-los? board origin target line-fn)
  (doc 'export #t)
  (doc 'description "Check line of sight using board's vision-blocking tile property")
  (doc 'type '(-> Board Coord Coord (-> Coord Coord (List Coord)) Bool))
  (has-line-of-sight?
   board origin target line-fn
   (lambda (coord)
           (let ([tile (board-get board coord)])
                (and tile (tile-get-prop tile 'blocks-vision #f))))))

(doc 'section 'field-of-view)

(define (calculate-fov board origin max-radius neighbor-fn line-fn blocks-vision-fn)
  (doc 'export #t)
  (doc 'description "Calculate field of view from origin within radius. Uses simple raycast FOV.")
  (doc 'type '(-> Board Coord Integer (-> Coord (List Coord)) (-> Coord Coord (List Coord)) (-> Coord Bool) (List Coord)))
  (doc 'param 'neighbor-fn "Function to get neighbors")
  (doc 'param 'line-fn "Function to draw line between two coordinates")
  (doc 'param 'blocks-vision-fn "Predicate checking if coordinate blocks vision")
  (doc 'returns "List of visible coordinates")
  (doc 'note "More sophisticated algorithms (shadowcasting) could be added later")
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

(define (board-fov board origin max-radius neighbor-fn line-fn)
  (doc 'export #t)
  (doc 'description "Calculate field of view using board's vision-blocking tile property")
  (doc 'type '(-> Board Coord Integer (-> Coord (List Coord)) (-> Coord Coord (List Coord)) (List Coord)))
  (calculate-fov
   board origin max-radius
   neighbor-fn
   line-fn
   (lambda (coord)
           (let ([tile (board-get board coord)])
                (and tile (tile-get-prop tile 'blocks-vision #f))))))

(doc 'section 'shadowcasting)

(define (shadowcast-fov board origin max-radius neighbor-fn line-fn blocks-vision-fn distance-fn)
  (doc 'export #t)
  (doc 'description "Calculate field of view using recursive shadowcasting. Currently a placeholder using raycast FOV.")
  (doc 'type '(-> Board Coord Integer (-> Coord (List Coord)) (-> Coord Coord (List Coord)) (-> Coord Bool) (-> Coord Coord Number) (List Coord)))
  (doc 'todo "Implement proper shadowcasting algorithm")
  ;; TODO: Implement proper shadowcasting
  ;; For now, use simple raycast FOV
  (calculate-fov board origin max-radius neighbor-fn line-fn blocks-vision-fn))

(doc 'section 'utilities)

(define (visible-enemies board origin max-radius neighbor-fn line-fn is-enemy-fn)
  (doc 'export #t)
  (doc 'description "Get all enemy positions visible from origin")
  (doc 'type '(-> Board Coord Integer (-> Coord (List Coord)) (-> Coord Coord (List Coord)) (-> Coord Bool) (List Coord)))
  (doc 'param 'is-enemy-fn "Predicate to check if coordinate has enemy")
  (doc 'returns "List of visible enemy coordinates")
  (let ([visible-tiles (board-fov board origin max-radius neighbor-fn line-fn)])
       (filter is-enemy-fn visible-tiles)))

(define (tiles-in-sight board origin target line-fn)
  (doc 'export #t)
  (doc 'description "Get all tiles along line of sight from origin to target")
  (doc 'type '(-> Board Coord Coord (-> Coord Coord (List Coord)) (List Coord)))
  (doc 'returns "List of coordinates along the line (including origin and target)")
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
