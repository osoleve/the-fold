;;; playpen/loom/core.ss — RPG SDK Core Types and Utilities
;;;
;;; The foundation for a 2D tile-based turn-based RPG system.
;;; Provides core data structures and utilities used across all RPG modules.
;;;
;;; This is Playpen code: the SDK for building roguelikes and dungeon crawlers.
;;;
;;; Dependencies:
;;;   - boundary/layout.ss (canvas, point, rect)
;;;   - boundary/ui/layers.ss (layer system)
;;;
;;; Exports:
;;;   Direction: 'north 'south 'east 'west 'northeast 'northwest 'southeast 'southwest
;;;   direction->delta, delta->direction, opposite-direction
;;;   manhattan-distance, chebyshev-distance
;;;   clamp, lerp, in-range?
;;;   alist-ref, alist-set, alist-update

;;; ====
;;; Direction System
;;; ====

;;; Direction : (+ 'north 'south 'east 'west
;;;               'northeast 'northwest 'southeast 'southwest 'none)
;;;
;;; The eight cardinal and intercardinal directions, plus 'none for stationary.

(define valid-directions
  '(north south east west northeast northwest southeast southwest none))

(define (direction? d)
  (if (memq d valid-directions) #t #f))

;;; direction->delta : Direction -> Point
;;; Convert a direction to a (dx . dy) delta.
;;; Note: Y increases downward (standard screen coordinates).
(define (direction->delta dir)
  (case dir
        [(north)     (cons  0 -1)]
        [(south)     (cons  0  1)]
        [(east)      (cons  1  0)]
        [(west)      (cons -1  0)]
        [(northeast) (cons  1 -1)]
        [(northwest) (cons -1 -1)]
        [(southeast) (cons  1  1)]
        [(southwest) (cons -1  1)]
        [(none)      (cons  0  0)]
        [else        (cons  0  0)]))

;;; delta->direction : Int × Int -> Direction
;;; Convert a delta to the closest direction.
(define (delta->direction dx dy)
  (cond
   [(and (= dx 0) (< dy 0)) 'north]
   [(and (= dx 0) (> dy 0)) 'south]
   [(and (> dx 0) (= dy 0)) 'east]
   [(and (< dx 0) (= dy 0)) 'west]
   [(and (> dx 0) (< dy 0)) 'northeast]
   [(and (< dx 0) (< dy 0)) 'northwest]
   [(and (> dx 0) (> dy 0)) 'southeast]
   [(and (< dx 0) (> dy 0)) 'southwest]
   [else 'none]))

;;; opposite-direction : Direction -> Direction
;;; Get the opposite direction.
(define (opposite-direction dir)
  (case dir
        [(north) 'south]
        [(south) 'north]
        [(east) 'west]
        [(west) 'east]
        [(northeast) 'southwest]
        [(northwest) 'southeast]
        [(southeast) 'northwest]
        [(southwest) 'northeast]
        [(none) 'none]
        [else 'none]))

;;; cardinal-directions : List Direction
;;; The four cardinal directions.
(define cardinal-directions '(north south east west))

;;; all-directions : List Direction
;;; All eight directions (no 'none).
(define all-directions
  '(north south east west northeast northwest southeast southwest))

;;; ====
;;; Distance Functions
;;; ====

;;; manhattan-distance : Point × Point -> Nat
;;; Calculate Manhattan (taxicab) distance between two points.
;;; Used for 4-directional movement costs.
(define (manhattan-distance p1 p2)
  (+ (abs (- (car p1) (car p2)))
     (abs (- (cdr p1) (cdr p2)))))

;;; chebyshev-distance : Point × Point -> Nat
;;; Calculate Chebyshev (chessboard) distance between two points.
;;; Used for 8-directional movement costs.
(define (chebyshev-distance p1 p2)
  (max (abs (- (car p1) (car p2)))
       (abs (- (cdr p1) (cdr p2)))))

;;; euclidean-distance-squared : Point × Point -> Nat
;;; Calculate squared Euclidean distance (avoids sqrt).
(define (euclidean-distance-squared p1 p2)
  (let ([dx (- (car p1) (car p2))]
        [dy (- (cdr p1) (cdr p2))])
       (+ (* dx dx) (* dy dy))))

;;; ====
;;; Numeric Utilities
;;; ====

;;; clamp : Num × Num × Num -> Num
;;; Clamp value between min and max.
(define (clamp val min-val max-val)
  (max min-val (min max-val val)))

;;; lerp : Num × Num × Num -> Num
;;; Linear interpolation between a and b by t (0-1).
(define (lerp a b t)
  (+ a (* (- b a) t)))

;;; in-range? : Num × Num × Num -> Bool
;;; Check if value is in range [min, max).
(define (in-range? val min-val max-val)
  (and (>= val min-val) (< val max-val)))

;;; ====
;;; Association List Utilities
;;; ====

;;; alist-ref : Alist × Symbol × [Any] -> Any
;;; Get value from alist, with optional default.
(define alist-ref
  (case-lambda
   [(alist key)
    (let ([pair (assq key alist)])
         (if pair (cdr pair) #f))]
   [(alist key default)
    (let ([pair (assq key alist)])
         (if pair (cdr pair) default))]))

;;; alist-set : Alist × Symbol × Any -> Alist
;;; Set or add a key-value pair in alist.
(define (alist-set alist key value)
  (let ([found #f])
       (let ([result (map (lambda (pair)
                                  (if (eq? (car pair) key)
                                      (begin (set! found #t)
                                             (cons key value))
                                      pair))
                          alist)])
            (if found
                result
                (cons (cons key value) alist)))))

;;; alist-update : Alist × Symbol × (Any -> Any) × [Any] -> Alist
;;; Update a value in alist using a function.
(define alist-update
  (case-lambda
   [(alist key fn)
    (alist-set alist key (fn (alist-ref alist key)))]
   [(alist key fn default)
    (alist-set alist key (fn (alist-ref alist key default)))]))

;;; alist-remove : Alist × Symbol -> Alist
;;; Remove a key from alist.
(define (alist-remove alist key)
  (filter (lambda (pair) (not (eq? (car pair) key))) alist))

;;; alist-keys : Alist -> List Symbol
;;; Get all keys from alist.
(define (alist-keys alist)
  (map car alist))

;;; alist-merge : Alist × Alist -> Alist
;;; Merge two alists, second takes precedence.
(define (alist-merge base override)
  (fold-left (lambda (acc pair)
                     (alist-set acc (car pair) (cdr pair)))
             base
             override))

;;; ====
;;; ID Generation
;;; ====

;;; Simple counter-based ID generation for entities.
;;; In production, this would use the block hash system.

(define *next-entity-id* 0)

;;; generate-id : -> Nat
;;; Generate a unique entity ID.
(define (generate-id)
  (let ([id *next-entity-id*])
       (set! *next-entity-id* (+ *next-entity-id* 1))
       id))

;;; reset-id-counter! : -> Void
;;; Reset the ID counter (for testing).
(define (reset-id-counter!)
  (set! *next-entity-id* 0))

;;; ====
;;; Point Utilities (extending boundary/layout.ss)
;;; ====

;;; point-add : Point × Point -> Point
;;; Add two points (or point + delta).
(define (point-add p1 p2)
  (cons (+ (car p1) (car p2))
        (+ (cdr p1) (cdr p2))))

;;; point-sub : Point × Point -> Point
;;; Subtract two points.
(define (point-sub p1 p2)
  (cons (- (car p1) (car p2))
        (- (cdr p1) (cdr p2))))

;;; point-scale : Point × Num -> Point
;;; Scale a point by a factor.
(define (point-scale p factor)
  (cons (* (car p) factor)
        (* (cdr p) factor)))

;;; point=? : Point × Point -> Bool
;;; Check if two points are equal.
(define (point=? p1 p2)
  (and (= (car p1) (car p2))
       (= (cdr p1) (cdr p2))))

;;; point-neighbors : Point × Bool -> List Point
;;; Get neighboring points (4 or 8 directions).
(define (point-neighbors pt include-diagonals?)
  (let ([dirs (if include-diagonals? all-directions cardinal-directions)])
       (map (lambda (dir)
                    (point-add pt (direction->delta dir)))
            dirs)))

;;; ====
;;; Random Utilities
;;; ====

;;; random-range : Int × Int -> Int
;;; Random integer in range [min, max).
(define (random-range min-val max-val)
  (+ min-val (random (- max-val min-val))))

;;; random-choice : List -> Any
;;; Pick a random element from a list.
(define (random-choice lst)
  (if (null? lst)
      #f
      (list-ref lst (random (length lst)))))

;;; shuffle : List -> List
;;; Shuffle a list (Fisher-Yates).
(define (shuffle lst)
  (let ([vec (list->vector lst)])
       (let loop ([i (- (vector-length vec) 1)])
            (if (<= i 0)
                (vector->list vec)
                (let* ([j (random (+ i 1))]
                       [temp (vector-ref vec i)])
                      (vector-set! vec i (vector-ref vec j))
                      (vector-set! vec j temp)
                      (loop (- i 1)))))))

;;; weighted-choice : List (× Any Num) -> Any
;;; Pick from weighted options. Each option is (value . weight).
(define (weighted-choice options)
  (let* ([total (fold-left (lambda (sum opt) (+ sum (cdr opt))) 0 options)]
         [roll (random total)])
        (let loop ([opts options] [cumulative 0])
             (if (null? opts)
                 #f
                 (let ([new-cumulative (+ cumulative (cdar opts))])
                      (if (< roll new-cumulative)
                          (caar opts)
                          (loop (cdr opts) new-cumulative)))))))

;;; ====
;;; Export Summary
;;; ====
;;;
;;; Directions:
;;;   valid-directions, direction?, direction->delta, delta->direction
;;;   opposite-direction, cardinal-directions, all-directions
;;;
;;; Distance:
;;;   manhattan-distance, chebyshev-distance, euclidean-distance-squared
;;;
;;; Math:
;;;   clamp, lerp, in-range?
;;;
;;; Alist:
;;;   alist-ref, alist-set, alist-update, alist-remove, alist-keys, alist-merge
;;;
;;; IDs:
;;;   generate-id, reset-id-counter!
;;;
;;; Points:
;;;   point-add, point-sub, point-scale, point=?, point-neighbors
;;;
;;; Random:
;;;   random-range, random-choice, shuffle, weighted-choice
