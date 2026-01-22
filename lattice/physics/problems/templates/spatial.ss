;;; lattice/physics/problems/templates/spatial.ss — Spatial Reasoning Problems
;;; @module spatial
;;; @requires physics-problem

(load "lattice/physics/problems/physics-problem.ss")

(doc 'module 'spatial)
(doc 'description "Spatial reasoning problem templates: size comparison, distance estimation, object counting. These are Tier 0 (direct observation) problems requiring no physics inference.")
(doc 'layer 'lattice)
(doc 'purity 'partial)

;;; ============================================================
;;; Part 1: Size Comparison Problems
;;; ============================================================
;;;
;;; "Which ball is larger?"
;;; Requires: Visual comparison of radii
;;; Answer: A, B, or same

(doc 'section 'size-comparison)

;;; size-comparison-params : ParamSet
;;; Parameters for size comparison problems
(define size-comparison-params
  (make-param-set
   (list (range 'r1 1.0 4.0)       ; radius of ball A
         (range 'r2 1.0 4.0)       ; radius of ball B
         (range 'x1 -10.0 -3.0)    ; x position of ball A
         (range 'x2 3.0 10.0)      ; x position of ball B
         (range 'y 5.0 15.0))      ; y position (same for both)
   ;; Constraint: balls should be visually distinct (ratio > 1.2 or < 0.8)
   (lambda (params)
     (let ([r1 (cdr (assq 'r1 params))]
           [r2 (cdr (assq 'r2 params))])
       (or (> (/ r1 r2) 1.2)
           (< (/ r1 r2) 0.8)
           ;; Allow "same" case with small probability
           (< (abs (- r1 r2)) 0.3))))))

;;; size-comparison-setup : Alist → World
;;; Create world for size comparison
(define (size-comparison-setup params)
  (let ([r1 (cdr (assq 'r1 params))]
        [r2 (cdr (assq 'r2 params))]
        [x1 (cdr (assq 'x1 params))]
        [x2 (cdr (assq 'x2 params))]
        [y (cdr (assq 'y params))])
    (setup-no-gravity-world
     (list (make-ball 'ball-a (vec2 x1 y) r1 1.0 (vec2 0 0))
           (make-ball 'ball-b (vec2 x2 y) r2 1.0 (vec2 0 0))))))

;;; size-comparison-question : Alist → String
(define (size-comparison-question params)
  "Look at the two balls shown. Which ball is larger?")

;;; size-comparison-answer : World × World × Alist → Symbol
(define (size-comparison-answer initial-world final-world params)
  (let ([r1 (cdr (assq 'r1 params))]
        [r2 (cdr (assq 'r2 params))])
    (cond
      [(> (- r1 r2) 0.3) 'A]      ; Ball A is larger
      [(> (- r2 r1) 0.3) 'B]      ; Ball B is larger
      [else 'same])))             ; Same size (within tolerance)

;;; size-comparison-problem : PhysicsProblem
(doc size-comparison-problem 'export #t)
(define size-comparison-problem
  (make-physics-problem
   'size-comparison
   'spatial
   size-comparison-params
   size-comparison-setup
   size-comparison-question
   size-comparison-answer
   spatial-difficulty))

;;; ============================================================
;;; Part 2: Distance Estimation Problems
;;; ============================================================
;;;
;;; "How far apart are the two objects?"
;;; Requires: Visual measurement and unit overlay interpretation
;;; Answer: Numeric (with unit)

(doc 'section 'distance)

;;; distance-params : ParamSet
(define distance-params
  (make-param-set
   (list (range 'x1 -15.0 -5.0)    ; position of object A
         (range 'x2 5.0 15.0)       ; position of object B
         (range 'y1 5.0 15.0)       ; y position of A
         (range 'y2 5.0 15.0)       ; y position of B
         (range 'r 1.0 2.0))        ; radius (same for both)
   ;; Constraint: minimum distance for visibility
   (lambda (params)
     (let ([x1 (cdr (assq 'x1 params))]
           [x2 (cdr (assq 'x2 params))]
           [y1 (cdr (assq 'y1 params))]
           [y2 (cdr (assq 'y2 params))])
       (> (sqrt (+ (* (- x2 x1) (- x2 x1))
                   (* (- y2 y1) (- y2 y1))))
          10.0)))))

;;; distance-setup : Alist → World
(define (distance-setup params)
  (let ([x1 (cdr (assq 'x1 params))]
        [x2 (cdr (assq 'x2 params))]
        [y1 (cdr (assq 'y1 params))]
        [y2 (cdr (assq 'y2 params))]
        [r (cdr (assq 'r params))])
    (setup-no-gravity-world
     (list (make-ball 'ball-a (vec2 x1 y1) r 1.0 (vec2 0 0))
           (make-ball 'ball-b (vec2 x2 y2) r 1.0 (vec2 0 0))))))

;;; distance-question : Alist → String
(define (distance-question params)
  "The image shows two balls. Using the scale indicator (each grid unit = 1 meter), estimate the distance between their centers.")

;;; distance-answer : World × World × Alist → Number
(define (distance-answer initial-world final-world params)
  (let* ([x1 (cdr (assq 'x1 params))]
         [x2 (cdr (assq 'x2 params))]
         [y1 (cdr (assq 'y1 params))]
         [y2 (cdr (assq 'y2 params))]
         [dx (- x2 x1)]
         [dy (- y2 y1)])
    ;; Round to nearest 0.5 for reasonable precision
    (* 0.5 (round (* 2 (sqrt (+ (* dx dx) (* dy dy))))))))

;;; distance-problem : PhysicsProblem
(doc distance-problem 'export #t)
(define distance-problem
  (make-physics-problem
   'distance-estimation
   'spatial
   distance-params
   distance-setup
   distance-question
   distance-answer
   spatial-difficulty))

;;; ============================================================
;;; Part 3: Object Counting Problems
;;; ============================================================
;;;
;;; "How many balls are shown?"
;;; Requires: Visual counting
;;; Answer: Integer

(doc 'section 'counting)

;;; count-params : ParamSet
(define count-params
  (make-param-set
   (list (range-int 'n 2 6)          ; number of objects
         (range 'spread 5.0 15.0))   ; how spread out
   (lambda (_) #t)))                 ; no constraint

;;; count-setup : Alist → World
(define (count-setup params)
  (let* ([n (cdr (assq 'n params))]
         [spread (cdr (assq 'spread params))]
         ;; Create n balls in a rough grid pattern
         [entities (make-count-entities n spread)])
    (setup-no-gravity-world entities)))

;;; make-count-entities : Nat × Number → (List Entity)
;;; Create n entities spread out for counting
(define (make-count-entities n spread)
  (let loop ([i 0] [entities '()])
    (if (>= i n)
        entities
        (let* ([row (quotient i 3)]
               [col (modulo i 3)]
               [x (+ -10 (* col spread))]
               [y (+ 5 (* row spread))]
               [id (string->symbol (string-append "ball-" (number->string i)))])
          (loop (+ i 1)
                (cons (make-ball id (vec2 x y) 1.5 1.0 (vec2 0 0))
                      entities))))))

;;; count-question : Alist → String
(define (count-question params)
  "Count the number of balls shown in the image.")

;;; count-answer : World × World × Alist → Integer
(define (count-answer initial-world final-world params)
  (cdr (assq 'n params)))

;;; count-problem : PhysicsProblem
(doc count-problem 'export #t)
(define count-problem
  (make-physics-problem
   'object-counting
   'count
   count-params
   count-setup
   count-question
   count-answer
   spatial-difficulty))

;;; ============================================================
;;; Part 4: Position Comparison Problems
;;; ============================================================
;;;
;;; "Which ball is further to the right/left/higher/lower?"
;;; Requires: Visual position comparison
;;; Answer: A or B

(doc 'section 'position)

;;; position-params : ParamSet
(define position-params
  (make-param-set
   (list (range 'x1 -15.0 -2.0)   ; Ball A always on left
         (range 'x2 2.0 15.0)     ; Ball B always on right
         (range 'y1 2.0 18.0)
         (range 'y2 2.0 18.0)
         (range 'r 1.5 2.5))
   ;; Constraint: positions should be sufficiently distinct
   (lambda (params)
     (let ([x1 (cdr (assq 'x1 params))]
           [x2 (cdr (assq 'x2 params))])
       (> (- x2 x1) 5.0)))))

;;; position-setup : Alist → World
(define (position-setup params)
  (let ([x1 (cdr (assq 'x1 params))]
        [x2 (cdr (assq 'x2 params))]
        [y1 (cdr (assq 'y1 params))]
        [y2 (cdr (assq 'y2 params))]
        [r (cdr (assq 'r params))])
    (setup-no-gravity-world
     (list (make-ball 'ball-a (vec2 x1 y1) r 1.0 (vec2 0 0))
           (make-ball 'ball-b (vec2 x2 y2) r 1.0 (vec2 0 0))))))

;;; position-question : Alist → String
(define (position-question params)
  "Which ball is further to the right? Ball A is on the left side of the frame, Ball B is on the right.")

;;; position-answer : World × World × Alist → Symbol
(define (position-answer initial-world final-world params)
  (let ([x1 (cdr (assq 'x1 params))]
        [x2 (cdr (assq 'x2 params))])
    (if (> x2 x1) 'B 'A)))

;;; position-problem : PhysicsProblem
(doc position-problem 'export #t)
(define position-problem
  (make-physics-problem
   'position-comparison
   'spatial
   position-params
   position-setup
   position-question
   position-answer
   spatial-difficulty))

;;; ============================================================
;;; Part 5: Convenience Exports
;;; ============================================================

(doc 'section 'exports)

;;; all-spatial-problems : (List PhysicsProblem)
(doc all-spatial-problems 'export #t)
(define all-spatial-problems
  (list size-comparison-problem
        distance-problem
        count-problem
        position-problem))

;;; generate-spatial-sample : Symbol × RNG × ProblemConfig → Sample
;;; Generate a sample from a named spatial problem
(define (generate-spatial-sample problem-type rng config)
  (doc 'export #t)
  (let ([problem (case problem-type
                   [(size-comparison size) size-comparison-problem]
                   [(distance distance-estimation) distance-problem]
                   [(count counting object-counting) count-problem]
                   [(position position-comparison) position-problem]
                   [else (error 'generate-spatial-sample "Unknown problem type" problem-type)])])
    (generate-problem-instance problem rng config)))
