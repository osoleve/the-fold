;;; user/creations/interval-search-visualizer.ss
;;;
;;; ASCII visualization of interval branch-and-bound optimization.
;;; Shows how the algorithm prunes the search space to find global minima.
;;;
;;; Run: scheme --script user/creations/interval-search-visualizer.ss
;;;
;;; Author: Claude Opus 4.5
;;; Created: 2026-01-16

(load "core/base/prelude.ss")
(load "lattice/interval/interval.ss")
(load "lattice/optimization/interval-global.ss")
(load "lattice/optimization/interval-contract.ss")

;;; ============================================================================
;;; ASCII Grid Renderer
;;; ============================================================================

(define *grid-width* 60)
(define *grid-height* 30)

;; Character palette for function values
(define (value->char val min-val max-val)
  (if (or (>= val +inf.0) (<= val -inf.0))
      #\#
      (let* ([range (- max-val min-val)]
             [normalized (if (= range 0) 0.5 (/ (- val min-val) range))]
             [idx (min 9 (max 0 (exact (floor (* normalized 10)))))]
             [chars "░▒▓█████■●"])
        (string-ref chars idx))))

;; Render a 2D function as ASCII art
(define (render-function f x-lo x-hi y-lo y-hi)
  (let* ([dx (/ (- x-hi x-lo) *grid-width*)]
         [dy (/ (- y-hi y-lo) *grid-height*)]
         ;; First pass: find min/max values
         [values (make-vector (* *grid-width* *grid-height*) 0)]
         [min-val +inf.0]
         [max-val -inf.0])

    ;; Evaluate function at each point
    (do ([j 0 (+ j 1)])
        ((= j *grid-height*))
      (let ([y (+ y-lo (* (- *grid-height* 1 j) dy))])
        (do ([i 0 (+ i 1)])
            ((= i *grid-width*))
          (let* ([x (+ x-lo (* i dx))]
                 [val (f x y)])
            (vector-set! values (+ (* j *grid-width*) i) val)
            (when (< val min-val) (set! min-val val))
            (when (> val max-val) (set! max-val val))))))

    ;; Render
    (display "┌")
    (do ([i 0 (+ i 1)]) ((= i *grid-width*)) (display "─"))
    (display "┐\n")

    (do ([j 0 (+ j 1)])
        ((= j *grid-height*))
      (display "│")
      (do ([i 0 (+ i 1)])
          ((= i *grid-width*))
        (let ([val (vector-ref values (+ (* j *grid-width*) i))])
          (display (value->char val min-val max-val))))
      (display "│")
      ;; Y-axis label at top and bottom
      (cond
        [(= j 0) (display (format " ~a" y-hi))]
        [(= j (- *grid-height* 1)) (display (format " ~a" y-lo))])
      (display "\n"))

    (display "└")
    (do ([i 0 (+ i 1)]) ((= i *grid-width*)) (display "─"))
    (display "┘\n")
    (display (format " ~a" x-lo))
    (display (make-string (- *grid-width* 8) #\space))
    (display (format "~a\n" x-hi))))

;; Render search space with candidate boxes highlighted
(define (render-with-boxes f x-lo x-hi y-lo y-hi boxes)
  (let* ([dx (/ (- x-hi x-lo) *grid-width*)]
         [dy (/ (- y-hi y-lo) *grid-height*)]
         [values (make-vector (* *grid-width* *grid-height*) 0)]
         [is-box (make-vector (* *grid-width* *grid-height*) #f)]
         [min-val +inf.0]
         [max-val -inf.0])

    ;; Evaluate and mark boxes
    (do ([j 0 (+ j 1)])
        ((= j *grid-height*))
      (let ([y (+ y-lo (* (- *grid-height* 1 j) dy))])
        (do ([i 0 (+ i 1)])
            ((= i *grid-width*))
          (let* ([x (+ x-lo (* i dx))]
                 [val (f x y)]
                 [in-box? (any (lambda (box)
                                 (and (interval-contains? (car box) x)
                                      (interval-contains? (cadr box) y)))
                               boxes)])
            (vector-set! values (+ (* j *grid-width*) i) val)
            (vector-set! is-box (+ (* j *grid-width*) i) in-box?)
            (when (< val min-val) (set! min-val val))
            (when (> val max-val) (set! max-val val))))))

    ;; Render
    (display "┌")
    (do ([i 0 (+ i 1)]) ((= i *grid-width*)) (display "─"))
    (display "┐\n")

    (do ([j 0 (+ j 1)])
        ((= j *grid-height*))
      (display "│")
      (do ([i 0 (+ i 1)])
          ((= i *grid-width*))
        (let ([idx (+ (* j *grid-width*) i)])
          (if (vector-ref is-box idx)
              (display "●")  ; Candidate box
              (display (value->char (vector-ref values idx) min-val max-val)))))
      (display "│\n"))

    (display "└")
    (do ([i 0 (+ i 1)]) ((= i *grid-width*)) (display "─"))
    (display "┘\n")))

;; Helper
(define (any pred lst)
  (cond
    [(null? lst) #f]
    [(pred (car lst)) #t]
    [else (any pred (cdr lst))]))

;;; ============================================================================
;;; Visualization Demos
;;; ============================================================================

(define pi 3.141592653589793)

;; Simple sphere for Demo 1
(define (sphere-fn x y) (+ (* x x) (* y y)))
(define (interval-sphere-fn box)
  (interval-add (interval-sqr (car box)) (interval-sqr (cadr box))))

(define (six-hump-camel x y)
  ;; Six-Hump Camel function: multiple local minima
  ;; Global minima at (±0.0898, ∓0.7126) with value ≈ -1.0316
  (+ (- (* 4 x x) (* 2.1 (expt x 4)) (* (/ 1 3) (expt x 6)))
     (* x y)
     (+ (- (* y y)) (* 4 (expt y 4)))))

;; Simplified Ackley function (easier interval extension)
;; Has many local minima with global minimum at origin = 0
(define (ackley x y)
  (let* ([a 20] [b 0.2] [c (* 2 pi)])
    (+ (- (* a (exp (* (- b) (sqrt (* 0.5 (+ (* x x) (* y y))))))))
       (- (exp (* 0.5 (+ (cos (* c x)) (cos (* c y))))))
       a
       (exp 1))))

(define (interval-ackley box)
  ;; Conservative but correct: exp(-b*sqrt(...)) ∈ (0, 1] for positive input
  ;; cos(c*x) ∈ [-1, 1], so exp(0.5*(cos + cos)) ∈ [exp(-1), exp(1)]
  ;; This gives a valid enclosure
  (let* ([x (car box)]
         [y (cadr box)]
         [x2 (interval-sqr x)]
         [y2 (interval-sqr y)]
         [sum-sq (interval-add x2 y2)]
         ;; sqrt(0.5 * sum) - need to handle carefully
         [half-sum (interval-scale sum-sq 0.5)]
         [sqrt-term (interval-sqrt half-sum)]
         ;; exp(-0.2 * sqrt) - decreasing function, so swap bounds
         [neg-b-sqrt (if (eq? sqrt-term 'domain-error)
                         (interval 0 0)
                         (interval-scale sqrt-term -0.2))]
         [exp-term1 (interval-exp neg-b-sqrt)]
         ;; -20 * exp(...) - scale by -20 (negates, so swap bounds again)
         [term1 (interval-scale exp-term1 -20)]
         ;; cos terms bounded by [-1, 1] each, sum by [-2, 2]
         ;; exp(0.5 * [-2, 2]) = exp([-1, 1]) = [1/e, e]
         [exp-cos (interval (exp -1) (exp 1))]
         [term2 (interval-neg exp-cos)]
         ;; Constants
         [const (interval-singleton (+ 20 (exp 1)))])
    (interval-add (interval-add term1 term2) const)))

(define (himmelblau x y)
  ;; Himmelblau function: four identical minima
  ;; Minima at (3,2), (-2.805,3.131), (-3.779,-3.283), (3.584,-1.848)
  ;; All with value 0
  (+ (* (+ (* x x) y -11) (+ (* x x) y -11))
     (* (+ x (* y y) -7) (+ x (* y y) -7))))

(define (interval-himmelblau box)
  (let* ([x (car box)]
         [y (cadr box)]
         [x2 (interval-sqr x)]
         [y2 (interval-sqr y)]
         [eleven (interval-singleton 11)]
         [seven (interval-singleton 7)]
         [term1 (interval-sub (interval-add x2 y) eleven)]
         [term2 (interval-sub (interval-add x y2) seven)])
    (interval-add (interval-sqr term1) (interval-sqr term2))))

(define (run-visual-demo)
  (display "\n")
  (display "╔═══════════════════════════════════════════════════════════════════╗\n")
  (display "║          INTERVAL OPTIMIZATION VISUALIZER                          ║\n")
  (display "╚═══════════════════════════════════════════════════════════════════╝\n\n")

  ;; Demo 1: Sphere function (simple, works perfectly)
  (display "═══════════════════════════════════════════════════════════════════\n")
  (display "SPHERE FUNCTION: BASIC GLOBAL MINIMUM\n")
  (display "═══════════════════════════════════════════════════════════════════\n\n")
  (display "f(x,y) = x² + y² has a unique global minimum at (0,0) = 0.\n")
  (display "This demonstrates basic interval B&B convergence.\n\n")

  (display "Function landscape (darker = lower value):\n\n")
  (render-function sphere-fn -3 3 -3 3)

  (display "\nRunning interval branch-and-bound...\n\n")

  (let* ([box (list (interval -3 3) (interval -3 3))]
         [criteria (make-interval-convergence 1e-2 2000)]
         [result (interval-minimize interval-sphere-fn box criteria
                                    (lambda (pt) (sphere-fn (car pt) (cadr pt))))])

    (display (format "Found ~a candidate region(s).\n" (length (ior-candidates result))))
    (display (format "Best value: ~a (expected: 0)\n" (ior-best-upper result)))
    (display (format "Best point: ~a (expected: (0, 0))\n" (ior-best-point result)))
    (display (format "Iterations: ~a\n\n" (ior-iterations result)))

    (display "Minimum location (● = candidate region):\n\n")
    (render-with-boxes sphere-fn -3 3 -3 3 (ior-candidates result)))

  (display "\n")

  ;; Demo 2: Himmelblau
  (display "═══════════════════════════════════════════════════════════════════\n")
  (display "HIMMELBLAU'S FUNCTION\n")
  (display "═══════════════════════════════════════════════════════════════════\n\n")
  (display "Himmelblau has FOUR identical global minima (value = 0) at:\n")
  (display "  (3, 2), (-2.805, 3.131), (-3.779, -3.283), (3.584, -1.848)\n\n")

  (display "Function landscape:\n\n")
  (render-function himmelblau -5 5 -5 5)

  (display "\nRunning interval branch-and-bound...\n\n")

  (let* ([box (list (interval -5 5) (interval -5 5))]
         [criteria (make-interval-convergence 1e-2 5000)]
         [result (interval-minimize interval-himmelblau box criteria
                                    (lambda (pt) (himmelblau (car pt) (cadr pt))))])

    (display (format "Found ~a candidate region(s).\n" (length (ior-candidates result))))
    (display (format "Best value: ~a (expected: 0)\n" (ior-best-upper result)))
    (display (format "Iterations: ~a\n\n" (ior-iterations result)))

    (display "All four minima locations (● = candidate regions):\n\n")
    (render-with-boxes himmelblau -5 5 -5 5 (ior-candidates result)))

  (display "\n")

  ;; Demo 3: Constrained visualization
  (display "═══════════════════════════════════════════════════════════════════\n")
  (display "CONSTRAINED OPTIMIZATION: VISIBLE PRUNING\n")
  (display "═══════════════════════════════════════════════════════════════════\n\n")

  (display "Minimize x² + y² subject to:\n")
  (display "  - (x-2)² + (y-2)² <= 2  (disk centered at (2,2))\n")
  (display "  - y >= 1               (above the floor)\n\n")

  (let* ([objective (lambda (x y) (+ (* x x) (* y y)))]
         [interval-obj (lambda (box)
                         (interval-add (interval-sqr (car box))
                                       (interval-sqr (cadr box))))]
         [box (list (interval -1 5) (interval -1 5))]
         [contractors (list
                       (make-sphere-contractor '(2 2) 2)
                       (make-bound-contractor 1 1 5))]
         [criteria (make-interval-convergence 1e-3 5000)]
         [result (interval-minimize-constrained interval-obj box criteria contractors)])

    (display "Unconstrained objective (x² + y²):\n\n")
    (render-function objective -1 5 -1 5)

    (display "\nConstrained solution (● = feasible minimum region):\n\n")
    (render-with-boxes objective -1 5 -1 5 (ior-candidates result))

    (display (format "\nBest point: ~a\n" (ior-best-point result)))
    (display (format "Best value: ~a\n" (ior-best-upper result)))
    (display (format "Iterations: ~a\n" (ior-iterations result))))

  (display "\n")
  (display "═══════════════════════════════════════════════════════════════════\n")
  (display "LEGEND\n")
  (display "═══════════════════════════════════════════════════════════════════\n")
  (display "  ░▒▓█■● - Increasing function value (lighter = lower)\n")
  (display "  ●      - Candidate region containing global minimum\n")
  (display "\nInterval B&B provides GUARANTEED bounds on all optima.\n"))

;;; Run
(run-visual-demo)
