;; Tests for metaheuristic optimization
;; Run: scheme --script lattice/optimization/test-metaheuristic.ss

(load "core/lang/module.ss")
(load "core/testing/test-framework.ss")
(load "lattice/optimization/metaheuristic.ss")

;;; ============================================================================
;;; Test Functions
;;; ============================================================================

;; Sphere: f(x) = sum(x_i^2), minimum at origin = 0
(define (sphere x)
  (fold-left + 0 (map (lambda (xi) (* xi xi)) x)))

;; Rosenbrock: f(x,y) = (1-x)^2 + 100*(y-x^2)^2, minimum at (1,1) = 0
(define (rosenbrock args)
  (let ([x (car args)]
        [y (cadr args)])
       (+ (* (- 1 x) (- 1 x))
          (* 100 (- y (* x x)) (- y (* x x))))))

;; Rastrigin: multimodal function, minimum at origin = 0
(define (rastrigin x)
  (let ([n (length x)]
        [pi 3.141592653589793])
       (+ (* 10 n)
          (fold-left + 0
                     (map (lambda (xi)
                                  (- (* xi xi) (* 10 (cos (* 2 pi xi)))))
                          x)))))

;; Ackley: multimodal, minimum at origin ≈ 0
(define (ackley args)
  (let* ([x (car args)]
         [y (cadr args)]
         [pi 3.141592653589793]
         [e 2.718281828459045]
         [a 20]
         [b 0.2]
         [c (* 2 pi)]
         [sum-sq (+ (* x x) (* y y))]
         [sum-cos (+ (cos (* c x)) (cos (* c y)))])
        (+ (- a)
           (- e)
           (* a (exp (* (- b) (sqrt (/ sum-sq 2)))))
           (exp (/ sum-cos 2)))))

;;; ============================================================================
;;; Utility Tests
;;; ============================================================================

(test-group "utilities"
  (define-test "point-add works"
    (assert-equal '(3 5 7) (point-add '(1 2 3) '(2 3 4))))

  (define-test "point-sub works"
    (assert-equal '(-1 -1 -1) (point-sub '(1 2 3) '(2 3 4))))

  (define-test "point-scale works"
    (assert-equal '(2 4 6) (point-scale 2 '(1 2 3))))

  (define-test "clip-to-bounds clips correctly"
    (assert-equal '(0 2 3) (clip-to-bounds '(-1 2 5) '(0 0 0) '(3 3 3))))

  (define-test "random-point-in-box generates valid points"
    (let* ([rng (make-pcg 42 1)]
           [lower '(-5 -5)]
           [upper '(5 5)]
           [result (random-point-in-box rng lower upper)]
           [point (cdr result)])
          (assert-true (and (>= (car point) -5) (<= (car point) 5)))
          (assert-true (and (>= (cadr point) -5) (<= (cadr point) 5))))))

;;; ============================================================================
;;; Simulated Annealing Tests
;;; ============================================================================

(test-group "simulated-annealing"
  (define-test "exponential cooling decreases temperature"
    (let ([cooling (sa-exponential-cooling 100 0.95)])
         (assert-true (> (cooling 0) (cooling 10)))
         (assert-true (> (cooling 10) (cooling 100)))))

  (define-test "linear cooling reaches minimum"
    (let ([cooling (sa-linear-cooling 100 1 100)])
         (assert-equal 100 (cooling 0))
         (assert-equal 1 (cooling 100))))

  (define-test "SA finds sphere minimum"
    (let* ([rng (make-pcg 42 1)]
           [lower '(-5 -5)]
           [upper '(5 5)]
           [x0 '(3 3)]
           [cooling (sa-exponential-cooling 10 0.98)]
           [result (simulated-annealing sphere x0 lower upper cooling 1000 rng)]
           [sol (cdr result)]
           [x (meta-x sol)]
           [f (meta-f sol)])
          ;; Should get close to (0,0) with f ≈ 0
          (assert-true (< f 0.1))))

  (define-test "sa-minimize works with defaults"
    (let* ([rng (make-pcg 123 1)]
           [lower '(-5 -5)]
           [upper '(5 5)]
           [x0 '(2 2)]
           [result (sa-minimize sphere x0 lower upper rng)]
           [sol (cdr result)])
          (assert-true (< (meta-f sol) 0.5))))

  (define-test "SA handles zero temperature (greedy mode)"
    ;; With temp=0, SA should only accept improvements
    (let* ([rng (make-pcg 42 1)]
           [lower '(-5 -5)]
           [upper '(5 5)]
           [x0 '(1 1)]
           [zero-cooling (lambda (k) 0)]  ; Always zero temperature
           [result (simulated-annealing sphere x0 lower upper zero-cooling 100 rng)]
           [sol (cdr result)])
          ;; Should complete without error and find something reasonable
          (assert-true (meta-result? sol)))))

;;; ============================================================================
;;; Genetic Algorithm Tests
;;; ============================================================================

(test-group "genetic-algorithm"
  (define-test "tournament selection picks from population"
    (let* ([rng (make-pcg 42 1)]
           [pop (vector '(1 1) '(2 2) '(3 3) '(4 4))]
           [fit (vector 10 20 30 40)]
           [result (ga-tournament-select rng pop fit 2)]
           [selected (cdr result)])
          ;; member returns the tail of the list if found, so check for non-#f
          (assert-true (if (member selected (vector->list pop)) #t #f))))

  (define-test "SBX crossover produces valid children"
    (let* ([rng (make-pcg 42 1)]
           [p1 '(0 0)]
           [p2 '(10 10)]
           [lower '(-20 -20)]
           [upper '(20 20)]
           [result (ga-sbx-crossover rng p1 p2 2 lower upper)]
           [c1 (cadr result)]
           [c2 (caddr result)])
          ;; Children should be somewhere in bounds
          (assert-true (and (>= (car c1) -20) (<= (car c1) 20)))
          (assert-true (and (>= (car c2) -20) (<= (car c2) 20)))))

  (define-test "polynomial mutation stays in bounds"
    (let* ([rng (make-pcg 42 1)]
           [x '(5 5)]
           [lower '(0 0)]
           [upper '(10 10)]
           [result (ga-polynomial-mutate rng x 20 1.0 lower upper)]  ; 100% mutation
           [mutated (cdr result)])
          (assert-true (and (>= (car mutated) 0) (<= (car mutated) 10)))
          (assert-true (and (>= (cadr mutated) 0) (<= (cadr mutated) 10)))))

  (define-test "GA finds sphere minimum"
    (let* ([rng (make-pcg 42 1)]
           [lower '(-5 -5)]
           [upper '(5 5)]
           [result (genetic-algorithm sphere lower upper 20 50 0.9 0.1 rng)]
           [sol (cdr result)])
          (assert-true (< (meta-f sol) 1.0)))))

;;; ============================================================================
;;; Particle Swarm Tests
;;; ============================================================================

(test-group "particle-swarm"
  (define-test "particle structure works"
    (let ([p (make-particle '(1 2) '(0.1 0.2) '(0 0) 0.5)])
         (assert-true (particle? p))
         (assert-equal '(1 2) (particle-x p))
         (assert-equal '(0.1 0.2) (particle-v p))
         (assert-equal '(0 0) (particle-pbest p))
         (assert-equal 0.5 (particle-pbest-f p))))

  (define-test "PSO finds sphere minimum"
    (let* ([rng (make-pcg 42 1)]
           [lower '(-5 -5)]
           [upper '(5 5)]
           [result (particle-swarm sphere lower upper 20 100 0.729 1.49 1.49 rng)]
           [sol (cdr result)])
          (assert-true (< (meta-f sol) 0.5))))

  (define-test "pso-minimize works with defaults"
    (let* ([rng (make-pcg 456 1)]
           [lower '(-5 -5)]
           [upper '(5 5)]
           [result (pso-minimize sphere lower upper rng)]
           [sol (cdr result)])
          (assert-true (< (meta-f sol) 0.1)))))

;;; ============================================================================
;;; Differential Evolution Tests
;;; ============================================================================

(test-group "differential-evolution"
  (define-test "DE finds sphere minimum"
    (let* ([rng (make-pcg 42 1)]
           [lower '(-5 -5)]
           [upper '(5 5)]
           [result (differential-evolution sphere lower upper 20 50 0.8 0.9 rng)]
           [sol (cdr result)])
          (assert-true (< (meta-f sol) 0.5))))

  (define-test "de-minimize works with defaults"
    (let* ([rng (make-pcg 789 1)]
           [lower '(-5 -5)]
           [upper '(5 5)]
           [result (de-minimize sphere lower upper rng)]
           [sol (cdr result)])
          (assert-true (< (meta-f sol) 0.01))))

  (define-test "DE rejects pop-size < 4"
    ;; DE mutation requires 4 distinct indices
    (assert-error
      (lambda ()
        (differential-evolution sphere '(-5 -5) '(5 5) 3 10 0.8 0.9 (make-pcg 42 1))))))

;;; ============================================================================
;;; Multi-objective Tests
;;; ============================================================================

(test-group "multi-objective"
  (define-test "pareto-dominates identifies dominance"
    (assert-true (pareto-dominates? '(1 1) '(2 2)))   ; strictly better
    (assert-true (pareto-dominates? '(1 2) '(2 2)))   ; better in one, equal in other
    (assert-false (pareto-dominates? '(1 2) '(2 1))) ; trade-off
    (assert-false (pareto-dominates? '(2 2) '(1 1)))) ; worse

  (define-test "pareto-nondominated filters correctly"
    (let* ([pop '((1) (2) (3) (4))]
           [obj '((1 4) (2 3) (3 2) (4 1))]  ; all on Pareto front (trade-offs)
           [front (pareto-nondominated pop obj)])
          ;; All should be non-dominated
          (assert-equal 4 (length front))))

  (define-test "pareto-nondominated removes dominated"
    (let* ([pop '((1) (2) (3))]
           [obj '((1 1) (2 2) (3 3))]  ; (1,1) dominates others
           [front (pareto-nondominated pop obj)])
          (assert-equal 1 (length front))
          (assert-equal '(1) (car (car front)))))

  (define-test "nsga2-crowding-distance computes distances"
    ;; Front format: list of (solution objectives)
    ;; Three points on a 2D Pareto front
    (let* ([front '(((1 0) (0 4))    ; solution (1,0), objectives (0, 4)
                    ((2 1) (2 2))    ; solution (2,1), objectives (2, 2) - middle
                    ((3 2) (4 0)))]  ; solution (3,2), objectives (4, 0)
           [dists (nsga2-crowding-distance front)])
          ;; Boundary points should have infinite distance
          (assert-equal +inf.0 (car dists))
          (assert-equal +inf.0 (caddr dists))
          ;; Middle point should have finite positive distance
          (assert-true (> (cadr dists) 0))
          (assert-true (< (cadr dists) +inf.0)))))

;;; ============================================================================
;;; Unified Interface Tests
;;; ============================================================================

(test-group "unified-interface"
  (define-test "global-minimize works with SA"
    (let* ([rng (make-pcg 42 1)]
           [lower '(-5 -5)]
           [upper '(5 5)]
           [result (global-minimize sphere lower upper rng 'sa)]
           [sol (cdr result)])
          (assert-true (< (meta-f sol) 1.0))))

  (define-test "global-minimize works with GA"
    (let* ([rng (make-pcg 42 1)]
           [lower '(-5 -5)]
           [upper '(5 5)]
           [result (global-minimize sphere lower upper rng 'ga)]
           [sol (cdr result)])
          (assert-true (< (meta-f sol) 1.0))))

  (define-test "global-minimize works with PSO"
    (let* ([rng (make-pcg 42 1)]
           [lower '(-5 -5)]
           [upper '(5 5)]
           [result (global-minimize sphere lower upper rng 'pso)]
           [sol (cdr result)])
          (assert-true (< (meta-f sol) 0.5))))

  (define-test "global-minimize works with DE (default)"
    (let* ([rng (make-pcg 42 1)]
           [lower '(-5 -5)]
           [upper '(5 5)]
           [result (global-minimize sphere lower upper rng)]
           [sol (cdr result)])
          (assert-true (< (meta-f sol) 0.5)))))

;;; ============================================================================
;;; Challenging Test Functions
;;; ============================================================================

(test-group "challenging-functions"
  (define-test "DE handles Rosenbrock"
    (let* ([rng (make-pcg 42 1)]
           [lower '(-5 -5)]
           [upper '(5 5)]
           [result (de-minimize rosenbrock lower upper rng)]
           [sol (cdr result)]
           [x (meta-x sol)])
          ;; Should get reasonably close to (1,1)
          (assert-true (< (meta-f sol) 10))))

  (define-test "PSO handles multimodal Rastrigin"
    (let* ([rng (make-pcg 42 1)]
           [lower '(-5.12 -5.12)]
           [upper '(5.12 5.12)]
           [result (pso-minimize rastrigin lower upper rng)]
           [sol (cdr result)])
          ;; Rastrigin is tricky; just verify we get something reasonable
          (assert-true (< (meta-f sol) 5)))))

;;; ============================================================================
;;; Result Type Tests
;;; ============================================================================

(test-group "result-type"
  (define-test "meta-result structure works"
    (let ([r (make-meta-result '(1 2) 0.5 100 500 'max-iter)])
         (assert-true (meta-result? r))
         (assert-equal '(1 2) (meta-x r))
         (assert-equal 0.5 (meta-f r))
         (assert-equal 100 (meta-iterations r))
         (assert-equal 500 (meta-evaluations r))
         (assert-equal 'max-iter (meta-reason r))))

  (define-test "non-result fails predicate"
    (assert-false (meta-result? '(1 2 3)))
    (assert-false (meta-result? 'foo))
    (assert-false (meta-result? #f))))

(run-all-tests)
