;;; core/dynamics/test-discrete.ss --- Tests for Discrete Dynamical Systems
;;;
;;; NOTE: Run from project root: scheme --script core/dynamics/test-discrete.ss

(load "core/test-framework.ss")
(load "core/dynamics/discrete.ss")

(display "\n")
(display "==============================================================\n")
(display "         DISCRETE DYNAMICAL SYSTEMS TESTS\n")
(display "==============================================================\n")

;;; ============================================================
;;; Construction Tests
;;; ============================================================

(test-group construction
            (define-test make-deterministic-dds
              (let ([sys (make-deterministic-dds (lambda (x) (* 2 x)) 0)])
                   (assert-true (dds? sys))
                   (assert-equal 0 (dds-dimension sys))
                   (assert-false (dds-stochastic? sys))))
            
            (define-test make-stochastic-dds
              (let ([sys (make-stochastic-dds (lambda (x rng) (cons x rng)) 0)])
                   (assert-true (dds? sys))
                   (assert-true (dds-stochastic? sys))))
            
            (define-test logistic-map-construction
              (let ([sys (logistic-map 3.5)])
                   (assert-true (dds? sys))
                   (assert-equal 0 (dds-dimension sys))
                   (assert-false (dds-stochastic? sys))))
            
            (define-test henon-map-construction
              (let ([sys (henon-map 1.4 0.3)])
                   (assert-true (dds? sys))
                   (assert-equal 2 (dds-dimension sys))
                   (assert-false (dds-stochastic? sys))))
            
            (define-test tent-map-construction
              (let ([sys (tent-map 2.0)])
                   (assert-true (dds? sys))
                   (assert-equal 0 (dds-dimension sys))))
            
            (define-test baker-map-construction
              (assert-true (dds? baker-map))
              (assert-equal 2 (dds-dimension baker-map)))
            
            (define-test shift-map-construction
              (assert-true (dds? shift-map))
              (assert-equal 0 (dds-dimension shift-map)))
            
            (define-test circle-rotation-construction
              (let ([sys (circle-rotation 0.618)])
                   (assert-true (dds? sys))
                   (assert-equal 0 (dds-dimension sys))))
            
            (define-test linear-congruence-construction
              (let ([sys (linear-congruence 1103515245 12345 (expt 2 31))])
                   (assert-true (dds? sys))
                   (assert-equal 0 (dds-dimension sys)))))

;;; ============================================================
;;; Orbit Computation Tests
;;; ============================================================

(test-group orbit-computation
            (define-test orbit-length
              (let* ([sys (make-deterministic-dds (lambda (x) (+ x 1)) 0)]
                     [orb (orbit sys 0 5)])
                    (assert-equal 6 (length orb))))  ;; Includes initial state
            
            (define-test orbit-values
              (let* ([sys (make-deterministic-dds (lambda (x) (* 2 x)) 0)]
                     [orb (orbit sys 1 4)])
                    (assert-equal '(1 2 4 8 16) orb)))
            
            (define-test orbit-logistic
              (let* ([sys (logistic-map 2.0)]
                     [orb (orbit sys 0.5 3)])
                    ;; x_0 = 0.5
                    ;; x_1 = 2 * 0.5 * 0.5 = 0.5
                    ;; Fixed point!
                    (assert-equal 4 (length orb))
                    (assert-true (< (abs (- (car orb) 0.5)) 1e-10))
                    (assert-true (< (abs (- (cadr orb) 0.5)) 1e-10))))
            
            (define-test orbit-henon
              (let* ([sys (henon-map 1.4 0.3)]
                     [initial (vector 0.0 0.0)]
                     [orb (orbit sys initial 3)])
                    (assert-equal 4 (length orb))
                    ;; Check first iteration: (1 - 1.4*0 + 0, 0.3*0) = (1, 0)
                    (assert-true (< (abs (- (vector-ref (cadr orb) 0) 1.0)) 1e-10))
                    (assert-true (< (abs (- (vector-ref (cadr orb) 1) 0.0)) 1e-10))))
            
            (define-test iterate-simple
              (let* ([sys (make-deterministic-dds (lambda (x) (+ x 1)) 0)]
                     [result (iterate sys 0 10)])
                    (assert-equal 10 result)))
            
            (define-test iterate-doubling
              (let* ([sys (make-deterministic-dds (lambda (x) (* 2 x)) 0)]
                     [result (iterate sys 1 5)])
                    (assert-equal 32 result)))
            
            (define-test iterate-until-threshold
              (let* ([sys (make-deterministic-dds (lambda (x) (+ x 1)) 0)]
                     [result (iterate-until sys 0 (lambda (x) (> x 5)) 100)])
                    (assert-equal 6 (car result))
                    (assert-equal 6 (cdr result))))
            
            (define-test iterate-until-fuel-exhausted
              (let* ([sys (make-deterministic-dds (lambda (x) (+ x 1)) 0)]
                     [result (iterate-until sys 0 (lambda (x) (> x 1000)) 10)])
                    (assert-equal 10 (car result))
                    (assert-equal 10 (cdr result)))))

;;; ============================================================
;;; Fixed Point Tests
;;; ============================================================

(test-group fixed-points
            (define-test fixed-point-check-true
              ;; Logistic map at r=2 has fixed point at x=0.5
              (let ([sys (logistic-map 2.0)])
                   (assert-true (fixed-point? sys 0.5 1e-10))))
            
            (define-test fixed-point-check-false
              (let ([sys (logistic-map 3.5)])
                   (assert-false (fixed-point? sys 0.5 1e-10))))
            
            (define-test find-fixed-point-success
              ;; x^2 has fixed points at 0 and 1
              (let* ([sys (make-deterministic-dds (lambda (x) (* x x)) 0)]
                     [result (find-fixed-point sys 0.1 100 1e-10)])
                    (assert-true (ok? result))
                    (assert-true (< (abs (unwrap-ok result)) 1e-8))))
            
            (define-test find-fixed-point-logistic
              ;; r=2 has fixed point at (r-1)/r = 0.5, which is stable
              (let* ([sys (logistic-map 2.0)]
                     [result (find-fixed-point sys 0.3 200 1e-8)])
                    (assert-true (ok? result))
                    (let ([fp (unwrap-ok result)])
                         (assert-true (< (abs (- fp 0.5)) 1e-6)))))
            
            (define-test classify-stable-fixed-point
              ;; f(x) = 0.5x has stable fixed point at 0
              (let* ([sys (make-deterministic-dds (lambda (x) (* 0.5 x)) 0)]
                     [class (classify-fixed-point-1d sys 0.0 1e-6)])
                    (assert-equal 'stable class)))
            
            (define-test classify-unstable-fixed-point
              ;; f(x) = 2x has unstable fixed point at 0
              (let* ([sys (make-deterministic-dds (lambda (x) (* 2 x)) 0)]
                     [class (classify-fixed-point-1d sys 0.0 1e-6)])
                    (assert-equal 'unstable class)))
            
            (define-test classify-neutral-fixed-point
              ;; f(x) = -x has derivative -1, which is neutral
              ;; (identity has derivative 1 which is also neutral)
              (let* ([sys (make-deterministic-dds (lambda (x) (- x)) 0)]
                     [class (classify-fixed-point-1d sys 0.0 1e-6)])
                    (assert-equal 'neutral class)))
            
            (define-test fixed-point-vector
              ;; Henon map at origin is not a fixed point for typical parameters
              (let ([sys (henon-map 1.4 0.3)])
                   (assert-false (fixed-point? sys (vector 0.0 0.0) 1e-10)))))

;;; ============================================================
;;; Periodic Orbit Tests
;;; ============================================================

(test-group periodic-orbits
            (define-test period-fixed-point
              ;; Fixed point has period 1
              (let ([sys (logistic-map 2.0)])
                   (assert-equal 1 (period sys 0.5 100 1e-10))))
            
            (define-test period-2-cycle
              ;; Logistic map at r=3.2 has period-2 cycle
              (let ([sys (logistic-map 3.2)])
                   ;; Start near attractor
                   (let ([settled (iterate sys 0.5 100)])
                        (assert-equal 2 (period sys settled 100 1e-6)))))
            
            (define-test periodic-orbit-extraction
              ;; Period-2 orbit
              (let* ([sys (logistic-map 3.2)]
                     [orb (periodic-orbit sys 0.5 100 1e-6)])
                    (assert-true (pair? orb))
                    (assert-equal 2 (length orb))))
            
            (define-test period-none-chaotic
              ;; Chaotic logistic map has no short period
              (let ([sys (logistic-map 4.0)])
                   (assert-false (period sys 0.1234 50 1e-8))))
            
            (define-test period-linear-congruence
              ;; Verify that linear congruence produces periodic behavior
              ;; x -> (3x + 1) mod 4: 0 -> 1 -> 0 (period 2)
              (let* ([sys (linear-congruence 3 1 4)]
                     [orb (orbit sys 0 10)])
                    ;; Verify the orbit has the expected pattern
                    (assert-equal 0 (car orb))        ;; starts at 0
                    (assert-equal 1 (cadr orb))       ;; 0 -> 1
                    (assert-equal 0 (caddr orb)))))   ;; 1 -> 0

;;; ============================================================
;;; Cobweb Diagram Tests
;;; ============================================================

(test-group cobweb
            (define-test cobweb-points-length
              (let* ([sys (logistic-map 3.5)]
                     [pts (cobweb-points sys 0.5 5)])
                    ;; Initial + 2 points per iteration (vertical + horizontal)
                    (assert-equal 11 (length pts))))
            
            (define-test cobweb-points-structure
              (let* ([sys (make-deterministic-dds (lambda (x) (* 2 x)) 0)]
                     [pts (cobweb-points sys 0.25 1)])
                    ;; pts should be: ((0.25 . 0.25) (0.25 . 0.5) (0.5 . 0.5))
                    (assert-equal 3 (length pts))
                    (assert-equal 0.25 (caar pts))
                    (assert-equal 0.25 (cdar pts))))
            
            (define-test cobweb-segments-count
              (let* ([sys (logistic-map 3.5)]
                     [segs (cobweb-segments sys 0.5 5)])
                    ;; 2 segments per iteration
                    (assert-equal 10 (length segs))))
            
            (define-test cobweb-segment-structure
              (let* ([sys (make-deterministic-dds (lambda (x) (* 2 x)) 0)]
                     [segs (cobweb-segments sys 0.25 1)])
                    ;; Each segment is a list of two pairs
                    (assert-equal 2 (length segs))
                    (assert-equal 2 (length (car segs))))))

;;; ============================================================
;;; Bifurcation Diagram Tests
;;; ============================================================

(test-group bifurcation
            (define-test bifurcation-data-length
              (let* ([data (bifurcation-data logistic-map 2.5 3.5 5 100 10 0.5)])
                    ;; 5 parameter values, 10 samples each
                    (assert-equal 50 (length data))))
            
            (define-test bifurcation-data-structure
              (let* ([data (bifurcation-data logistic-map 2.5 3.5 3 50 5 0.5)]
                     [first-point (car data)])
                    ;; Each point is (parameter . state)
                    (assert-true (pair? first-point))
                    (assert-true (number? (car first-point)))
                    (assert-true (number? (cdr first-point)))))
            
            (define-test bifurcation-parameter-range
              (let* ([data (bifurcation-data logistic-map 2.0 4.0 3 50 5 0.5)]
                     [params (map car data)])
                    ;; Check parameters span the range
                    (assert-true (<= 2.0 (fold-left min +inf.0 params)))
                    (assert-true (>= 4.0 (fold-left max -inf.0 params))))))

;;; ============================================================
;;; Lyapunov Exponent Tests
;;; ============================================================

(test-group lyapunov
            (define-test lyapunov-positive-chaos
              ;; Logistic map at r=4 is chaotic, positive Lyapunov exponent
              (let* ([sys (logistic-map 4.0)]
                     [le (lyapunov-exponent-1d sys 0.1 100 1000)])
                    (assert-true (> le 0))))
            
            (define-test lyapunov-negative-stable
              ;; Logistic map at r=2 is stable, negative Lyapunov exponent
              (let* ([sys (logistic-map 2.0)]
                     [le (lyapunov-exponent-1d sys 0.4 100 1000)])
                    (assert-true (< le 0))))
            
            (define-test lyapunov-known-value
              ;; Logistic map at r=4 has Lyapunov exponent = log(2) ~ 0.693
              ;; More stable than shift map for numerical computation
              (let* ([sys (logistic-map 4.0)]
                     [le (lyapunov-exponent-1d sys 0.3 100 1000)])
                    (assert-true (< (abs (- le (log 2))) 0.1)))))

;;; ============================================================
;;; Attractor Analysis Tests
;;; ============================================================

(test-group attractor
            (define-test attractor-bounds-logistic
              (let* ([sys (logistic-map 3.5)]
                     [bounds (attractor-bounds sys 0.5 100 1000)])
                    (assert-equal 1 (length bounds))
                    ;; Logistic map stays in [0, 1]
                    (assert-true (>= (caar bounds) 0))
                    (assert-true (<= (cdar bounds) 1))))
            
            (define-test attractor-bounds-henon
              (let* ([sys (henon-map 1.4 0.3)]
                     [bounds (attractor-bounds sys (vector 0.0 0.0) 100 500)])
                    (assert-equal 2 (length bounds))
                    ;; Both dimensions should have non-trivial bounds
                    (assert-true (< (caar bounds) (cdar bounds)))
                    (assert-true (< (car (cadr bounds)) (cdr (cadr bounds)))))))

;;; ============================================================
;;; Time Series Analysis Tests
;;; ============================================================

(test-group time-series
            (define-test time-average-fixed-point
              ;; Average of fixed point is the fixed point
              (let* ([sys (logistic-map 2.0)]
                     [avg (time-average sys 0.5 10 100 identity)])
                    (assert-true (< (abs (- avg 0.5)) 1e-6))))
            
            (define-test time-average-chaotic
              ;; Logistic map at r=4 is uniformly distributed, average ~ 0.5
              (let* ([sys (logistic-map 4.0)]
                     [avg (time-average sys 0.3 200 10000 identity)])
                    (assert-true (< (abs (- avg 0.5)) 0.1))))
            
            (define-test autocorrelation-length
              (let* ([sys (logistic-map 3.5)]
                     [acf (autocorrelation sys 0.5 100 100 10 identity)])
                    (assert-equal 11 (length acf))))  ;; Lags 0 through 10
            
            (define-test autocorrelation-zero-lag
              ;; Autocorrelation at lag 0 is always 1 (or close to it)
              (let* ([sys (logistic-map 3.5)]
                     [acf (autocorrelation sys 0.5 100 500 5 identity)])
                    (assert-true (< (abs (- (car acf) 1.0)) 0.01)))))

;;; ============================================================
;;; Recurrence Analysis Tests
;;; ============================================================

(test-group recurrence
            (define-test recurrence-matrix-size
              (let* ([sys (logistic-map 3.5)]
                     [rm (recurrence-matrix sys 0.5 100 20 0.1)])
                    (assert-equal 20 (length rm))
                    (assert-equal 20 (length (car rm)))))
            
            (define-test recurrence-matrix-diagonal
              ;; Diagonal is always true (each point recurs with itself)
              (let* ([sys (logistic-map 3.5)]
                     [rm (recurrence-matrix sys 0.5 100 10 0.1)])
                    (assert-true (list-ref (car rm) 0))       ;; R[0,0]
                    (assert-true (list-ref (cadr rm) 1))      ;; R[1,1]
                    (assert-true (list-ref (list-ref rm 5) 5))))  ;; R[5,5]
            
            (define-test recurrence-rate-fixed-point
              ;; Fixed point has recurrence rate 1
              (let* ([sys (logistic-map 2.0)]
                     [rr (recurrence-rate sys 0.5 10 20 0.01)])
                    (assert-equal 1 rr)))
            
            (define-test recurrence-rate-bounded
              ;; Recurrence rate is between 0 and 1
              (let* ([sys (logistic-map 3.9)]
                     [rr (recurrence-rate sys 0.5 100 50 0.1)])
                    (assert-true (>= rr 0))
                    (assert-true (<= rr 1)))))

;;; ============================================================
;;; System Composition Tests
;;; ============================================================

(test-group composition
            (define-test compose-dds-simple
              (let* ([double (make-deterministic-dds (lambda (x) (* 2 x)) 0)]
                     [add-one (make-deterministic-dds (lambda (x) (+ x 1)) 0)]
                     [composed (compose-dds double add-one)]
                     [f (dds-transition composed)])
                    ;; f(x) = 2 * (x + 1)
                    (assert-equal 6 (f 2))))  ;; 2 * (2 + 1) = 6
            
            (define-test iterate-system-test
              (let* ([double (make-deterministic-dds (lambda (x) (* 2 x)) 0)]
                     [triple-double (iterate-system double 3)]
                     [f (dds-transition triple-double)])
                    ;; f(x) = 2^3 * x = 8x
                    (assert-equal 16 (f 2))))
            
            (define-test coupled-system-scalar
              (let* ([sys1 (make-deterministic-dds (lambda (x) (* 0.5 x)) 0)]
                     [sys2 (make-deterministic-dds (lambda (y) (* 0.5 y)) 0)]
                     [coupled (coupled-system sys1 sys2
                                              (lambda (x y) (+ x (* 0.1 y)))
                                              (lambda (x y) (+ y (* 0.1 x))))]
                     [f (dds-transition coupled)])
                    (assert-true (dds? coupled))
                    (assert-equal 2 (dds-dimension coupled))
                    ;; Test one iteration
                    (let ([result (f (vector 1.0 2.0))])
                         (assert-true (vector? result))
                         (assert-equal 2 (vector-length result))))))

;;; ============================================================
;;; Stochastic System Tests
;;; ============================================================

(test-group stochastic
            (define-test stochastic-orbit-length
              (let* ([sys (stochastic-logistic 3.5 0.01)]
                     [rng (make-pcg 42 1)]
                     [result (orbit-stochastic sys 0.5 10 rng)]
                     [orb (car result)])
                    (assert-equal 11 (length orb))))
            
            (define-test add-noise-makes-stochastic
              (let* ([det (logistic-map 3.5)]
                     [stoch (add-noise det (gaussian-noise-1d 0.01))])
                    (assert-false (dds-stochastic? det))
                    (assert-true (dds-stochastic? stoch))))
            
            (define-test stochastic-orbit-different-seeds
              ;; Different seeds should give different orbits
              (let* ([sys (stochastic-logistic 3.5 0.1)]
                     [rng1 (make-pcg 42 1)]
                     [rng2 (make-pcg 43 1)]
                     [orb1 (car (orbit-stochastic sys 0.5 10 rng1))]
                     [orb2 (car (orbit-stochastic sys 0.5 10 rng2))])
                    ;; After a few iterations, orbits should diverge
                    (assert-false (equal? orb1 orb2)))))

;;; ============================================================
;;; Display Tests
;;; ============================================================

(test-group display-tests
            (define-test dds->string-test
              (let* ([sys (logistic-map 3.5)]
                     [str (dds->string sys)])
                    (assert-true (string? str))
                    (assert-true (> (string-length str) 0)))))

;;; ============================================================
;;; Edge Cases
;;; ============================================================

(test-group edge-cases
            (define-test orbit-zero-iterations
              (let* ([sys (logistic-map 3.5)]
                     [orb (orbit sys 0.5 0)])
                    (assert-equal 1 (length orb))
                    (assert-equal 0.5 (car orb))))
            
            (define-test iterate-zero
              (let* ([sys (logistic-map 3.5)]
                     [result (iterate sys 0.5 0)])
                    (assert-equal 0.5 result)))
            
            (define-test circle-rotation-irrational
              ;; Golden ratio rotation - never periodic
              (let* ([golden (/ (- (sqrt 5) 1) 2)]
                     [sys (circle-rotation golden)]
                     [p (period sys 0.0 1000 1e-10)])
                    (assert-false p)))
            
            (define-test baker-map-iteration
              ;; Test baker's map produces valid output
              (let* ([initial (vector 0.25 0.25)]
                     [orb (orbit baker-map initial 5)])
                    (assert-equal 6 (length orb))
                    ;; All points should stay in [0,1]^2
                    (assert-true (andmap (lambda (v)
                                                 (and (>= (vector-ref v 0) 0)
                                                      (<= (vector-ref v 0) 1)
                                                      (>= (vector-ref v 1) 0)
                                                      (<= (vector-ref v 1) 1)))
                                         orb)))))

;;; ============================================================
;;; Summary
;;; ============================================================

(display "\n")
(display "==============================================================\n")
(printf "Tests passed: ~a\n" *tests-passed*)
(printf "Tests failed: ~a\n" *tests-failed*)
(printf "Total tests:  ~a\n" *tests-run*)

(if (= *tests-failed* 0)
    (display "\n[SUCCESS] All discrete dynamical systems tests passed.\n")
    (display "\n[FAILURE] Some discrete dynamical systems tests failed.\n"))
