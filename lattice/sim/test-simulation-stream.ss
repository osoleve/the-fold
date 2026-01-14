;;; core/sim/test-simulation-stream.ss --- Tests for Simulation Stream Abstraction

(load "core/test-framework.ss")
(load "lattice/sim/simulation-stream.ss")

(display "\n")
(display "====\n")
(display "         SIMULATION STREAM TESTS\n")
(display "====\n")

;;; ====
;;; SimState Type Tests
;;; ====

(test-group sim-state-type
            (define-test make-sim-state-test
              (let ([s (make-sim-state 0.0 'data 'meta)])
                   (assert-true (sim-state? s))
                   (assert-equal 0.0 (sim-state-time s))
                   (assert-equal 'data (sim-state-data s))
                   (assert-equal 'meta (sim-state-metadata s))))
            
            (define-test sim-state-with-time-test
              (let* ([s (make-sim-state 0.0 'data 'meta)]
                     [s2 (sim-state-with-time s 1.5)])
                    (assert-equal 1.5 (sim-state-time s2))
                    (assert-equal 'data (sim-state-data s2))))
            
            (define-test sim-state-with-data-test
              (let* ([s (make-sim-state 0.0 'old-data 'meta)]
                     [s2 (sim-state-with-data s 'new-data)])
                    (assert-equal 0.0 (sim-state-time s2))
                    (assert-equal 'new-data (sim-state-data s2)))))

;;; ====
;;; Unfold Tests
;;; ====

(test-group unfold-tests
            (define-test sim-unfold-finite-test
              ;; Unfold that terminates
              (let* ([step (lambda (n)
                                   (if (> n 5)
                                       #f
                                       (cons n (+ n 1))))]
                     [s (sim-unfold step 1)])
                    (assert-equal '(1 2 3 4 5) (stream->list 10 s))))
            
            (define-test sim-unfold-infinite-test
              ;; Infinite stream from unfold
              (let* ([step (lambda (n) (cons n (+ n 1)))]
                     [s (sim-unfold-infinite step 0)])
                    (assert-equal '(0 1 2 3 4 5 6 7 8 9) (stream->list 10 s))))
            
            (define-test sim-unfold-with-state-test
              ;; Stream of states themselves
              (let* ([step (lambda (n) (* n 2))]
                     [s (sim-unfold-with-state step 1)])
                    ;; 1, 2, 4, 8, 16, ...
                    (assert-equal '(1 2 4 8 16) (stream->list 5 s))))
            
            (define-test sim-unfold-empty-test
              ;; Immediate termination
              (let* ([step (lambda (n) #f)]
                     [s (sim-unfold step 0)])
                    (assert-true (stream-nil? s)))))

;;; ====
;;; Simulate Tests
;;; ====

(test-group simulate-tests
            (define-test simulate-basic-test
              ;; Simple counter simulation
              (let* ([initial (make-sim-state 0.0 0 '())]
                     [step-fn (lambda (s)
                                      (sim-state-with-data s (+ 1 (sim-state-data s))))]
                     [stream (simulate initial step-fn 1.0)]
                     [first-5 (stream->list 5 stream)])
                    ;; Check data increments
                    (assert-equal 0 (sim-state-data (list-ref first-5 0)))
                    (assert-equal 1 (sim-state-data (list-ref first-5 1)))
                    (assert-equal 2 (sim-state-data (list-ref first-5 2)))
                    ;; Check time increments
                    (assert-equal 0.0 (sim-state-time (list-ref first-5 0)))
                    (assert-equal 1.0 (sim-state-time (list-ref first-5 1)))
                    (assert-equal 2.0 (sim-state-time (list-ref first-5 2)))))
            
            (define-test simulate-with-dt-test
              ;; Simulation that uses dt
              (let* ([initial (make-sim-state 0.0 0 '())]
                     [step-fn (lambda (s dt)
                                      (sim-state-with-data s (+ (sim-state-data s) (* 10 dt))))]
                     [stream (simulate-with-dt initial step-fn 0.1)]
                     [first-3 (stream->list 3 stream)])
                    ;; Data: 0, 0+1=1, 1+1=2
                    (assert-equal 0 (sim-state-data (list-ref first-3 0)))
                    (assert-equal 1.0 (sim-state-data (list-ref first-3 1)))
                    ;; Time: 0, 0.1, 0.2
                    (assert-true (< (abs (- 0.1 (sim-state-time (list-ref first-3 1)))) 0.0001))))
            
            (define-test simulate-infinite-stream-test
              ;; Verify we can take many elements
              (let* ([initial (make-sim-state 0.0 0 '())]
                     [step-fn (lambda (s)
                                      (sim-state-with-data s (+ 1 (sim-state-data s))))]
                     [stream (simulate initial step-fn 0.016)]
                     [many (stream->list 1000 stream)])
                    (assert-equal 1000 (length many))
                    (assert-equal 999 (sim-state-data (list-ref many 999))))))

;;; ====
;;; Scan Tests
;;; ====

(test-group scan-tests
            (define-test sim-scan-sum-test
              ;; Running sum
              (let* ([s (list->stream '(1 2 3 4 5))]
                     [sums (sim-scan + 0 s)])
                    ;; 0, 0+1=1, 1+2=3, 3+3=6, 6+4=10, 10+5=15
                    (assert-equal '(0 1 3 6 10 15) (stream->list 10 sums))))
            
            (define-test sim-scan-product-test
              ;; Running product
              (let* ([s (list->stream '(1 2 3 4 5))]
                     [prods (sim-scan * 1 s)])
                    ;; 1, 1, 2, 6, 24, 120
                    (assert-equal '(1 1 2 6 24 120) (stream->list 10 prods))))
            
            (define-test sim-scan-map-test
              ;; Scan with mapping
              (let* ([states (list->stream (list (make-sim-state 0.0 10 '())
                                                 (make-sim-state 1.0 20 '())
                                                 (make-sim-state 2.0 30 '())))]
                     [sums (sim-scan-map + 0 sim-state-data states)])
                    ;; 0, 0+10=10, 10+20=30, 30+30=60
                    (assert-equal '(0 10 30 60) (stream->list 5 sums))))
            
            (define-test sim-accumulate-test
              ;; Accumulate on simulation states
              (let* ([initial (make-sim-state 0.0 1 '())]
                     [step-fn (lambda (s)
                                      (sim-state-with-data s (* 2 (sim-state-data s))))]
                     [stream (simulate initial step-fn 1.0)]
                     ;; Accumulate sum of all data values seen
                     [acc-fn (lambda (total state) (+ total (sim-state-data state)))]
                     [accumulated (sim-accumulate acc-fn 0 stream)]
                     [first-5 (stream->list 5 accumulated)])
                    ;; States: data=1,2,4,8,16,...
                    ;; Accumulated: 0, 0+1=1, 1+2=3, 3+4=7, 7+8=15
                    (assert-equal '(0 1 3 7 15) first-5))))

;;; ====
;;; Physics State Tests
;;; ====

(test-group physics-state-tests
            (define-test make-physics-state-test
              (let ([s (make-physics-state (vec2 10 20) (vec2 1 -1) 5.0 0.0)])
                   (assert-true (physics-state? s))
                   (assert-true (vec2-nearly-equal? (vec2 10 20) (physics-pos s) 0.001))
                   (assert-true (vec2-nearly-equal? (vec2 1 -1) (physics-vel s) 0.001))
                   (assert-equal 5.0 (physics-mass s))
                   (assert-equal 0.2 (physics-inv-mass s))))
            
            (define-test physics-static-body-test
              ;; Static body has mass=0
              (let ([s (make-physics-state (vec2 0 0) (vec2 0 0) 0 0.0)])
                   (assert-equal 0 (physics-mass s))
                   (assert-equal 0 (physics-inv-mass s))))
            
            (define-test make-updated-physics-test
              (let* ([s (make-physics-state (vec2 0 0) (vec2 0 0) 1.0 0.0)]
                     [s2 (make-updated-physics s (vec2 10 20) (vec2 5 5))])
                    (assert-true (vec2-nearly-equal? (vec2 10 20) (physics-pos s2) 0.001))
                    (assert-true (vec2-nearly-equal? (vec2 5 5) (physics-vel s2) 0.001))
                    ;; Mass unchanged
                    (assert-equal 1.0 (physics-mass s2)))))

;;; ====
;;; Physics Integration Tests
;;; ====

(test-group physics-integration-tests
            (define-test euler-step-no-force-test
              ;; No force = constant velocity
              (let* ([initial (make-physics-state (vec2 0 0) (vec2 10 0) 1.0 0.0)]
                     [zero-force (lambda (s) (vec2 0 0))]
                     [step-fn (euler-step zero-force 0.1)]
                     [next (step-fn initial)])
                    ;; Position should advance by velocity*dt
                    (assert-true (vec2-nearly-equal? (vec2 1 0) (physics-pos next) 0.001))
                    ;; Velocity unchanged
                    (assert-true (vec2-nearly-equal? (vec2 10 0) (physics-vel next) 0.001))))
            
            (define-test euler-step-gravity-test
              ;; Constant gravity force
              (let* ([initial (make-physics-state (vec2 0 100) (vec2 0 0) 1.0 0.0)]
                     [gravity (const-gravity 10)]  ; 10 m/s^2 downward
                     [step-fn (euler-step gravity 0.1)]
                     [after-1 (step-fn initial)]
                     [after-2 (step-fn after-1)])
                    ;; After 0.1s: v = 0 + 10*0.1 = 1, pos = 100 + 1*0.1 = 100.1
                    (assert-true (vec2-nearly-equal? (vec2 0 1) (physics-vel after-1) 0.001))
                    ;; After 0.2s: v = 1 + 10*0.1 = 2, pos = 100.1 + 2*0.1 = 100.3
                    (assert-true (vec2-nearly-equal? (vec2 0 2) (physics-vel after-2) 0.001))))
            
            (define-test symplectic-euler-test
              ;; Symplectic Euler should be more stable
              (let* ([initial (make-physics-state (vec2 0 0) (vec2 1 0) 1.0 0.0)]
                     [zero-force (lambda (s) (vec2 0 0))]
                     [step-fn (symplectic-euler-step zero-force 0.1)])
                    ;; Should work the same as regular Euler for constant velocity
                    (let ([next (step-fn initial)])
                         (assert-true (vec2-nearly-equal? (vec2 0.1 0) (physics-pos next) 0.001)))))
            
            (define-test verlet-step-test
              ;; Verlet should be second-order accurate
              (let* ([initial (make-physics-state (vec2 0 0) (vec2 0 0) 1.0 0.0)]
                     [gravity (const-gravity 10)]
                     [step-fn (verlet-step gravity 0.1)]
                     [after-1 (step-fn initial)])
                    ;; Verlet position update: x + v*dt + 0.5*a*dt^2
                    ;; = 0 + 0*0.1 + 0.5*10*0.01 = 0.05
                    (assert-true (< (abs (- 0.05 (vec2-y (physics-pos after-1)))) 0.01)))))

;;; ====
;;; Force Function Tests
;;; ====

(test-group force-function-tests
            (define-test const-gravity-test
              (let* ([s (make-physics-state (vec2 0 0) (vec2 0 0) 2.0 0.0)]
                     [grav (const-gravity 9.8)]
                     [force (grav s)])
                    ;; F = m*g = 2*9.8 = 19.6
                    (assert-true (vec2-nearly-equal? (vec2 0 19.6) force 0.001))))
            
            (define-test spring-force-test
              (let* ([anchor (vec2 0 0)]
                     [s (make-physics-state (vec2 10 0) (vec2 0 0) 1.0 0.0)]
                     [spring (spring-force-fn anchor 1.0 0.0)]
                     [force (spring s)])
                    ;; Displacement = (0,0) - (10,0) = (-10, 0)
                    ;; Spring force = k * displacement = 1.0 * (-10, 0) = (-10, 0)
                    (assert-true (vec2-nearly-equal? (vec2 -10 0) force 0.001))))
            
            (define-test spring-with-damping-test
              (let* ([anchor (vec2 0 0)]
                     [s (make-physics-state (vec2 10 0) (vec2 5 0) 1.0 0.0)]
                     [spring (spring-force-fn anchor 1.0 0.5)]
                     [force (spring s)])
                    ;; Spring: (-10, 0)
                    ;; Damping: -0.5 * (5, 0) = (-2.5, 0)
                    ;; Total: (-12.5, 0)
                    (assert-true (vec2-nearly-equal? (vec2 -12.5 0) force 0.001))))
            
            (define-test drag-force-test
              (let* ([s (make-physics-state (vec2 0 0) (vec2 10 5) 1.0 0.0)]
                     [drag (drag-force-fn 0.1)]
                     [force (drag s)])
                    ;; Drag = -c * v = -0.1 * (10, 5) = (-1, -0.5)
                    (assert-true (vec2-nearly-equal? (vec2 -1 -0.5) force 0.001))))
            
            (define-test combine-forces-test
              (let* ([s (make-physics-state (vec2 0 0) (vec2 0 0) 1.0 0.0)]
                     [f1 (lambda (s) (vec2 10 0))]
                     [f2 (lambda (s) (vec2 0 20))]
                     [f3 (lambda (s) (vec2 -5 -5))]
                     [combined (combine-force-fns (list f1 f2 f3))]
                     [force (combined s)])
                    ;; (10,0) + (0,20) + (-5,-5) = (5, 15)
                    (assert-true (vec2-nearly-equal? (vec2 5 15) force 0.001)))))

;;; ====
;;; Full Simulation Tests
;;; ====

(test-group full-simulation-tests
            (define-test projectile-simulation-test
              ;; Simulate projectile motion
              (let* ([initial (make-physics-state (vec2 0 0) (vec2 10 10) 1.0 0.0)]
                     [gravity (const-gravity 10)]
                     [step-fn (symplectic-euler-step gravity 0.01)]
                     [stream (simulate initial step-fn 0.01)]
                     ;; Take 100 steps = 1 second
                     [states (stream->list 101 stream)]
                     [final (list-ref states 100)])
                    ;; After 1s with gravity=10, vy should be approximately 10 + 10*1 = 20
                    ;; (starting vy=10, falling at 10 m/s^2)
                    (let ([final-vy (vec2-y (physics-vel final))])
                         (assert-true (< (abs (- 20 final-vy)) 1)))))  ; Allow for integration error
            
            (define-test spring-oscillation-test
              ;; Simulate spring oscillation
              (let* ([initial (make-physics-state (vec2 10 0) (vec2 0 0) 1.0 0.0)]
                     [anchor (vec2 0 0)]
                     [spring (spring-force-fn anchor 1.0 0.0)]  ; k=1, no damping
                     [step-fn (verlet-step spring 0.01)]
                     [stream (simulate initial step-fn 0.01)]
                     [states (stream->list 629 stream)])  ; ~2*pi seconds (one period)
                    ;; After one period, should be near starting position
                    (let* ([final (list-ref states 628)]
                           [final-x (vec2-x (physics-pos final))])
                          ;; Allow tolerance for numerical integration
                          (assert-true (< (abs (- 10 final-x)) 1)))))
            
            (define-test damped-oscillation-test
              ;; Damped spring should lose energy
              (let* ([initial (make-physics-state (vec2 10 0) (vec2 0 0) 1.0 0.0)]
                     [anchor (vec2 0 0)]
                     [spring (spring-force-fn anchor 1.0 0.1)]  ; With damping
                     [step-fn (verlet-step spring 0.01)]
                     [stream (simulate initial step-fn 0.01)]
                     [states (stream->list 1001 stream)]
                     [initial-energy (+ (* 0.5 1 0)  ; KE
                                        (* 0.5 1 100))]  ; PE = 0.5*k*x^2
                     [final (list-ref states 1000)]
                     [final-x (vec2-x (physics-pos final))]
                     [final-v (vec2-x (physics-vel final))]
                     [final-energy (+ (* 0.5 1 (* final-v final-v))
                                      (* 0.5 1 (* final-x final-x)))])
                    ;; Energy should decrease due to damping
                    (assert-true (< final-energy initial-energy)))))

;;; ====
;;; Observable Stream Tests
;;; ====

(test-group observable-tests
            (define-test sim-positions-test
              (let* ([s1 (make-physics-state (vec2 0 0) (vec2 0 0) 1.0 0.0)]
                     [s2 (make-physics-state (vec2 1 1) (vec2 0 0) 1.0 1.0)]
                     [s3 (make-physics-state (vec2 2 4) (vec2 0 0) 1.0 2.0)]
                     [stream (list->stream (list s1 s2 s3))]
                     [positions (sim-positions stream)]
                     [pos-list (stream->list 3 positions)])
                    (assert-true (vec2-nearly-equal? (vec2 0 0) (list-ref pos-list 0) 0.001))
                    (assert-true (vec2-nearly-equal? (vec2 1 1) (list-ref pos-list 1) 0.001))
                    (assert-true (vec2-nearly-equal? (vec2 2 4) (list-ref pos-list 2) 0.001))))
            
            (define-test sim-velocities-test
              (let* ([s1 (make-physics-state (vec2 0 0) (vec2 1 0) 1.0 0.0)]
                     [s2 (make-physics-state (vec2 0 0) (vec2 2 0) 1.0 1.0)]
                     [stream (list->stream (list s1 s2))]
                     [velocities (sim-velocities stream)]
                     [vel-list (stream->list 2 velocities)])
                    (assert-true (vec2-nearly-equal? (vec2 1 0) (list-ref vel-list 0) 0.001))
                    (assert-true (vec2-nearly-equal? (vec2 2 0) (list-ref vel-list 1) 0.001))))
            
            (define-test sim-times-test
              (let* ([s1 (make-sim-state 0.0 'a '())]
                     [s2 (make-sim-state 0.5 'b '())]
                     [s3 (make-sim-state 1.0 'c '())]
                     [stream (list->stream (list s1 s2 s3))]
                     [times (sim-times stream)]
                     [time-list (stream->list 3 times)])
                    (assert-equal '(0.0 0.5 1.0) time-list)))
            
            (define-test sim-kinetic-energy-test
              (let* ([s (make-physics-state (vec2 0 0) (vec2 3 4) 2.0 0.0)]
                     [stream (list->stream (list s))]
                     [ke-stream (sim-kinetic-energy stream)]
                     [ke (stream-head ke-stream)])
                    ;; KE = 0.5 * m * v^2 = 0.5 * 2 * (9+16) = 25
                    (assert-equal 25.0 ke)))
            
            (define-test sim-total-energy-test
              ;; Test energy conservation in free fall
              ;; Use y-ref = 0 so we have positive PE initially
              (let* ([initial (make-physics-state (vec2 0 100) (vec2 0 0) 1.0 0.0)]
                     [gravity (const-gravity 10)]
                     [step-fn (symplectic-euler-step gravity 0.01)]
                     [stream (simulate initial step-fn 0.01)]
                     [energies (sim-total-energy 10 0 stream)]  ; y-ref=0
                     [e-list (stream->list 100 energies)])
                    ;; Total energy should be roughly conserved
                    ;; Initial: KE=0, PE=m*g*h=1*10*100=1000
                    (let ([e0 (car e-list)]
                          [e99 (list-ref e-list 99)])
                         ;; Energy should stay close to initial (within 5%)
                         ;; Symplectic Euler is good at energy conservation
                         (assert-true (< (abs (- e0 e99)) (* 0.05 (abs e0))))))))

;;; ====
;;; Utility Function Tests
;;; ====

(test-group utility-tests
            (define-test sim-take-test
              (let* ([stream (list->stream '(1 2 3 4 5))]
                     [taken (sim-take 3 stream)])
                    (assert-equal '(1 2 3) taken)))
            
            (define-test sim-sample-variable-dt-test
              ;; Test that sim-sample works correctly with variable timesteps
              ;; Create a stream with non-uniform time steps: 0.0, 0.1, 0.3, 0.4, 0.9, 1.0, 1.5, 2.0, 2.1
              (let* ([times '(0.0 0.1 0.3 0.4 0.9 1.0 1.5 2.0 2.1)]
                     [states (map (lambda (t) (make-sim-state t t '())) times)]
                     [stream (list->stream states)]
                     ;; Sample at interval 1.0
                     [sampled (sim-take 3 (sim-sample 1.0 10 stream))]
                     [sample-times (map sim-state-time sampled)])
                    ;; Should get states at times 0.0, 1.0, 2.0 (or first state >= those times)
                    (assert-equal 3 (length sampled))
                    (assert-equal 0.0 (car sample-times))      ; t=0.0
                    (assert-equal 1.0 (cadr sample-times))     ; t>=1.0, first is 1.0
                    (assert-equal 2.0 (caddr sample-times))))  ; t>=2.0, first is 2.0
            
            (define-test sim-until-test
              (let* ([initial (make-sim-state 0.0 0 '())]
                     [step-fn (lambda (s)
                                      (sim-state-with-data s (+ 1 (sim-state-data s))))]
                     [stream (simulate initial step-fn 1.0)]
                     [result (sim-until (lambda (s) (>= (sim-state-data s) 5)) 100 stream)])
                    ;; Should get states with data 0,1,2,3,4,5
                    (assert-equal 6 (length result))
                    (assert-equal 5 (sim-state-data (list-ref result 5)))))
            
            (define-test sim-at-time-test
              (let* ([initial (make-sim-state 0.0 'start '())]
                     [step-fn (lambda (s) s)]
                     [stream (simulate initial step-fn 0.5)]
                     [state (sim-at-time 2.0 stream 100)])
                    (assert-true (>= (sim-state-time state) 2.0)))))

;;; ====
;;; N-Body Simulation Tests
;;; ====

(test-group n-body-tests
            (define-test make-n-body-state-test
              (let* ([b1 (make-physics-state (vec2 0 0) (vec2 0 0) 1.0 0.0)]
                     [b2 (make-physics-state (vec2 10 0) (vec2 0 0) 1.0 0.0)]
                     [state (make-n-body-state (list b1 b2) 0.0)])
                    (assert-true (n-body-state? state))
                    (assert-equal 2 (length (n-body-states state)))))
            
            (define-test n-body-step-test
              ;; Two bodies, constant forces
              (let* ([b1 (make-physics-state (vec2 0 0) (vec2 0 0) 1.0 0.0)]
                     [b2 (make-physics-state (vec2 10 0) (vec2 0 0) 1.0 0.0)]
                     [initial (make-n-body-state (list b1 b2) 0.0)]
                     [force-fn (lambda (bodies)
                                       (list (vec2 1 0)    ; Push first body right
                                             (vec2 -1 0)))] ; Push second body left
                     [step-fn (n-body-step force-fn 0.1)]
                     [next (step-fn initial)]
                     [bodies (n-body-states next)])
                    ;; First body should move right
                    (assert-true (> (vec2-x (physics-pos (car bodies))) 0))
                    ;; Second body should move left
                    (assert-true (< (vec2-x (physics-pos (cadr bodies))) 10)))))

;;; ====
;;; Simulation Combinator Tests
;;; ====

(test-group combinator-tests
            (define-test sim-seq-test
              ;; Sequential composition
              (let* ([step1 (lambda (s) (sim-state-with-data s (+ 1 (sim-state-data s))))]
                     [step2 (lambda (s) (sim-state-with-data s (* 2 (sim-state-data s))))]
                     [combined (sim-seq (list step1 step2))]
                     [initial (make-sim-state 0.0 5 '())]
                     [result (combined initial)])
                    ;; (5 + 1) * 2 = 12
                    (assert-equal 12 (sim-state-data result))))
            
            (define-test sim-when-true-test
              (let* ([step (lambda (s) (sim-state-with-data s 'modified))]
                     [conditional (sim-when (lambda (s) #t) step)]
                     [initial (make-sim-state 0.0 'original '())]
                     [result (conditional initial)])
                    (assert-equal 'modified (sim-state-data result))))
            
            (define-test sim-when-false-test
              (let* ([step (lambda (s) (sim-state-with-data s 'modified))]
                     [conditional (sim-when (lambda (s) #f) step)]
                     [initial (make-sim-state 0.0 'original '())]
                     [result (conditional initial)])
                    (assert-equal 'original (sim-state-data result))))
            
            (define-test sim-switch-test
              (let* ([cases (list (cons 'a (lambda (s) (sim-state-with-data s 'case-a)))
                                  (cons 'b (lambda (s) (sim-state-with-data s 'case-b))))]
                     [switched (sim-switch (lambda (s) (sim-state-metadata s)) cases)]
                     [initial-a (make-sim-state 0.0 'orig 'a)]
                     [initial-b (make-sim-state 0.0 'orig 'b)])
                    (assert-equal 'case-a (sim-state-data (switched initial-a)))
                    (assert-equal 'case-b (sim-state-data (switched initial-b))))))

;;; ====
;;; Edge Cases
;;; ====

(test-group edge-cases
            (define-test zero-dt-test
              ;; dt=0 should not cause issues
              (let* ([initial (make-physics-state (vec2 0 0) (vec2 10 0) 1.0 0.0)]
                     [gravity (const-gravity 10)]
                     [step-fn (euler-step gravity 0.0)]
                     [next (step-fn initial)])
                    ;; Position and velocity should be unchanged
                    (assert-true (vec2-nearly-equal? (vec2 0 0) (physics-pos next) 0.001))
                    (assert-true (vec2-nearly-equal? (vec2 10 0) (physics-vel next) 0.001))))
            
            (define-test large-dt-test
              ;; Large dt should still work (though less accurate)
              (let* ([initial (make-physics-state (vec2 0 0) (vec2 0 0) 1.0 0.0)]
                     [gravity (const-gravity 10)]
                     [step-fn (euler-step gravity 10.0)]
                     [next (step-fn initial)])
                    ;; After 10s free fall: v = 100 m/s, pos = 1000 m
                    (assert-true (vec2-nearly-equal? (vec2 0 100) (physics-vel next) 0.001))
                    (assert-true (vec2-nearly-equal? (vec2 0 1000) (physics-pos next) 0.001))))
            
            (define-test empty-force-list-test
              (let* ([s (make-physics-state (vec2 0 0) (vec2 0 0) 1.0 0.0)]
                     [combined (combine-force-fns '())]
                     [force (combined s)])
                    (assert-true (vec2-nearly-equal? (vec2 0 0) force 0.001)))))

;;; ====
;;; Summary
;;; ====

(display "\n")
(display "====\n")
(printf "Tests passed: ~a\n" *tests-passed*)
(printf "Tests failed: ~a\n" *tests-failed*)
(printf "Total tests:  ~a\n" *tests-run*)

(if (= *tests-failed* 0)
    (display "\n[SUCCESS] All simulation stream tests passed.\n")
    (display "\n[FAILURE] Some simulation stream tests failed.\n"))
