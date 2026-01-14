;;; lattice/physics/classical/test-particles.ss — Tests for Particle System

;;; NOTE: Run from project root: scheme --script lattice/physics/classical/test-particles.ss

(load "core/testing/test-framework.ss")
(load "lattice/physics/classical/particles.ss")

;;; Helper for numeric comparison
(define (assert-= actual expected tolerance)
  (unless (<= (abs (- actual expected)) tolerance)
          (set! *tests-failed* (+ *tests-failed* 1))
          (display "    ✗ ")
          (display *current-test-name*)
          (newline)
          (display "      Expected: ")
          (display expected)
          (display " ± ")
          (display tolerance)
          (newline)
          (display "      Got:      ")
          (display actual)
          (newline)))

;;; Helper for vec2 comparison
(define (assert-vec2-= actual expected tolerance)
  (assert-= (vec2-x actual) (vec2-x expected) tolerance)
  (assert-= (vec2-y actual) (vec2-y expected) tolerance))

(display "
══════════════════════════════════════════════════════════
              PARTICLE SYSTEM TESTS
══════════════════════════════════════════════════════════
")

;;; ====
;;; Particle Creation Tests
;;; ====

(test-group particle-creation-tests
            (define-test make-particle-test
              (let ([p (make-particle (vec2 10 20) (vec2 1 2) 60 60 5 'red #f)])
                   (assert-true (particle? p))
                   (assert-vec2-= (particle-pos p) (vec2 10 20) 0.0001)
                   (assert-vec2-= (particle-vel p) (vec2 1 2) 0.0001)
                   (assert-= (particle-lifetime p) 60 0.0001)
                   (assert-= (particle-max-life p) 60 0.0001)
                   (assert-= (particle-size p) 5 0.0001)
                   (assert-equal (particle-color p) 'red)
                   (assert-false (particle-user-data p))))
            
            (define-test particle-alive-test
              (let ([p1 (make-particle (vec2 0 0) (vec2 0 0) 10 10 1 'white #f)]
                    [p2 (make-particle (vec2 0 0) (vec2 0 0) 0 10 1 'white #f)])
                   (assert-true (particle-alive? p1))
                   (assert-false (particle-alive? p2))))
            
            (define-test particle-age-test
              (let ([p1 (make-particle (vec2 0 0) (vec2 0 0) 10 10 1 'white #f)]
                    [p2 (make-particle (vec2 0 0) (vec2 0 0) 5 10 1 'white #f)]
                    [p3 (make-particle (vec2 0 0) (vec2 0 0) 0 10 1 'white #f)])
                   (assert-= (particle-age p1) 0.0 0.0001)
                   (assert-= (particle-age p2) 0.5 0.0001)
                   (assert-= (particle-age p3) 1.0 0.0001)))
            
            (define-test particle-with-pos-test
              (let* ([p1 (make-particle (vec2 0 0) (vec2 1 1) 10 10 1 'white #f)]
                     [p2 (particle-with-pos p1 (vec2 50 50))])
                    (assert-vec2-= (particle-pos p2) (vec2 50 50) 0.0001)
                    (assert-vec2-= (particle-vel p2) (vec2 1 1) 0.0001))))

;;; ====
;;; Force Field Tests
;;; ====

(test-group force-field-tests
            (define-test gravity-field-test
              (let ([field (gravity-field (vec2 0 9.8))]
                    [p (make-particle (vec2 0 0) (vec2 0 0) 10 10 1 'white #f)])
                   (let ([force (field p)])
                        (assert-vec2-= force (vec2 0 9.8) 0.0001))))
            
            (define-test wind-field-test
              (let ([field (wind-field (vec2 1 0) 5.0)]
                    [p (make-particle (vec2 0 0) (vec2 0 0) 10 10 1 'white #f)])
                   (let ([force (field p)])
                        (assert-vec2-= force (vec2 5.0 0) 0.0001))))
            
            (define-test drag-field-test
              (let ([field (drag-field 0.1)]
                    [p (make-particle (vec2 0 0) (vec2 10 20) 10 10 1 'white #f)])
                   (let ([force (field p)])
                        (assert-vec2-= force (vec2 -1 -2) 0.0001))))
            
            (define-test attractor-field-test
              (let ([field (attractor-field (vec2 100 0) 10 1)]
                    [p (make-particle (vec2 0 0) (vec2 0 0) 10 10 1 'white #f)])
                   (let ([force (field p)])
                        ;; Attract toward (100, 0), distance 100, strength 10, linear falloff
                        (assert-= (vec2-x force) 0.1 0.0001)
                        (assert-= (vec2-y force) 0 0.0001))))
            
            (define-test combine-fields-test
              (let* ([gravity (gravity-field (vec2 0 10))]
                     [wind (wind-field (vec2 1 0) 5)]
                     [combined (combine-fields (list gravity wind))]
                     [p (make-particle (vec2 0 0) (vec2 0 0) 10 10 1 'white #f)])
                    (let ([force (combined p)])
                         (assert-vec2-= force (vec2 5 10) 0.0001)))))

;;; ====
;;; Particle Integration Tests
;;; ====

(test-group particle-integration-tests
            (define-test integrate-no-force
              (let* ([p (make-particle (vec2 0 0) (vec2 10 5) 10 10 1 'white #f)]
                     [field (lambda (p) (vec2 0 0))]
                     [p2 (integrate-particle p field 1.0)])
                    (assert-vec2-= (particle-pos p2) (vec2 10 5) 0.0001)
                    (assert-vec2-= (particle-vel p2) (vec2 10 5) 0.0001)
                    (assert-= (particle-lifetime p2) 9 0.0001)))
            
            (define-test integrate-with-gravity
              (let* ([p (make-particle (vec2 0 0) (vec2 0 0) 10 10 1 'white #f)]
                     [field (gravity-field (vec2 0 10))]
                     [p2 (integrate-particle p field 1.0)])
                    ;; After 1s: vel = (0, 10), pos = (0, 10)
                    (assert-vec2-= (particle-vel p2) (vec2 0 10) 0.0001)
                    (assert-vec2-= (particle-pos p2) (vec2 0 10) 0.0001)))
            
            (define-test update-particles-removes-dead
              (let* ([p1 (make-particle (vec2 0 0) (vec2 0 0) 0.5 1 1 'white #f)]
                     [p2 (make-particle (vec2 0 0) (vec2 0 0) 2 2 1 'white #f)]
                     [particles (list p1 p2)]
                     [field (lambda (p) (vec2 0 0))]
                     [updated (update-particles particles field 1.0)])
                    ;; p1 dies (0.5 - 1 = -0.5), p2 survives (2 - 1 = 1)
                    (assert-= (length updated) 1 0))))

;;; ====
;;; Emitter Tests
;;; ====

(test-group emitter-tests
            (define-test make-emitter-test
              (let ([e (make-simple-emitter (vec2 100 100) 10)])
                   (assert-true (emitter? e))
                   (assert-vec2-= (emitter-pos e) (vec2 100 100) 0.0001)
                   (assert-= (emitter-rate e) 10 0.0001)))
            
            (define-test emitter-with-pos-test
              (let* ([e1 (make-simple-emitter (vec2 0 0) 10)]
                     [e2 (emitter-with-pos e1 (vec2 50 50))])
                    (assert-vec2-= (emitter-pos e2) (vec2 50 50) 0.0001)
                    (assert-= (emitter-rate e2) 10 0.0001)))
            
            (define-test emitter-emit-test
              (let* ([e (make-simple-emitter (vec2 0 0) 60)]  ; 60/s = 1/frame at 60fps
                     [rng (make-pcg 12345 1)]
                     [result (emitter-emit e (/ 1 60) rng)]
                     [particles (car result)]
                     [new-emitter (cadr result)])
                    ;; Should emit ~1 particle
                    (assert-true (>= (length particles) 0))
                    (assert-true (<= (length particles) 2))
                    (assert-true (emitter? new-emitter)))))

;;; ====
;;; Particle System Tests
;;; ====

(test-group particle-system-tests
            (define-test make-empty-system-test
              (let ([ps (make-empty-system)])
                   (assert-true (particle-system? ps))
                   (assert-= (particle-system-count ps) 0 0)))
            
            (define-test system-add-emitter-test
              (let* ([ps (make-empty-system)]
                     [e (make-simple-emitter (vec2 0 0) 10)]
                     [ps2 (particle-system-add-emitter ps e)])
                    (assert-= (length (particle-system-emitters ps2)) 1 0)))
            
            (define-test system-add-particles-test
              (let* ([ps (make-empty-system)]
                     [p1 (make-particle (vec2 0 0) (vec2 0 0) 10 10 1 'white #f)]
                     [p2 (make-particle (vec2 10 10) (vec2 0 0) 10 10 1 'white #f)]
                     [ps2 (particle-system-add-particles ps (list p1 p2))])
                    (assert-= (particle-system-count ps2) 2 0)))
            
            (define-test system-step-test
              (let* ([e (make-simple-emitter (vec2 0 0) 60)]
                     [ps (particle-system-add-emitter (make-empty-system) e)]
                     [rng (make-pcg 12345 1)]
                     [result (particle-system-step ps (/ 1 60) rng)]
                     [ps2 (car result)])
                    ;; After one step, should have some particles
                    (assert-true (particle-system? ps2))))
            
            (define-test system-stats-test
              (let* ([p1 (make-particle (vec2 0 0) (vec2 10 0) 5 10 1 'white #f)]
                     [p2 (make-particle (vec2 0 0) (vec2 0 10) 5 10 1 'white #f)]
                     [ps (particle-system-add-particles (make-empty-system) (list p1 p2))]
                     [stats (particle-system-stats ps)])
                    (assert-= (cdr (assq 'particle-count stats)) 2 0)
                    (assert-= (cdr (assq 'emitter-count stats)) 0 0)
                    (assert-= (cdr (assq 'average-age stats)) 0.5 0.0001)
                    (assert-= (cdr (assq 'average-speed stats)) 10 0.0001))))

;;; ====
;;; Burst Tests
;;; ====

(test-group burst-tests
            (define-test explosion-burst-test
              (let* ([rng (make-pcg 12345 1)]
                     [result (explosion-burst (vec2 100 100) 20 rng)]
                     [particles (car result)])
                    (assert-= (length particles) 20 0)
                    ;; All particles should start at (100, 100)
                    (assert-true (for-all (lambda (p)
                                                  (vec2-nearly-equal? (particle-pos p)
                                                                      (vec2 100 100)
                                                                      0.0001))
                                          particles)))))

;;; ====
;;; Query Tests
;;; ====

(test-group query-tests
            (define-test particles-in-radius-test
              (let* ([p1 (make-particle (vec2 0 0) (vec2 0 0) 10 10 1 'white #f)]
                     [p2 (make-particle (vec2 5 0) (vec2 0 0) 10 10 1 'white #f)]
                     [p3 (make-particle (vec2 100 100) (vec2 0 0) 10 10 1 'white #f)]
                     [particles (list p1 p2 p3)]
                     [nearby (particles-in-radius particles (vec2 0 0) 10)])
                    ;; p1 at (0,0), p2 at (5,0) are within 10, p3 is not
                    (assert-= (length nearby) 2 0)))
            
            (define-test particle-in-bounds-test
              (let ([p1 (make-particle (vec2 50 50) (vec2 0 0) 10 10 1 'white #f)]
                    [p2 (make-particle (vec2 150 50) (vec2 0 0) 10 10 1 'white #f)])
                   (assert-true (particle-in-bounds? p1 (vec2 0 0) (vec2 100 100)))
                   (assert-false (particle-in-bounds? p2 (vec2 0 0) (vec2 100 100))))))

;;; ====
;;; Summary
;;; ====

(print-summary)
