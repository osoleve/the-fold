;;; shell/tests/test-particles.ss — Tests for particles.ss
;;;
;;; Tests for the particle effects system.
;;;
;;; NOTE: Run from project root: scheme --script shell/tests/test-particles.ss

(load "core/test-framework.ss")
(load "core/base/prelude.ss")
(load "shell/ui/color.ss")
(load "shell/ui/layout-color.ss")
(load "shell/ui/animation.ss")
(load "shell/ui/particles.ss")

(display "\n")
(display "====\n")
(display "         PARTICLE EFFECTS SYSTEM TESTS\n")
(display "====\n")

;;; ====
;;; Particle Construction Tests
;;; ====

(test-group particle-construction
            (define-test make-particle-basic
              (let ([p (make-particle (point 10 20) (point 0.5 -0.3) #\* color-yellow 30)])
                   ;; Verify all fields are accessible
                   (assert-equal 10 (point-x (particle-position p)))
                   (assert-equal 20 (point-y (particle-position p)))
                   (assert-equal 0.5 (point-x (particle-velocity p)))
                   (assert-equal -0.3 (point-y (particle-velocity p)))
                   (assert-equal #\* (particle-char p))
                   (assert-equal color-yellow (particle-color p))
                   (assert-equal 30 (particle-lifetime p))
                   (assert-equal 30 (particle-max-life p))))
            
            (define-test particle-alive-when-lifetime-positive
              (let ([p (make-particle (point 0 0) (point 0 0) #\. color-white 10)])
                   (assert-true (particle-alive? p))))
            
            (define-test particle-dead-when-lifetime-zero
              ;; Manually construct particle with 0 lifetime via updates
              (let* ([p (make-particle (point 0 0) (point 0 0) #\. color-white 1)]
                     [p (update-particle p)])  ; lifetime becomes 0
                    (assert-false (particle-alive? p)))))

;;; ====
;;; Particle Age Tests
;;; ====

(test-group particle-age
            (define-test particle-age-at-birth
              ;; Just created particle has age 0
              (let ([p (make-particle (point 0 0) (point 0 0) #\. color-white 100)])
                   (assert-equal 0.0 (particle-age p))))
            
            (define-test particle-age-at-half-life
              ;; Particle at half lifetime has age 0.5
              (let* ([p (make-particle (point 0 0) (point 0 0) #\. color-white 100)]
                     ;; Simulate 50 updates
                     [p (let loop ([i 0] [particle p])
                             (if (>= i 50)
                                 particle
                                 (loop (+ i 1) (update-particle particle))))])
                    (assert-true (< (abs (- (particle-age p) 0.5)) 0.01))))
            
            (define-test particle-age-near-death
              ;; Particle near end of life has age close to 1
              (let* ([p (make-particle (point 0 0) (point 0 0) #\. color-white 10)]
                     ;; Update 9 times (lifetime goes from 10 to 1)
                     [p (let loop ([i 0] [particle p])
                             (if (>= i 9)
                                 particle
                                 (loop (+ i 1) (update-particle particle))))])
                    (assert-true (> (particle-age p) 0.8)))))

;;; ====
;;; Particle Update Tests
;;; ====

(test-group particle-update
            (define-test update-particle-moves-by-velocity
              ;; Particle should move by velocity each update
              (let* ([p (make-particle (point 10.0 20.0) (point 2.0 -1.0) #\* color-red 100)]
                     [p (update-particle p)])
                    (assert-equal 12.0 (point-x (particle-position p)))
                    (assert-equal 19.0 (point-y (particle-position p)))))
            
            (define-test update-particle-decreases-lifetime
              (let* ([p (make-particle (point 0 0) (point 0 0) #\. color-white 50)]
                     [p (update-particle p)])
                    (assert-equal 49 (particle-lifetime p))))
            
            (define-test update-particle-preserves-other-fields
              (let* ([p (make-particle (point 0 0) (point 1 1) #\@ color-cyan 100)]
                     [p (update-particle p)])
                    (assert-equal #\@ (particle-char p))
                    (assert-equal color-cyan (particle-color p))
                    (assert-equal 100 (particle-max-life p)))))

;;; ====
;;; Update Particles List Tests
;;; ====

(test-group update-particles-list
            (define-test update-particles-removes-dead
              ;; Dead particles should be filtered out
              (let* ([alive (make-particle (point 0 0) (point 0 0) #\A color-green 100)]
                     [dying (make-particle (point 0 0) (point 0 0) #\D color-red 1)]
                     [particles (list alive dying)]
                     [result (update-particles particles)])
                    ;; Only the alive particle should remain
                    (assert-equal 1 (length result))))
            
            (define-test update-particles-updates-all
              (let* ([p1 (make-particle (point 0 0) (point 1 0) #\1 color-white 100)]
                     [p2 (make-particle (point 0 0) (point 0 1) #\2 color-white 100)]
                     [result (update-particles (list p1 p2))])
                    ;; Both should have moved
                    (assert-equal 1.0 (point-x (particle-position (car result))))
                    (assert-equal 1.0 (point-y (particle-position (cadr result))))))
            
            (define-test update-particles-empty-list
              (assert-equal '() (update-particles '()))))

;;; ====
;;; Heart Emitter Tests
;;; ====

(test-group heart-emitter
            (define-test emit-hearts-produces-particles
              (let ([hearts (emit-hearts (point 10 20))])
                   (assert-true (list? hearts))
                   (assert-true (> (length hearts) 0))))
            
            (define-test emit-hearts-have-heart-chars
              (let ([hearts (emit-hearts (point 0 0))])
                   ;; At least one should have heart character
                   (assert-true (exists (lambda (p)
                                                (or (char=? #\(string-ref "\u2665" 0) (particle-char p))  ; Black heart
                                                (char=? #\(string-ref "\u2661" 0) (particle-char p)))) ; White heart
                   hearts))))

            (define-test emit-hearts-move-upward
              ;; Hearts should have negative y velocity (up in screen coords)
              (let ([hearts (emit-hearts (point 50 50))])
                   (assert-true (exists (lambda (p)
                                                (< (point-y (particle-velocity p)) 0))
                                        hearts)))))

;;; ====
;;; Sparkle Emitter Tests
;;; ====

(test-group sparkle-emitter
            (define-test emit-sparkles-produces-particles
              (let ([sparkles (emit-sparkles (point 10 20))])
                   (assert-true (list? sparkles))
                   (assert-true (> (length sparkles) 0))))
            
            (define-test emit-sparkles-radiate-outward
              ;; Sparkles should have various velocities
              (let ([sparkles (emit-sparkles (point 50 50))])
                   (assert-true (> (length sparkles) 2))  ; Multiple particles
                   ;; Should have both positive and negative x velocities
                   (let ([xs (map (lambda (p) (point-x (particle-velocity p))) sparkles)])
                        (assert-true (exists positive? xs))
                        (assert-true (exists negative? xs))))))

;;; ====
;;; Bubble Emitter Tests
;;; ====

(test-group bubble-emitter
            (define-test emit-bubbles-produces-particles
              (let ([bubbles (emit-bubbles (point 10 20))])
                   (assert-true (list? bubbles))
                   (assert-true (> (length bubbles) 0))))
            
            (define-test emit-bubbles-float-up
              ;; Bubbles should have negative y velocity (float up)
              (let ([bubbles (emit-bubbles (point 50 50))])
                   (assert-true (for-all (lambda (p)
                                                 (< (point-y (particle-velocity p)) 0))
                                         bubbles)))))

;;; ====
;;; Ripple Emitter Tests
;;; ====

(test-group ripple-emitter
            (define-test emit-ripple-produces-particles
              (let ([ripple (emit-ripple (point 10 20))])
                   (assert-true (list? ripple))
                   (assert-true (> (length ripple) 0))))
            
            (define-test emit-ripple-expands-outward
              ;; Ripple particles should have outward velocities
              (let ([ripple (emit-ripple (point 50 50))])
                   ;; Should have particles moving in different directions
                   (let ([xs (map (lambda (p) (point-x (particle-velocity p))) ripple)]
                         [ys (map (lambda (p) (point-y (particle-velocity p))) ripple)])
                        (assert-true (exists positive? xs))
                        (assert-true (exists negative? xs))))))

;;; ====
;;; Star Emitter Tests
;;; ====

(test-group star-emitter
            (define-test emit-stars-produces-particles
              (let ([stars (emit-stars (point 10 20))])
                   (assert-true (list? stars))
                   (assert-true (> (length stars) 0))))
            
            (define-test emit-stars-stationary
              ;; Stars typically don't move (twinkle in place)
              (let ([stars (emit-stars (point 50 50))])
                   (assert-true (for-all (lambda (p)
                                                 (and (= 0 (point-x (particle-velocity p)))
                                                      (= 0 (point-y (particle-velocity p)))))
                                         stars)))))

;;; ====
;;; ZZZ (Sleep) Emitter Tests
;;; ====

(test-group zzz-emitter
            (define-test emit-zzz-produces-particles
              (let ([zzz (emit-zzz (point 10 20))])
                   (assert-true (list? zzz))
                   (assert-true (> (length zzz) 0))))
            
            (define-test emit-zzz-has-z-chars
              ;; Should contain Z characters
              (let ([zzz (emit-zzz (point 0 0))])
                   (assert-true (exists (lambda (p)
                                                (or (char=? #\Z (particle-char p))
                                                    (char=? #\z (particle-char p))))
                                        zzz))))
            
            (define-test emit-zzz-drift-up
              ;; Z's should drift upward and slightly sideways
              (let ([zzz (emit-zzz (point 50 50))])
                   (assert-true (for-all (lambda (p)
                                                 (< (point-y (particle-velocity p)) 0))
                                         zzz)))))

;;; ====
;;; Music Notes Emitter Tests
;;; ====

(test-group notes-emitter
            (define-test emit-notes-produces-particles
              (let ([notes (emit-notes (point 10 20))])
                   (assert-true (list? notes))
                   (assert-true (> (length notes) 0))))
            
            (define-test emit-notes-float-upward
              (let ([notes (emit-notes (point 50 50))])
                   (assert-true (for-all (lambda (p)
                                                 (< (point-y (particle-velocity p)) 0))
                                         notes)))))

;;; ====
;;; Exclamation Emitter Tests
;;; ====

(test-group exclamation-emitter
            (define-test emit-exclamation-produces-particles
              (let ([ex (emit-exclamation (point 10 20))])
                   (assert-true (list? ex))
                   (assert-true (> (length ex) 0))))
            
            (define-test emit-exclamation-has-bang-chars
              ;; Should contain ! characters
              (let ([ex (emit-exclamation (point 0 0))])
                   (assert-true (exists (lambda (p)
                                                (char=? #\! (particle-char p)))
                                        ex)))))

;;; ====
;;; Particle System Tests
;;; ====

(test-group particle-system
            (define-test make-particle-system-empty
              (let ([sys (make-particle-system)])
                   (assert-true (null? sys))))
            
            (define-test add-particles-to-system
              (let* ([sys (make-particle-system)]
                     [hearts (emit-hearts (point 50 50))]
                     [sys (add-particles sys hearts)])
                    (assert-equal (length hearts) (length sys))))
            
            (define-test add-multiple-emitter-outputs
              (let* ([sys (make-particle-system)]
                     [sys (add-particles sys (emit-hearts (point 10 10)))]
                     [sys (add-particles sys (emit-sparkles (point 20 20)))])
                    (assert-true (> (length sys) 5))))
            
            (define-test update-particle-system-test
              (let* ([sys (make-particle-system)]
                     [sys (add-particles sys (emit-hearts (point 50 50)))]
                     [initial-count (length sys)]
                     [sys (update-particle-system sys)])
                    ;; Particles should still be alive after 1 update
                    (assert-equal initial-count (length sys)))))

;;; ====
;;; Mood-Based Emission Tests
;;; ====

(test-group mood-emission
            (define-test emit-by-mood-happy
              (let ([particles (emit-by-mood (point 50 50) 'happy)])
                   (assert-true (list? particles))
                   (assert-true (> (length particles) 0))))
            
            (define-test emit-by-mood-sleepy
              (let ([particles (emit-by-mood (point 50 50) 'sleepy)])
                   (assert-true (list? particles))))
            
            (define-test emit-by-mood-lonely
              ;; Lonely mood produces no particles
              (let ([particles (emit-by-mood (point 50 50) 'lonely)])
                   (assert-true (null? particles))))
            
            (define-test emit-by-mood-unknown
              ;; Unknown mood produces empty list
              (let ([particles (emit-by-mood (point 50 50) 'unknown-mood)])
                   (assert-true (null? particles)))))

;;; ====
;;; Interaction-Based Emission Tests
;;; ====

(test-group interaction-emission
            (define-test emit-by-interaction-pet
              (let ([particles (emit-by-interaction (point 50 50) 'pet)])
                   (assert-true (list? particles))
                   (assert-true (> (length particles) 0))))
            
            (define-test emit-by-interaction-feed
              (let ([particles (emit-by-interaction (point 50 50) 'feed)])
                   (assert-true (> (length particles) 0))))
            
            (define-test emit-by-interaction-splash
              (let ([particles (emit-by-interaction (point 50 50) 'splash)])
                   (assert-true (> (length particles) 0))))
            
            (define-test emit-by-interaction-unknown
              ;; Unknown interaction produces empty list
              (let ([particles (emit-by-interaction (point 50 50) 'unknown)])
                   (assert-true (null? particles)))))

;;; ====
;;; Rendering Tests
;;; ====

(test-group particle-rendering
            (define-test render-particle-on-canvas
              (let* ([c (make-canvas 20 10)]
                     [p (make-particle (point 10.0 5.0) (point 0 0) #\* color-yellow 100)]
                     [c (render-particle c p)])
                    (assert-equal #\* (cell%-char (canvas-ref c 10 5)))))
            
            (define-test render-particle-out-of-bounds
              ;; Rendering out of bounds should not crash
              (let* ([c (make-canvas 10 10)]
                     [p (make-particle (point 100.0 100.0) (point 0 0) #\X color-red 100)]
                     [c (render-particle c p)])
                    ;; Should return canvas unchanged
                    (assert-true (canvas%? c))))
            
            (define-test render-particles-multiple
              (let* ([c (make-canvas 30 20)]
                     [p1 (make-particle (point 5.0 5.0) (point 0 0) #\1 color-red 100)]
                     [p2 (make-particle (point 15.0 10.0) (point 0 0) #\2 color-green 100)]
                     [c (render-particles c (list p1 p2))])
                    (assert-equal #\1 (cell%-char (canvas-ref c 5 5)))
                    (assert-equal #\2 (cell%-char (canvas-ref c 15 10)))))
            
            (define-test render-particle-system-test
              (let* ([c (make-canvas 40 30)]
                     [sys (make-particle-system)]
                     [sys (add-particles sys (emit-sparkles (point 20 15)))]
                     [c (render-particle-system c sys)])
                    ;; Should have rendered something
                    (assert-true (canvas%? c)))))

;;; ====
;;; Summary
;;; ====

(display "\n")
(display "====\n")
(printf "Tests passed: ~a\n" *tests-passed*)
(printf "Tests failed: ~a\n" *tests-failed*)
(printf "Total tests:  ~a\n" *tests-run*)

(if (= *tests-failed* 0)
    (display "\n[SUCCESS] All particle system tests passed.\n")
    (display "\n[FAILURE] Some particle system tests failed.\n"))
