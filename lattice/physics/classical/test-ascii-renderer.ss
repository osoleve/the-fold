;;; user/physics/test-ascii-renderer.ss — Tests for ASCII Physics Renderer

(load "core/test-framework.ss")
(load "lattice/physics/classical/ascii-renderer.ss")

(display "
==============================================================
              ASCII PHYSICS RENDERER TESTS
==============================================================
")

;;; ============================================================
;;; Configuration Tests
;;; ============================================================

(test-group config-tests
            (define-test make-render-config-test
              (let ([config (make-render-config 80 40 2.0 40 20)])
                   (assert-equal 80 (config-width config))
                   (assert-equal 40 (config-height config))
                   (assert-equal 2.0 (config-scale config))
                   (assert-equal 40 (config-origin-x config))
                   (assert-equal 20 (config-origin-y config))))
            
            (define-test default-render-config-test
              (let ([config (default-render-config)])
                   (assert-equal 80 (config-width config))
                   (assert-equal 40 (config-height config))))
            
            (define-test make-debug-options-test
              (let ([debug (make-debug-options #t #f #t #f)])
                   (assert-equal #t (debug-show-velocity debug))
                   (assert-equal #f (debug-show-contacts debug))
                   (assert-equal #t (debug-show-constraints debug))
                   (assert-equal #f (debug-show-aabb debug))))
            
            (define-test make-render-style-test
              (let ([style (make-render-style #\O #\# #\- #\| #\+ #\> #\*)])
                   (assert-equal #\O (style-fill-char style))
                   (assert-equal #\# (style-static-char style))
                   (assert-equal #\- (style-box-h style))
                   (assert-equal #\| (style-box-v style))
                   (assert-equal #\+ (style-corner style))
                   (assert-equal #\> (style-velocity-char style))
                   (assert-equal #\* (style-contact-char style)))))

;;; ============================================================
;;; Coordinate Transformation Tests
;;; ============================================================

(test-group coordinate-tests
            (define-test world->screen-origin
              (let* ([config (make-render-config 80 40 2.0 40 20)]
                     [screen (world->screen (vec2 0 0) config)])
                    (assert-equal 40 (car screen))
                    (assert-equal 20 (cdr screen))))
            
            (define-test world->screen-positive
              (let* ([config (make-render-config 80 40 2.0 40 20)]
                     [screen (world->screen (vec2 5 5) config)])
                    (assert-equal 50 (car screen))  ; 40 + 5*2
                    (assert-equal 30 (cdr screen)))) ; 20 + 5*2
            
            (define-test world->screen-negative
              (let* ([config (make-render-config 80 40 2.0 40 20)]
                     [screen (world->screen (vec2 -5 -5) config)])
                    (assert-equal 30 (car screen))  ; 40 - 5*2
                    (assert-equal 10 (cdr screen)))) ; 20 - 5*2
            
            (define-test screen->world-roundtrip
              (let* ([config (make-render-config 80 40 2.0 40 20)]
                     [original (vec2 3 7)]
                     [screen (world->screen original config)]
                     [back (screen->world (car screen) (cdr screen) config)])
                    ;; Should approximately round-trip
                    (assert-equal 3.0 (vec2-x back))
                    (assert-equal 7.0 (vec2-y back)))))

;;; ============================================================
;;; Line Drawing Tests
;;; ============================================================

(test-group line-tests
            (define-test horizontal-line
              (let ([frame (make-frame 20 10 #\space)]
                    [config (make-render-config 20 10 1.0 0 0)])
                   (draw-line! frame 2 5 8 5 #\- config)
                   (assert-equal #\- (frame-ref frame 2 5))
                   (assert-equal #\- (frame-ref frame 5 5))
                   (assert-equal #\- (frame-ref frame 8 5))
                   (assert-equal #\space (frame-ref frame 1 5))))
            
            (define-test vertical-line
              (let ([frame (make-frame 20 10 #\space)]
                    [config (make-render-config 20 10 1.0 0 0)])
                   (draw-line! frame 10 2 10 7 #\| config)
                   (assert-equal #\| (frame-ref frame 10 2))
                   (assert-equal #\| (frame-ref frame 10 5))
                   (assert-equal #\| (frame-ref frame 10 7))
                   (assert-equal #\space (frame-ref frame 10 1))))
            
            (define-test diagonal-line
              (let ([frame (make-frame 20 10 #\space)]
                    [config (make-render-config 20 10 1.0 0 0)])
                   (draw-line! frame 0 0 5 5 #\\ config)
                   (assert-equal #\\ (frame-ref frame 0 0))
                   (assert-equal #\\ (frame-ref frame 3 3))
                   (assert-equal #\\ (frame-ref frame 5 5)))))

;;; ============================================================
;;; Circle Rendering Tests
;;; ============================================================

(test-group circle-tests
            (define-test filled-circle-center
              (let ([frame (make-frame 20 20 #\space)]
                    [config (make-render-config 20 20 1.0 0 0)])
                   (draw-filled-circle! frame 10 10 3 #\O config)
                   ;; Center should be filled
                   (assert-equal #\O (frame-ref frame 10 10))
                   ;; Cardinal directions at radius should be filled
                   (assert-equal #\O (frame-ref frame 10 7))
                   (assert-equal #\O (frame-ref frame 10 13))
                   (assert-equal #\O (frame-ref frame 7 10))
                   (assert-equal #\O (frame-ref frame 13 10))))
            
            (define-test filled-circle-outside-empty
              (let ([frame (make-frame 20 20 #\space)]
                    [config (make-render-config 20 20 1.0 0 0)])
                   (draw-filled-circle! frame 10 10 3 #\O config)
                   ;; Outside should be empty
                   (assert-equal #\space (frame-ref frame 0 0))
                   (assert-equal #\space (frame-ref frame 19 19))
                   ;; Just outside radius
                   (assert-equal #\space (frame-ref frame 10 5))
                   (assert-equal #\space (frame-ref frame 10 15))))
            
            (define-test small-circle
              (let ([frame (make-frame 20 20 #\space)]
                    [config (make-render-config 20 20 1.0 0 0)])
                   (draw-filled-circle! frame 5 5 1 #\o config)
                   (assert-equal #\o (frame-ref frame 5 5))
                   (assert-equal #\o (frame-ref frame 4 5))
                   (assert-equal #\o (frame-ref frame 6 5)))))

;;; ============================================================
;;; AABB Rendering Tests
;;; ============================================================

(test-group aabb-tests
            (define-test filled-aabb-interior
              (let ([frame (make-frame 20 20 #\space)]
                    [config (make-render-config 20 20 1.0 0 0)]
                    [aabb (make-aabb (vec2 2 2) (vec2 6 6))])
                   (draw-filled-aabb! frame aabb #\# config)
                   ;; Interior should be filled
                   (assert-equal #\# (frame-ref frame 3 3))
                   (assert-equal #\# (frame-ref frame 4 4))
                   (assert-equal #\# (frame-ref frame 5 5))
                   ;; Corners
                   (assert-equal #\# (frame-ref frame 2 2))
                   (assert-equal #\# (frame-ref frame 6 6))))
            
            (define-test filled-aabb-outside-empty
              (let ([frame (make-frame 20 20 #\space)]
                    [config (make-render-config 20 20 1.0 0 0)]
                    [aabb (make-aabb (vec2 5 5) (vec2 10 10))])
                   (draw-filled-aabb! frame aabb #\# config)
                   (assert-equal #\space (frame-ref frame 0 0))
                   (assert-equal #\space (frame-ref frame 4 5))
                   (assert-equal #\space (frame-ref frame 11 10))))
            
            (define-test aabb-outline
              (let ([frame (make-frame 20 20 #\space)]
                    [config (make-render-config 20 20 1.0 0 0)]
                    [style (default-render-style)]
                    [aabb (make-aabb (vec2 2 2) (vec2 8 6))])
                   (draw-aabb-outline! frame aabb style config)
                   ;; Corners should have corner char
                   (assert-equal #\+ (frame-ref frame 2 2))
                   (assert-equal #\+ (frame-ref frame 8 2))
                   (assert-equal #\+ (frame-ref frame 2 6))
                   (assert-equal #\+ (frame-ref frame 8 6))
                   ;; Horizontal edges
                   (assert-equal #\- (frame-ref frame 5 2))
                   (assert-equal #\- (frame-ref frame 5 6))
                   ;; Vertical edges
                   (assert-equal #\| (frame-ref frame 2 4))
                   (assert-equal #\| (frame-ref frame 8 4))
                   ;; Interior should be empty
                   (assert-equal #\space (frame-ref frame 5 4)))))

;;; ============================================================
;;; Polygon Rendering Tests
;;; ============================================================

(test-group polygon-tests
            (define-test triangle-fill
              (let ([frame (make-frame 20 20 #\space)]
                    [config (make-render-config 20 20 1.0 0 0)]
                    [triangle (list (vec2 10 2) (vec2 15 10) (vec2 5 10))])
                   (draw-filled-polygon! frame triangle #\^ config)
                   ;; Centroid area should be filled
                   (assert-equal #\^ (frame-ref frame 10 6))
                   (assert-equal #\^ (frame-ref frame 10 8))
                   ;; Outside should be empty
                   (assert-equal #\space (frame-ref frame 0 0))
                   (assert-equal #\space (frame-ref frame 19 19))))
            
            (define-test square-fill
              (let ([frame (make-frame 20 20 #\space)]
                    [config (make-render-config 20 20 1.0 0 0)]
                    [square (list (vec2 3 3) (vec2 7 3) (vec2 7 7) (vec2 3 7))])
                   (draw-filled-polygon! frame square #\# config)
                   ;; Center
                   (assert-equal #\# (frame-ref frame 5 5))
                   ;; Interior points (scanline may not include exact max-y edge)
                   (assert-equal #\# (frame-ref frame 3 3))
                   (assert-equal #\# (frame-ref frame 6 6)))))

;;; ============================================================
;;; Arrow Direction Tests
;;; ============================================================

(test-group arrow-tests
            (define-test arrow-right
              (assert-equal #\> (angle->arrow-char 0)))
            
            (define-test arrow-down
              (let ([pi 3.141592653589793])
                   (assert-equal #\v (angle->arrow-char (/ pi 2)))))
            
            (define-test arrow-left
              (let ([pi 3.141592653589793])
                   (assert-equal #\< (angle->arrow-char pi))))
            
            (define-test arrow-up
              (let ([pi 3.141592653589793])
                   (assert-equal #\^ (angle->arrow-char (* 1.5 pi))))))

;;; ============================================================
;;; Integration Tests
;;; ============================================================

(test-group integration-tests
            (define-test render-simple-world
              (let* ([config (make-render-config 40 20 2.0 20 10)]
                     [frame (make-frame 40 20 #\space)]
                     [world (make-world (vec2 0 9.8))]
                     [material (make-material 0.5 0.3 0.3)])
                    ;; Add a circle entity
                    (world-add-entity! world
                                       (make-circle-entity 'ball (vec2 0 0) 1.0 1.0 material))
                    ;; Render
                    (render-world-simple! frame world config)
                    ;; Should have something at origin
                    (assert-equal #\O (frame-ref frame 20 10))))
            
            (define-test world-renderer-with-debug
              (let* ([config (make-render-config 40 20 2.0 20 10)]
                     [debug (make-debug-options #f #f #f #t)]  ; AABBs only
                     [style (default-render-style)]
                     [renderer (make-world-renderer config debug style)]
                     [frame (make-frame 40 20 #\space)]
                     [world (make-world (vec2 0 9.8))]
                     [material (make-material 0.5 0.3 0.3)])
                    (world-add-entity! world
                                       (make-circle-entity 'ball (vec2 0 0) 2.0 1.0 material))
                    (render-world! frame world renderer #f)
                    ;; Entity should be rendered
                    (assert-equal #\O (frame-ref frame 20 10)))))

;;; ============================================================
;;; Run All Tests
;;; ============================================================

(run-all-tests)
