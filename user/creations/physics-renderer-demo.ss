;;; user/creations/physics-renderer-demo.ss --- Physics ASCII Renderer Demo
;;;
;;; A showcase of the new ASCII physics renderer with:
;;;   - Swinging pendulums
;;;   - Bouncing balls
;;;   - Chain dynamics
;;;   - Velocity vectors

(load "lattice/physics/classical/ascii-renderer.ss")
(load "user/creations/ascii-video-export.ss")

;;; ============================================================
;;; Configuration
;;; ============================================================

(define *width* 70)
(define *height* 35)
(define *scale* 2.5)
(define *origin-x* 35)
(define *origin-y* 8)

(define config (make-render-config *width* *height* *scale* *origin-x* *origin-y*))
(define debug (make-debug-options #t #f #t #f))  ; velocity + constraints
(define style (default-render-style))
(define renderer (make-world-renderer config debug style))

;;; ============================================================
;;; Create the Demo World
;;; ============================================================

(define (make-demo-world)
  (let ([w (make-world (vec2 0 9.8))]
        [mat (make-material 0.7 0.3 0.2)])
       
       ;; === Triple Pendulum (left) ===
       (world-add-entity! w (make-static-circle 'piv1 (vec2 -12 0) 0.4 mat))
       (world-add-entity! w (make-circle-entity 'p1 (vec2 -12 4) 1.2 1.5 mat))
       (world-add-entity! w (make-circle-entity 'p2 (vec2 -12 8) 1.0 1.2 mat))
       (world-add-entity! w (make-circle-entity 'p3 (vec2 -10 11) 0.8 1.0 mat))
       
       (world-add-constraint! w (make-distance-joint 'rod1 'piv1 'p1 (vec2 0 0) (vec2 0 0) 4))
       (world-add-constraint! w (make-distance-joint 'rod2 'p1 'p2 (vec2 0 0) (vec2 0 0) 4))
       (world-add-constraint! w (make-distance-joint 'rod3 'p2 'p3 (vec2 0 0) (vec2 0 0) 4))
       
       ;; Give it an initial push
       (set-velocity! w 'p3 (vec2 12 -5))
       
       ;; === Bouncing Chain (center) ===
       (world-add-entity! w (make-static-circle 'piv2 (vec2 0 -2) 0.4 mat))
       (world-add-entity! w (make-circle-entity 'c1 (vec2 0 1) 0.7 0.8 mat))
       (world-add-entity! w (make-circle-entity 'c2 (vec2 0 4) 0.7 0.8 mat))
       (world-add-entity! w (make-circle-entity 'c3 (vec2 0 7) 0.7 0.8 mat))
       (world-add-entity! w (make-circle-entity 'c4 (vec2 0 10) 1.0 1.5 mat))
       
       (world-add-constraint! w (make-distance-joint 'ch1 'piv2 'c1 (vec2 0 0) (vec2 0 0) 3))
       (world-add-constraint! w (make-distance-joint 'ch2 'c1 'c2 (vec2 0 0) (vec2 0 0) 3))
       (world-add-constraint! w (make-distance-joint 'ch3 'c2 'c3 (vec2 0 0) (vec2 0 0) 3))
       (world-add-constraint! w (make-distance-joint 'ch4 'c3 'c4 (vec2 0 0) (vec2 0 0) 3))
       
       (set-velocity! w 'c4 (vec2 8 0))
       
       ;; === Spring Oscillator (right) ===
       (world-add-entity! w (make-static-circle 'piv3 (vec2 12 0) 0.4 mat))
       (world-add-entity! w (make-circle-entity 'spring-ball (vec2 12 10) 1.5 2.0 mat))
       
       (world-add-constraint! w (make-spring-joint 'spring1 'piv3 'spring-ball
                                                   (vec2 0 0) (vec2 0 0)
                                                   6 35 2))
       
       w))

;;; ============================================================
;;; Custom Entity Styling
;;; ============================================================

(define (entity->char entity)
  (let ([id (entity-id entity)])
       (cond
        ;; Pivots/anchors
        [(memq id '(piv1 piv2 piv3)) #\#]
        ;; Triple pendulum
        [(eq? id 'p1) #\@]
        [(eq? id 'p2) #\O]
        [(eq? id 'p3) #\o]
        ;; Chain
        [(memq id '(c1 c2 c3)) #\o]
        [(eq? id 'c4) #\@]
        ;; Spring
        [(eq? id 'spring-ball) #\*]
        ;; Default
        [else #\O])))

;;; ============================================================
;;; Render Frame with Title
;;; ============================================================

(define (render-demo-frame! frame world step)
  (frame-clear! frame #\space)
  
  ;; Draw border
  (frame-draw-box! frame 0 0 *width* *height*)
  
  ;; Title
  (frame-put-string! frame 2 1 "2D Physics ASCII Renderer")
  (frame-put-string! frame 2 2 "Triple Pendulum | Chain | Spring")
  
  ;; Render physics world
  (render-world! frame world renderer entity->char)
  
  ;; Frame counter
  (frame-put-string! frame (- *width* 12) 1
                     (string-append "Frame " (number->string step))))

;;; ============================================================
;;; Animation
;;; ============================================================

(define (run-demo)
  (display "Creating physics world...\n")
  (let* ([world (make-demo-world)]
         [video (make-video)]
         [frame (make-frame *width* *height* #\space)]
         [dt 0.016]
         [num-frames 180]
         [steps-per-frame 2])
        
        (display "Recording ")
        (display num-frames)
        (display " frames...\n")
        
        (do ([i 0 (+ i 1)])
            ((>= i num-frames))
            ;; Step physics (must use returned world for time accumulator)
            (do ([s 0 (+ s 1)])
                ((>= s steps-per-frame))
                (set! world (world-step! world dt)))
            ;; Render
            (render-demo-frame! frame world i)
            (video-add-frame! video frame)
            ;; Progress
            (when (= (mod i 30) 0)
                  (display "  Frame ")
                  (display i)
                  (display "/")
                  (display num-frames)
                  (newline)))
        
        (display "Animation recorded: ")
        (display (video-frame-count video))
        (display " frames\n")
        video))

;;; ============================================================
;;; Export
;;; ============================================================

(define (export-demo)
  (let ([video (run-demo)])
       (display "\nExporting to GIF...\n")
       (video->gif video "user/creations/physics-renderer-demo.gif" 50)
       (display "Done! Saved to user/creations/physics-renderer-demo.gif\n")))

;;; Run it
(display "\n")
(display "========================================\n")
(display "  PHYSICS ASCII RENDERER DEMO\n")
(display "  Triple pendulum, chain, spring\n")
(display "========================================\n\n")

(export-demo)
