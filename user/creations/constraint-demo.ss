;;; user/creations/constraint-demo.ss --- Physics Constraint System Demo
;;;
;;; Shows off the 2D physics constraint system:
;;;   - Swinging pendulum (distance constraint)
;;;   - Chain of bodies (multiple distance constraints)
;;;   - Spring oscillation
;;;
;;; Renders to ASCII video and exports as GIF.

(load "user/creations/ascii-video.ss")
(load "user/creations/ascii-video-export.ss")
(load "lattice/physics/classical/world.ss")

;;; ====
;;; Configuration
;;; ====

(define *width* 60)
(define *height* 30)
(define *scale* 2.0)  ; pixels per physics unit
(define *origin-x* 30)
(define *origin-y* 5)

;;; Convert physics position to screen coordinates
(define (world->screen pos)
  (let ([x (vec2-x pos)]
        [y (vec2-y pos)])
       (cons (inexact->exact (round (+ *origin-x* (* x *scale*))))
             (inexact->exact (round (+ *origin-y* (* y *scale*)))))))

;;; ====
;;; Rendering
;;; ====

(define test-material (make-material 0.5 0.3 0.3))

;;; Draw a circle entity
(define (draw-entity! frame world id char)
  (let ([entity (world-get-entity world id)])
       (when entity
             (let* ([body (entity-body entity)]
                    [pos (if (rigid-body? body)
                             (rigid-body-pos body)
                             (body-pos body))]
                    [screen (world->screen pos)]
                    [sx (car screen)]
                    [sy (cdr screen)])
                   (when (and (>= sx 0) (< sx *width*)
                              (>= sy 0) (< sy *height*))
                         (frame-set! frame sx sy char))))))

;;; Draw a constraint as a line
(define (draw-constraint! frame world constraint char)
  (let* ([entity-a-id (constraint-entity-a constraint)]
         [entity-b-id (constraint-entity-b constraint)]
         [entity-a (world-get-entity world entity-a-id)]
         [entity-b (and entity-b-id (world-get-entity world entity-b-id))])
        (when entity-a
              (let* ([pos-a (constraint-anchor-world-a world constraint)]
                     [pos-b (constraint-anchor-world-b world constraint)]
                     [screen-a (world->screen pos-a)]
                     [screen-b (world->screen pos-b)])
                    (draw-line! frame
                                (car screen-a) (cdr screen-a)
                                (car screen-b) (cdr screen-b)
                                char)))))

;;; Simple line drawing (Bresenham)
(define (draw-line! frame x0 y0 x1 y1 char)
  (let* ([dx (abs (- x1 x0))]
         [dy (abs (- y1 y0))]
         [sx (if (< x0 x1) 1 -1)]
         [sy (if (< y0 y1) 1 -1)]
         [err (- dx dy)])
        (let loop ([x x0] [y y0] [err err])
             (when (and (>= x 0) (< x *width*)
                        (>= y 0) (< y *height*))
                   (frame-set! frame x y char))
             (unless (and (= x x1) (= y y1))
                     (let ([e2 (* 2 err)])
                          (let ([new-x (if (> e2 (- dy)) (+ x sx) x)]
                                [new-y (if (< e2 dx) (+ y sy) y)]
                                [new-err (+ err
                                            (if (> e2 (- dy)) (- dy) 0)
                                            (if (< e2 dx) dx 0))])
                               (loop new-x new-y new-err)))))))

;;; ====
;;; Scene: Pendulum + Chain + Spring
;;; ====

(define (make-demo-world)
  (let ([w (make-world (vec2 0 9.8))])  ; Gravity down
       
       ;; === Pendulum (left side) ===
       ;; Anchor at (-10, 0), ball swings from (-5, 5)
       (world-add-entity! w (make-static-circle 'anchor1 (vec2 -10 0) 0.3 test-material))
       (world-add-entity! w (make-circle-entity 'pendulum (vec2 -5 5) 1.0 2 test-material))
       (world-add-constraint! w (make-distance-joint 'rod1 'pendulum 'anchor1
                                                     (vec2 0 0) (vec2 0 0) 7))
       
       ;; === Chain (center) ===
       ;; 4-segment chain from anchor at (0, 0)
       (world-add-entity! w (make-static-circle 'anchor2 (vec2 0 0) 0.3 test-material))
       (world-add-entity! w (make-circle-entity 'c1 (vec2 0 3) 0.6 1 test-material))
       (world-add-entity! w (make-circle-entity 'c2 (vec2 0 6) 0.6 1 test-material))
       (world-add-entity! w (make-circle-entity 'c3 (vec2 0 9) 0.6 1 test-material))
       (world-add-entity! w (make-circle-entity 'c4 (vec2 2 10) 0.8 1.5 test-material))
       
       (world-add-constraint! w (make-distance-joint 'chain1 'anchor2 'c1 (vec2 0 0) (vec2 0 0) 3))
       (world-add-constraint! w (make-distance-joint 'chain2 'c1 'c2 (vec2 0 0) (vec2 0 0) 3))
       (world-add-constraint! w (make-distance-joint 'chain3 'c2 'c3 (vec2 0 0) (vec2 0 0) 3))
       (world-add-constraint! w (make-distance-joint 'chain4 'c3 'c4 (vec2 0 0) (vec2 0 0) 3))
       
       ;; Give chain some initial horizontal push
       (set-velocity! w 'c4 (vec2 15 0))
       
       ;; === Spring (right side) ===
       ;; Anchor at (10, 0), mass bounces on spring
       (world-add-entity! w (make-static-circle 'anchor3 (vec2 10 0) 0.3 test-material))
       (world-add-entity! w (make-circle-entity 'spring-mass (vec2 10 8) 1.0 2 test-material))
       (world-add-constraint! w (make-spring-joint 'spring1 'anchor3 'spring-mass
                                                   (vec2 0 0) (vec2 0 0)
                                                   5 40 3))  ; rest=5, stiffness=40, damping=3
       
       w))

(define (render-frame! frame world)
  ;; Clear
  (frame-clear! frame #\space)
  
  ;; Draw border
  (frame-draw-box! frame 0 0 *width* *height*)
  
  ;; Draw title
  (frame-put-string! frame 2 1 "Physics Constraints Demo")
  
  ;; Labels
  (frame-put-string! frame 5 3 "Pendulum")
  (frame-put-string! frame 25 3 "Chain")
  (frame-put-string! frame 45 3 "Spring")
  
  ;; Draw constraints (rods/springs as lines)
  (for-each (lambda (c)
                    (let ([type (constraint-type c)])
                         (cond [(eq? type 'distance)
                                (draw-constraint! frame world c #\-)]
                               [(eq? type 'spring)
                                (draw-constraint! frame world c #\~)])))
            (world-constraint-list world))
  
  ;; Draw entities
  ;; Anchors
  (draw-entity! frame world 'anchor1 #\+)
  (draw-entity! frame world 'anchor2 #\+)
  (draw-entity! frame world 'anchor3 #\+)
  
  ;; Pendulum
  (draw-entity! frame world 'pendulum #\O)
  
  ;; Chain
  (draw-entity! frame world 'c1 #\o)
  (draw-entity! frame world 'c2 #\o)
  (draw-entity! frame world 'c3 #\o)
  (draw-entity! frame world 'c4 #\@)
  
  ;; Spring mass
  (draw-entity! frame world 'spring-mass #\O))

;;; ====
;;; Animation
;;; ====

(define (run-demo)
  (display "Creating physics world...\n")
  (let* ([world (make-demo-world)]
         [video (make-video)]
         [frame (make-frame *width* *height* #\space)]
         [dt (config-fixed-dt (world-config world))]
         [num-frames 150]
         [steps-per-frame 2])
        
        (display "Recording ")
        (display num-frames)
        (display " frames...\n")
        
        ;; Record frames
        (do ([i 0 (+ i 1)])
            ((>= i num-frames))
            ;; Step physics
            (do ([s 0 (+ s 1)])
                ((>= s steps-per-frame))
                (world-step! world dt))
            ;; Render
            (render-frame! frame world)
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

;;; ====
;;; Export
;;; ====

(define (export-demo)
  (let ([video (run-demo)])
       (display "\nExporting to GIF...\n")
       (video->gif video "user/creations/constraint-demo.gif" 67)))  ; ~15fps

;;; Run it
(display "\n")
(display "====\n")
(display "  PHYSICS CONSTRAINT DEMO\n")
(display "  Distance joints, chains, springs\n")
(display "====\n\n")

(export-demo)
