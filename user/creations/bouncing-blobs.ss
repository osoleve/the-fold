;;; bouncing-blobs.ss --- "Bouncing Blobs"
;;;
;;; 5 spheroid blobs bouncing in a 3D box with smooth interactions.
;;; Uses sinusoidal motion with integer frequencies for perfect looping.
;;; Blobs merge smoothly when they touch (smooth union).

(load "user/creations/loop-scene.ss")

(display "\n")
(display "============================================================\n")
(display "  BOUNCING BLOBS\n")
(display "  5 blobs in a box with smooth collision interactions\n")
(display "============================================================\n\n")

;;; ============================================================
;;; Blob Bounce Physics (Periodic for Perfect Looping)
;;; ============================================================

;;; Each blob bounces with sinusoidal motion on each axis.
;;; Using integer frequencies ensures perfect looping.
;;; Different phases create asynchronous, natural-looking motion.

(define (make-bouncing-centers)
  "Create 5 blob centers that bounce around inside a box.
   Box bounds: roughly -1.2 to 1.2 on each axis."
  (let* (;; Blob 1: Slow diagonal bounce
         [b1-fx 2] [b1-fy 3] [b1-fz 2]
         [b1-px 0.0] [b1-py 0.0] [b1-pz 1.0]
         ;; Blob 2: Fast vertical bouncer
         [b2-fx 3] [b2-fy 5] [b2-fz 2]
         [b2-px 1.5] [b2-py 0.5] [b2-pz 0.0]
         ;; Blob 3: Horizontal zigzag
         [b3-fx 4] [b3-fy 2] [b3-fz 3]
         [b3-px 0.8] [b3-py 2.0] [b3-pz 1.2]
         ;; Blob 4: Chaotic-ish (higher frequencies)
         [b4-fx 5] [b4-fy 4] [b4-fz 3]
         [b4-px 2.5] [b4-py 1.0] [b4-pz 2.0]
         ;; Blob 5: Slow wanderer
         [b5-fx 2] [b5-fy 2] [b5-fz 4]
         [b5-px 3.14] [b5-py 1.57] [b5-pz 0.5]
         ;; Box size (amplitude of motion)
         [amp 1.0])
        
        (lambda (t)
                (list
                 ;; Blob 1
                 (vec3 (* amp (sin (+ (* b1-fx t) b1-px)))
                       (* amp (sin (+ (* b1-fy t) b1-py)))
                       (* amp (sin (+ (* b1-fz t) b1-pz))))
                 ;; Blob 2
                 (vec3 (* amp (sin (+ (* b2-fx t) b2-px)))
                       (* amp (sin (+ (* b2-fy t) b2-py)))
                       (* amp (sin (+ (* b2-fz t) b2-pz))))
                 ;; Blob 3
                 (vec3 (* amp (sin (+ (* b3-fx t) b3-px)))
                       (* amp (sin (+ (* b3-fy t) b3-py)))
                       (* amp (sin (+ (* b3-fz t) b3-pz))))
                 ;; Blob 4
                 (vec3 (* amp (sin (+ (* b4-fx t) b4-px)))
                       (* amp (sin (+ (* b4-fy t) b4-py)))
                       (* amp (sin (+ (* b4-fz t) b4-pz))))
                 ;; Blob 5
                 (vec3 (* amp (sin (+ (* b5-fx t) b5-px)))
                       (* amp (sin (+ (* b5-fy t) b5-py)))
                       (* amp (sin (+ (* b5-fz t) b5-pz))))))))

;;; ============================================================
;;; Box Wireframe (edges only)
;;; ============================================================

(define (box-edge p1 p2 thickness)
  "Create a capsule/cylinder edge between two points"
  (let ([v1 (apply vec3 p1)]
        [v2 (apply vec3 p2)])
       (make-static-sphere thickness v1)))  ; Simplified - just corner spheres

(define (make-box-frame size thickness)
  "Create a wireframe box as static element.
   Returns list of corner spheres for visibility."
  (let ([s size]
        [r thickness])
       ;; 8 corner spheres to outline the box
       (list
        (make-static-sphere r (vec3 s s s))
        (make-static-sphere r (vec3 s s (- s)))
        (make-static-sphere r (vec3 s (- s) s))
        (make-static-sphere r (vec3 s (- s) (- s)))
        (make-static-sphere r (vec3 (- s) s s))
        (make-static-sphere r (vec3 (- s) s (- s)))
        (make-static-sphere r (vec3 (- s) (- s) s))
        (make-static-sphere r (vec3 (- s) (- s) (- s))))))

;;; ============================================================
;;; The Scene
;;; ============================================================

(define bouncing-blobs
  (build-loop-scene-animated-camera
   '((duration . 5.0)      ; 5 seconds
     (fps . 30)
     (width . 200)
     (height . 100))
   
   ;; Camera orbits around Y axis: distance 5.5, height 2.5, 1 rotation per loop
   (make-orbiting-camera 5.5 2.5 '(0 0 0) 1)
   
   ;; THE BLOBS - 5 bouncing spheres with smooth union
   (dynamic-blob
    :center-fn (make-bouncing-centers)
    :r 0.34                ; Blob radius (25% smaller)
    :k-base 0.35           ; Smooth union factor (higher = more merge)
    :k-range 0.15          ; Breathing effect on smoothness
    :cycles 3)             ; Smoothness oscillates 3x per loop
   
   ;; BOX CORNERS - 8 small spheres marking the containment box
   (ball :r 0.08 :at '(1.3 1.3 1.3))
   (ball :r 0.08 :at '(1.3 1.3 -1.3))
   (ball :r 0.08 :at '(1.3 -1.3 1.3))
   (ball :r 0.08 :at '(1.3 -1.3 -1.3))
   (ball :r 0.08 :at '(-1.3 1.3 1.3))
   (ball :r 0.08 :at '(-1.3 1.3 -1.3))
   (ball :r 0.08 :at '(-1.3 -1.3 1.3))
   (ball :r 0.08 :at '(-1.3 -1.3 -1.3))
   
   ))

(display "Rendering Bouncing Blobs...\n\n")

;;; Render to GIF (using animated camera renderer)
(render-animated-camera-loop! bouncing-blobs "user/creations/bouncing-blobs.gif")

(display "\nDone! Checking output...\n")
(system "ls -lh user/creations/bouncing-blobs.gif")

;;; Post to Discord
(display "\nPosting to Discord #art...\n")
(load "shell/discord/post-with-files.ss")

(discord-post-files 'art
                    "Bouncing Blobs"
                    "5 spheroid blobs bouncing inside a 3D box with smooth collision interactions. When blobs touch, they merge smoothly (animated smooth union). Uses sinusoidal motion with integer frequencies (2,3,4,5) for perfect looping. Camera slowly orbits around the Y axis (1 rotation per loop). 200x100 ASCII at 30fps, 5-second loop."
                    '("user/creations/bouncing-blobs.gif"))

(display "\n")
(display "============================================================\n")
(display "  Posted to Discord #art!\n")
(display "============================================================\n")
