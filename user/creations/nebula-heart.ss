;;; nebula-heart.ss --- "Nebula Heart"
;;;
;;; A second complex scene - breathing blobs with orbiting tracers.
;;; Demonstrates the blob smooth-union and multiple Lissajous tracers.

(load "user/creations/loop-scene.ss")

(display "\n")
(display "============================================================\n")
(display "  NEBULA HEART\n")
(display "  (Breathing blob with orbiting energy)\n")
(display "============================================================\n\n")

(define nebula-heart
  (build-loop-scene
   `((duration . 5.0)      ; 5 seconds
     (fps . 30)            ; 30fps
     (width . 100)
     (height . 50)
     (camera . ,(look-at '(0 0 -5) '(0 0 0))))
   
   ;; The core: breathing metaballs that pulse together
   (blob :positions '((0 0 0) (0.7 0.3 0) (-0.7 0.3 0) (0 -0.6 0.2) (0 0.5 -0.3))
         :r 0.5
         :k-base 0.6
         :k-range 0.25
         :cycles 3)
   
   ;; Orbiting energy rings
   (ring :R 1.8 :r 0.04 :axis '(0.3 1 0) :cycles 2)
   (ring :R 2.2 :r 0.03 :axis '(1 0.2 0.5) :cycles 3)
   
   ;; Two tracers weaving opposite Lissajous patterns
   (tracer :r 0.1 :a 2 :b 3 :delta 0 :scale 2.0 :cycles 1)
   (tracer :r 0.08 :a 3 :b 2 :delta 1.57 :scale 1.8 :cycles 2)
   ))

(display "Rendering Nebula Heart...\n\n")

;;; Render to GIF
(render-loop! nebula-heart "user/creations/nebula-heart.gif")

(display "\nDone! Checking output...\n")
(system "ls -lh user/creations/nebula-heart.gif")

;;; Post to Discord
(display "\nPosting to Discord #art...\n")
(load "shell/discord/post-with-files.ss")

(discord-post-files 'art
                    "Nebula Heart"
                    "A breathing blob core with orbiting rings and twin Lissajous tracers. 5 metaball centers with animated smooth-union (k oscillates 0.35-0.85). 100x50 ASCII at 30fps, 5-second loop. Built with loop-scene DSL."
                    '("user/creations/nebula-heart.gif"))

(display "\n")
(display "============================================================\n")
(display "  Posted to Discord #art!\n")
(display "============================================================\n")
