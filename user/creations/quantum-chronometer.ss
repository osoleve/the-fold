;;; quantum-chronometer.ss --- "The Quantum Chronometer"
;;;
;;; A complex looping scene designed in collaboration with Gemini 3 Pro.
;;; Showcases morphing, coprime rhythms, Lissajous path tracing, and orbiting camera.
;;;
;;; Visual: A living crystal morphs between icosahedron and sphere,
;;; contained by three tumbling rings on different axes, with an energy
;;; particle weaving a Lissajous knot around everything.

(load "user/creations/loop-scene.ss")

(display "\n")
(display "============================================================\n")
(display "  THE QUANTUM CHRONOMETER\n")
(display "  (Designed with Gemini 3 Pro)\n")
(display "============================================================\n\n")

(define quantum-chronometer
  (build-loop-scene-animated-camera
   ;; Scene Config
   '((duration . 6.0)      ; 6 seconds allows 2, 3, and 5 cycles to sync perfectly
     (fps . 30)            ; Standard smoothness
     (width . 100)         ; Wide aspect for cinematic feel
     (height . 50))        ; Tall enough for the vertical tumbles
   
   ;; 1. Camera: Slowly orbits the artifact once per loop
   (make-orbiting-camera 4.5 1.8 '(0 0 0) 1)
   
   ;; 2. The Core: Morphs between Order (Icosahedron) and Perfection (Sphere)
   ;;    Fast oscillation (3 cycles) creates a beating heart effect.
   (morph
    (icosahedron :scale 0.7 :axis '(0 1 0) :cycles 3)
    (ball :r 0.72 :at '(0 0 0))
    :cycles 3)
   
   ;; 3. The Containment Field (Tumbling Rings)
   ;;    Ring 1: Tumbles around X-axis (Vertical flip)
   (ring :R 1.4 :r 0.05 :axis '(1 0 0) :cycles 2)
   
   ;;    Ring 2: Tumbles around Z-axis (Sideways flip)
   (ring :R 1.7 :r 0.05 :axis '(0 0 1) :cycles 3)
   
   ;;    Ring 3: Tumbles around diagonal axis (Complex wobble)
   (ring :R 2.0 :r 0.05 :axis '(1 1 1) :cycles 5)
   
   ;; 4. The Energy Trace (Lissajous Knot)
   ;;    A high-speed particle weaving a cage.
   ;;    Frequencies 3 and 4 create a complex knot.
   (tracer :r 0.12 :a 3 :b 4 :delta 1.57 :scale 2.6 :cycles 1)
   ))

(display "Rendering The Quantum Chronometer...\n\n")

;;; Render to GIF
(render-animated-camera-loop! quantum-chronometer "user/creations/quantum-chronometer.gif")

(display "\nDone! Checking output...\n")
(system "ls -lh user/creations/quantum-chronometer.gif")
(system "file user/creations/quantum-chronometer.gif")

;;; Post to Discord
(display "\nPosting to Discord #art...\n")
(load "shell/discord/post-with-files.ss")

(discord-post-files 'art
                    "The Quantum Chronometer (Claude + Gemini Collab)"
                    "A complex looping scene designed in collaboration with Gemini 3 Pro. Features: morphing icosahedron↔sphere core, 3 tumbling rings on coprime axes (2, 3, 5 cycles), Lissajous energy trace, orbiting camera. 100x50 ASCII at 30fps, 6-second perfect loop. Built with the new loop-scene DSL."
                    '("user/creations/quantum-chronometer.gif"))

(display "\n")
(display "============================================================\n")
(display "  Posted to Discord #art!\n")
(display "============================================================\n")
