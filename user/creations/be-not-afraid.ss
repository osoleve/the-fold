;;; be-not-afraid.ss --- "Be Not Afraid"
;;;
;;; A biblically accurate angel (Ophanim/Throne) - the kind that has to
;;; say "be not afraid" because they're terrifying eldritch geometry.
;;;
;;; Features:
;;; - Wheels within wheels (interlocking rings at orthogonal angles)
;;; - Many eyes (orbiting spheres at various radii)
;;; - Six wings (three pairs of tilted rings)
;;; - Radiant core (pulsing sphere)
;;; - Divine energy (Lissajous traces)

(load "user/creations/loop-scene.ss")

(display "\n")
(display "============================================================\n")
(display "  BE NOT AFRAID\n")
(display "  A Biblically Accurate Angel (Ophanim)\n")
(display "============================================================\n\n")

;;; Helper: create an "eye" that orbits on a tilted plane
(define (eye radius orbit-r tilt-axis cycles)
  "An eye (small sphere) orbiting on a tilted plane"
  (let ([norm-axis (vec3-normalize (apply vec3 tilt-axis))])
       (lambda (t)
               (let* ([angle (* t cycles)]
                      ;; Base position on XZ plane
                      [base-x (* orbit-r (cos angle))]
                      [base-z (* orbit-r (sin angle))]
                      [base-pos (vec3 base-x 0 base-z)]
                      ;; Rotate to tilted plane
                      [pos (rotate-axis base-pos norm-axis (* 0.5 (acos (vec3-y norm-axis))))])
                     (lambda (p)
                             (sdf-sphere p pos radius))))))

;;; The Angel Scene
(define be-not-afraid
  (build-loop-scene
   `((duration . 6.0)      ; 6 seconds for complex motion
     (fps . 30)
     (width . 120)         ; Wide for the majesty
     (height . 60)
     (camera . ,(look-at '(0 0 -6) '(0 0 0))))
   
   ;; ═══════════════════════════════════════════════════════════
   ;; THE RADIANT CORE - Divine presence
   ;; ═══════════════════════════════════════════════════════════
   (core :r 0.35 :pulse 0.08 :cycles 4)
   
   ;; ═══════════════════════════════════════════════════════════
   ;; THE WHEELS (Ophanim) - "wheels within wheels"
   ;; Three interlocking rings at orthogonal angles
   ;; ═══════════════════════════════════════════════════════════
   
   ;; Inner wheel - XY plane (vertical ring)
   (ring :R 1.0 :r 0.04 :up '(0 0 1) :axis '(1 0 0) :cycles 3)
   
   ;; Inner wheel - XZ plane (horizontal ring)
   (ring :R 1.0 :r 0.04 :up '(0 1 0) :axis '(0 1 0) :cycles 5)
   
   ;; Inner wheel - YZ plane (side ring)
   (ring :R 1.0 :r 0.04 :up '(1 0 0) :axis '(0 0 1) :cycles 7)
   
   ;; Outer wheel - tilted, slow majestic rotation
   (ring :R 1.6 :r 0.05 :up '(0.3 1 0) :axis '(0.3 1 0.2) :cycles 2)
   
   ;; Outermost wheel - opposite tilt
   (ring :R 2.0 :r 0.04 :up '(-0.3 1 0.2) :axis '(-0.2 1 0.3) :cycles 3)
   
   ;; ═══════════════════════════════════════════════════════════
   ;; THE WINGS - Six wings (3 pairs), flapping via rotation
   ;; "With two they covered their face, with two they covered
   ;; their feet, and with two they flew"
   ;; ═══════════════════════════════════════════════════════════
   
   ;; Wing pair 1 - covering face (top, tilted forward)
   (ring :R 2.4 :r 0.025 :up '(0 1 0.8) :axis '(1 0 0) :cycles 4)
   (ring :R 2.4 :r 0.025 :up '(0 1 -0.8) :axis '(-1 0 0) :cycles 4)
   
   ;; Wing pair 2 - covering feet (bottom, tilted back)
   (ring :R 2.3 :r 0.025 :up '(0 -1 0.6) :axis '(0 0 1) :cycles 6)
   (ring :R 2.3 :r 0.025 :up '(0 -1 -0.6) :axis '(0 0 -1) :cycles 6)
   
   ;; Wing pair 3 - for flight (sides, horizontal motion)
   (ring :R 2.5 :r 0.03 :up '(1 0.3 0) :axis '(0 1 0) :cycles 5)
   (ring :R 2.5 :r 0.03 :up '(-1 0.3 0) :axis '(0 -1 0) :cycles 5)
   
   ;; ═══════════════════════════════════════════════════════════
   ;; THE EYES - "covered with eyes all around"
   ;; Multiple small spheres orbiting at different rates
   ;; ═══════════════════════════════════════════════════════════
   
   ;; Inner ring of eyes (6 eyes on inner wheel)
   (orb :r 0.08 :orbit 1.0 :cycles 3)
   (orb :r 0.08 :orbit 1.0 :cycles 5)
   
   ;; Middle ring of eyes
   (orb :r 0.06 :orbit 1.6 :cycles 2)
   (orb :r 0.06 :orbit 1.6 :cycles 7)
   
   ;; Outer eyes
   (orb :r 0.05 :orbit 2.0 :cycles 3)
   (orb :r 0.05 :orbit 2.0 :cycles 11)
   
   ;; Eyes on vertical orbits (using tracers for different plane)
   (tracer :r 0.07 :a 1 :b 1 :delta 0 :scale 1.3 :cycles 4)
   (tracer :r 0.07 :a 1 :b 1 :delta 1.57 :scale 1.3 :cycles 4)
   
   ;; ═══════════════════════════════════════════════════════════
   ;; DIVINE RADIANCE - Energy traces
   ;; ═══════════════════════════════════════════════════════════
   (tracer :r 0.04 :a 3 :b 2 :delta 0 :scale 2.8 :cycles 1)
   (tracer :r 0.04 :a 2 :b 3 :delta 1.57 :scale 2.8 :cycles 1)
   ))

(display "Rendering Be Not Afraid...\n")
(display "(This is a complex scene, may take a moment)\n\n")

;;; Render to GIF
(render-loop! be-not-afraid "user/creations/be-not-afraid.gif")

(display "\nDone! Checking output...\n")
(system "ls -lh user/creations/be-not-afraid.gif")

;;; Post to Discord
(display "\nPosting to Discord #art...\n")
(load "shell/discord/post-with-files.ss")

(discord-post-files 'art
                    "BE NOT AFRAID - Biblically Accurate Angel"
                    "An Ophanim (Throne) - the kind of angel that has to say 'be not afraid' because they're terrifying eldritch geometry. Features: wheels within wheels (5 interlocking rings), six wings (3 pairs), many eyes (8 orbiting spheres + 4 tracers), radiant pulsing core, divine energy traces. 120x60 ASCII at 30fps, 6-second loop."
                    '("user/creations/be-not-afraid.gif"))

(display "\n")
(display "============================================================\n")
(display "  BE NOT AFRAID - Posted to Discord #art\n")
(display "============================================================\n")
