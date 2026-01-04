;;; cosmic-garden.ss — Procedural Alien Plant Generator
;;;
;;; A generative art system that creates otherworldly plants using:
;;; - Fractal branching patterns
;;; - Organic curves and tendrils
;;; - Vibrant cosmic color palettes
;;; - L-system inspired growth rules
;;; - Geometric crystal formations
;;;
;;; 12 Plant Types: Fractal trees, Tendril clusters, Blooms, Spirals,
;;;                 Hybrid plants, Vines, Mushrooms, Crystals, Coral,
;;;                 Lightning, Bamboo, Tentacles
;;;
;;; Usage:
;;;   (apply-patch 'turtle)
;;;   (load "user/creations/cosmic-garden.ss")
;;;   (grow-cosmic-plant)          ; Generate a random plant
;;;   (cosmic-garden 3)            ; Generate a garden with 3 plants
;;;   (define p (grow-cosmic-plant))
;;;   (save-svg (turtle->drawing p) "my-plant.svg")

;;; ============================================================
;;; Color Palettes — Cosmic Themes
;;; ============================================================

(define *cosmic-palettes*
  '((nebula . (#xf06 #xfb5 #xff8 #xffb #x83e))
    (aurora . (#x0fa #x0f5 #x0bf #x709 #xf25))
    (sunset . (#xf46 #xff6 #xff9 #xffc #xff8))
    (ocean  . (#x07b #x09c #x0bd #x48c #x90e))
    (forest . (#x5a6 #x8b9 #xac0 #xbf2 #xd47))
    (void   . (#x94e #xc7f #xe0a #xc9a #xfcd))))

(define (random-palette)
  (let* ([palettes (map cdr *cosmic-palettes*)]
         [n (length palettes)])
        (list-ref palettes (random n))))

(define (random-color palette)
  (color12-from-int (list-ref palette (random (length palette)))))

;;; ============================================================
;;; Random Utilities
;;; ============================================================

(define (random-float min max)
  ;; Generate random float in range [min, max)
  (+ min (* (/ (random 10000) 10000.0) (- max min))))

(define (random-int min max-val)
  (+ min (random (max 1 (- max-val min)))))

;;; ============================================================
;;; Turtle State Stack (for save/restore)
;;; ============================================================

;;; turtle-state : Turtle → (x y heading pen-down?)
(define (turtle-state t)
  (list (turtle-x t)
        (turtle-y t)
        (turtle-heading t)
        (turtle-pen-down? t)))

;;; restore-turtle-state : Turtle × State → Turtle
;;; Restore position/heading/pen from state, preserving paths and other attributes
(define (restore-turtle-state t state)
  (let* ([paths (turtle-paths t)]
         [t (turtle-with-position t (car state) (cadr state) paths)]
         [t (turtle-with-heading t (caddr state))])
        (if (cadddr state)
            (turtle-with-pen-down t #t)
            (turtle-with-pen-down t #f))))

;;; ============================================================
;;; Fractal Branch System
;;; ============================================================

;;; fractal-branch : Turtle × Nat × Real × Real × Real × Palette → Turtle
;;; Draw a fractal branch with organic variation
(define (fractal-branch t depth length angle-var thickness palette)
  (if (<= depth 0)
      t
      (let* ([color (random-color palette)]
             [twist (random-float (- angle-var) angle-var)]
             ;; Draw current branch
             [t (setpw t (max 1 (inexact->exact (floor thickness))))]
             [t (setpc t color)]
             [t (fd t length)]
             ;; Save state
             [state (turtle-state t)]
             ;; Branch left
             [t (lt t (+ 30 twist))]
             [t (fractal-branch t
                                (- depth 1)
                                (* length 0.7)
                                angle-var
                                (* thickness 0.7)
                                palette)]
             ;; Restore and branch right
             [t (restore-turtle-state t state)]
             [t (rt t (+ 30 twist))]
             [t (fractal-branch t
                                (- depth 1)
                                (* length 0.7)
                                angle-var
                                (* thickness 0.7)
                                palette)])
            t)))

;;; ============================================================
;;; Organic Tendrils
;;; ============================================================

;;; draw-tendril : Turtle × Nat × Real × Real × Palette → Turtle
;;; Draw an organic, curving tendril
(define (draw-tendril t segments length curve-strength palette)
  (let loop ([t t] [i 0])
       (if (>= i segments)
           t
           (let* ([color (random-color palette)]
                  [twist (* (sin (* i 0.3)) curve-strength)]
                  [seg-length (* length (+ 0.8 (* 0.2 (cos (* i 0.2)))))]
                  [width (max 1 (inexact->exact (floor (- 5 (* i 0.1)))))]
                  [t (setpc t color)]
                  [t (setpw t width)]
                  [t (fd t seg-length)]
                  [t (rt t twist)])
                 (loop t (+ i 1))))))

;;; ============================================================
;;; Flower/Bloom Patterns
;;; ============================================================

;;; draw-bloom : Turtle × Nat × Real × Palette → Turtle
;;; Draw a radial flower/bloom pattern
(define (draw-bloom t petals radius palette)
  (let ([angle-step (/ 360 petals)])
       (let loop ([t t] [i 0])
            (if (>= i petals)
                t
                (let* ([state (turtle-state t)]
                       ;; Rotate to petal position
                       [t (rt t (* i angle-step))]
                       ;; Draw petal
                       [t (setpc t (random-color palette))]
                       [t (setpw t 3)]
                       [t (fd t radius)]
                       ;; Draw petal curve - left
                       [state2 (turtle-state t)]
                       [t (lt t 60)]
                       [t (fd t (* radius 0.3))]
                       [t (restore-turtle-state t state2)]
                       ;; Draw petal curve - right
                       [t (rt t 60)]
                       [t (fd t (* radius 0.3))]
                       ;; Restore to center
                       [t (restore-turtle-state t state)])
                      (loop t (+ i 1)))))))

;;; ============================================================
;;; Vine Plants
;;; ============================================================

;;; draw-vine : Turtle × Nat × Real × Palette → Turtle
;;; Draw a twisting vine with leaves
(define (draw-vine t segments base-length palette)
  (let loop ([t t] [i 0] [length base-length] [angle 0])
       (if (>= i segments)
           t
           (let* ([twist (+ 15 (* 10 (sin (* i 0.5))))]
                  [leaf-size (* length 0.4)]
                  ;; Draw vine segment
                  [t (setpc t (random-color palette))]
                  [t (setpw t 4)]
                  [t (fd t length)]
                  [t (rt t twist)]
                  ;; Maybe add a leaf
                  [t (if (= (modulo i 3) 0)
                         (let* ([state (turtle-state t)]
                                [t (lt t 60)]
                                [t (setpw t 2)]
                                [t (setpc t (random-color palette))]
                                ;; Simple leaf shape
                                [t (fd t leaf-size)]
                                [t (rt t 120)]
                                [t (fd t leaf-size)]
                                [t (restore-turtle-state t state)])
                               t)
                         t)])
                 (loop t (+ i 1) (* length 0.95) (+ angle twist))))))

;;; ============================================================
;;; Mushroom Plants
;;; ============================================================

;;; draw-mushroom : Turtle × Real × Real × Palette → Turtle
;;; Draw a mushroom with stem and cap
(define (draw-mushroom t stem-height cap-radius palette)
  (let* (;; Draw stem
         [t (setpc t (random-color palette))]
         [t (setpw t 8)]
         [t (fd t stem-height)]
         ;; Draw cap (as a filled circle made of lines)
         [state (turtle-state t)]
         [t (setpc t (random-color palette))]
         [t (setpw t 3)])
        ;; Draw cap as radial lines
        (let loop ([t t] [i 0])
             (if (>= i 24)
                 t
                 (let* ([state2 (turtle-state t)]
                        [angle (* i 15)]
                        [t (rt t angle)]
                        [t (fd t cap-radius)]
                        [t (restore-turtle-state t state2)])
                       (loop t (+ i 1)))))))

;;; ============================================================
;;; Crystal Plants
;;; ============================================================

;;; draw-crystal : Turtle × Nat × Real × Palette → Turtle
;;; Draw a geometric crystal structure
(define (draw-crystal t depth size palette)
  (if (<= depth 0)
      t
      (let* ([sides 6]
             [angle (/ 360 sides)]
             [t (setpc t (random-color palette))]
             [t (setpw t 2)])
            ;; Draw crystal polygon
            (let loop ([t t] [i 0])
                 (if (>= i sides)
                     ;; Add smaller crystals on some vertices
                     (if (> depth 1)
                         (let recurse-loop ([t t] [j 0])
                              (if (>= j 3)
                                  t
                                  (let* ([state (turtle-state t)]
                                         [t (rt t (* j (/ 360 3)))]
                                         [t (fd t (* size 0.5))]
                                         [t (draw-crystal t (- depth 1) (* size 0.5) palette)]
                                         [t (restore-turtle-state t state)])
                                        (recurse-loop t (+ j 1)))))
                         t)
                     (let* ([t (fd t size)]
                            [t (rt t angle)])
                           (loop t (+ i 1))))))))

;;; ============================================================
;;; Coral Plants
;;; ============================================================

;;; draw-coral : Turtle × Nat × Real × Real × Palette → Turtle
;;; Draw coral-like branching structure (rounder than trees)
(define (draw-coral t depth length thickness palette)
  (if (<= depth 0)
      t
      (let* ([branches (random-int 2 5)]
             [t (setpc t (random-color palette))]
             [t (setpw t (max 1 (inexact->exact (floor thickness))))]
             [t (fd t length)])
            ;; Create multiple branches in a fan pattern
            (let loop ([t t] [i 0])
                 (if (>= i branches)
                     t
                     (let* ([state (turtle-state t)]
                            [angle (- (* (/ i (- branches 1)) 120) 60)]
                            [t (rt t angle)]
                            [t (draw-coral t (- depth 1) (* length 0.65) (* thickness 0.7) palette)]
                            [t (restore-turtle-state t state)])
                           (loop t (+ i 1))))))))

;;; ============================================================
;;; Lightning Plants
;;; ============================================================

;;; draw-lightning : Turtle × Nat × Real × Palette → Turtle
;;; Draw jagged, angular lightning-like paths
(define (draw-lightning t segments length palette)
  (let loop ([t t] [i 0])
       (if (>= i segments)
           t
           (let* ([angle (random-int -60 60)]
                  [seg-len (random-float (* length 0.5) (* length 1.5))]
                  [t (setpc t (random-color palette))]
                  [t (setpw t (random-int 1 4))]
                  [t (fd t seg-len)]
                  [t (rt t angle)])
                 ;; Occasionally branch
                 (if (< (random-int 0 100) 30)
                     (let* ([state (turtle-state t)]
                            [branch-angle (if (< (random-int 0 2) 1) -45 45)]
                            [t (rt t branch-angle)]
                            [t (draw-lightning t (quotient segments 2) (* length 0.6) palette)]
                            [t (restore-turtle-state t state)])
                           (loop t (+ i 1)))
                     (loop t (+ i 1)))))))

;;; ============================================================
;;; Bamboo Plants
;;; ============================================================

;;; draw-bamboo : Turtle × Nat × Real × Palette → Turtle
;;; Draw segmented bamboo-like vertical growth
(define (draw-bamboo t segments segment-height palette)
  (let loop ([t t] [i 0])
       (if (>= i segments)
           t
           (let* ([t (setpc t (random-color palette))]
                  [t (setpw t 6)]
                  ;; Draw segment
                  [t (fd t segment-height)]
                  ;; Draw node (thicker ring)
                  [t (setpw t 8)]
                  [t (fd t 3)]
                  [t (setpw t 6)])
                 ;; Maybe add a leaf
                 (if (and (> i 2) (= (modulo i 2) 0))
                     (let* ([state (turtle-state t)]
                            [leaf-dir (if (= (modulo i 4) 0) -45 45)]
                            [t (rt t leaf-dir)]
                            [t (setpw t 2)]
                            [t (setpc t (random-color palette))]
                            [t (fd t 30)]
                            [t (restore-turtle-state t state)])
                           (loop t (+ i 1)))
                     (loop t (+ i 1)))))))

;;; ============================================================
;;; Tentacle Plants
;;; ============================================================

;;; draw-tentacle : Turtle × Nat × Real × Palette → Turtle
;;; Draw chaotic, writhing tentacle-like forms
(define (draw-tentacle t segments base-length palette)
  (let loop ([t t] [i 0] [angle 0])
       (if (>= i segments)
           t
           (let* ([chaos (* (random-float -1.0 1.0) 30)]
                  [wave (* (sin (* i 0.3)) 20)]
                  [twist (+ chaos wave)]
                  [length (* base-length (+ 0.7 (* 0.3 (cos (* i 0.2)))))]
                  [width (max 1 (inexact->exact (floor (- 8 (* i 0.2)))))]
                  [t (setpc t (random-color palette))]
                  [t (setpw t width)]
                  [t (fd t length)]
                  [t (rt t twist)])
                 ;; Occasional sucker/bump
                 (if (= (modulo i 4) 0)
                     (let* ([state (turtle-state t)]
                            [t (lt t 90)]
                            [t (setpw t 2)]
                            [t (fd t 5)]
                            [t (restore-turtle-state t state)])
                           (loop t (+ i 1) (+ angle twist)))
                     (loop t (+ i 1) (+ angle twist)))))))

;;; ============================================================
;;; Plant Structure Generators
;;; ============================================================

;;; random-plant-structure : Turtle × Palette → Turtle
;;; Generate a random plant using various patterns
(define (random-plant-structure t palette)
  (let ([plant-type (random-int 0 12)])
       (case plant-type
             [(0) ;; Fractal tree
              (let* ([t (pu t)]
                     [t (fd t -200)]
                     [t (pd t)])
                    (fractal-branch t 5 80 15 8 palette))]
             
             [(1) ;; Tendril cluster
              (let loop ([t t] [i 0])
                   (if (>= i 5)
                       t
                       (let* ([state (turtle-state t)]
                              [t (rt t (* i 72))]
                              [t (draw-tendril t 30 4 8 palette)]
                              [t (restore-turtle-state t state)])
                             (loop t (+ i 1)))))]
             
             [(2) ;; Bloom burst
              (draw-bloom t (random-int 8 16) 80 palette)]
             
             [(3) ;; Spiral plant (golden angle)
              (let loop ([t t] [i 0] [radius 5])
                   (if (>= i 40)
                       t
                       (let* ([t (setpc t (random-color palette))]
                              [t (setpw t 3)]
                              [t (fd t radius)]
                              [t (rt t 137.5)])
                             (loop t (+ i 1) (+ radius 2)))))]
             
             [(4) ;; Hybrid: stem + bloom + tendrils
              ;; Draw stem, bloom, and tendrils
              (let* ([t (setpc t (random-color palette))]
                     [t (setpw t 6)]
                     [t (pu t)]
                     [t (fd t -150)]
                     [t (pd t)]
                     [t (fd t 150)]
                     [t (draw-bloom t 12 60 palette)]
                     [state (turtle-state t)]
                     [t (pu t)]
                     [t (fd t -80)]
                     [t (pd t)]
                     [state2 (turtle-state t)]
                     [t (lt t 45)]
                     [t (draw-tendril t 15 3 5 palette)]
                     [t (restore-turtle-state t state2)]
                     [t (rt t 45)]
                     [t (draw-tendril t 15 3 5 palette)])
                    t)]
             
             [(5) ;; Vine with leaves
              (let* ([t (pu t)]
                     [t (fd t -180)]
                     [t (pd t)])
                    (draw-vine t 25 8 palette))]
             
             [(6) ;; Mushroom
              (let* ([t (pu t)]
                     [t (fd t -120)]
                     [t (pd t)])
                    (draw-mushroom t 80 50 palette))]
             
             [(7) ;; Crystal formation
              (draw-crystal t 3 40 palette)]
             
             [(8) ;; Coral branching
              (let* ([t (pu t)]
                     [t (fd t -150)]
                     [t (pd t)])
                    (draw-coral t 4 50 10 palette))]
             
             [(9) ;; Lightning plant
              (let* ([t (pu t)]
                     [t (fd t -100)]
                     [t (pd t)])
                    (draw-lightning t 15 12 palette))]
             
             [(10) ;; Bamboo
              (let* ([t (pu t)]
                     [t (fd t -200)]
                     [t (pd t)])
                    (draw-bamboo t 8 25 palette))]
             
             [(11) ;; Tentacles
              (let* ([t (pu t)]
                     [t (fd t -100)]
                     [t (pd t)])
                    (draw-tentacle t 30 6 palette))])))

;;; ============================================================
;;; Main Plant Generator
;;; ============================================================

(define *last-plant-turtle* #f)

;;; grow-cosmic-plant : → Turtle
;;; Generate a single cosmic plant with random structure and palette
(define (grow-cosmic-plant)
  (let* ([t (make-turtle)]
         [palette (random-palette)])
        
        (display "🌱 Growing cosmic plant...\n")
        (let ([t (random-plant-structure t palette)])
             (set! *last-plant-turtle* t)
             (display "✨ Plant grown! Use (save-svg (turtle->drawing t) \"file.svg\")\n")
             t)))

;;; cosmic-garden : Nat → Turtle
;;; Generate a garden with N plants arranged in a row
(define (cosmic-garden n)
  (display (format "🌌 Growing a garden with ~a plants...\n" n))
  (let* ([spacing 400]
         [start-x (- (* spacing (floor (/ n 2))))])
        (let loop ([t (make-turtle)] [i 0])
             (if (>= i n)
                 (begin
                  (display "✨ Garden complete! Use (save-svg (turtle->drawing t) \"file.svg\")\n")
                  t)
                 (let* ([palette (random-palette)]
                        [x (+ start-x (* i spacing))]
                        ;; Position and grow
                        [t (pu t)]
                        [t (setxy t x 240)]
                        [t (pd t)]
                        [t (random-plant-structure t palette)])
                       (loop t (+ i 1)))))))

;;; ============================================================
;;; Layered Garden with Perspective
;;; ============================================================

;;; scale-plant : Turtle × Real → Turtle
;;; Scale a plant by adjusting its drawing commands
;;; Note: This is a simplified approach - we draw smaller/larger versions
(define (draw-scaled-plant t palette scale y-offset)
  ;; Position plant
  (let* ([plant-type (random-int 0 12)]
         [t (pu t)]
         [base-y (+ 240 y-offset)]
         [t (setxy t (turtle-x t) base-y)]
         [t (pd t)])
        ;; Draw plant with scaled parameters
        (case plant-type
              [(0) (fractal-branch t (inexact->exact (floor (* 5 scale)))
                                   (* 80 scale) 15 (* 8 scale) palette)]
              [(1) (let loop ([t t] [i 0])
                        (if (>= i 5) t
                            (let* ([state (turtle-state t)]
                                   [t (rt t (* i 72))]
                                   [t (draw-tendril t 30 (* 4 scale) 8 palette)]
                                   [t (restore-turtle-state t state)])
                                  (loop t (+ i 1)))))]
              [(2) (draw-bloom t (random-int 8 16) (* 80 scale) palette)]
              [(3) (let loop ([t t] [i 0] [radius (* 5 scale)])
                        (if (>= i 40) t
                            (let* ([t (setpc t (random-color palette))]
                                   [t (setpw t (max 1 (inexact->exact (floor (* 3 scale)))))]
                                   [t (fd t radius)]
                                   [t (rt t 137.5)])
                                  (loop t (+ i 1) (+ radius (* 2 scale))))))]
              [(4) (let* ([t (setpc t (random-color palette))]
                          [t (setpw t (max 1 (inexact->exact (floor (* 6 scale)))))]
                          [t (fd t (* 150 scale))]
                          [t (draw-bloom t 12 (* 60 scale) palette)])
                         t)]
              [(5) (draw-vine t 25 (* 8 scale) palette)]
              [(6) (draw-mushroom t (* 80 scale) (* 50 scale) palette)]
              [(7) (draw-crystal t 3 (* 40 scale) palette)]
              [(8) (draw-coral t 4 (* 50 scale) (* 10 scale) palette)]
              [(9) (draw-lightning t 15 (* 12 scale) palette)]
              [(10) (draw-bamboo t 8 (* 25 scale) palette)]
              [(11) (draw-tentacle t 30 (* 6 scale) palette)]
              [else t])))

;;; layered-garden : Nat → Turtle
;;; Create a garden with multiple depth layers for perspective
(define (layered-garden plants-per-layer)
  (display "🎨 Creating layered garden with perspective...\n")
  (let* ([layers 3]
         [layer-scales '(0.5 0.75 1.0)]     ; Background to foreground
         [layer-y-offsets '(-80 -40 0)]     ; Y position offsets
         [total-width 640]
         [spacing (/ total-width (+ plants-per-layer 1))])
        (let layer-loop ([t (make-turtle)] [layer 0])
             (if (>= layer layers)
                 (begin
                  (display "✨ Layered garden complete!\n")
                  t)
                 (let* ([scale (list-ref layer-scales layer)]
                        [y-offset (list-ref layer-y-offsets layer)])
                       (display (format "  Layer ~a (scale ~a)...\n" (+ layer 1) scale))
                       (let plant-loop ([t t] [i 0])
                            (if (>= i plants-per-layer)
                                (layer-loop t (+ layer 1))
                                (let* ([palette (random-palette)]
                                       [x (* (+ i 1) spacing)]
                                       [t (pu t)]
                                       [t (setxy t x 240)]
                                       [t (pd t)]
                                       [t (draw-scaled-plant t palette scale y-offset)])
                                      (plant-loop t (+ i 1))))))))))

;;; ============================================================
;;; Interactive Commands
;;; ============================================================

(display "\n╔══════════════════════════════════════════════════════════╗\n")
(display "║              🌌 COSMIC GARDEN LOADED 🌌                 ║\n")
(display "╚══════════════════════════════════════════════════════════╝\n\n")
(display "  Commands:\n")
(display "    (define p (grow-cosmic-plant))    Generate a single plant\n")
(display "    (define g (cosmic-garden 5))      Generate 5 plants in a row\n")
(display "    (define L (layered-garden 4))     Generate layered garden (PERSPECTIVE!)\n")
(display "    (save-svg (turtle->drawing p) \"plant.svg\")  Export to SVG\n")
(display "\n  12 plant types: trees, tendrils, blooms, spirals, hybrids,\n")
(display "                  vines, mushrooms, crystals, coral, lightning,\n")
(display "                  bamboo, tentacles\n\n")
