#!/usr/bin/env scheme --script
;;; demo-layers.ss — Layering & Depth System Demo
;;;
;;; Demonstrates the canvas layering system with DUCKIE.
;;; Shows how to create complex scenes using multiple layers:
;;;   - Background layer (environment/pond)
;;;   - Sprite layer (DUCKIE)
;;;   - UI layer (information overlay)
;;;
;;; Run with: scheme --script demo-layers.ss

;;; ============================================================
;;; Load Dependencies
;;; ============================================================

(load "shell/layout.ss")
(load "shell/layers.ss")

;;; ============================================================
;;; Demo 1: Basic Layering Concept
;;; ============================================================

(define (demo-basic-layering)
  (display "\n")
  (display "═══════════════════════════════════════════════════════════\n")
  (display " Demo 1: Basic Layering — Three Layers\n")
  (display "═══════════════════════════════════════════════════════════\n\n")

  ;; Create three layers at different depths
  (let* ([width 50]
         [height 12]

         ;; Layer 0: Background (filled with dots)
         [bg-layer (make-background-layer width height)]
         [bg-layer (layer-fill-rect bg-layer
                                    (make-rect (point 0 0) width height)
                                    #\.)]

         ;; Layer 50: Middle layer (a box)
         [mid-layer (make-sprite-layer 'box 20 6 (point 15 3))]
         [mid-layer (layer-draw-box mid-layer
                                    (make-rect (point 0 0) 20 6)
                                    'heavy)]
         [mid-layer (layer-draw-string mid-layer
                                       (point 4 2)
                                       "MIDDLE")]

         ;; Layer 100: Foreground (text overlay)
         [fg-layer (make-ui-layer width height)]
         [fg-layer (layer-draw-box fg-layer
                                   (make-rect (point 0 0) width height)
                                   'light)]
         [fg-layer (layer-draw-string fg-layer
                                      (point 2 0)
                                      "[ Layering Demo ]")]

         ;; Create stack and add layers
         [stack (make-layer-stack)]
         [stack (stack-add-layer stack bg-layer)]
         [stack (stack-add-layer stack mid-layer)]
         [stack (stack-add-layer stack fg-layer)]

         ;; Flatten and render
         [final-canvas (flatten-layers stack width height)])

    (display "Stack composition:\n")
    (display (stack->string stack))
    (display "\n")
    (display "Rendered output:\n")
    (display (canvas->string final-canvas))
    (display "\n")))

;;; ============================================================
;;; Demo 2: DUCKIE with Layers
;;; ============================================================

(define duckie-sprite-happy
  '("   \\  /   "
    "   (o>   "
    "  _(()_  "
    "  | () | "
    "   \\^^/  "))

(define duckie-sprite-curious
  '("    __   "
    "   (O>  "
    "  _(()_  "
    "  | () | "
    "   \\  /  "
    "    \\/   "))

(define (draw-pond-background layer)
  ;; Draw a simple pond scene
  (let* ([w (canvas-width (layer-canvas layer))]
         [h (canvas-height (layer-canvas layer))]

         ;; Sky (top half)
         [layer (layer-fill-rect layer
                                (make-rect (point 0 0) w (quotient h 2))
                                #\space)]

         ;; Add some clouds
         [layer (layer-draw-string layer (point 5 1) "  ~~~  ")]
         [layer (layer-draw-string layer (point 35 2) " ~~~~  ")]

         ;; Water line
         [water-y (quotient h 2)]
         [layer (layer-draw-string layer
                                   (point 0 water-y)
                                   (make-string w #\~))]

         ;; Pond/water (bottom half)
         [layer (layer-fill-rect layer
                                (make-rect (point 0 (+ water-y 1))
                                          w
                                          (- h water-y 1))
                                #\space)]

         ;; Water ripples
         [layer (layer-draw-string layer (point 10 (+ water-y 1)) "~ ~")]
         [layer (layer-draw-string layer (point 30 (+ water-y 2)) "~  ~")]
         [layer (layer-draw-string layer (point 5 (+ water-y 3)) " ~  ~")]

         ;; Some reeds/plants
         [layer (layer-draw-string layer (point 2 (- water-y 1)) "|||")]
         [layer (layer-draw-string layer (point 2 water-y) "|||")]
         [layer (layer-draw-string layer (point 45 (- water-y 1)) "||")]
         [layer (layer-draw-string layer (point 45 water-y) "||")])
    layer))

(define (draw-ui-overlay layer name mood energy)
  (let* ([w (canvas-width (layer-canvas layer))]
         [h (canvas-height (layer-canvas layer))]

         ;; Draw border
         [layer (layer-draw-box layer
                               (make-rect (point 0 0) w h)
                               'light)]

         ;; Title bar
         [layer (layer-draw-string layer
                                   (point 2 0)
                                   (string-append "[ " name " - "
                                                 (symbol->string mood) " ]"))]

         ;; Status bar at bottom
         [status-text (string-append "Energy: "
                                    (make-string (quotient energy 10) #\■)
                                    (make-string (- 10 (quotient energy 10)) #\□)
                                    " " (number->string energy) "%")]
         [layer (layer-draw-string layer
                                   (point 2 (- h 1))
                                   status-text)])
    layer))

(define (demo-duckie-layered name mood sprite)
  (let* ([width 50]
         [height 16]

         ;; Layer 0: Background (pond environment)
         [bg-layer (make-background-layer width height)]
         [bg-layer (draw-pond-background bg-layer)]

         ;; Layer 50: DUCKIE sprite
         [duckie-x 18]
         [duckie-y 5]
         [sprite-layer (make-sprite-layer 'duckie 12 8 (point duckie-x duckie-y))]

         ;; Draw sprite lines onto the sprite layer
         [sprite-layer (let loop ([lines sprite]
                                 [y 0]
                                 [layer sprite-layer])
                        (if (null? lines)
                            layer
                            (loop (cdr lines)
                                  (+ y 1)
                                  (layer-draw-string layer
                                                    (point 0 y)
                                                    (car lines)))))]

         ;; Layer 100: UI overlay
         [ui-layer (make-ui-layer width height)]
         [ui-layer (draw-ui-overlay ui-layer name mood 85)]

         ;; Create stack
         [stack (make-layer-stack)]
         [stack (stack-add-layer stack bg-layer)]
         [stack (stack-add-layer stack sprite-layer)]
         [stack (stack-add-layer stack ui-layer)]

         ;; Flatten
         [final-canvas (flatten-layers stack width height)])

    (display (canvas->string final-canvas))
    (display "\n")))

(define (demo-duckie-scenes)
  (display "\n")
  (display "═══════════════════════════════════════════════════════════\n")
  (display " Demo 2: DUCKIE in a Layered Scene\n")
  (display "═══════════════════════════════════════════════════════════\n\n")

  (display "Scene 1: Proto is HAPPY by the pond\n\n")
  (demo-duckie-layered "Proto" 'happy duckie-sprite-happy)

  (display "\nScene 2: Proto is CURIOUS about something\n\n")
  (demo-duckie-layered "Proto" 'curious duckie-sprite-curious))

;;; ============================================================
;;; Demo 3: Layer Visibility and Reordering
;;; ============================================================

(define (demo-layer-operations)
  (display "\n")
  (display "═══════════════════════════════════════════════════════════\n")
  (display " Demo 3: Layer Operations — Visibility & Reordering\n")
  (display "═══════════════════════════════════════════════════════════\n\n")

  (let* ([width 40]
         [height 10]

         ;; Create three colored boxes at different positions
         [layer-a (make-layer 'box-a
                             (make-transparent-canvas 15 5)
                             10
                             (point 5 2))]
         [layer-a (layer-fill-rect layer-a
                                  (make-rect (point 0 0) 15 5)
                                  #\A)]
         [layer-a (layer-draw-string layer-a (point 5 2) "A")]

         [layer-b (make-layer 'box-b
                             (make-transparent-canvas 15 5)
                             20
                             (point 12 3))]
         [layer-b (layer-fill-rect layer-b
                                  (make-rect (point 0 0) 15 5)
                                  #\B)]
         [layer-b (layer-draw-string layer-b (point 5 2) "B")]

         [layer-c (make-layer 'box-c
                             (make-transparent-canvas 15 5)
                             30
                             (point 19 4))]
         [layer-c (layer-fill-rect layer-c
                                  (make-rect (point 0 0) 15 5)
                                  #\C)]
         [layer-c (layer-draw-string layer-c (point 5 2) "C")]

         ;; Create stack
         [stack (make-layer-stack)]
         [stack (stack-add-layer stack layer-a)]
         [stack (stack-add-layer stack layer-b)]
         [stack (stack-add-layer stack layer-c)])

    ;; Show original order
    (display "Original order (A=10, B=20, C=30):\n")
    (display (canvas->string (flatten-layers stack width height)))
    (display "\n\n")

    ;; Hide middle layer
    (let ([stack2 (stack-hide-layer stack 'box-b)])
      (display "With layer B hidden:\n")
      (display (canvas->string (flatten-layers stack2 width height)))
      (display "\n\n"))

    ;; Reorder: move A to front
    (let ([stack3 (stack-reorder stack 'box-a 40)])
      (display "With layer A moved to front (depth=40):\n")
      (display (canvas->string (flatten-layers stack3 width height)))
      (display "\n\n"))))

;;; ============================================================
;;; Demo 4: Animated Layering
;;; ============================================================

(define (demo-animation)
  (display "\n")
  (display "═══════════════════════════════════════════════════════════\n")
  (display " Demo 4: Animation with Layers — DUCKIE Swimming\n")
  (display "═══════════════════════════════════════════════════════════\n\n")

  (display "Showing DUCKIE at different positions (simulated animation):\n\n")

  ;; Show DUCKIE at 3 different positions
  (let loop ([positions '((5 . 5) (15 . 6) (25 . 5))]
             [frame 1])
    (when (pair? positions)
      (let* ([pos (car positions)]
             [x (car pos)]
             [y (cdr pos)]
             [width 50]
             [height 14]

             ;; Background
             [bg-layer (make-background-layer width height)]
             [bg-layer (draw-pond-background bg-layer)]

             ;; DUCKIE
             [sprite-layer (make-sprite-layer 'duckie 12 8 (point x y))]
             [sprite-layer (let loop-s ([lines duckie-sprite-happy]
                                       [sy 0]
                                       [layer sprite-layer])
                            (if (null? lines)
                                layer
                                (loop-s (cdr lines)
                                       (+ sy 1)
                                       (layer-draw-string layer
                                                         (point 0 sy)
                                                         (car lines)))))]

             ;; UI
             [ui-layer (make-ui-layer width height)]
             [ui-layer (layer-draw-box ui-layer
                                      (make-rect (point 0 0) width height)
                                      'light)]
             [ui-layer (layer-draw-string ui-layer
                                         (point 2 0)
                                         (string-append "[ Frame "
                                                       (number->string frame)
                                                       " ]"))]

             ;; Stack
             [stack (make-layer-stack)]
             [stack (stack-add-layer stack bg-layer)]
             [stack (stack-add-layer stack sprite-layer)]
             [stack (stack-add-layer stack ui-layer)]

             [final-canvas (flatten-layers stack width height)])

        (display (string-append "Frame " (number->string frame) ":\n"))
        (display (canvas->string final-canvas))
        (display "\n")
        (loop (cdr positions) (+ frame 1))))))

;;; ============================================================
;;; Main Entry Point
;;; ============================================================

(define (main)
  (display "\n")
  (display "╔═══════════════════════════════════════════════════════════╗\n")
  (display "║                                                           ║\n")
  (display "║        The Fold — Layering & Depth System Demo           ║\n")
  (display "║                                                           ║\n")
  (display "║  Demonstrating transparent layers, z-ordering,           ║\n")
  (display "║  and composition for rich ASCII/ANSI graphics.           ║\n")
  (display "║                                                           ║\n")
  (display "╚═══════════════════════════════════════════════════════════╝\n")

  ;; Run all demos
  (demo-basic-layering)
  (demo-duckie-scenes)
  (demo-layer-operations)
  (demo-animation)

  (display "\n")
  (display "═══════════════════════════════════════════════════════════\n")
  (display " End of Demo\n")
  (display "═══════════════════════════════════════════════════════════\n\n")

  (display "Summary:\n")
  (display "  • Transparent cells (#\\nul) allow sprites to overlay cleanly\n")
  (display "  • Layers have depth (z-order) for back-to-front composition\n")
  (display "  • Layers can be shown/hidden and reordered dynamically\n")
  (display "  • flatten-layers composites all visible layers into one canvas\n")
  (display "\n")
  (display "See shell/layers.ss for the full API.\n\n"))

;;; Run the demo
(main)
