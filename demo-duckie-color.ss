;;; demo-duckie-color.ss — See DUCKIE in Living Color
;;; Run with: scheme --script demo-duckie-color.ss
;;;
;;; DUCKIE sprites rendered with mood-based colors!

;;; Load color-enabled layout
(load "shell/layout-color.ss")

;;; ============================================================
;;; DUCKIE Sprites (ASCII Art)
;;; ============================================================

(define duckie-happy
  '("  \\  /  "
    "  (o>  "
    " _(()_ "
    " | () |"
    "  \\^^/ "
    "        "))

(define duckie-curious
  '("   __  "
    "  (O> "
    " _(()_ "
    " | () |"
    "  \\  / "
    "   \\/  "))

(define duckie-content
  '("   __  "
    "  (o> "
    " _(()_ "
    " | () |"
    "  \\  / "
    "   \\/  "))

(define duckie-playful
  '(" \\ __ /"
    "  (o> "
    " _(()_ "
    " | () |"
    " / ^^ \\"
    "        "))

(define duckie-lonely
  '("        "
    "   __  "
    "  <o) "
    "  _((__"
    "  | () "
    "   \\/  "))

(define duckie-sleepy
  '("   __  "
    "  (-< "
    " _(()_ "
    " | () |"
    "  \\  / "
    "   Zz  "))

;;; ============================================================
;;; Sprite Rendering with Color
;;; ============================================================

;;; draw-sprite-colored : Canvas × Point × List[String] × Color → Canvas
;;; Draw a sprite with foreground color.
(define (draw-sprite-colored c pt sprite fg)
  (let loop ([lines sprite] [row 0] [canvas c])
    (if (null? lines)
        canvas
        (let ([line (car lines)]
              [y (+ (point-y pt) row)])
          (loop (cdr lines)
                (+ row 1)
                (draw-string-colored canvas (point (point-x pt) y)
                                    line fg color-default))))))

;;; ============================================================
;;; Render DUCKIE with Mood Colors
;;; ============================================================

;;; render-duckie-colored : String × Symbol × List[String] × Nat → ()
;;; Render DUCKIE with mood-based coloring.
(define (render-duckie-colored name mood sprite energy)
  (let* ([c (make-canvas 50 16)]
         [mood-color (mood->color mood)]
         [energy-color (energy->color energy)]

         ;; Draw outer box with mood color
         [c (draw-box-colored c (make-rect (point 0 0) 50 16)
                             'double mood-color color-default)]

         ;; Title with name
         [c (draw-string-colored c (point 2 0)
                                (string-append "[ " name " ]")
                                color-bright-white color-default)]

         ;; Mood indicator
         [c (draw-string-colored c (point 30 0)
                                (string-append "[ " (symbol->string mood) " ]")
                                mood-color color-default)]

         ;; Draw DUCKIE sprite in mood color
         [c (draw-sprite-colored c (point 18 4) sprite mood-color)]

         ;; Energy bar
         [c (draw-string-colored c (point 2 11) "Energy:"
                                color-default color-default)]
         [c (draw-box c (make-rect (point 11 11) 32 1) 'light)]
         [bar-len (quotient (* energy 30) 100)]
         [c (let loop ([i 0] [canvas c])
              (if (>= i bar-len)
                  canvas
                  (let ([x (+ 12 i)])
                    (loop (+ i 1)
                          (draw-char-colored canvas (point x 11) #\█
                                           energy-color color-default)))))]
         [c (draw-string-colored c (point 44 11)
                                (string-append (number->string energy) "%")
                                energy-color color-default)]

         ;; Water/ground with cyan color
         [c (draw-string-colored c (point 2 13)
                                "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
                                color-cyan color-default)]
         [c (draw-string-colored c (point 2 14)
                                "  ~ ripples ~    ~ ~ ~       "
                                (darken color-cyan 0.7) color-default)])

    (display (canvas->string c))))

;;; ============================================================
;;; Mood Selector
;;; ============================================================

;;; Map moods to sprites
(define (mood->sprite mood)
  (case mood
    [(happy) duckie-happy]
    [(curious) duckie-curious]
    [(sleepy) duckie-sleepy]
    [(content) duckie-content]
    [(lonely) duckie-lonely]
    [(playful) duckie-playful]
    [else duckie-curious]))

;;; ============================================================
;;; Show All Moods
;;; ============================================================

(display "\n")
(display "╔═══════════════════════════════════════════════════╗\n")
(display "║                                                   ║\n")
(display "║        DUCKIE IN LIVING COLOR                    ║\n")
(display "║        Each mood has its own hue                 ║\n")
(display "║                                                   ║\n")
(display "╚═══════════════════════════════════════════════════╝\n\n")

(display "Proto is HAPPY (Gold/Yellow — Bright & Cheerful):\n")
(render-duckie-colored "Proto" 'happy (mood->sprite 'happy) 100)
(display "\n\n")

(display "Proto is CURIOUS (Light Blue — Inquisitive):\n")
(render-duckie-colored "Proto" 'curious (mood->sprite 'curious) 85)
(display "\n\n")

(display "Proto is PLAYFUL (Pink — Energetic & Fun):\n")
(render-duckie-colored "Proto" 'playful (mood->sprite 'playful) 95)
(display "\n\n")

(display "Proto is CONTENT (Soft Green — Peaceful):\n")
(render-duckie-colored "Proto" 'content (mood->sprite 'content) 70)
(display "\n\n")

(display "Proto is SLEEPY (Soft Purple — Drowsy):\n")
(render-duckie-colored "Proto" 'sleepy (mood->sprite 'sleepy) 40)
(display "\n\n")

(display "Proto is LONELY (Muted Blue — Melancholy):\n")
(render-duckie-colored "Proto" 'lonely (mood->sprite 'lonely) 55)
(display "\n\n")

(display "═══════════════════════════════════════════\n")
(display "     The duck exists. Now in color.\n")
(display "     Each emotion painted in its hue.\n")
(display "═══════════════════════════════════════════\n\n")
