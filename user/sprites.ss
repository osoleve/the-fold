;;; playpen/sprites.ss — DUCKIE's Visual Forms
;;;
;;; ASCII art sprites for each DUCKIE mood and animation state.
;;; Each sprite is a list of strings (one per row).
;;; All sprites are 8 wide × 6 tall for consistent rendering.
;;;
;;; This is Playpen code: creative, experimental, the body of the duck.

;;; ====
;;; Mood Sprites — One Per Emotional State
;;; ====

;;; DUCKIE-HAPPY: Bouncing, bright, joyful
;;; Wings up in excitement, beak open in smile
(define duckie-happy
  '("  \\  /  "
    "  (o>  "
    " _(()_ "
    " | () |"
    "  \\^^/ "
    "        "))

;;; DUCKIE-CURIOUS: Head tilted, inquisitive
;;; One eye slightly larger, leaning forward with interest
(define duckie-curious
  '("   __  "
    "  (O> "
    " _(()_ "
    " | () |"
    "  \\  / "
    "   \\/  "))

;;; DUCKIE-SLEEPY: Eyes droopy, ready to rest
;;; Head drooping down, Zzz floating nearby
(define duckie-sleepy
  '("   __  "
    "  (-< "
    " _(()_ "
    " | () |"
    "  \\  / "
    "   Zz  "))

;;; DUCKIE-CONTENT: Peaceful, settled, at ease
;;; Slight smile, relaxed posture, everything is good
(define duckie-content
  '("   __  "
    "  (o> "
    " _(()_ "
    " | () |"
    "  \\  / "
    "   \\/  "))

;;; DUCKIE-LONELY: Looking around, smaller posture
;;; Head turned to the side, searching for company
(define duckie-lonely
  '("        "
    "   __  "
    "  <o) "
    "  _((__"
    "  | () "
    "   \\/  "))

;;; DUCKIE-PLAYFUL: Energetic, wings spread
;;; Full of energy, ready to play and interact
(define duckie-playful
  '("  \\ __ /"
    "   (o> "
    "  _(()_"
    "  | () |"
    "  / ^^ \\"
    "        "))

;;; ====
;;; Animation Frames — Bring DUCKIE to Life
;;; ====

;;; DUCKIE-IDLE: Subtle breathing animation (2 frames)
;;; Frame 1: Normal position
(define duckie-idle-1
  '("   __  "
    "  (o> "
    " _(()_ "
    " | () |"
    "  \\  / "
    "   \\/  "))

;;; Frame 2: Slight rise (breathing in)
(define duckie-idle-2
  '("   __  "
    "  (o> "
    " _(()_ "
    " | () |"
    "  \\  / "
    "   \\/  "))

;;; DUCKIE-WADDLE: Walking animation (4 frames)
;;; Frame 1: Left foot forward
(define duckie-waddle-1
  '("   __  "
    "  (o> "
    " _(()_ "
    " | () |"
    " /\\  / "
    "      \\ "))

;;; Frame 2: Center position
(define duckie-waddle-2
  '("   __  "
    "  (o> "
    " _(()_ "
    " | () |"
    "  \\  / "
    "   \\/  "))

;;; Frame 3: Right foot forward
(define duckie-waddle-3
  '("   __  "
    "  (o> "
    " _(()_ "
    " | () |"
    " \\  /\\ "
    "/       "))

;;; Frame 4: Center position
(define duckie-waddle-4
  '("   __  "
    "  (o> "
    " _(()_ "
    " | () |"
    "  \\  / "
    "   \\/  "))

;;; DUCKIE-WAVE: Greeting animation (3 frames)
;;; Frame 1: Wing starting to rise
(define duckie-wave-1
  '("   __  "
    "  (o> "
    " _(()_\\"
    " | () |"
    "  \\  / "
    "   \\/  "))

;;; Frame 2: Wing fully raised
(define duckie-wave-2
  '("   __ /"
    "  (o> "
    " _(()_ "
    " | () |"
    "  \\  / "
    "   \\/  "))

;;; Frame 3: Wing lowering
(define duckie-wave-3
  '("   __  "
    "  (o> "
    " _(()_\\"
    " | () |"
    "  \\  / "
    "   \\/  "))

;;; DUCKIE-BLINK: Eye closing animation (2 frames)
;;; Frame 1: Eyes open (normal)
(define duckie-blink-1
  '("   __  "
    "  (o> "
    " _(()_ "
    " | () |"
    "  \\  / "
    "   \\/  "))

;;; Frame 2: Eyes closed
(define duckie-blink-2
  '("   __  "
    "  (-> "
    " _(()_ "
    " | () |"
    "  \\  / "
    "   \\/  "))

;;; ====
;;; Bonus Sprites — Extra Expressions
;;; ====

;;; DUCKIE-SURPRISED: Startled reaction
(define duckie-surprised
  '("    !  "
    "  (O> "
    " _(()_ "
    " | () |"
    "  \\  / "
    "   \\/  "))

;;; DUCKIE-EATING: Nibbling on something
(define duckie-eating
  '("   __  "
    "  (o>~"
    " _(()_ "
    " | () |"
    "  \\  / "
    "   \\/  "))

;;; ====
;;; Sprite Registry — Map Moods to Visual Forms
;;; ====

;;; All mood sprites in one association list
(define duckie-sprites
  `((happy     . ,duckie-happy)
    (curious   . ,duckie-curious)
    (sleepy    . ,duckie-sleepy)
    (content   . ,duckie-content)
    (lonely    . ,duckie-lonely)
    (playful   . ,duckie-playful)
    (surprised . ,duckie-surprised)
    (eating    . ,duckie-eating)))

;;; Animation frame sequences
(define duckie-animations
  `((idle   . (,duckie-idle-1 ,duckie-idle-2))
    (waddle . (,duckie-waddle-1 ,duckie-waddle-2
               ,duckie-waddle-3 ,duckie-waddle-4))
    (wave   . (,duckie-wave-1 ,duckie-wave-2 ,duckie-wave-3))
    (blink  . (,duckie-blink-1 ,duckie-blink-2))))

;;; ====
;;; Helper Functions — Working with Sprites
;;; ====

;;; sprite-width : Sprite → Nat
;;; Get the width of a sprite (assumes all rows are same length)
(define (sprite-width sprite)
  (if (null? sprite)
      0
      (string-length (car sprite))))

;;; sprite-height : Sprite → Nat
;;; Get the height of a sprite (number of rows)
(define (sprite-height sprite)
  (length sprite))

;;; get-sprite : Symbol → Sprite | #f
;;; Retrieve a mood sprite by name
(define (get-sprite mood)
  (let ([entry (assq mood duckie-sprites)])
       (if entry
           (cdr entry)
           #f)))

;;; get-animation : Symbol → (List Sprite) | #f
;;; Retrieve animation frames by name
(define (get-animation anim-name)
  (let ([entry (assq anim-name duckie-animations)])
       (if entry
           (cdr entry)
           #f)))

;;; get-animation-frame : Symbol × Nat → Sprite | #f
;;; Get a specific frame from an animation sequence
(define (get-animation-frame anim-name frame-index)
  (let ([frames (get-animation anim-name)])
       (if (and frames (< frame-index (length frames)))
           (list-ref frames frame-index)
           #f)))

;;; ====
;;; Canvas Integration — Drawing Sprites
;;; ====

;;; draw-sprite : Canvas × Point × Sprite → Canvas
;;; Draw a sprite onto the canvas at the given position.
;;; Each string in the sprite is drawn as a horizontal line.
;;;
;;; This integrates with boundary/layout.ss primitives:
;;; - point: (cons x y)
;;; - draw-string: Canvas × Point × String → Canvas
(define (draw-sprite canvas position sprite)
  (let ([x (car position)]
        [y (cdr position)])
       (let loop ([rows sprite]
                  [current-y y]
                  [current-canvas canvas])
            (if (null? rows)
                current-canvas
                (loop (cdr rows)
                      (+ current-y 1)
                      (draw-string current-canvas
                                   (cons x current-y)
                                   (car rows)))))))

;;; draw-sprite-mood : Canvas × Point × Symbol → Canvas
;;; Draw DUCKIE in a specific mood at the given position
(define (draw-sprite-mood canvas position mood)
  (let ([sprite (get-sprite mood)])
       (if sprite
           (draw-sprite canvas position sprite)
           canvas)))  ; If mood not found, return canvas unchanged

;;; ====
;;; Sprite Information
;;; ====

;;; list-moods : () → (List Symbol)
;;; Get all available mood names
(define (list-moods)
  (map car duckie-sprites))

;;; list-animations : () → (List Symbol)
;;; Get all available animation names
(define (list-animations)
  (map car duckie-animations))

;;; animation-frame-count : Symbol → Nat
;;; Get the number of frames in an animation
(define (animation-frame-count anim-name)
  (let ([frames (get-animation anim-name)])
       (if frames
           (length frames)
           0)))

;;; ====
;;; Notes for Future Builders
;;; ====

;;; The sprites are designed at 8×6 to be compact yet expressive.
;;; Each mood should be visually distinct at a glance.
;;; Animation frames create smooth transitions.
;;;
;;; To extend:
;;; - Add new mood sprites to duckie-sprites
;;; - Add new animation sequences to duckie-animations
;;; - Consider variable-size sprites if needed
;;; - Add color/ANSI codes for richer rendering
;;;
;;; The sprite format is simple: a list of equal-length strings.
;;; This makes it easy to create and compose.
;;;
;;; Integration points:
;;; - playpen/duckie.ss — provides mood types
;;; - boundary/layout.ss — provides canvas and drawing primitives
;;; - Future: main loop will cycle through animation frames
;;;
;;; — Sonnet, Builder
;;;    Christmas Day, giving the duck a body
