;; Load physics renderer first (it loads ascii-video.ss which has conflicting names)
(load "lattice/physics/classical/ascii-renderer.ss")
;; Then load our ansi module (overwrites the conflict with correct definitions)
(load "boundary/ui/ansi.ss")

(doc 'module 'physics-colors)
(doc 'description "Colored Physics ASCII Rendering — Adds ANSI color to the physics ASCII renderer output. Works by post-processing rendered frames with color mappings.")
(doc 'layer 'boundary)
(doc 'purity 'partial)
(doc 'note "Usage: (load \"boundary/ui/physics-colors.ss\") (define world (make-world ...)) (physics-display-colored world)")
(doc 'note "Dependencies: boundary/ui/ansi.ss, lattice/physics/classical/ascii-renderer.ss")

(doc *physics-ansi-reset* 'type String)
(doc *physics-ansi-reset* 'description "Local ANSI reset string (to avoid conflicts with ascii-video.ss)")
(define *physics-ansi-reset* "\x1B;[0m")

(doc 'section 'color-palette)

(doc physics-static-color 'type Color)
(doc physics-static-color 'description "Static bodies color")
(define physics-static-color color-gray)

(doc physics-dynamic-color 'type Color)
(doc physics-dynamic-color 'description "Dynamic bodies color")
(define physics-dynamic-color color-cyan)

(doc physics-velocity-color 'type Color)
(doc physics-velocity-color 'description "Velocity vectors color")
(define physics-velocity-color color-green)

(doc physics-contact-color 'type Color)
(doc physics-contact-color 'description "Contact points color")
(define physics-contact-color color-red)

(doc physics-constraint-color 'type Color)
(doc physics-constraint-color 'description "Constraints color")
(define physics-constraint-color color-yellow)

(doc physics-aabb-color 'type Color)
(doc physics-aabb-color 'description "Bounding boxes color")
(define physics-aabb-color color-magenta)

(doc char->physics-color 'type (-> Char (Maybe Color)))
(doc char->physics-color 'description "Character → Color mapping for rendered output")
(define (char->physics-color char)
  (case char
    [(#\#) physics-static-color]      ; Static bodies
    [(#\O #\o #\@) physics-dynamic-color]   ; Dynamic bodies
    [(#\> #\< #\^ #\v #\\ #\/ #\.) physics-velocity-color] ; Velocity
    [(#\* #\:) physics-contact-color]   ; Contacts
    [(#\+ #\- #\~ #\=) physics-constraint-color] ; Constraints
    [(#\|) color-dark-gray]             ; AABB borders
    [else #f]))  ; No color (use default)

(doc 'section 'colored-frame-rendering)

(doc colorize-char 'type (-> Char String))
(doc colorize-char 'description "Apply ANSI color to a character based on its type.")
(define (colorize-char char)
  (let ([color (char->physics-color char)])
    (if color
        (string-append (style->ansi (style-fg color))
                       (string char)
                       *physics-ansi-reset*)
        (string char))))

(doc colorize-row 'type (-> String String))
(doc colorize-row 'description "Apply colors to each character in a row.")
(define (colorize-row row)
  (let ([len (string-length row)])
    (let loop ([i 0] [acc '()])
      (if (>= i len)
          (apply string-append (reverse acc))
          (loop (+ i 1)
                (cons (colorize-char (string-ref row i)) acc))))))

(doc frame-render-colored 'type (-> Frame String))
(doc frame-render-colored 'description "Render frame to string with ANSI colors.")
(define (frame-render-colored frame)
  (let ([h (frame-height frame)])
    (let loop ([y 0] [acc '()])
      (if (>= y h)
          (apply string-append (reverse acc))
          (loop (+ y 1)
                (cons (string-append (colorize-row (vector-ref frame y)) "\n")
                      acc))))))

(doc 'section 'high-level-api)

(doc physics-render-colored 'type (-> World Int Int String))
(doc physics-render-colored 'description "Render physics world to colored ASCII string.")
(define (physics-render-colored world width height)
  (let* ([config (make-render-config width height 3.0 (/ width 2) (/ height 4))]
         [renderer (make-world-renderer config (full-debug-options) (default-render-style))]
         [frame (make-frame width height #\space)])
    (render-world! frame world renderer #f)
    (frame-render-colored frame)))

(doc physics-display-colored 'type (-> World Void))
(doc physics-display-colored 'description "Display physics world with colors (default 80x30).")
(define (physics-display-colored world)
  (physics-display-colored-size world 80 30))

(doc physics-display-colored-size 'type (-> World Int Int Void))
(doc physics-display-colored-size 'description "Display physics world with colors at specified size.")
(define (physics-display-colored-size world width height)
  (display (physics-render-colored world width height)))

(doc physics-display-colored-debug 'type (-> World DebugOptions Void))
(doc physics-display-colored-debug 'description "Display with specific debug options.")
(define (physics-display-colored-debug world debug-opts)
  (let* ([width 80]
         [height 30]
         [config (make-render-config width height 3.0 (/ width 2) (/ height 4))]
         [renderer (make-world-renderer config debug-opts (default-render-style))]
         [frame (make-frame width height #\space)])
    (render-world! frame world renderer #f)
    (display (frame-render-colored frame))))

(doc 'section 'styled-render-configuration)

(doc make-colored-render-style 'type (-> RenderStyle))
(doc make-colored-render-style 'description "Style optimized for colored display (uses simpler characters).")
(define (make-colored-render-style)
  ;; With colors, we can use simpler characters since color conveys type
  (make-render-style #\o #\# #\- #\| #\+ #\> #\*))

(doc 'section 'legend-display)

(doc display-physics-legend 'type (-> Void))
(doc display-physics-legend 'description "Show color legend for physics rendering.")
(define (display-physics-legend)
  (display (colored-text (make-style color-cyan #f #t #f #f #f) "Physics Color Legend:"))
  (newline)
  (display "  ")
  (display (colored-text (style-fg physics-static-color) "# "))
  (display "Static bodies")
  (newline)
  (display "  ")
  (display (colored-text (style-fg physics-dynamic-color) "O "))
  (display "Dynamic bodies")
  (newline)
  (display "  ")
  (display (colored-text (style-fg physics-velocity-color) "> "))
  (display "Velocity vectors")
  (newline)
  (display "  ")
  (display (colored-text (style-fg physics-contact-color) "* "))
  (display "Contact points")
  (newline)
  (display "  ")
  (display (colored-text (style-fg physics-constraint-color) "+ "))
  (display "Constraints")
  (newline))

(doc 'section 'module-load-message)

(display (colored-text (style-fg color-cyan) "physics-colors.ss loaded."))
(newline)
(display "  (physics-display-colored world)   - Display world with colors")
(newline)
(display "  (physics-render-colored world w h) - Render to colored string")
(newline)
(display "  (display-physics-legend)          - Show color key")
(newline)
